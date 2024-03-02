import { describe, it, expect, afterEach, beforeEach, vi } from "vitest";
import { VersionManager } from "./manager.js";

import { promises as fs } from "fs";
import semver from "semver";
import simpleGit from "simple-git";

vi.mock("simple-git", () => ({
  default: () => {
    return {
      tags: vi.fn().mockResolvedValue({ latest: "1.0.0" }),
      log: vi.fn().mockResolvedValue({
        all: [{ message: "fix: a bug fix", date: "2021-01-01" }],
      }),
      add: vi.fn().mockResolvedValue(true),
      commit: vi.fn().mockResolvedValue(true),
      tag: vi.fn().mockResolvedValue(true),
      push: vi.fn().mockResolvedValue(true),
      status: vi.fn().mockResolvedValue({ files: [], current: "main" }),
      getRemotes: vi.fn().mockResolvedValue([{ name: "origin" }]),
      stash: vi.fn().mockResolvedValue(true),
    };
  },
}));

vi.mock("semver", () => {
  return {
    default: {
      inc: vi.fn((version, release) => `${version}-incremented-${release}`),
    },
  };
});
vi.mock("fs", () => ({
  promises: {
    readFile: vi.fn().mockResolvedValue("1.0.0"),
    writeFile: vi.fn().mockResolvedValue(true),
  },
}));

vi.mock("inquirer", () => ({
  default: () => {
    return {
      prompt: vi.fn().mockResolvedValue({
        newVersion: "1.0.1",
        messagesStringFinal: "Mocked messages",
        confirmPush: true,
        remotesSelected: ["origin"],
      }),
    };
  },
}));

describe("VersionManager", () => {
  let versionManager;
  let gitMock;

  beforeEach(() => {
    vi.clearAllMocks();
    gitMock = simpleGit();
    versionManager = new VersionManager({
      file_changelog: "changes.md",
      file_version: "version.txt",
      dry_run: false,
      git: gitMock,
    });
  });

  describe("getVersionFromFile", () => {
    it("should read the correct file and return its content", async () => {
      const mockVersion = "1.0.0";
      fs.readFile.mockResolvedValue(`${mockVersion}\n`);

      const version = await versionManager.getVersionFromFile("version.txt");

      expect(version).toBe(mockVersion);
      expect(fs.readFile).toHaveBeenCalledWith("version.txt", "utf8");
    });
  });

  describe("saveVersionToFile", () => {
    const mockVersion = "1.0.1";

    it("should write the correct version to the specified file", async () => {
      versionManager.dry_run = false;
      await versionManager.saveVersionToFile(mockVersion);

      expect(fs.writeFile).toHaveBeenCalledWith(
        "version.txt",
        mockVersion,
        "utf8"
      );
    });

    it("should not write to file on dry run", async () => {
      versionManager.dry_run = true;
      await versionManager.saveVersionToFile(mockVersion, true);
      expect(fs.writeFile).not.toHaveBeenCalled();
    });
  });

  describe("updateChangeLog", () => {
    const title = "## Changelog\n";
    const entries = " - abcd\n -xyz";
    const newEntry = "- 1.0.1 [2024-03-01 – 2024-03-01]\n    -Fixed bug";
    const changelog = `${title}\n${entries}`;
    const newChangelog = `${title}\n${newEntry}${entries}`;

    it("correctly updates the changelog with new version message", async () => {
      versionManager.dry_run = false;

      fs.readFile.mockResolvedValue(changelog);
      fs.writeFile.mockResolvedValue(undefined);

      await versionManager.updateChangeLog(newEntry);

      expect(fs.readFile).toHaveBeenCalledWith(
        versionManager.file_changelog,
        "utf8"
      );

      expect(fs.writeFile).toHaveBeenCalledWith(
        versionManager.file_changelog,
        newChangelog,
        "utf8"
      );
    });

    it("Skip updates changelog in dry mode", async () => {
      versionManager.dry_run = true;

      await versionManager.updateChangeLog(newEntry);

      expect(fs.readFile).toHaveBeenCalledWith(
        versionManager.file_changelog,
        "utf8"
      );

      expect(fs.writeFile).not.toHaveBeenCalledWith(
        versionManager.file_changelog,
        newChangelog,
        "utf8"
      );
    });
  });

  describe("checkForUncommittedChanges", () => {
    it("does return false in case of uncommitted changes", async () => {
      const x = await versionManager.checkForUncommittedChanges();
      expect(x).toBeFalsy();
    });

    it("does return true in case of uncommitted changes", async () => {
      gitMock.status.mockResolvedValueOnce({
        files: [{ path: "modifiedFile.js", working_dir: "M" }],
        current: "main",
      });
      const x = await versionManager.checkForUncommittedChanges(gitMock);
      expect(x).toBeTruthy();
    });
  });

  describe("proposeNextVersions", () => {
    it("proposes next versions correctly based on current version", () => {
      const currentVersion = "1.0.0";
      const proposedVersions =
        versionManager.proposeNextVersions(currentVersion);

      expect(proposedVersions).toEqual({
        nextAlpha: semver.inc(currentVersion, "prerelease", "alpha"),
        nextBeta: semver.inc(currentVersion, "prerelease", "beta"),
        nextPatch: semver.inc(currentVersion, "patch"),
        nextFeature: semver.inc(currentVersion, "minor"),
        nextMajor: semver.inc(currentVersion, "major"),
      });
    });
  });

  describe("getFormattedVersionMessage", () => {
    const versionNew = "1.0.1";
    const versionLatest = "1.0.1";
    const expectedTitle = `- ${versionNew} [ 2024-02-27 – 2024-02-28 ]`;

    const commits = [
      { message: "feat: new feature", date: "2024-02-28" },
      { message: "fix: bug fix", date: "2024-02-27" },
    ];

    it("formats version message correctly with commits", async () => {
      gitMock.tags.mockResolvedValue({ latest: versionLatest });
      gitMock.log.mockResolvedValue({
        all: commits,
      });

      const message = await versionManager.getFormattedVersionMessage(
        versionNew
      );

      expect(message).toContain(expectedTitle);
      expect(message).toContain("feat: new feature");
      expect(message).toContain("fix: bug fix");
    });
  });
});
