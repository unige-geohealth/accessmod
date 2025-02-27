import { promises as fs } from "fs";
import inquirer from "inquirer";
import semver from "semver";
import simpleGit from "simple-git";

const options_default = {
  file_version: "version.txt",
  file_changelog: "changes.md",
  json_update_list: ["package.json"],
  dry_run: false,
  allowed_branches: [],
};

export class VersionManager {
  constructor(options) {
    options = Object.assign({}, options_default, options);
    this.file_version = options.file_version;
    this.file_changelog = options.file_changelog;
    this.json_update_list = options.json_update_list;
    this.allowed_branches = options.allowed_branches;
    this.dry_run = !!options.dry_run;
    this.git = options.git || simpleGit();
  }

  async create() {
    try {
      await this.checkForUncommittedChanges(true);
      await this.checkAllowedBranch();
      const currentVersion = await this.getVersionFromFile(this.file_version);
      const newVersion = await this.promptNewVersion(currentVersion);
      const messagesString = await this.getFormattedVersionMessage(newVersion);
      const messagesStringFinal = await this.promptEditMessages(messagesString);
      await this.updateChangeLog(messagesStringFinal);
      await this.saveVersionToFile(newVersion);
      await this.saveVersionToJsonList(newVersion);
      await this.commitAndTagVersion(newVersion);
      await this.pushToRemote();
      return true;
    } catch (error) {
      console.error("Error:", error);
      await this.autoStash();
      process.exit(1);
    }
  }

  async checkForUncommittedChanges(blocking) {
    const status = await this.git.status();
    const hasChange = status.files.length > 0;
    if(hasChange && blocking){
      console.error('Project has uncommited changes')
      process.exit(1);
    }
  }

  async checkAllowedBranch() {
    const status = await this.git.status();
    const currentBranch = status.current;
    const hasAllowedBranch = this.allowed_branches.length > 0;
    const isIncluded = this.allowed_branches.includes(currentBranch);
    const isAllowed = !hasAllowedBranch || isIncluded;
    if (!isAllowed) {
      throw new Error(
        `Error: Current branch '${currentBranch}' is not allowed. Allowed branches: ${this.allowed_branches.join(
          ", "
        )}`
      );
    }
  }

  async getVersionFromFile(filePath) {
    const data = await fs.readFile(filePath, "utf8");
    return data.trim();
  }

  async saveVersionToFile(version) {
    if (this.dry_run) {
      console.log(
        `Dry run: Version would be saved as ${version} to ${this.file_version}`
      );
      return;
    }
    await fs.writeFile(this.file_version, version + "\n", "utf8");
  }

  async saveVersionToJsonList(version) {
    if (this.dry_run) {
      console.log(
        `Dry run: Version would be saved in these files ${JSON.stringify(
          this.json_update_list
        )}`
      );
      return;
    }

    for (const file of this.json_update_list) {
      const str = await fs.readFile(file, "utf8");
      const content = JSON.parse(str);
      if (content.version) {
        content.version = version;
        const strUpdated = JSON.stringify(content, null, 2);
        await fs.writeFile(file, strUpdated, "utf8");
      }
    }
  }

  async updateChangeLog(newVersionMessage) {
    const changelogContent = await fs.readFile(this.file_changelog, "utf8");
    const endOfTitleBlock = changelogContent.indexOf("\n\n") + 2;
    const head = changelogContent.slice(0, endOfTitleBlock);
    const body = newVersionMessage;
    const logs = changelogContent.slice(endOfTitleBlock);

    const changelogNew = `${head}${body}${logs}`;
    const changelogShort = changelogContent.slice(0, 1000);

    if (this.dry_run) {
      console.log(
        `Dry run: changelog preview: ${this.file_changelog}:\n${changelogShort}`
      );
      return;
    }

    await fs.writeFile(this.file_changelog, changelogNew, "utf8");
  }

  async getFormattedVersionMessage(newVersion) {
    // Get all tags sorted by date
    const allTags = await this.git.tags(["--sort=-creatordate"]);

    let commits;
    if (allTags.all.length > 0) {
      // Get commits since the most recent tag
      const latestTag = allTags.all[0];
      commits = await this.git.log({ from: latestTag, to: "HEAD" });
    } else {
      // If no tags exist, get all commits
      commits = await this.git.log();
    }

    // Only process commits if we have any
    if (!commits || !commits.all || commits.all.length === 0) {
      return `- ${newVersion} [ ${new Date().toISOString().substring(0, 10)} ]`;
    }

    // Get date range from actual commits
    const dates = commits.all.reduce(
      (acc, commit) => {
        const date = new Date(commit.date);
        return {
          from: date < acc.from ? date : acc.from,
          to: date > acc.to ? date : acc.to,
        };
      },
      { from: new Date(), to: new Date(0) }
    );

    const dateFrom = dates.from.toISOString().substring(0, 10);
    const dateTo = dates.to.toISOString().substring(0, 10);

    const title = `- ${newVersion} [ ${dateFrom} – ${dateTo} ]`;
    const body = commits.all
      .map((commit) => `    -${commit.message}`)
      .join("\n");

    return `${title}\n${body}`;
  }

  async promptNewVersion(currentVersion) {
    // Step 1: Determine type of changes
    const { changeType } = await inquirer.prompt([
      {
        type: "list",
        name: "changeType",
        message: "What type of changes are included?",
        choices: [
          { name: "Breaking changes (major)", value: "major" },
          { name: "New features (minor)", value: "minor" },
          { name: "Bug fixes (patch)", value: "patch" },
        ],
      },
    ]);

    // Step 2: Check if preliminary release
    const { preliminary } = await inquirer.prompt([
      {
        type: "list",
        name: "preliminary",
        message: "Is this a preliminary release?",
        choices: [
          { name: "No (stable release)", value: "stable" },
          { name: "Yes, Alpha (early development)", value: "alpha" },
          { name: "Yes, Beta (feature complete, testing)", value: "beta" },
        ],
      },
    ]);

    // Generate suggested version
    let suggestedVersion;
    const isPrerelease = semver.prerelease(currentVersion);

    if (isPrerelease && preliminary !== "stable") {
      // If current version is a prerelease and we're staying in prerelease
      suggestedVersion = semver.inc(currentVersion, "prerelease");
    } else if (preliminary !== "stable") {
      // If we're moving to a prerelease
      suggestedVersion = `${semver.inc(
        currentVersion,
        changeType
      )}-${preliminary}.0`;
    } else {
      // Standard version increment
      suggestedVersion = semver.inc(currentVersion, changeType);
    }

    // Step 3: Allow manual version input with validation
    let isValid = false;
    let newVersion;

    while (!isValid) {
      const { version } = await inquirer.prompt([
        {
          type: "input",
          name: "version",
          message: `Suggested version: ${suggestedVersion}\nEnter version:`,
          default: suggestedVersion,
        },
      ]);

      // Validate version
      if (!semver.valid(version)) {
        console.error(
          "Invalid version format. Please use semver format (e.g., 1.2.3 or 1.2.3-alpha.0)"
        );
        continue;
      }

      if (!semver.gt(version, currentVersion)) {
        console.error(
          `New version must be greater than current version (${currentVersion})`
        );
        continue;
      }

      newVersion = version;
      isValid = true;
    }

    return newVersion;
  }

  async promptEditMessages(messagesString) {
    const { messagesStringFinal } = await inquirer.prompt([
      {
        type: "editor",
        name: "messagesStringFinal",
        message: "Edit the messages",
        default: messagesString,
        postfix: ".md",
      },
    ]);
    return messagesStringFinal;
  }

  async commitAndTagVersion(version) {
    if (this.dry_run) {
      console.log(
        `Dry run: Git commit and tag for version ${version} would be created.`
      );
      return;
    }
    await this.git.add(".");
    await this.git.commit(`version ${version}`);
    await this.git.tag([version]);
  }

  async pushToRemote() {
    const status = await this.git.status();
    const branch = status.current;
    const remotes = await this.git.getRemotes(true);

    if (remotes.length === 0) {
      throw new Error("No git remotes found.");
    }

    const choices = remotes.map((remote) => ({
      name: remote.name,
      value: remote.name,
      checked: true,
    }));

    const { remotesSelected } = await inquirer.prompt([
      {
        type: "checkbox",
        name: "remotesSelected",
        message: "Select remotes to push to:",
        choices: choices,
      },
    ]);

    const remotesSelectedString = JSON.stringify(remotesSelected);

    const { confirmPush } = await inquirer.prompt([
      {
        type: "confirm",
        name: "confirmPush",
        message: `Are you sure you want to push to ${remotesSelectedString}:${branch}?`,
      },
    ]);

    if (confirmPush) {
      for (const remote of remotesSelected) {
        if (this.dry_run) {
          console.log(
            `Dry run: Changes would be pushed to remote ${remote} on branch ${branch}`
          );
          continue;
        }
        await this.git.push(remote, branch, { "--tags": null });
        console.log(
          `Pushed to remote ${remote} on branch ${branch} successfully.`
        );
      }
    } else {
      throw new Error("Cancel in push step");
    }
  }

  async autoStash() {
    const hasChanges = await this.checkForUncommittedChanges();
    if (!hasChanges) {
      return;
    }
    if (this.dry_run) {
      console.log("Dry run: Uncommitted changes would be stashed.");
      return;
    }
    await this.git.stash();
    console.log("Uncommitted changes have been stashed.");
  }
}
