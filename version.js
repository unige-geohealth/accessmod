const dryRun = process.argv.includes("--dry-run");
import { VersionManager } from "./version_manager/index.js";

const options = {
  file_version: "./version.txt",
  file_changelog: "./changes.md",
  json_update_list : ["./package.json","./electron/package.json"],
  dry_run: dryRun,
};

const version = new VersionManager(options);

// create a version
await version.create();
