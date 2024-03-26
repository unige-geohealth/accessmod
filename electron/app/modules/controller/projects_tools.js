import prompt from "electron-dynamic-prompt";
import streamToPromise from "stream-to-promise";
import { dialog } from "electron";
import path from "node:path";
import fs from "node:fs/promises";

export class ProjectsTools {
  constructor() {}

  /**
   * Get projects list
   */
  async getProjectsList() {
    const ctr = this;
    const dbPath = ctr.getState("grass_db_location");
    const opt = {
      Cmd: [
        "Rscript",
        "-e",
        `library(jsonlite);print(toJSON(list.files('${dbPath}')))`,
      ],
      AttachStdout: true,
      AttachStderr: true,
    };

    const name = ctr.getState("container_name");
    const cont = ctr.getContainerByName(name);
    const cmdExec = await cont.exec(opt);
    const stream = await cmdExec.start();
    const buf = await streamToPromise(stream);
    const str = ctr.dockerBufferToString(buf);
    const strClean = str.match(/\[.*\]/g);

    if (!strClean) return [];

    return JSON.parse(strClean[0]);
  }

  /**
   * Check if projects exists
   */
  async projectExists(name) {
    const ctr = this;
    const projectList = await ctr.getProjectsList();

    return projectList.includes(name);
  }

  /**
   * Import project
   */
  async importProject(path, name) {
    const ctr = this;

    try {
      const volume = ctr.getState("data_location");
      const dbgrass = ctr.getState("grass_db_location");
      const archivePath = ctr.randomString(`/tmp/import_${name}_`, ".zip");

      const res = await ctr.workerRun({
        binds: [`${volume}:${dbgrass}`, `${path}:${archivePath}`],
        cmd: [
          "sh",
          "/app/sh/import_project_archive.sh",
          name,
          archivePath,
          dbgrass,
        ],
      });

      return res;
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  /**
   * Projects upload handler
   */
  async dialogProjectUpload() {
    const ctr = this;

    try {
      const projectName = await prompt(
        {
          modal: true,
          title: "New project imporation",
          header: "Project direct import",
          description:
            "This tool will load an archived project (*.am5p ) into the database. It should be faster and more reliable than the classic uploader, especially for large projects.\nThe name should have at least 4 alphanumeric characters and start with a letter. No special characters allowed except underscore.",
          height: 400,
          fields: [
            {
              id: "pname",
              label: "Project name",
              type: "input",
              attrs: {
                placeholder: "Name",
                required: true,
              },
            },
          ],
          validator: async (args) => {
            const exists = await ctr.projectExists(args.pname);

            if (exists) throw Error("Project already exists");

            const valid =
              args.pname.length > 3 &&
              args.pname.match(/^[a-zA-Z]/) &&
              !args.pname.match(/\W+/);

            if (!valid) throw Error("Name invalid");
          },
        } //ctr._mainWindow
      );

      if (!projectName) return;

      const projectFiles = await dialog.showOpenDialog(ctr._mainWindow, {
        title: "Select project file",
        properties: ["openFile"],
        filters: [
          {
            name: "AccessMod Archive Project",
            extensions: ["am5p"],
          },
        ],
      });

      if (!projectFiles || projectFiles.canceled) return;

      /**
       * Import
       */
      const [file] = projectFiles.filePaths;
      const fileBaseName = path.basename(file);
      const fileStat = await fs.stat(file);
      const fileSize = Math.round(fileStat.size / 1024 ** 2);
      const large = fileSize > 900;

      const confirmImport = await dialog.showMessageBox(ctr._mainWindow, {
        type: "question",
        buttons: ["Cancel", "Import"],
        title: "Confirmation",
        message: `
      Archive: '${fileBaseName}' ~ ${fileSize}MB \n
      Project: '${projectName.pname}' \n
      The importation should take less than ${large ? 5 : 1} minute${
          large ? "s" : ""
        }. A message will be displayed at the end of the process`,
        defaultId: 0,
      });

      if (!confirmImport.response) return;

      const res = await ctr.importProject(
        projectFiles.filePaths[0],
        projectName.pname
      );

      if (res.StatusCode) {
        ctr.dialogShowError(
          `An error occured during importation: ${res.Error}`
        );
        return;
      }

      const confirmRestart = await dialog.showMessageBox(ctr._mainWindow, {
        type: "question",
        buttons: ["Cancel", "Reload"],
        title: "Restart now ?",
        message: "The project has been imported. Reload now?",
        defaultId: 0,
      });

      if (!confirmRestart.response) return;

      ctr.reload();
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }
}
