import fs from "node:fs/promises";
import { tl } from "../translate/index.js";
import { constants as fs_constants } from "node:fs";
import { dialog } from "electron";

export class DataLocationTools {
  constructor() {}

  /**
   * Test data loc path
   * @param {String} loc Path to data location folder or volume
   * @return {Promise<Boolean>} Valid path or id
   */
  async testLoc(loc) {
    const ctr = this;
    const dockVol = ctr.getState("docker_volume");
    if (loc === dockVol) return true;

    return await ctr.checkPathWritable(loc);
  }

  async checkPathWritable(path) {
    try {
      await fs.access(path, fs_constants.W_OK);
      return true;
    } catch {
      return false;
    }
  }

  async checkPathExists(path) {
    try {
      await fs.access(path, fs_constants.F_OK);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Dialog location
   */
  async dialogDataLoc(opt) {
    const ctr = this;

    opt = {
      cancelable: false,
      ...opt,
    };
    const language = ctr.getState("language");
    let dataLoc = null;

    const buttons = [
      tl("data_loc_opt_docker_volume", language),
      tl("data_loc_opt_directory", language),
    ];

    if (opt.cancelable) buttons.push(tl("cancel"));

    const choice = await dialog.showMessageBox(ctr._mainWindow, {
      type: "question",
      buttons: buttons,
      title: tl("data_loc_options_title", language),
      message: tl("data_loc_options", language),
      defaultId: opt.cancelable ? 2 : 0,
    });


    switch (choice.response) {
      case 2:
        ctr.log("choice is cancel");
        dataLoc = false;
        break;

      case 1:
        {
          ctr.log("choice is select folder");
          let resp = await dialog.showOpenDialog({
            properties: ["openDirectory"],
          });
          [dataLoc] = resp.filePaths;
        }
        break;

      case 0:
        ctr.log("choice is docker volume");
        dataLoc = ctr.getState("docker_volume");
        break;

      default:
        ctr.log("choice is default");
        dataLoc = ctr.getState("docker_volume");
    }

    if (!dataLoc) return;

    const writable = await ctr.testLoc(dataLoc);

    ctr.log("Selected path", dataLoc, "writable", writable);

    if (!writable) await ctr.dialogDataLoc();
    else return dataLoc;
  }

  async initDataLocation(opt) {
    const ctr = this;

    opt = {
      reset: false,
      cancelable: false,
      ...opt,
    };
    let dataLoc = ctr.getState("data_location");

    if (!dataLoc) {
      dataLoc = ctr.getState("docker_volume");
      ctr.setState("data_location", dataLoc);
    }

    const writable = await ctr.testLoc(dataLoc);

    if (!writable || opt.reset) dataLoc = await ctr.dialogDataLoc(opt);

    if (dataLoc)
      await ctr.updateDataLocation({
        path: dataLoc,
        cancelable: opt.cancelable,
      });
  }

  /**
   * Test data loc path (grass db files) and save in state
   * @param {Object} opt Options
   * @param {String} opt.path Path to data location folder or volume
   * @param {Boolean} opt.cancelable Cancellable (don't force restart)
   */
  async updateDataLocation(opt) {
    const ctr = this;

    opt = {
      path: null,
      cancelable: true,
      ...opt,
    };
    const dataLoc = opt.path;
    const writable = await ctr.testLoc(opt.path);

    if (writable) {
      const oldLoc = ctr.getState("data_location");

      if (dataLoc !== oldLoc) {
        ctr.setState("data_location", dataLoc);
        if (!opt.cancelable) {
          await ctr.restart();
        } else {
          const restart = await dialog.showMessageBox(ctr._mainWindow, {
            type: "question",
            buttons: ["Yes", "No"],
            title: "Confirm",
            message: "Do you want to restart now?",
          });

          if (!restart.response) await ctr.restart();
        }
      } else if (opt.cancelable) {
        const ok = await dialog.showMessageBox(ctr._mainWindow, {
          type: "question",
          buttons: ["Yes", "No"],
          title: "Choose another location",
          message:
            "The location is the same as the previous one. Choose another location?",
        });

        if (!ok.response)
          await ctr.initDataLocation({
            reset: true,
            cancelable: true,
          });
      }
    } else {
      throw Error("Unexpected non-writable location");
    }
  }
}
