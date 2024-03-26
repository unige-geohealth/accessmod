import semver from "semver";
import fetch from "node-fetch";
import { dialog } from "electron";

const cache = {
  list_local: [],
  list_remote: [],
};

export class Versions {
  constructor(controller) {
    const vrs = this;
    vrs._ctr = controller;
  }

  async summary(force) {
    const vrs = this;

    // should match structure from ../../../../tools/R/amDockerHelpers.R
    return {
      local: await vrs.listLocal(),
      remote: await vrs.listRemote(force),
      remoteNew: await vrs.listRemoteNewer(),
      current: await vrs.current(),
      maxLocal: await vrs.maxLocal(),
      maxRemote: await vrs.maxRemote(),
      hasUpdate: await vrs.hasUpdate(),
      date: new Date().toISOString(),
    };
  }

  async getVersionsForMenu(force) {
    const vrs = this;
    const ctr = vrs._ctr;
    const sum = await vrs.summary(force);
    const out = [];

    // has update
    out.push({
      label: sum.hasUpdate
        ? `Update available ${sum.maxRemote}... `
        : "No update. Check now?",
      click: sum.hasUpdate
        ? () => vrs.dialogSetVersion(sum.maxRemote)
        : () => vrs.dialogCheckUpdate(),
    });
    out.push({
      role: "separator",
    });
    // current
    out.push({
      label: "Current",
      submenu: [
        {
          label: sum.current,
          click: () =>
            vrs.dialogSetVersion(sum.current).catch(ctr.dialogShowError),
        },
      ],
    });

    // remote
    if (semver.valid(sum.maxRemote))
      out.push({
        label: "Remote latest",
        submenu: [
          {
            label: sum.maxRemote,
            click: () =>
              vrs.dialogSetVersion(sum.maxRemote).catch(ctr.dialogShowError),
          },
        ],
      });

    // list local
    const subLoc = sum.local.map((v) => ({
      label: v,
      click: () => vrs.dialogSetVersion(v).catch(ctr.dialogShowError),
    }));

    out.push({
      label: "Local versions",
      submenu: subLoc,
    });

    // list remote
    const subRemoteAll = sum.remote.map((v) => ({
      label: v,
      click: () => vrs.dialogSetVersion(v).catch(ctr.dialogShowError),
    }));

    out.push({
      label: "Remote all",
      submenu: subRemoteAll,
    });

    const subRemoteNew = sum.remoteNew.map((v) => ({
      label: v,
      click: () => vrs.dialogSetVersion(v).catch(ctr.dialogShowError),
    }));

    out.push({
      label: "Remote new",
      submenu: subRemoteNew,
    });

    // remove old
    if (sum.local.length > 1) {
      out.push({
        role: "separator",
      });
      out.push({
        label: "Remove old versions...",
        click: () => vrs.dialogRemovePreviousVersions(),
      });
    }

    return out;
  }

  async cacheSet(id, value) {
    cache[id] = value;
  }

  async cacheGet(id) {
    return cache[id];
  }

  async max(list) {
    const vrs = this;
    const minSemver = vrs._ctr.getState("min_semver");

    return semver.maxSatisfying(list, minSemver, {
      includePrerelease: true,
    });
  }

  async maxLocal() {
    const vrs = this;
    const versions = await vrs.listLocal();

    return vrs.max(versions);
  }

  async maxRemote(force) {
    const vrs = this;
    const versions = await vrs.listRemote(force);

    return vrs.max(versions);
  }

  async listRemote(force) {
    const vrs = this;
    const ctr = vrs._ctr;
    const cacheList = await vrs.cacheGet("list_remote");

    try {
      if (!force && cacheList > 0) return cacheList;

      const hasNet = await ctr.hasInternet();

      if (!hasNet) return cacheList;

      const rUrl = vrs._ctr.getState("repo_url_api");
      const rName = vrs._ctr.getState("repo_name");
      const nRes = 100;
      const pNum = 1;
      const url = `${rUrl}/repositories/${rName}/tags/?page_size=${nRes}&page=${pNum}`;
      const res = await fetch(url);
      const data = await res.json();
      const versions = vrs.filterValid(data.results.map((r) => r.name));

      if (versions.length > 0) cache.list_remote = versions;

      return versions;
    } catch (e) {
      vrs._ctr.dialogShowError(e);
    }

    return cacheList;
  }

  async listRemoteNewer(force) {
    const vrs = this;
    const maxLocal = await vrs.maxLocal();
    const listRemote = await vrs.listRemote(force);

    const listNewer = listRemote.filter((v) => {
      semver.gt(v, maxLocal);
    });

    return listNewer;
  }

  async currentGreaterThan(vTest) {
    const vrs = this;
    const vCur = await vrs.current();

    return semver.gt(vCur, vTest);
  }

  async listLocal() {
    const vrs = this;
    const rTag = await vrs.listRepoTags();

    return vrs.filterValid(rTag.map((r) => r.split(":")[1]));
  }

  async hasUpdate(force) {
    const vrs = this;
    const vL = await vrs.maxLocal();
    const vR = await vrs.maxRemote(force);
    const v = semver.valid;

    return v(vL) && v(vR) && semver.gt(vR, vL);
  }

  async current() {
    const vrs = this;
    return vrs.currentSync();
  }

  currentSync() {
    const vrs = this;
    const ctr = vrs._ctr;

    return ctr.getState("version");
  }

  async removeByList(vList, force) {
    const vrs = this;
    const ctr = vrs._ctr;
    let n = 0;

    try {
      for (const v of vList) {
        const vTag = vrs.getRepoTag(v);
        const img = await ctr._docker.getImage(vTag);

        if (img)
          if (!force) {
            ctr.log(`Image ${vTag} removed [dry, use force to really remove]`);
          } else {
            await img.remove({
              force: true,
            });
            n++;
          }
      }
    } catch (e) {
      ctr.dialogShowError(e);
    }

    return n;
  }

  async pullRemote(version) {
    const vrs = this;
    const ctr = vrs._ctr;
    const rTag = vrs.getRepoTag(version);

    await ctr._docker.pull(rTag);
  }

  async setVersion(version) {
    const vrs = this;
    const ctr = vrs._ctr;

    if (version === "latest") {
      return vrs.updateLatest();
    }

    const vLoc = await vrs.hasVersionLocal(version);

    if (!vLoc) {
      /**
       * Test network
       */
      const hasNet = await ctr.hasInternet();

      if (!hasNet) {
        ctr.dialogNoNetwork();
        return;
      }

      /**
       * Check remote
       */
      const vRem = await vrs.hasVersionRemote(version);

      if (!vRem) {
        ctr.dialogShowError(`Version ${version} not found `);
        return;
      }

      await vrs.pullRemote(version);
    }

    ctr.setState("version", version);
    await ctr.restart();
  }

  async updateLatest() {
    const vrs = this;
    const ctr = vrs._ctr;
    const latest = vrs.getRepoTag("latest");

    await ctr._docker.pull(latest);
  }

  async hasVersionLocal(version) {
    const vrs = this;

    version = version || (await vrs.current());
    const loc = await vrs.listLocal();

    return loc.includes(version);
  }

  async hasVersion(version) {
    const vrs = this;

    return (
      (await vrs.hasVersionLocal(version)) ||
      (await vrs.hasVersionRemote(version))
    );
  }

  async hasVersionRemote(version) {
    const vrs = this;
    const rem = await vrs.listRemote();

    return rem.includes(version);
  }

  async listRepoTags() {
    const vrs = this;
    const imgs = await vrs.listImages();
    const tags = [];

    for (let img of imgs) {
      if (img.RepoTags) tags.push(...img.RepoTags);
    }

    return tags;
  }

  async listImages() {
    const vrs = this;
    const ctr = vrs._ctr;
    const image_name = ctr.getState("image_name");
    const ref = {};

    ref[image_name] = true;
    const imgs = await ctr._docker.listImages({
      filters: {
        reference: ref,
      },
    });

    return imgs;
  }

  /**
   * Dialogs
   */
  async dialogSetVersion(version) {
    const vrs = this;
    const ctr = vrs._ctr;
    const vCur = await vrs.current();
    const hasNet = await ctr.hasInternet();
    const vIsLocal = await vrs.hasVersionLocal(version);
    const vIsValid = semver.valid(version);

    if (!vIsValid) {
      dialog.showMessageBoxSync(ctr._mainWindow, {
        type: "warning",
        buttons: ["ok"],
        title: "Invalid",
        message: "The selected version is not valid",
      });
      return;
    }

    if (!vIsLocal && !hasNet) {
      ctr.dialogNoNetwork();
      return;
    }

    if (version === vCur) {
      dialog.showMessageBoxSync(ctr._mainWindow, {
        type: "info",
        buttons: ["ok"],
        title: "Nothing to do",
        message: "The version selected is the current version",
      });
    } else {
      const choice = dialog.showMessageBoxSync(ctr._mainWindow, {
        type: "question",
        buttons: ["Yes", "No"],
        title: "Confirm",
        message: `Are you sure you want to restart and use version ${version} ? `,
      });

      if (!choice) await vrs.setVersion(version);
    }
  }

  async dialogCheckUpdate() {
    const vrs = this;
    const ctr = vrs._ctr;
    const hasNet = await ctr.hasInternet(2000, 3);

    if (!hasNet) {
      ctr.dialogNoNetwork();
      return;
    }

    const vCur = await vrs.current();
    const hasUpdate = await vrs.hasUpdate(true);
    const vMaxRemote = await vrs.maxRemote();

    if (hasUpdate) await ctr.updateMenu();

    const noUpdateButListed = !hasUpdate && semver.gt(vMaxRemote, vCur);

    dialog.showMessageBoxSync(ctr._mainWindow, {
      type: "info",
      buttons: ["ok"],
      title: "Update check",
      message: hasUpdate
        ? `Update available (${vMaxRemote}) ! `
        : `No update ${
            noUpdateButListed
              ? ", but a more recent version is available in the menu. "
              : ""
          }`,
    });
  }

  async dialogRemovePreviousVersions() {
    const vrs = this;
    const ctr = vrs._ctr;
    const vCur = await vrs.current();
    const vLocal = await vrs.listLocal();

    const vBefore = vLocal.filter((v) => {
      return semver.lt(v, vCur);
    });

    const nBefore = vBefore.length;

    if (!nBefore) {
      dialog.showMessageBoxSync(ctr._mainWindow, {
        type: "info",
        buttons: ["ok"],
        title: "Remove previous versions",
        message: "No versions to remove",
      });
      return;
    }

    const nLeft = vLocal.length - nBefore;

    const choice = dialog.showMessageBoxSync(ctr._mainWindow, {
      type: "question",
      buttons: ["Yes", "No"],
      title: "Confirm",
      message: `This will remove ${nBefore} versions older than ${vCur} and freeing up disk space.\n ${nLeft} versions will still be available, including ${vCur}. \n  No data will be lost.  \n All those versions will still be available remotely. Confirm ? `,
    });

    if (!choice) {
      const nRemoved = await vrs.removeByList(vBefore, true);
      await ctr.updateMenu();
      dialog.showMessageBoxSync(ctr._mainWindow, {
        type: "info",
        buttons: ["ok"],
        title: "Versions removed",
        message: `${nRemoved}/${nBefore} versions removed.`,
      });
    }
  }

  /**
   * Helpers
   */
  filterValid(list) {
    return list.filter(semver.valid);
  }

  getRepoTag(tag) {
    const vrs = this;
    const ctr = vrs._ctr;
    const version = tag || ctr.getState("version");
    const image_name = ctr.getState("image_name");

    return `${image_name}:${version}`;
  }
}
