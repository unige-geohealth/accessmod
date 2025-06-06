import semver from "semver";
import { dialog } from "electron";
import { fetchCacheData } from "../../fetch";

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
    if (vrs.v(sum.maxRemote))
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
      if (!force && cacheList > 0) {
        return cacheList;
      }

      const hasNet = await ctr.hasInternet();

      if (!hasNet) {
        return cacheList;
      }

      const rUrl = vrs._ctr.getState("repo_url_api");
      const rName = vrs._ctr.getState("repo_name");
      const nRes = 100;
      const pNum = 1;
      const url = `${rUrl}/repositories/${rName}/tags/?page_size=${nRes}&page=${pNum}`;
      const data = await fetchCacheData(url, 60 * 30);
      const versions = vrs.filterValid(data.results.map((r) => r.name));

      if (versions.length > 0) {
        cache.list_remote = versions;
      }

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
      vrs.gt(v, maxLocal);
    });

    return listNewer;
  }

  async currentGreaterThan(vTest) {
    const vrs = this;
    const vCur = await vrs.current();

    return vrs.gt(vCur, vTest);
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
    return vrs.v(vL) && vrs.v(vR) && vrs.gt(vR, vL);
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

  v(version) {
    try {
      return semver.valid(version);
    } catch (e) {
      throw new Error(`Invalid version ${JSON.stringify(version)}`);
    }
  }
  gt(a, b) {
    try {
      if (!b) {
        // a 2 > b null
        return true;
      }

      if (!a) {
        // a null > b 2
        return false;
      }

      return semver.gt(a, b);
    } catch (e) {
      throw new Error(`Invalid gt version ${JSON.stringify({ a, b })}`);
    }
  }
  lt(a, b) {
    try {
      if (!b) {
        // a 2 < b null
        return false;
      }

      if (!a) {
        // a null < b 2
        return true;
      }
      return semver.lt(a, b);
    } catch (e) {
      throw new Error(`Invalid lt version ${JSON.stringify({ a, b })}`);
    }
  }
  async max(list) {
    const vrs = this;
    const minSemver = vrs._ctr.getState("min_semver");
    try {
      return semver.maxSatisfying(list, minSemver, {
        includePrerelease: true,
      });
    } catch (e) {
      throw new Error(`Invalid max version ${JSON.stringify(list)}`);
    }
  }

  async setVersion(version) {
    const vrs = this;
    const ctr = vrs._ctr;

    if (version === "latest") {
      return vrs.updateLatest();
    }
    const vIsValid = vrs.v(version);

    if (!vIsValid) {
      throw new Error(`Invalid version ${version}`);
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

    /**
     *     {
     *     "Containers": -1,
     *     "Created": 1705928421,
     *     "Id": "sha256:d317c1318293e7e31935282877a42473783dfa071385f9fe4df85be9adcf5b45",
     *     "Labels": {
     *         "maintainer": "email>"
     *     },
     *     "ParentId": "",
     *     "RepoDigests": [
     *         "fredmoser/accessmod@sha256:1113c8266f88e58b535a9ed9efff57e077749c8c62c33081efd33b0b55ad8e4b"
     *     ],
     *     "RepoTags": [
     *         "fredmoser/accessmod:5.8.2-alpha.2"
     *     ],
     *     "SharedSize": -1,
     *     "Size": 432105534
     * }
     */

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
    const vIsValid = vrs.v(version);

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
      // yes
      if (choice === 0) {
        await vrs.setVersion(version);
      }
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

    const noUpdateButListed = !hasUpdate && vrs.gt(vMaxRemote, vCur);

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
      return vrs.lt(v, vCur);
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
    const vrs = this;
    return list.filter(vrs.v);
  }

  getRepoTag(tag) {
    const vrs = this;
    const ctr = vrs._ctr;
    const version = tag || ctr.getState("version");
    const image_name = ctr.getState("image_name");

    return `${image_name}:${version}`;
  }
}
