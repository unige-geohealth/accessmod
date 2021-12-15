/**
 *         ___                                  __  ___            __   ______
 *        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
 *       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
 *      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
 *     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
 *
 *    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
 *
 *    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
const idElVersion = 'amVersion';
const changesLogs = {};
const urlBase = 'https://raw.githubusercontent.com/fxi/AccessMod_shiny';
const urlGithubRelease = 'https://github.com/fxi/AccessMod_shiny/releases/tag/';
const urlDockerHub = 'https://hub.docker.com/r/fredmoser/accessmod/tags?page=1&name=' ;
const el = El.el;

/**
 *  Check is version is available locally or remotely
 *  @param {String} Version string
 */

function validateVersion(version) {
  if (!version) {
    version = am?.versions?.current;
  }

  const vLocal = am?.versions?.local || [];
  const vRemote = am?.versions?.remote || [];
  const valid = vRemote.includes(version) || vLocal.includes(version);

  if (!valid) {
    return console.warn(`Version ${version} invalid`);
  }
  return version;
}

/**
 *  Get html parsed from markdown changelog from remote repos
 *  @param {String} Version string
 */

async function getChangelogHtml(version) {
  version = validateVersion(version);

  if (changesLogs[version]) {
    return changesLogs[version];
  }

  const r = await fetch(`${urlBase}/${version}/changes.md`);
  const c = await r.text();
  const h = marked(c);
  changesLogs[version] = h;
  return h;
}

/**
 *  Display a modal panel with the changelog
 *  @param {String} Version string
 */

async function modalChangelog(version) {
  version = validateVersion(version);
  const html = await getChangelogHtml(version);
  const elContent = el(
    'div',
    {
      style: {maxHeight: '400px', overflowY: 'auto'}
    },
    html
  );
  buildModal({title: `Changelog ${version}`, content: elContent});
}

/**
 * Race promise
 * @param {Promise} prom
 * @param {Number} ms number of ms to wait
 * @return {Boolean|Value}
 */
async function raceTimeout(prom, ms) {
  return Promise.race([prom, wait(ms)]);
}

/**
 * Wait async / timeout
 * @param {Number} ms number of ms to wait
 * @param {Any} value value to return after wait (true by default)
 * @return {Boolean|Value}
 */
async function wait(ms, value) {
  return new Promise((resolve) => {
    setTimeout(() => resolve(value || true), ms | 0);
  });
}

/**
 * Versions check
 * Check remote and local images for available versions:
 * - save the result in "window.am"
 * - Update version text
 */

async function amCheckVersions() {
  const el = El.el;
  const elVersionContainer = document.getElementById(idElVersion);
  if (!navigator.onLine) {
    alert('No network');
    return;
  }
  const elWait = el('span', 'Versions lookup..');
  elVersionContainer.innerHTML = '';
  elVersionContainer.appendChild(elWait);
  await waitAsync(500);
  const summary = await raceTimeout(getVersionsSummary(), 5000);
  if (!summary) {
    elWait.remove();
    const elRecheck = el(
      'div',
      el('span', 'Versions lookup: an issue occured.'),
      el(
        'a',
        {
          style: {padding: '3px'},
          on: ['click', amCheckVersions]
        },
        el('span', 'Check now...')
      )
    );
    elVersionContainer.appendChild(elRecheck);
    return;
  }

  Object.assign(window.am.versions, summary);
  /**
   * Build
   */

  const elReleaseDownload = el(
    'div',
    el('span', `New version available: ${am.versions.maxRemote}`),
    el(
      'ul',
      el(
        'li',
        el(
          'a',
          {
            target: '#',
            on: [
              'click',
              function() {
                modalChangelog(am.versions.maxRemote);
              }
            ]
          },
          'Read changes...'
        )
      ),
      el(
        'li',
        el(
          'a',
          {
            href: `${urlGithubRelease}/${am.versions.maxRemote}`,
            target: '_blank'
          },
          'Download app / ova / source from Github...'
        )
      ),
      el(
        'li',
        el(
          'a',
          {
            href: `${urlDockerHub}=${am.versions.maxRemote}`,
            target: '_blank'
          },
          'Download image from Docker Hub...'
        )
      )
    )
  );

  const elRecheck = el(
    'div',
    el('span', 'Up to date'),
    el(
      'a',
      {
        style: {padding: '3px'},
        on: ['click', amCheckVersions]
      },
      el('span', 'Check now...')
    )
  );

  const elVersion = el(
    'div',
    el('span', 'Version'),
    el(
      'a',
      {
        style: {padding: '3px'},
        href: '#',
        on: [
          'click',
          function() {
            modalChangelog();
          }
        ]
      },
      am.versions.current
    ),
    summary.hasUpdate ? elReleaseDownload : elRecheck
  );
  elWait.remove();
  elVersionContainer.appendChild(elVersion);
}

/**
 * Small helper for waiting async timeout
 * @param {Number} ms Number of ms to wait
 */

function waitAsync(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms || 0);
  });
}
