function getSettings() {
  const s = Object.assign(
    {},
    {
      httpPort: '5080',
      httpHost: 'localhost',
      httpProtocol: 'http:'
    },
    window.am.settings
  );
  return s;
}

function urlRoute(route) {
  const s = getSettings();
  return `${s.httpProtocol}//${s.httpHost}:${s.httpPort}/${route}`;
}

/**
 * Stop current process
 *
 * Simple breaker : a call to progress/stop with no param will write a file in /tmp and
 * when the process call pbc ( progressBarControl ) server side, the process is exited
 *
 * @param {Boolean} stop Stop process ( before the next step )
 * @return {Boolean} stop
 */
async function stopProcess(stop) {
  let res = false;
  try {
    if (stop === true) {
      const urlProgressStop = urlRoute('progress/stop');
      const msg = amSearchDict('progress_stop_confirm');
      res = confirm(msg);
      if (res === true) {
        const r = await fetch(urlProgressStop);
        const txt = await r.text();
        console.log(txt);
      }
    }
  } catch (e) {
    console.warn(e);
  }
  return res;
}

/**
 * Get version summary
 *
 * @return {Object} sumamry
 */
async function getVersionsSummary() {
  try {
    const urlVersions = urlRoute('versions/summary.json');
    const r = await fetch(urlVersions);
    return await r.json();
  } catch (e) {
    console.warn(e);
  }
}
