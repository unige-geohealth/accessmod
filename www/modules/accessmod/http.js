function getSettings() {
  const s = {
    httpPort: window.location.port,
    httpHost: window.location.hostname,
    httpPath : window.location.pathname,
    httpProtocol: window.location.protocol,
  };
  return s;
}

function urlRoute(route) {
  const s = getSettings();
  return `${s.httpProtocol}//${s.httpHost}:${s.httpPort}${s.httpPath}${route}`;
}

function isNested(){
  return window.parent !== window
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
  const nested = isNested();
  try {
    if (stop === true) {
      const urlProgressStop = urlRoute("progress/stop");
      const msg = amSearchDict("progress_stop_confirm");
      res = confirm(msg);
      if (res === true) {
        console.log("stop_process_requested");
        if (nested) {
          window.parent.postMessage("restart", "*");
          return;
        }
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
    const urlVersions = urlRoute("versions");
    const r = await fetch(urlVersions);
    const data = await r.json();
    return data;
  } catch (e) {
    console.warn(e);
  }
}
