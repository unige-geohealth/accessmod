/**
 * From the shiny client, talk to the iframe manager
 */
window.addEventListener("load", (event) => {
  const elBtnTest = document.getElementById("btnTest");
  const elBtnRestart = document.getElementById("btnRestart");
  elBtnRestart.addEventListener("click", () => {
    console.log("restart requested");
    if (window.parent) {
      window.parent.postMessage("restart", "*");
    }
  });
  elBtnTest.addEventListener("click", () => {
    console.log("clicked");
  });
});
