// Route output messages from the developer application (Shpadoinkle.DeveloperTools.outputState)
// to the developer tools (Main.listenForOutput)
window.addEventListener("message", e => {
  if (e.source != window || e.data.type != "shpadoinkle_output_state") return;
  chrome.runtime.sendMessage(e.data);
});


// Route setting messages from developer tools (Main.sendHistory)
// to the developer application (Shpadoinkle.DeveloperTools.listenForSetState)
chrome.runtime.onMessage.addListener(e => {
  if(e && e.type == "shpadoinkle_set_state"){
    window.postMessage(e, "*");
  }
});
