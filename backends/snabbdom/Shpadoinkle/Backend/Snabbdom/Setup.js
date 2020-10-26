(() => {

const head = document.getElementsByTagName('head')[0];
const addScript = x => {
  const s = document.createElement('script');
  s.type = 'text/javascript';
  s.src = x;
  head.appendChild(s);
}

const cdnjs = x => "https://cdnjs.cloudflare.com/ajax/libs/snabbdom/0.7.4/" + x;

addScript(cdnjs("snabbdom.min.js"));
addScript(cdnjs("snabbdom-class.min.js"));
addScript(cdnjs("snabbdom-props.min.js"));
addScript(cdnjs("snabbdom-style.min.js"));
addScript(cdnjs("snabbdom-attributes.min.js"));
addScript(cdnjs("snabbdom-eventlisteners.min.js"));
addScript(cdnjs("h.min.js"));

window.startApp = cb => setTimeout(() => {
  const patch = snabbdom.init([
    snabbdom_props.default,
    snabbdom_class.default,
    snabbdom_attributes.default,
    snabbdom_eventlisteners.default
  ]);
  window.patchh = (a,b) => patch(a,b);
  window.vnode = h.default;
  window.potato = (n, e) => n.elm.appendChild(e)
  window.container = document.createElement('div');
  document.body.innerHTML = "";
  document.body.appendChild(container);
  cb();
}, 1000);

})();
