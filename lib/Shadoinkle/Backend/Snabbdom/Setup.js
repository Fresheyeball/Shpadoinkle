(() => {

const head = document.getElementsByTagName('head')[0];
const addScript = x => {
  const s = document.createElement('script');
  s.type = 'text/javascript';
  s.src = x;
  head.appendChild(s);
}

const cdnjs = x => "https://lib.arvancloud.com/ar/snabbdom/0.7.2/" + x;

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
    snabbdom_eventlisteners.default
  ]);
  var old;
  window.patch = (a,b) => old ? old = patch(old, b) : old = patch(a, b);
  window.vnode = h.default;
  window.container = document.createElement('div');
  document.body.appendChild(container);
  cb();
}, 1000);

})();
