(() => {

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
  cb();
}, 1000);

})();
