(() => {

const animClasses =
  [ ".feature__card"
  , ".feature-button"
  , ".feature__title"
  , ".footer__wrapper img"
  , ".minisandbox"
  , ".footer__nav__category"
  , ".footer__nav__links li"
  , ".hero-title"
  , ".hero-button"
  ]


const observer = new IntersectionObserver((entries, _) =>
  entries.forEach(entry => entry.target.classList.toggle("animate-show", entry.isIntersecting)),
  { root: null, rootMargin: '0px', threshold: 0.3 })


const init = () => animClasses.forEach(x => {
  for (const elm of document.querySelectorAll(x)){
    elm.classList.add("animate")
    observer.observe(elm)
  }
})


window.history.pushState = new Proxy(window.history.pushState, {
  apply: (target, thisArg, argArray) => {
    setTimeout(init,300)
    return target.apply(thisArg, argArray)
  }
})


setTimeout(init, 0)

})()
