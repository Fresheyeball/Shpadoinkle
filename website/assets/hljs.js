(() => {

const copyCode = code => {
  code.title = "Click To Copy"
  code.addEventListener("click", () =>
    navigator.clipboard.writeText(code.textContent))
}

const highlight = () => {
  for (const code of document.getElementsByTagName("code")){
    if(!code.light && code.classList.length){
      hljs.highlightElement(code)
      if(code.classList.contains("language-bash")){
        copyCode(code)
      }
      code.light = true
    }
  }
}

const observer = new MutationObserver(highlight)

const init = () => {
  highlight()
  observer.observe(document, { childList: true, subtree: true })
}

setTimeout(init, 0)

})()
