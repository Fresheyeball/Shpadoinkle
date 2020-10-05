var inited = false
function init(){
  if(inited) return
  inited = true
  // const codes = document.getElementsByTagName("code")
  const codes = document.getElementsByClassName("language-bash")
  for(i = 0; i < codes.length; i++){ copyCode(codes[i]) }
}
init()
window.addEventListener("load", init)

function copyCode(code){
  code.title = "Click To Copy"
  code.addEventListener("click", () => {
    const textArea = document.createElement('textarea')
    textArea.style.position = "absolute"
    textArea.style.left = "-99999px"
    textArea.textContent = code.textContent
    document.body.append(textArea)
    textArea.select()
    document.execCommand("copy")
    document.body.removeChild(textArea)
  })
}
