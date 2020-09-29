function p(){
  hpack $1 | ack --passthru 'generated' &
}
p core
p backends/static
p backends/snabbdom
p backends/pardiff
p console
p html
p router
p lens
p widgets
p examples
p tests
