module Shpadoinkle.Html.Element
  ( text , h1 , h1_ , h2 , h2_ , h3 , h3_ , h4 , h4_ , h5 , h5_ , h6 , h6_ , p , p_ , br , br_ , hr , hr_ , abbr , abbr_ , address , address_ , b , b_ , bdi , bdi_ , bdo , bdo_ , big , big_ , blockquote , blockquote_ , center , center_ , cite , cite_ , code , code_ , del , del_ , dfn , dfn_ , em , em_ , font , font_ , i , i_ , ins , ins_ , kbd , kbd_ , mark , mark_ , meter , meter_ , pre , pre_ , progress , progress_ , q , q_ , rp , rp_ , rt , rt_ , ruby , ruby_ , s , s_ , samp , samp_ , small , small_ , strike , strike_ , strong , strong_ , sub , sub_ , sup , sup_ , time , time_ , tt , tt_ , u , u_ , var , var_ , wbr , wbr_ , form , form_ , input , input_ , textarea , textarea_ , button , button_ , select , select_ , optgroup , optgroup_ , option , option_ , label , label_ , fieldset , fieldset_ , legend , legend_ , datalist , datalist_ , keygen , keygen_ , output , output_ , frame , frame_ , frameset , frameset_ , noframes , noframes_ , iframe , iframe_ , img , img_ , area , area_ , canvas , canvas_ , figcaption , figcaption_ , figure , figure_ , audio , audio_ , source , source_ , track , track_ , video , video_ , a , a_ , link , link_ , nav , nav_ , ul , ul_ , ol , ol_ , li , li_ , dir , dir_ , dl , dl_ , dt , dt_ , dd , dd_ , menu , menu_ , menuitem , menuitem_ , table , table_ , caption , caption_ , th , th_ , tr , tr_ , td , td_ , thead , thead_ , tbody , tbody_ , tfoot , tfoot_ , col , col_ , colgroup , colgroup_ , style' , style_ , div , div_ , span , span_ , header , header_ , footer , footer_ , main , main_ , section , section_ , article , article_ , aside , aside_ , details , details_ , dialog , dialog_ , summary , summary_ , head , head_ , meta , meta_ , base , base_ , basefont , basefont_ , script , script_ , noscript , noscript_ , applet , applet_ , embed , embed_ , object, object_, param, param_
  , module Shpadoinkle.Class
  , node
  ) where


import           Prelude           hiding (Element, div, head, link, span)
import           Shpadoinkle.Class


type Element = forall m o. [(Text, Prop m o)] -> [Html m o] -> Html m o
type Element_ = forall m o. [Html m o] -> Html m o
type ElementV = forall m o. [(Text, Prop m o)] -> Html m o
type ElementV_ = forall m o. Html m o


node :: Text -> Element
node = Node
text :: Text -> Html m o
text = TextNode
h1 :: Element
h1 = node "h1"
h1_ :: Element_
h1_ = h1 []
h2 :: Element
h2 = node "h2"
h2_ :: Element_
h2_ = h2 []
h3 :: Element
h3 = node "h3"
h3_ :: Element_
h3_ = h3 []
h4 :: Element
h4 = node "h4"
h4_ :: Element_
h4_ = h4 []
h5 :: Element
h5 = node "h5"
h5_ :: Element_
h5_ = h5 []
h6 :: Element
h6 = node "h6"
h6_ :: Element_
h6_ = h6 []
p :: Element
p = node "p"
p_ :: Element_
p_ = p []
br :: ElementV
br = flip (node "br") []
br_ :: ElementV_
br_ = br []
hr :: Element
hr = node "hr"
hr_ :: Element_
hr_ = hr []
abbr :: Element
abbr = node "abbr"
abbr_ :: Element_
abbr_ = abbr []
address :: Element
address = node "address"
address_ :: Element_
address_ = address []
b :: Element
b = node "b"
b_ :: Element_
b_ = b []
bdi :: Element
bdi = node "bdi"
bdi_ :: Element_
bdi_ = bdi []
bdo :: Element
bdo = node "bdo"
bdo_ :: Element_
bdo_ = bdo []
big :: Element
big = node "big"
big_ :: Element_
big_ = big []
blockquote :: Element
blockquote = node "blockquote"
blockquote_ :: Element_
blockquote_ = blockquote []
center :: Element
center = node "center"
center_ :: Element_
center_ = center []
cite :: Element
cite = node "cite"
cite_ :: Element_
cite_ = cite []
code :: Element
code = node "code"
code_ :: Element_
code_ = code []
del :: Element
del = node "del"
del_ :: Element_
del_ = del []
dfn :: Element
dfn = node "dfn"
dfn_ :: Element_
dfn_ = dfn []
em :: Element
em = node "em"
em_ :: Element_
em_ = em []
font :: Element
font = node "font"
font_ :: Element_
font_ = font []
i :: Element
i = node "i"
i_ :: Element_
i_ = i []
ins :: Element
ins = node "ins"
ins_ :: Element_
ins_ = ins []
kbd :: Element
kbd = node "kbd"
kbd_ :: Element_
kbd_ = kbd []
mark :: Element
mark = node "mark"
mark_ :: Element_
mark_ = mark []
meter :: Element
meter = node "meter"
meter_ :: Element_
meter_ = meter []
pre :: Element
pre = node "pre"
pre_ :: Element_
pre_ = pre []
progress :: Element
progress = node "progress"
progress_ :: Element_
progress_ = progress []
q :: Element
q = node "q"
q_ :: Element_
q_ = q []
rp :: Element
rp = node "rp"
rp_ :: Element_
rp_ = rp []
rt :: Element
rt = node "rt"
rt_ :: Element_
rt_ = rt []
ruby :: Element
ruby = node "ruby"
ruby_ :: Element_
ruby_ = ruby []
s :: Element
s = node "s"
s_ :: Element_
s_ = s []
samp :: Element
samp = node "samp"
samp_ :: Element_
samp_ = samp []
small :: Element
small = node "small"
small_ :: Element_
small_ = small []
strike :: Element
strike = node "strike"
strike_ :: Element_
strike_ = strike []
strong :: Element
strong = node "strong"
strong_ :: Element_
strong_ = strong []
sub :: Element
sub = node "sub"
sub_ :: Element_
sub_ = sub []
sup :: Element
sup = node "sup"
sup_ :: Element_
sup_ = sup []
time :: Element
time = node "time"
time_ :: Element_
time_ = time []
tt :: Element
tt = node "tt"
tt_ :: Element_
tt_ = tt []
u :: Element
u = node "u"
u_ :: Element_
u_ = u []
var :: Element
var = node "var"
var_ :: Element_
var_ = var []
wbr :: Element
wbr = node "wbr"
wbr_ :: Element_
wbr_ = wbr []
form :: Element
form = node "form"
form_ :: Element_
form_ = form []
input :: ElementV
input = flip (node "input") []
input_ :: ElementV_
input_ = input []
textarea :: Element
textarea = node "textarea"
textarea_ :: Element_
textarea_ = textarea []
button :: Element
button = node "button"
button_ :: Element_
button_ = button []
select :: Element
select = node "select"
select_ :: Element_
select_ = select []
optgroup :: Element
optgroup = node "optgroup"
optgroup_ :: Element_
optgroup_ = optgroup []
option :: Element
option = node "option"
option_ :: Element_
option_ = option []
label :: Element
label = node "label"
label_ :: Element_
label_ = label []
fieldset :: Element
fieldset = node "fieldset"
fieldset_ :: Element_
fieldset_ = fieldset []
legend :: Element
legend = node "legend"
legend_ :: Element_
legend_ = legend []
datalist :: Element
datalist = node "datalist"
datalist_ :: Element_
datalist_ = datalist []
keygen :: Element
keygen = node "keygen"
keygen_ :: Element_
keygen_ = keygen []
output :: Element
output = node "output"
output_ :: Element_
output_ = output []
frame :: Element
frame = node "frame"
frame_ :: Element_
frame_ = frame []
frameset :: Element
frameset = node "frameset"
frameset_ :: Element_
frameset_ = frameset []
noframes :: Element
noframes = node "noframes"
noframes_ :: Element_
noframes_ = noframes []
iframe :: Element
iframe = node "iframe"
iframe_ :: Element_
iframe_ = iframe []
img :: Element
img = node "img"
img_ :: Element_
img_ = img []
area :: ElementV
area = flip (node "area") []
area_ :: ElementV_
area_ = area []
canvas :: Element
canvas = node "canvas"
canvas_ :: Element_
canvas_ = canvas []
figcaption :: Element
figcaption = node "figcaption"
figcaption_ :: Element_
figcaption_ = figcaption []
figure :: Element
figure = node "figure"
figure_ :: Element_
figure_ = figure []
audio :: Element
audio = node "audio"
audio_ :: Element_
audio_ = audio []
source :: Element
source = node "source"
source_ :: Element_
source_ = source []
track :: Element
track = node "track"
track_ :: Element_
track_ = track []
video :: Element
video = node "video"
video_ :: Element_
video_ = video []
a :: Element
a = node "a"
a_ :: Element_
a_ = a []
link :: Element
link = node "link"
link_ :: Element_
link_ = link []
nav :: Element
nav = node "nav"
nav_ :: Element_
nav_ = nav []
ul :: Element
ul = node "ul"
ul_ :: Element_
ul_ = ul []
ol :: Element
ol = node "ol"
ol_ :: Element_
ol_ = ol []
li :: Element
li = node "li"
li_ :: Element_
li_ = li []
dir :: Element
dir = node "dir"
dir_ :: Element_
dir_ = dir []
dl :: Element
dl = node "dl"
dl_ :: Element_
dl_ = dl []
dt :: Element
dt = node "dt"
dt_ :: Element_
dt_ = dt []
dd :: Element
dd = node "dd"
dd_ :: Element_
dd_ = dd []
menu :: Element
menu = node "menu"
menu_ :: Element_
menu_ = menu []
menuitem :: Element
menuitem = node "menuitem"
menuitem_ :: Element_
menuitem_ = menuitem []
table :: Element
table = node "table"
table_ :: Element_
table_ = table []
caption :: Element
caption = node "caption"
caption_ :: Element_
caption_ = caption []
th :: Element
th = node "th"
th_ :: Element_
th_ = th []
tr :: Element
tr = node "tr"
tr_ :: Element_
tr_ = tr []
td :: Element
td = node "td"
td_ :: Element_
td_ = td []
thead :: Element
thead = node "thead"
thead_ :: Element_
thead_ = thead []
tbody :: Element
tbody = node "tbody"
tbody_ :: Element_
tbody_ = tbody []
tfoot :: Element
tfoot = node "tfoot"
tfoot_ :: Element_
tfoot_ = tfoot []
col :: ElementV
col = flip (node "col") []
col_ :: ElementV_
col_ = col []
colgroup :: Element
colgroup = node "colgroup"
colgroup_ :: Element_
colgroup_ = colgroup []
style' :: Element
style' = node "style"
style_ :: Element_
style_ = style' []
div :: Element
div = node "div"
div_ :: Element_
div_ = div []
span :: Element
span = node "span"
span_ :: Element_
span_ = span []
header :: Element
header = node "header"
header_ :: Element_
header_ = header []
footer :: Element
footer = node "footer"
footer_ :: Element_
footer_ = footer []
main :: Element
main = node "main"
main_ :: Element_
main_ = main []
section :: Element
section = node "section"
section_ :: Element_
section_ = section []
article :: Element
article = node "article"
article_ :: Element_
article_ = article []
aside :: Element
aside = node "aside"
aside_ :: Element_
aside_ = aside []
details :: Element
details = node "details"
details_ :: Element_
details_ = details []
dialog :: Element
dialog = node "dialog"
dialog_ :: Element_
dialog_ = dialog []
summary :: Element
summary = node "summary"
summary_ :: Element_
summary_ = summary []
head :: Element
head = node "head"
head_ :: Element_
head_ = head []
meta :: Element
meta = node "meta"
meta_ :: Element_
meta_ = meta []
base :: ElementV
base = flip (node "base") []
base_ :: ElementV_
base_ = base []
basefont :: Element
basefont = node "basefont"
basefont_ :: Element_
basefont_ = basefont []
script :: Element
script = node "script"
script_ :: Element_
script_ = script []
noscript :: Element
noscript = node "noscript"
noscript_ :: Element_
noscript_ = noscript []
applet :: Element
applet = node "applet"
applet_ :: Element_
applet_ = applet []
embed :: Element
embed = node "embed"
embed_ :: Element_
embed_ = embed []
object :: Element
object = node "object"
object_ :: Element_
object_ = object []
param :: Element
param = node "param"
param_ :: Element_
param_ = param []
