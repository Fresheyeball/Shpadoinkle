{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | This module provides a DSL of HTML elements
-- This DSL is entirely optional. You may use the 'Html' constuctors
-- provided by Shpadoinkle core and completely ignore this module.
-- You can write your code `h` style and not use this module. But for
-- those who like a typed DSL with named functions for different tags
-- this is for you.
--
-- Each HTML element comes in 4 flavors. Delicous flavors. Plain (IE 'div'),
-- prime (IE 'div''), underscore (IE 'div_'), and both (IE 'div_''). The following should hold
--
-- @
--   x [] = x'
--   flip x [] = x_
--   x [] [] = x'_
--   h "x" = x
-- @
--
-- So plain versions like 'div' are for cases where we care about properties
-- as well as children. `div\'' is for cases where we care about children
-- but not properties. And 'div_' is for cases where we care about properties
-- but not children.
--
-- Due to 'OverloadedStrings' this yields a pleasent DSL
--
-- @
--  div "foo" [ "hiya" ]
--  > <div class="foo"\>hiya</div\>
-- @


module Shpadoinkle.Html.Element where


import           Control.Monad       (msum)
import           Prelude             hiding (div, head, span)

import           Shpadoinkle
import           Shpadoinkle.Html.TH


$(msum <$> mapM mkElement
  [ "h1"
  , "h2"
  , "h3"
  , "h4"
  , "h5"
  , "h6"
  , "p"
  , "br"
  , "hr"
  , "abbr"
  , "address"
  , "b"
  , "bdi"
  , "bdo"
  , "big"
  , "blockquote"
  , "center"
  , "cite"
  , "code"
  , "del"
  , "dfn"
  , "em"
  , "font"
  , "i"
  , "ins"
  , "kbd"
  , "mark"
  , "meter"
  , "pre"
  , "progress"
  , "q"
  , "rp"
  , "rt"
  , "ruby"
  , "s"
  , "samp"
  , "small"
  , "strike"
  , "strong"
  , "sub"
  , "sup"
  , "time"
  , "tt"
  , "u"
  , "var"
  , "wbr"
  , "form"
  , "input"
  , "textarea"
  , "button"
  -- , "select"
  -- , "optgroup"
  -- , "option"
  , "label"
  , "fieldset"
  , "legend"
  , "datalist"
  , "keygen"
  , "output"
  , "frame"
  , "frameset"
  , "noframes"
  , "iframe"
  , "img"
  , "area"
  , "canvas"
  , "figcaption"
  , "figure"
  , "a"
  , "link"
  , "nav"
  , "ul"
  , "ol"
  , "li"
  , "dir"
  , "dl"
  , "dt"
  , "dd"
  , "menu"
  , "menuitem"
  , "table"
  , "caption"
  , "th"
  , "tr"
  , "td"
  , "thead"
  , "tbody"
  , "tfoot"
  , "col"
  , "colgroup"
  , "style"
  , "div"
  , "span"
  , "header"
  , "footer"
  , "main"
  , "section"
  , "article"
  , "aside"
  , "details"
  , "dialog"
  , "summary"
  , "head"
  , "meta"
  , "base"
  , "basefont"
  , "script"
  , "noscript"
  , "applet"
  , "embed"
  , "object"
  , "param"
  ])
