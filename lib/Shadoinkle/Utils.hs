{-# LANGUAGE QuasiQuotes #-}


module Shpadoinkle.Utils (setTitle) where


import           Language.Javascript.JSaddle
import qualified NeatInterpolation           as NI


setTitle :: Text -> JSM ()
setTitle t = void $ eval [NI.text| document.title = "$t"; |]
