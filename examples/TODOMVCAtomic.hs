{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import           Control.Lens                     (set, to, (%~), (.~), (?~),
                                                   (^.), _Wrapped)
import qualified Data.Set                         as Set
import           Data.Text                        hiding (count, filter, length)
import           Prelude                          hiding (div, unwords)
import           Shpadoinkle
import           Shpadoinkle.Backend.Snabbdom
import           Shpadoinkle.Html                 hiding (main)
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Html.Memo
import           Shpadoinkle.Html.Utils
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp
#endif

import           TODOMVCAtomic.Types
import           TODOMVCAtomic.Update


filterHtml :: Applicative m => Eq v => Show v => v -> v -> Html m v
filterHtml = memo $ \cur item -> li_
  [ a (href "#" : onClick item : [className "selected" | cur == item]) [ text . pack $ show item ]
  ]


htmlIfTasks :: [b] -> [Html m a] -> [Html m a]
htmlIfTasks m h' = if Prelude.null m then [] else h'


taskView :: MonadJSM m => Maybe TaskId -> Task -> Html m (Model -> Model)
taskView = memo $ \ed (Task (Description d) c tid) ->
  li [ id' . pack . show $ tid ^. _Wrapped
     , className . Set.fromList $ [ "completed" | c == Complete ]
                               ++ [ "editing"   | Just tid == ed ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , className "toggle"
             , onChange $ toggleCompleted tid
             , checked $ c == Complete
             ]
    , label [ onDblclick $ editing ?~ tid ] [ text d ]
   , button' [ className "destroy", onClick $ tasks %~ filter ((/= tid) . _taskId) ]
    ]
  , form [ onSubmit $ editing .~ Nothing ]
    [ input' [ className "edit"
             , value d
             , onInput $ updateTaskDescription tid . Description
             , autofocus True
             , onBlur $ editing .~ Nothing
             ]
    ]
  ]


listFooter :: Applicative m => Int -> Int -> Visibility -> Html m (Model -> Model)
listFooter = memo $ \ic cc v -> footer "footer" $
  [ Shpadoinkle.Html.span "todo-count"
    [ strong_ [ text . pack $ show ic ]
    , text $ " item" <> (if ic == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ fmap (set visibility) . filterHtml v <$> [minBound..maxBound]
  ] ++ (if cc == 0 then [] else
  [ button [ className "clear-completed", onClick clearComplete ] [ "Clear completed" ]
  ])



info :: Html m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: MonadJSM m => Description -> Html m (Model -> Model)
newTaskForm = memo $ \desc -> form [ className "todo-form", onSubmit appendItem' ]
  [ input' [ className "new-todo"
           , value $ desc ^. _Wrapped
           , onInput . set $ current . _Wrapped
           , placeholder "What needs to be done?" ]
  ]


todoList :: MonadJSM m => Maybe TaskId -> Visibility -> [Task] -> Html m (Model -> Model)
todoList = memo $ \ed v ts -> ul "todo-list" $ taskView ed <$> toVisible v ts


toggleAllBtn :: Applicative m => [Html m (Model -> Model)]
toggleAllBtn =
  [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox", onChange toggleAll ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


apply :: Functor m => a -> Html m (a -> b) -> Html m b
apply m v = ($ m) <$> v



render :: MonadJSM m => Model -> Html m Model
render m = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ], apply m . newTaskForm $ m ^. current ]
    : htmlIfTasks (m ^. tasks)
    [ section "main" $ (apply m <$> toggleAllBtn) ++ [ apply m $ todoList (m ^. editing) (m ^. visibility) (m ^. tasks) ]
    , apply m $ listFooter (m ^. tasks . to (count Incomplete))
                           (m ^. tasks . to (count Complete))
                           (m ^. visibility)
    ]
  , info
  ]


app :: JSM ()
app = do
  model <- manageLocalStorage "todo" emptyModel
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.css"
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.2.0/index.css"

  shpadoinkle id runSnabbdom emptyModel model render stage


main :: IO ()
#ifdef ghcjs_HOST_OS
main = app
#else
main = run 8080 app
#endif

