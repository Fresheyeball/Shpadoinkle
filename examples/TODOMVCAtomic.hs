{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import           Control.Lens                     (Lens', set, view, (%~), (&),
                                                   (.~), (?~), (^.), _Wrapped)
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


focus :: forall a b s. (a -> b) -> Lens' s a -> Lens' s b -> s -> s
focus f x y z = set y (f $ view x z) z


filterHtml :: Applicative m => Visibility -> Visibility -> Html m Visibility
filterHtml = memo2 $ \cur item -> li_
  [ a (href "#" : onClick item : [className "selected" | cur == item]) [ text . pack $ show item ]
  ]


htmlIfTasks :: Model -> [Html m a] -> [Html m a]
htmlIfTasks m h' = if Prelude.null (_tasks m) then [] else h'


taskView :: MonadJSM m => Model -> Task -> Html m Model
taskView m = memo $ \(Task (Description d) c tid) ->
  li [ id' . pack . show $ tid ^. _Wrapped
     , className . Set.fromList $ [ "completed" | c == Complete ]
                               ++ [ "editing"   | Just tid == m ^. editing ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , className "toggle"
             , onChange $ toggleCompleted m tid
             , checked $ c == Complete
             ]
    , label [ onDblclick (m & editing ?~ tid) ] [ text d ]
    , button' [ className "destroy", onClick (m & tasks %~ removeTask tid) ]
    ]
  , form [ onSubmit $ m & editing .~ Nothing ]
    [ input' [ className "edit"
             , value d
             , onInput $ updateTaskDescription m tid . Description
             , autofocus True
             , onBlur $ m & editing .~ Nothing
             ]
    ]
  ]


listFooter :: Applicative m => Model -> Html m Model
listFooter model = footer "footer" $
  [ Shpadoinkle.Html.span "todo-count" $ let co = count Incomplete $ _tasks model in
    [ strong_ [ text . pack $ show co ]
    , text $ " item" <> (if co == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ fmap (\v -> model { _visibility = v })
                <$> (filterHtml (_visibility model) <$> [minBound..maxBound])
  ] ++ (if count Complete (_tasks model) == 0 then [] else
  [ button [ className "clear-completed", onClick $ clearComplete model ] [ "Clear completed" ]
  ])



info :: Html m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: MonadJSM m => Model -> Html m Model
newTaskForm model = form [ className "todo-form", onSubmit (appendItem model) ]
  [ input' [ className "new-todo"
           , value $ model ^. current . _Wrapped
           , onInput $ flip (set current) model . Description
           , placeholder "What needs to be done?" ]
  ]


todoList :: MonadJSM m => Model -> Html m Model
todoList model = ul "todo-list" $ taskView model <$> toVisible (_visibility model) (_tasks model)


toggleAllBtn :: Applicative m => Model -> [Html m Model]
toggleAllBtn model =
  [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox", onChange (toggleAll model) ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


render :: MonadJSM m => Model -> Html m Model
render model = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ], newTaskForm model ]
    : htmlIfTasks model
    [ section "main" $ toggleAllBtn model ++ [ todoList model ]
    , listFooter model
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

