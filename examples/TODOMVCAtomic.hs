{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import           Control.Lens                     (Lens', set, view, (%~), (&),
                                                   (.~), (?~), (^.), _Unwrapped,
                                                   _Wrapped)
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


focus2 :: Functor m => (a -> b -> Html m (a, b)) -> Lens' s a -> Lens' s b -> s -> Html m s
focus2 f x y z = (\(j,g) -> set x j $ set y g z) <$> f (view x z) (view y z)


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
    , button' [ className "destroy", onClick $ m & tasks %~ filter ((/= tid) . _taskId) ]
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
  , ul "filters" $ fmap (\v -> model & visibility .~ v) . filterHtml (model ^. visibility) <$> [minBound..maxBound]
  ] ++ (if count Complete (_tasks model) == 0 then [] else
  [ button [ className "clear-completed", onClick $ clearComplete model ] [ "Clear completed" ]
  ])



info :: Html m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: MonadJSM m => Description -> [Task] -> Html m (Description, [Task])
newTaskForm desc ts = form [ className "todo-form", onSubmit (appendItem desc ts) ]
  [ input' [ className "new-todo"
           , value $ desc ^. _Wrapped
           , onInput $ \d -> (d ^. _Unwrapped, ts)
           , placeholder "What needs to be done?" ]
  ]

newTaskForm' :: MonadJSM m => Description -> Html m (Model -> Model)
newTaskForm' = memo $ \desc -> form [ className "todo-form", onSubmit appendItem' ]
  [ input' [ className "new-todo"
           , value $ desc ^. _Wrapped
           , onInput . set $ current . _Wrapped
           , placeholder "What needs to be done?" ]
  ]

todoList :: MonadJSM m => Model -> Html m Model
todoList model = ul "todo-list" $ taskView model <$> toVisible (_visibility model) (_tasks model)


toggleAllBtn :: Applicative m => Model -> [Html m Model]
toggleAllBtn model =
  [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox", onChange (toggleAll model) ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


apply :: Functor m => a -> Html m (a -> b) -> Html m b
apply m v = ($ m) <$> v



render :: MonadJSM m => Model -> Html m Model
render m = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ], apply m . newTaskForm' $ m ^. current ]
    : htmlIfTasks m
    [ section "main" $ toggleAllBtn m ++ [ todoList m ]
    , listFooter m
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

