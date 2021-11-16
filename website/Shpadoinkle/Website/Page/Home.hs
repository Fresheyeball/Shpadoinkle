{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}


module Shpadoinkle.Website.Page.Home where


import           Control.Lens                                   ((^.))
import           Data.Generics.Labels                           ()
import           Data.Text                                      (Text)
import           GHC.Generics                                   (Generic)
import           Prelude                                        hiding (div,
                                                                 span)
import           Shpadoinkle.Html                               as H hiding
                                                                     (title)
import           Shpadoinkle.Html.TH.AssetLink                  (assetLink)
import           Shpadoinkle.Lens                               (onRecord,
                                                                 onRecordEndo)
import           Shpadoinkle.Router                             (MonadJSM)
import           Shpadoinkle.Website.Component.LiveExample      as Live (example)
import           Shpadoinkle.Website.Style                      as Style
import           Shpadoinkle.Website.Types.Effects.Example      (ExampleEffects)
import           Shpadoinkle.Website.Types.Example              (Example)
import           Shpadoinkle.Website.Types.Home                 (ExampleLens,
                                                                 Examples, Home)
import           Shpadoinkle.Website.Types.Nav                  (Nav (NGettingStarted),
                                                                 toRoute)
import           Shpadoinkle.Website.Types.Route                (Route (RGettingStarted))
import           Shpadoinkle.Website.Types.Route.GettingStarted (Route (RGSIndex))
import           Shpadoinkle.Website.Types.SPA                  (goTo)


view :: (MonadJSM m, ExampleEffects m) => Home -> Html m Home
view x = h "main" [] [ hero', featureSection, onRecordEndo #examples componentsSection x ]


heroTitle :: Text -> Html m a
heroTitle = p [ class' hero_title ] . pure . text


hero' :: MonadJSM m => Html m a
hero' = div_
  [ div [ class' hero ]
    [ div [ class' hero_title_wrapper ] $ heroTitle <$>
      [ "A new Functional UI"
      , "programming"
      , "paradigm"
      ]
    , div [ class' hero_button ]
      [ div [ class' $ split_button <> hero_button__content ]
        [ a (class' split_button__button : goTo (toRoute NGettingStarted))
          [ span [ class' $ split_button__split1 <> hero_button__split ]
            [ "Get Started" ]
          , span [ class' $ split_button__split2 <> hero_button__split ]
            [ img' [ alt "", src $(assetLink "/assets/right-arrow-white.svg") ]
            ]
          ]
        ]
      ]
    ]
  , img' [ width 2112, height 996, alt "", class' hero_bg,        src $(assetLink "/assets/hero_image.svg") ]
  , img' [ width 322,  height 379, alt "", class' hero_bg_mobile, src $(assetLink "/assets/mobile/hero_image.svg") ]
  ]


data FeatureCard = FeatureCard
  { card'icon        :: Text
  , card'title       :: Text
  , card'description :: Text
  }


featureCard :: FeatureCard -> Html m a
featureCard fc = div [ class' feature__card ]
  [ div [ class' feature__card__content ]
    [ div [ class' feature__card__heading ]
      [ img' [ alt "", src $ card'icon fc ]
      , p [ class' feature__card__title ]
        [ text $ card'title fc ]
      ]
    , p [ class' feature__card__description ]
      [ text $ card'description fc ]
    ]
  ]


features :: [FeatureCard]
features =
  [ FeatureCard
    { card'icon        = $(assetLink "/assets/fast_icon.svg")
    , card'title       = "Fast"
    , card'description = "Shpadoinkle does little work. The renderer is modular, so you can always benefit from the latest advances in virtual DOM rendering."
    }
  , FeatureCard
    { card'icon        = $(assetLink "/assets/declarative_icon.svg")
    , card'title       = "Declarative"
    , card'description = "Your Shpadoinkle code is high-level. You need not worry about low-level details, causality, or when DOM nodes get replaced."
    }
  , FeatureCard
    { card'icon        = $(assetLink "/assets/composable_icon.svg")
    , card'title       = "Composable"
    , card'description = "Shpadoinkle avoids elaborate passing of messages and payloads. Components are highly composable with Lenses"
    }
  , FeatureCard
    { card'icon        = $(assetLink "/assets/reliable_icon.svg")
    , card'title       = "Reliable"
    , card'description = "Shpadoinkle UIs are composed of components with no side-effects. Runtime errors are exceedingly rare. Code is easy to test because model updates are pure functions."
    }
  , FeatureCard
    { card'icon        = $(assetLink "/assets/lorem_ipsum_logo.svg")
    , card'title       = "Type Safe"
    , card'description = "Shpadoinkle facilitates type safe UI code. Everything from client server communication with Servant, to compile time checked asset paths, is designed with types in mind."
    }
  ]


featureSection :: forall m a. MonadJSM m => Html m a
featureSection = div [ class' feature__section ]
  [ div [ class' feature__wrapper ]
    [ p [ class' feature__title ]
      [ "A programming model for declarative user interface." ]
    , div [ class' feature__cards ] $ featureCard <$> features
    ]
  , div [ class' feature_button ]
    [ div [ class' $ split_button <> feature_button__content ]
      [ a (class' split_button__button : goTo (RGettingStarted RGSIndex))
        [ span [ class' $ split_button__split1 <> feature_button__split ]
          [ "Checkout the getting started guide" ]
        , span [ class' $ split_button__split2 <> feature_button__split ]
          [ img' [ alt "", src $(assetLink "/assets/right-arrow.svg") ]
          ]
        ]
      ]
    ]
  , img' [ alt "", class' transition, src $(assetLink "/assets/orange_purple_transition.svg") ]
  ]


data ComponentExample = ComponentExample
  { title'      :: Text
  , description :: Text
  } deriving Generic


componentExample
  :: (MonadJSM m, ExampleEffects m)
  => ExampleLens -> Examples Example -> ComponentExample -> Html m (Examples Example)
componentExample l exs cex = onRecord l $ div [ class' component ]
  [ div [ class' component__info ]
    [ h2 [ class' component__info__title ]
      [ text $ cex ^. #title' ]
    , p_
      [ text $ cex ^. #description ]
    ]
  , example l (exs ^. l)
  ]


componentsSection :: (MonadJSM m, ExampleEffects m) => Examples Example -> Html m (Examples Example)
componentsSection exs = div [ class' components__section ]
  [ componentExample #helloWorld exs $ ComponentExample
    { title'      = "Hello World"
    , description = "Shpadoinkle components are expressed as functions, no classes, inheritance, or JSX style new syntax. Purity of view logic is guaranteed by the type system."
    }
  , componentExample #counter exs $ ComponentExample
    { title'      = "State is easy"
    , description = "Components are simply functions that accept a state argument to render. Event listeners provide updates to the state. No props management, digest cycle, hooks, dispatch, or other such ceremony."
    }
  , componentExample #todo exs $ ComponentExample
    { title'      = "An Application"
    , description = "Using event handlers, and pure functions we can compose applications without any further abstraction."
    }
  , img' [ alt "", className "transition", src $(assetLink "/assets/purple_black_transition.svg") ]
  ]
