module Tetris.Component.HTML.Header where

import Prelude

import Tetris.Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Tetris.Data.Profile (ProfileRep)
import Tetris.Data.Route (Route(..))
import Tetris.Data.Username as Username
import Data.Maybe (Maybe, isNothing)
import Data.Monoid (guard)
import Halogen.HTML as HH

header :: forall i p r. Maybe { | ProfileRep r } -> Route -> HH.HTML i p
header currentUser route =
  HH.nav
    [ css "navbar navbar-light" ]
    [ HH.div
        [ css "container" ]
        [ HH.a
            [ css "navbar-brand"
            , safeHref Home
            ]
            [ HH.text "conduit" ]
        , HH.ul
            [ css "nav navbar-nav pull-xs-right" ]
            [ navItem Home
                [ HH.text "Home" ]
            , maybeElem currentUser \profile ->
                navItem (Profile profile.username)
                  [ HH.text $ Username.toString profile.username
                  ]
            , whenElem (isNothing currentUser) \_ ->
                navItem Login
                  [ HH.text "Log in" ]
            , whenElem (isNothing currentUser) \_ ->
                navItem Signup
                  [ HH.text "Sign up" ]
            ]
        ]
    ]

  where
  navItem r html =
    HH.li
      [ css "nav-item" ]
      [ HH.a
          [ css $ "nav-link" <> guard (route == r) " active"
          , safeHref r
          ]
          html
      ]
