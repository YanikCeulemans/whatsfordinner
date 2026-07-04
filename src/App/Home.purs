module App.Home where

import Prelude

import App.FormField (FormField(..))
import App.FormField as FormField
import App.Layout as Layout
import App.Shared (eventTargetInputValue, preventDefault)
import Capabilities.Resource.ManageSpaces (class ManageSpaces)
import Capabilities.Resource.ManageSpaces as ManageSpaces
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NonEmptyString
import Data.Traversable (traverse_)
import Data.ULID as DULID
import Debug as Debug
import Domain.Id as Id
import Domain.Space (Space)
import Domain.SpaceId (SpaceId)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Web.Event.Event (Event)

theSpaceId :: SpaceId
theSpaceId =
  Id.MkId $ Either.fromRight' crash $ DULID.parse "01KNW48VB0PNCFC0KZ8SW289ZA"
  where
  crash _ = unsafeCrashWith "invalid space id ULID"

data HomeOutput
  = InviteAccepted SpaceId
  | SpaceSelected Space

type HomeSlot query = H.Slot query HomeOutput

type InitializedState =
  { spaces :: Array Space
  , spaceIdField :: FormField
  }

data State
  = NotInitialized
  | Initialized InitializedState

data Action
  = Initialize
  | ClickedAccept Event
  | ClickedSelect Space
  | SpaceIdChanged Event

updateSpaceIdField :: String -> State -> State
updateSpaceIdField spaceIdCandidate = case _ of
  NotInitialized -> NotInitialized
  Initialized state ->
    Initialized $ state { spaceIdField = updatedSpaceIdField }
    where
    updatedSpaceIdField =
      case DULID.parse spaceIdCandidate of
        Left _ -> FormField.Invalid spaceIdCandidate
        Right _ -> FormField.Valid spaceIdCandidate

spaceView :: forall m. Space -> H.ComponentHTML Action () m
spaceView space =
  HH.li [ HP.class_ $ H.ClassName "no-list-style" ]
    [ HH.article
        [ HP.class_ $ H.ClassName "flex spaced items-center transparent-border"
        ]
        [ HH.span [ HP.class_ $ H.ClassName "select-description pointer" ]
            [ HH.text $ NonEmptyString.toString space.name ]
        , HH.button
            [ HE.onClick $ const $ ClickedSelect space
            ]
            [ HH.text "Select" ]
        ]
    ]

component
  :: forall query input m
   . MonadAff m
  => ManageSpaces m
  => H.Component query input HomeOutput m
component =
  H.mkComponent
    { initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize, handleAction = handleAction }
    }

  where
  initialState :: input -> State
  initialState _ = NotInitialized

  handleAction
    :: forall slots. Action -> H.HalogenM State Action slots HomeOutput m Unit
  handleAction = case _ of
    Initialize -> do
      spaces <- ManageSpaces.loadSpaces
      H.modify_ \_ -> Initialized { spaces, spaceIdField: FormField.Pristine }

    ClickedAccept event -> do
      preventDefault event
      H.raise $ InviteAccepted theSpaceId

    ClickedSelect space -> do
      H.raise $ SpaceSelected space

    SpaceIdChanged event -> do
      spaceIdValue <- eventTargetInputValue event
      traverse_ (H.modify_ <<< updateSpaceIdField) spaceIdValue

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main' (Layout.defaultMainConfig { includeFooter = false }) $
      case state of
        NotInitialized -> HH.p_ [ HH.text "loading" ]
        Initialized initializedState ->
          HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
            [ HH.h1_ [ HH.text "What's for dinner" ]
            , HH.h2_ [ HH.text "Space shared with you" ]
            , HH.form [ HE.onSubmit ClickedAccept ]
                [ HH.fieldset
                    ( join
                        [ [ HP.attr (H.AttrName "role") "group" ]
                        , FormField.ariaValidity
                            initializedState.spaceIdField
                        ]
                    )
                    [ HH.input
                        ( join
                            [ [ HP.type_ HP.InputText
                              , HP.name "space-id"
                              , HP.placeholder "Enter space id"
                              , HE.onChange SpaceIdChanged
                              ]
                            , FormField.ariaValidity
                                initializedState.spaceIdField
                            ]
                        )
                    , HH.input
                        [ HP.type_ HP.InputSubmit
                        , HP.value "Accept"
                        , HP.disabled $ isAcceptDisabled initializedState
                        ]
                    ]
                , HH.small [ HP.class_ $ H.ClassName "white-space-pre-wrap" ]
                    [ case initializedState.spaceIdField of
                        Invalid _ -> HH.text "Please enter a valid space id"
                        _ -> HH.text " "
                    ]
                ]
            , HH.h2_ [ HH.text "Saved spaces" ]
            , HH.ul [ HP.class_ $ H.ClassName "no-padding select-list" ] $
                map spaceView initializedState.spaces
            ]
    where
    isAcceptDisabled initializedState =
      case initializedState.spaceIdField of
        Invalid _ -> true
        _ -> false

