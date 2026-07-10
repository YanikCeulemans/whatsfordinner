module App.Home where

import Prelude

import App.FormField (FormField(..))
import App.FormField as FormField
import App.Layout as Layout
import App.RemoteData (RemoteData(..))
import App.Shared (eventTargetInputValue, preventDefault)
import App.Shared as S
import Capabilities.Resource.ManageSpaces (class ManageSpaces)
import Capabilities.Resource.ManageSpaces as ManageSpaces
import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Data.String.NonEmpty as NonEmptyString
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.ULID as DULID
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
  { spaces :: Map SpaceId Space
  , spaceIdField :: FormField
  , acceptedSpace :: RemoteData String Space
  }

data State
  = NotInitialized
  | Initialized InitializedState

data Action
  = Initialize
  | ClickedAccept Event
  | SpaceIdChanged Event

loadSpace
  :: forall m
   . ManageSpaces m
  => MonadState State m
  => State
  -> m (Maybe Space)
loadSpace = case _ of
  NotInitialized -> pure Nothing
  Initialized state -> do
    let
      spaceIdCandidate =
        FormField.validFieldValue state.spaceIdField
          # map DULID.parse
          # map Either.hush
          # join
          # map Id.MkId
    H.put $ Initialized $ state { acceptedSpace = Loading }
    spaceCandidate <- join <$> traverse ManageSpaces.loadSpace spaceIdCandidate
    H.put $ Initialized $ state
      { acceptedSpace = case spaceCandidate of
          Nothing -> Error "No such space exists"
          Just space -> Success space
      }
    pure spaceCandidate

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

upsertSpace :: Space -> State -> State
upsertSpace space =
  case _ of
    NotInitialized -> NotInitialized
    Initialized state -> Initialized $
      state { spaces = upsert state.spaces }
  where
  upsert spaces = Map.insert space.id space spaces

clearSpaceIdField :: State -> State
clearSpaceIdField = case _ of
  NotInitialized -> NotInitialized
  Initialized state -> Initialized $ state { spaceIdField = Pristine }

spaceView :: forall m. Space -> H.ComponentHTML Action () m
spaceView space =
  HH.li [ HP.class_ $ H.ClassName "no-list-style" ]
    [ HH.article
        [ HP.class_ $ H.ClassName "flex spaced items-center transparent-border"
        ]
        [ HH.span [ HP.class_ $ H.ClassName "select-description" ]
            [ S.link
                (Route.SpaceRoute space.id Route.Schedule)
                [ HH.text $ NonEmptyString.toString space.name ]
            ]
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
      H.modify_ \_ -> Initialized
        { spaces:
            Map.fromFoldable $ map (\space -> Tuple space.id space) spaces
        , spaceIdField: FormField.Pristine
        , acceptedSpace: NotRequested
        }

    ClickedAccept event -> do
      preventDefault event
      spaceCandidate <- loadSpace =<< H.get
      for_ spaceCandidate \space -> do
        H.raise $ SpaceSelected space
        H.modify_ $ upsertSpace space >>> clearSpaceIdField

    SpaceIdChanged event -> do
      spaceIdValue <- eventTargetInputValue event
      traverse_ (H.modify_ <<< updateSpaceIdField) spaceIdValue

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main' (Layout.defaultMainConfig { spaceId = Nothing }) $
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
                              , HP.autocomplete HP.AutocompleteOff
                              , HP.value $ FormField.fieldValue
                                  initializedState.spaceIdField
                              ]
                            , FormField.ariaValidity
                                initializedState.spaceIdField
                            ]
                        )
                    , HH.button
                        ( join
                            [ [ HP.type_ HP.ButtonSubmit
                              , HP.disabled $ isAcceptDisabled initializedState
                              ]
                            , S.ariaBusy' initializedState.acceptedSpace
                            ]
                        )
                        [ HH.text "Accept" ]
                    ]
                , HH.small [ HP.class_ $ H.ClassName "white-space-pre-wrap" ]
                    [ HH.text $
                        case initializedState.spaceIdField of
                          Invalid _ ->
                            "Please enter a valid space id"
                          _ ->
                            ensureHeightIsRenderedText
                            where
                            ensureHeightIsRenderedText = " "
                    ]
                ]
            , HH.span [ HP.class_ $ H.ClassName "white-space-pre-wrap" ]
                [ HH.text $
                    case initializedState.acceptedSpace of
                      Error e -> e
                      _ ->
                        ensureHeightIsRenderedText
                        where
                        ensureHeightIsRenderedText = " "
                ]
            , HH.h2_ [ HH.text "Saved spaces" ]
            , HH.ul [ HP.class_ $ H.ClassName "no-padding select-list" ] $
                map spaceView initializedState.spaces
                  # Map.values
                  # Array.fromFoldable
            ]
    where
    isAcceptDisabled { spaceIdField, acceptedSpace } =
      case spaceIdField, acceptedSpace of
        Invalid _, Loading -> true
        _, _ -> false

