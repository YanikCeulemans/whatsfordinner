module App.Home where

import Prelude

import App.Layout as Layout
import Capabilities.Resource.ManageSpaces (class ManageSpaces)
import Capabilities.Resource.ManageSpaces as ManageSpaces
import Data.Either as Either
import Data.Maybe (Maybe(..))
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

theSpaceId :: SpaceId
theSpaceId =
  Id.MkId $ Either.fromRight' crash $ DULID.parse "01KNW48VB0PNCFC0KZ8SW289ZA"
  where
  crash _ = unsafeCrashWith "invalid space id ULID"

data HomeOutput
  = InviteAccepted SpaceId
  | SpaceSelected SpaceId

type HomeSlot query = H.Slot query HomeOutput

type InitializedState =
  { spaces :: Array Space
  }

data State
  = NotInitialized
  | Initialized InitializedState

data Action
  = Initialize
  | ClickedAccept
  | ClickedSelect

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
      H.modify_ \_ -> Initialized { spaces }

    ClickedAccept -> do
      H.raise $ InviteAccepted theSpaceId

    ClickedSelect -> do
      H.raise $ SpaceSelected theSpaceId

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main $
      case state of
        NotInitialized -> HH.p_ [ HH.text "loading" ]
        Initialized _initializedState ->
          HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
            [ HH.h1_ [ HH.text "What's for dinner" ]
            , HH.button [ HE.onClick $ const $ ClickedAccept ]
                [ HH.text "accept" ]
            , HH.button [ HE.onClick $ const $ ClickedSelect ]
                [ HH.text "select" ]
            , HH.text "TODO"
            ]

