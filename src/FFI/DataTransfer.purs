module FFI.DataTransfer (DragOperation(..), setEffectAllowed) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Web.HTML.Event.DataTransfer (DataTransfer)

data DragOperation
  --| The item may not be dropped.
  = None
  -- | A copy of the source item may be made at the new location.
  | Copy
  -- | A copy or link operation is permitted.
  | CopyLink
  --| A copy or move operation is permitted.
  | CopyMove
  --| A link may be established to the source at the new location.
  | Link
  -- | A link or move operation is permitted.
  | LinkMove
  -- | An item may be moved to a new location.
  | Move
  -- | All operations are permitted.
  | All
  -- | The default value when the effect has not been set, equivalent to all.
  | Uninitialized

instance Show DragOperation where
  show = case _ of
    None -> "none"
    Copy -> "copy"
    CopyLink -> "copyLink"
    CopyMove -> "copyMove"
    Link -> "link"
    LinkMove -> "linkMove"
    Move -> "move"
    All -> "all"
    Uninitialized -> "uninitialized"

foreign import _setEffectAllowedImpl :: EffectFn2 String DataTransfer Unit

setEffectAllowed :: DragOperation -> DataTransfer -> Effect Unit
setEffectAllowed dragOperation =
  runEffectFn2 _setEffectAllowedImpl stringifiedDragOperation
  where
  stringifiedDragOperation = show dragOperation
