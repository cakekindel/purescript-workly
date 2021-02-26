module Effect.Worker where

import Prelude

import Effect (Effect)

--| Type binding for the Worker class, parameterized over
--| the request and response object types.
--|
--| Uses JSON (de)serialization for encoding messages across
--| the worker boundary.
foreign import data Worker :: Type -> Type -> Type
