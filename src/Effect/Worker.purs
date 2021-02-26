module Effect.Worker where

import Prelude

import Effect (Effect)

--| A relpath to a module containing worker code
newtype ModulePath = ModulePath String

--| Type binding for the `Worker` class, parameterized over
--| the request and response object types.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker)
foreign import data Worker :: Type -> Type -> Type
