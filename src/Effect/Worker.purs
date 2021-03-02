module Effect.Worker where

import Prelude

import Data.Newtype (class Newtype)
import Web.Event.Message (MessageEvent)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried ( EffectFn1
                        , EffectFn2
                        , EffectFn3
                        , runEffectFn1
                        , runEffectFn2
                        , runEffectFn3
                        )

--| Type binding for the `Worker` class, parameterized over
--| the message types being sent and received.
--|
--| > How do I create my own `Worker`?
--| 1. Create a worker module like `src/Workers/Hello.purs`:
--|      ```purs
--|      module Workers.Hello where
--|        import Prelude
--|        import Effect (Effect)
--|        import Effect.Console (log)
--|        import Effect.Worker (Worker)
--|
--|        -- I send and receive nothing
--|        type HelloWorker = Worker Unit Unit
--|
--|        main :: Effect Unit
--|        main = log "Hello, world!"
--|      ```
--| 1. Next, create a corresponding FFI module (`src/Workers/Hello.js`):
--|      ```js
--|      // If it weren't for me,
--|      // `main` would not be invoked when you import the worker.
--|      const {main} = require('./index');
--|      main();
--|      ```
--| 1. Next, in the module that will spawn the worker thread (ex. `src/Main.purs`):
--|      ```purs
--|      module Main where
--|        import Prelude
--|        import Effect (Effect)
--|
--|        -- We have to call FFI code so your bundler of choice can
--|        -- transform the path.
--|        foreign import helloWorker :: Effect HelloWorker
--|
--|        main :: Effect Unit
--|        main = do
--|                 _ <- helloWorker
--|                 -- "Hello, World!" is logged
--|                 mempty
--|      ```
--| 1. Finally, we implement the `foreign import helloWorker :: Effect HelloWorker` (`src/Main.js`):
--|      ```js
--|      // Hard-coded paths suck.
--|      // The up-shot is, this should work with:
--|      // - parcel
--|      // - webpack
--|      // - browserify
--|      // - others?
--|      exports.helloWorker = function() {return new Worker("../output/Workers.Hello/foreign.js");}
--|      ```
--|
--| # I know, this sucks.
--| One of the driving goals of this library is to make it as unopinionated as possible, and be compatible with any bundler.
--|
--| The root of this issue is that Workers *must* be constructed with a URL to the JS module to be spawned.
--|
--| This makes compiling & bundling PureScript modules as worker code (in a non-bundler-dependent way) very difficult.
--|
--| If there is another solution for getting the path references to work with _any_ bundler
--| (as this does), I'm open to suggestions at the [project repo](https://github.com/cakekindel/purescript-workly/issues).
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker)
foreign import data Worker :: Type -> Type -> Type

--| Internal binding for `Worker#postMessage`
--|
--| This will cause a listener attached in the child module
--| (by way of `Worker.Child.onMsg`) to fire with this message.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker)
sendMsg :: ∀ req. Worker req _ -> req -> Effect Unit
sendMsg = runEffectFn2 sendMsg_

--| Internal binding for attaching a listener to `Worker#onmessage`.
--|
--| Any previously attached listener will be removed.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker/onmessage)
onMsg :: ∀ res. Worker _ res -> (MessageEvent res -> Effect Unit) -> Effect Unit
onMsg = runEffectFn3 onMsg_ $ unsafePerformEffect

foreign import sendMsg_ :: ∀ req. EffectFn2 (Worker req _) req Unit
foreign import onMsg_   :: ∀ a res. EffectFn3 (Effect a -> a) (Worker _ res) (MessageEvent res -> Effect Unit) Unit
