# Workly
Simple & Clean PureScript Web Worker API

## Who can / should use this?
This should hypothetically be usable in any modern browser,
I have successfully run the test suite in Firefox 85 and Chrome 83.

## How to use
Suppose we have a requirement that we write a web worker that we can send messages to,
and it immediately echoes those messages back (via [postMessage]).

We would write it in the workly way like so:

<details>
  <summary>click to expand example</summary>
  

`./src/Worker.Echo.purs`
```purescript
module Worker.Echo where

import Prelude

import Effect (Effect)
import Effect.Worker (Worker)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Worker.Link (parentLink)
import Effect.Aff.Link as Link
import Control.Monad.Rec.Class (forever)

type EchoWorker = Worker String String

main :: Effect Unit
main = launchAff_ $ do
         -- create a message link that allows us to:
         -- - wait for messages from the parent thread
         -- - send messages back
         link <- parentLink :: Aff (Link.Link String String)
         _ <- forever do
                        msg <- Link.recv link -- wait for msg
                        Link.send msg link    -- echo msg back
         pure unit
```

`./src/Worker.Echo.js`
```js
// see comment in `./src/Main.js` for explanation on JS files
const {main} = require('./index');

main();
```

`./src/Main.purs`
```purescript
module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)

import Worker.Echo (EchoWorker)

import Effect.Aff.Link as Link

foreign import spawnWorker :: Effect EchoWorker

main :: Effect Unit
main = launchAff_ do
  worker <- spawnWorker
  link   <- Link.workerLink worker
  Link.send "hello!" link
  resp   <- Link.recv link
  -- resp should be "hello!"
```

`./src/Main.js`
```js
// This is required because bundlers need to have a static path in a `new Worker()` invocation
// in order to link the files properly, and Purescript doesn't give us a way to:
//   - write a non-Main binary (hence `Worker.Echo.js` that just invokes `main`)
//   - statically link / get relpaths to other PS modules (hence this file that links directly to `Worker.Echo.js`)
// 
// Forcing you to write & manage foreign modules is far from an ideal solution to me,
// but given these issues this was the best solution I could find.
//
// If you have a workaround, please file an issue or open a PR!
exports.spawnWorker = function() {
  return new Worker('../Workers.Echo/foreign.js');
};
```
</details>

### Running / Developing
This project uses `spago` for development, and `npm` for scripts & tool management.

#### Setup
```sh
npm ci
```

#### Building
```sh
spago -x test.dhall build
```

#### Running Tests
First, in a terminal window build & host the mocha test runner with:
```sh
npm run test:start
```

Then, run the tests with the UI by visiting `localhost:1234` in your browser,
or in another terminal run the tests with `mocha-headless-chrome` using:
```sh
npm test
```

#### What is `test.dhall`?
A spago package configuration with test files and test deps included,
running `spago -x test.dhall build` will build files in `src` as well as `test`.

## Contributing
This repo uses the regular ole fork process:
- Check out the [issues](https://www.github.com/cakekindel/purescript-workly/issues)
for some work to be done
- Comment on it something to the tune of "I can do this!"
- Fork this repository
- Solve it, make sure the code still builds and passes tests
- Open a Pull Request to merge your changes into this repository

[postMessage]: https://developer.mozilla.org/en-US/docs/Web/API/Worker/postMessage
