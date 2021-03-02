# Workly
Simple & Clean PureScript Web Worker API

## Who can / should use this?
This should hypothetically be usable in any modern browser,
I have successfully run the test suite in Firefox 85 and Chrome 83.

## Running / Developing
This project uses `spago` for development, and `npm` for scripts.

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
