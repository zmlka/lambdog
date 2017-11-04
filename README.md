# lambdog

A watchful GutHub bot that only takes commands from its approved owners.

## How to build

- Install NPM.
- Install Purescript:
  ```
  npm install -g purescript pulp bower
  ```
- Install dependencies and build:
  ```
  npm install
  bower install
  pulp build
  ```

## Deploy

Deploy as google cloud function:

```
serverless deploy
```

(needs authentication setup)

## How to deploy locally for testing

Full instructions at [documentation](https://cloud.google.com/functions/docs/emulator).

```
npm install -g @google-cloud/functions-emulator
functions-emulator start
functions-emulator call http --data='{"json": "payload here"}'
```

View the logs:
```
functions-emulator logs read
```
