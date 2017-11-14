# LAMBDOG

A serverless bot for rich management and approval of GitHub Pull Requests. It allows you to set approval conditions for Pull Requests, only merging them in after they have been met.

Key features:

- Serverless
- Triggered by WebHooks
- Complex PR approval conditions (eg: one member of group "developers" and two "operations**)
- Text-file based configuration stored on GitHub
- Controlled through comments
- Strong audit trail

For an example of this this in action, check PRs in [Lambdog's repo]().

----

## Why

If you are going full devops, there are two interesting new things going on in your workflow: everything is code, and everything is automated.

A byproduct of this is that you probably have everything on GitHub. Your application code, your Terraform templates, your kubernetes configuration, etc, with everything being connected to a Continuous Integration service that automatically deploys to production.

The modern workflow is:

1. Commit on GitHub
1. CI runs tests
1. If successful, deploy
1. Commit on GitHub
1. CI runs tests
1. etc

In the real world, you need more checks and balances than satisfying tests. You need a **strict approval process** and **audit trail**.

Lambdog was made because GitHub's native approver functionality is not powerful enough.

If your project has sufficient complexity, is in an enterprise or regulated environment, GitHub's built in approve feature might not cut it.

The advantage of having all interactions and configurations stored in GitHub is that you *are already using GitHub*. Nothing new to take care of.

## How it works

1. Configuration files sepcifying approve conditions are stored in a GitHub Repo
1. When a new PR or PR comment (create, edit, detete) takes place, Lambdog is triggered
1. If conditions are met Lambdog will merge in the PR.


## Configuring



## Installing


### How to build

- Install NPM.
- Install Purescript: w/
  ```
  npm install -g purescript pulp bower
  ```
- Install dependencies and build:
  ```
  npm install
  bower install
  pulp build
  ```

### Deploy

See [serverless documentation](https://serverless.com/framework/docs/providers/google/guide/quick-start/).

Deploy as google cloud function:

```
serverless deploy
```

(needs authentication setup)

### How to deploy locally for testing

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
