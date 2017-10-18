"use strict";

var GitHubApi = require("github");
var fs = require('fs');

// read the credentials:
var creds = JSON.parse(fs.readFileSync('github_credentials.json', 'utf8'));

// Setup the GitHub API

var github = new GitHubApi({
  //debug: true,
  headers: {
      "Accept": "application/vnd.github.v3+json"
  }
});

// Authentical using token set in env:

var token = creds.token;

if (! token) {
    console.log("WARNING: Env variable not set: LAMBDOG_GITHUB_TOKEN");
}
github.authenticate({
    type: "oauth",
    token: token
});

// Helper function to export to purescript-aff
var makeExport1 = function(f){
    return function(o){
        return function(onError, onSuccess) {
            f(o, function(err, res){
                if (err != null) {
                    onError(err);
                } else {
                    onSuccess(res);
                }
                // Return a canceler, which is just another Aff effect.
                return function (cancelError, cancelerError, cancelerSuccess) {
                    // now way to cancel this?
                    cancelerSuccess(); // invoke the success callback for the canceler
                };
            });
        };
    };
};

exports._issuesGetForRepo = makeExport1(github.issues.getForRepo);
exports._pullRequestsGetReviews = makeExport1(github.pullRequests.getReviews);
exports._issuesGetComments = makeExport1(github.issues.getComments);

exports._reposGetContent = makeExport1(github.repos.getContent);
