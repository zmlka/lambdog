"use strict";

var GitHubApi = require("github");

// Setup the GitHub API

var github = new GitHubApi({
  //debug: true,
  headers: {
      "Accept": "application/vnd.github.v3+json"
  }
});

// Authentical using token set in env:

var token = process.env.LAMBDOG_GITHUB_TOKEN;

if (!token) {
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
