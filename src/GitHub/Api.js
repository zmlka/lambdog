"use strict";

var GitHubApi = require("github");

var uname = "dummyname";
var pword = "dummypass";

var github = new GitHubApi({
  debug: true,
  headers: {
      "Accept": "application/vnd.github.v3+json"
  }
});

github.authenticate({
        type: "basic",
        username: uname,
        password: pword
});

exports._getFollowers = function(username){
    return function(onError, onSuccess) {
        github.issues.getForRepo({ owner: "zmlka", repo: "lambdog"}, function(err, res){
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
