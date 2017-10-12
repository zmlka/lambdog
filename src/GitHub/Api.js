"use strict";

var GitHubApi = require("github");

var uname = "dummyname";
var pword = "dummypass";

var github = new GitHubApi({
  // optional
  debug: true,
  //pathPrefix: "/api/v3", // for some GHEs; none for GitHub
  //protocol: "https",
  //port: 9898,
  //ca: "whatever",
  headers: {
      "Accept": "application/vnd.github.v3+json"
  }
  //requestMedia: "application/vnd.github.something-custom",
  //followRedirects: false, // default: true; there's currently an issue with non-get redirects, so allow disabling follow-redirects
}).authenticate({
    type: "basic",
    username: uname,
    password: pword
});

exports._getFollowers = function(username){
    return function(onError, onSuccess) {
        github.users.getFollowingForUser({ username: username }, function(err, res){
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
