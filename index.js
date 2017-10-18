'use strict';

const lambdog = require('./output/Main');

exports.http = (request, response) => {
    console.log("am here");
    console.log("num of args: " + lambdog.wow(request)(response).length);
    lambdog.wow(request)(response)();
    //response.status(200).send(lambdog.testy(request.body.name));
};

exports.event = (event, callback) => {
  callback();
};
