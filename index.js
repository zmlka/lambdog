'use strict';

const lambdog = require('./output/Main');

exports.http = (request, response) => {
    console.log("HELLO STARTING");
    lambdog.wowzaEff(request)(response)();
    console.log("BYE ENDING");
    //response.status(200).send(lambdog.testy(request.body.name));
};

exports.event = (event, callback) => {
  callback();
};
