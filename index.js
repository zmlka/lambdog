'use strict';

const lambdog = require('./output/Main');

exports.http = (request, response) => {
    response.status(200).send(lambdog.testy(request.body.name));
};

exports.event = (event, callback) => {
  callback();
};
