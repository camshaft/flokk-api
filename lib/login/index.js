/**
 * Module dependencies.
 */
var express = require('express')
  , passport = require('passport')
  , login = require('connect-ensure-login');

var app = module.exports = express();

app.get("/", function(req, res, next) {
  res.render('login');
});

app.post("/", passport.authenticate('local', {
  successReturnToOrRedirect: '/'
}));
