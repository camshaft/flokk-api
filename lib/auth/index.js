/**
 * Module dependencies
 */
var express = require("express")
  , hal = require("../hal");

/**
 * Expose the app
 */
var app = module.exports = express();

app.acl = function(name) {
  return function(req, res, next) {
    // TODO check to see if the user is a part of the group
    next();
  };
};

/**
 * Routes
 */
app.get("/captcha/:id.png", function(req, res, next){
  // TODO send an image
});

app.get("/signup", randomCaptcha, function(req, res, next) {
  var obj = hal()
    .link("self", req.resolve("signup"))
    .link("root", req.resolve(".."))
    // .link("captcha", req.resolve("captcha",res.locals.captcha+".png"))
    .link("captcha", "http://placehold.it/250x250")
    .form("signup", "POST", req.resolve("signup"), {
      captcha: {type: "hidden", value: res.locals.captcha},
      name: {type: "text"},
      password: {type: "password"},
      email: {type: "email"},
      captcha_response: {type: "text"}
    });

  res.send(obj);
});

app.post("/signup", function(req, res, next) {
  // TODO handle a user posting some signup info
  res.send(503);
});

function randomCaptcha(req, res, next) {
  // TODO get a random captcha id
  res.locals.captcha = 1;
  next();
};
