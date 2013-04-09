/**
 * Module dependencies
 */
var express = require("express")
  , hal = require("../hal");

/**
 * Expose the app
 */
var app = module.exports = express();

/**
 * Routes
 */
app.get("/:user", function(req, res, next){
  // It's not the logged in user
  if(req.user.id !== req.params.user) return next('route');

  var user = hal()
    .link("self", req.resolve("."+req.url))
    .link("root", req.resolve(".."))
    .link("avatar", req.user.avatar)
    .link("history", req.resolve(req.user.id,"history"));

  user
    .prop("name", req.user.name)
    .prop("address", req.user.address);

  res.send(user);
});

app.get("/:user", function(req, res, next){
  // TODO fetch the user info
  res.send({});
});

/**
 * Retrieve user purchase history
 */
// app.get("/:user/history", function(req, res, next) {});
