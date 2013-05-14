/**
 * Module dependencies.
 */
var express = require('express')
  , oauth2orize = require('oauth2orize')
  , passport = require('passport')
  , login = require('connect-ensure-login')
  , clients = require('simple-db')('auth')
  , codes = require('simple-db')('authorization-codes')
  , tokens = require('simple-db')('access-tokens');

/**
 * Create the oauth server
 */
var server = oauth2orize.createServer();

server.serializeClient(function(client, done) {
  return done(null, client.id);
});

server.deserializeClient(function(id, done) {
  clients
    .get("clients", id)
    .end(function(err, client) {
      if(err) return done(err);
      if(!client) return done();

      client.body.id = id;
      return done(null, client.body);
    });
});

server.grant(oauth2orize.grant.code(function(client, redirectURI, user, ares, done) {
  codes
    .post("codes")
    .send({clientID: client.id, redirectURI: redirectURI, userID: user.id})
    .end(function(err, code) {
      if(err) return done(err);

      done(null, code.id);
    });
}));

server.exchange(oauth2orize.exchange.code(function(client, code, redirectURI, done) {
  codes
    .get("codes", code)
    .end(function(err, authCode) {
      if(err) return done(err);
      if(client.id !== authCode.clientID) return done(null, false);
      if(redirectURI !== authCode.redirectURI) return done(null, false);

      tokens
        .post("tokens")
        .send({userID: authCode.userID, clientID: authCode.clientID})
        .end(function(err, token) {
          if(err) return done(err);

          done(null, token.id);
        });
    });
}));

/**
 * Expose the app
 */
var app = module.exports = express();

app.post("/token", [
  passport.authenticate(['basic', 'oauth2-client-password'], { session: false }),
  server.token(),
  server.errorHandler()
]);

app.get("/authorize", [
  login.ensureLoggedIn(),
  server.authorization(function(id, redirectURI, done) {
    clients
      .get("clients", id)
      .end(function(err, client) {
        if(err) return done(err);
        if(!client) return done();

        // TODO check that the redirectURI matches the client's

        client.body.id = id;
        return done(null, client.body, redirectURI);
      });
  }),
  function(req, res){
    res.render('dialog', {
      transactionID: req.oauth2.transactionID,
      user: req.user,
      client: req.oauth2.client
    });
  }
]);

app.get("/authorize/decision", [
  login.ensureLoggedIn(),
  server.decision()
]);
