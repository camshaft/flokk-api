/**
 * Module dependencies.
 */
var passport = require('passport')
  , LocalStrategy = require('passport-local').Strategy
  , BasicStrategy = require('passport-http').BasicStrategy
  , ClientPasswordStrategy = require('passport-oauth2-client-password').Strategy
  , BearerStrategy = require('passport-http-bearer').Strategy
  , clients = require('simple-db')("clients")
  , tokens = require('simple-db')("tokens")
  , users = require('simple-db')("users");

/**
 * LocalStrategy
 *
 * This strategy is used to authenticate users based on a username and password.
 * Anytime a request is made to authorize an application, we must ensure that
 * a user is logged in before asking them to approve the request.
 */
passport.use(new LocalStrategy(
  function(username, password, done) {
    users
      .get("users", username)
      .end(function(err, user) {
        if(err) return done(err);
        if(!user) return done(null, false);
        // TODO check password
        
        user.body.id = username;
        done(null, user.body);
      });
  }
));

passport.serializeUser(function(user, done) {
  done(null, user.id);
});

passport.deserializeUser(function(id, done) {
  users
    .get("users", id)
    .end(function(err, user) {
      if(err) return done(err);
      if(!user) return done(null, false);

      user.body.id = id;
      done(null, user.body);
    });
});

/**
 * BasicStrategy & ClientPasswordStrategy
 *
 * These strategies are used to authenticate registered OAuth clients.  They are
 * employed to protect the `token` endpoint, which consumers use to obtain
 * access tokens.  The OAuth 2.0 specification suggests that clients use the
 * HTTP Basic scheme to authenticate.  Use of the client password strategy
 * allows clients to send the same credentials in the request body (as opposed
 * to the `Authorization` header).  While this approach is not recommended by
 * the specification, in practice it is quite common.
 */
passport.use(new BasicStrategy(
  function(id, secret, done) {
    client
      .get("clients", id)
      .end(function(err, client) {
        if(err) return done(err);
        if(!client) return done(null, false);
        if(client.body.clientSecret !== secret) return done(null, false);

        client.body.id = id;
        done(null, client.body);
      });
  }
));

passport.use(new ClientPasswordStrategy(
  function(id, secret, done) {
    client
      .get("clients", id)
      .end(function(err, client) {
        if(err) return done(err);
        if(!client) return done(null, false);
        if(client.body.clientSecret !== secret) return done(null, false);

        client.body.id = id;
        done(null, client.body);
      });
  }
));

/**
 * BearerStrategy
 *
 * This strategy is used to authenticate users based on an access token (aka a
 * bearer token).  The user must have previously authorized a client
 * application, which is issued an access token to make requests on behalf of
 * the authorizing user.
 */
passport.use(new BearerStrategy(
  function(accessToken, done) {
    tokens
      .get("tokens", accessToken)
      .end(function(err, token) {
        if(err) return done(err);
        if(!token) return done(null, false);

        users
          .get("users", token.userID)
          .end(function(err, user) {
            if(err) return done(err);
            if(!user) return done(null, false);

            var info = {scope: "*"};
            user.body.id = token.userID;
            done(null, user.body, info);
          });
      });
  }
));
