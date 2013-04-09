/**
 * Module dependencies
 */
var express = require("express")
  , auth = require("../auth")
  , db = require("simple-db")("watchers");

/**
 * Expose the app
 */
var app = module.exports = express();

/**
 * Configure
 */
app.configure(function() {
  app.use(function(req, res, next) {
    req.metric = req.metric.context({db:"watch"});
    next();
  });
});

/**
 * Get a product watch info
 *
 * @api admin
 */
app.get("/:product", auth.acl("admin"), function(req, res, next) {
  var done = req.metric.profile("response_time", {action: "product_watchers"});

  db.getAll(req.params.product, function(err, response) {
    // Stop the profiling
    if(err) done({err: err.message});
    done();

    if(err) return next(err);

    // TODO build a hypermedia response
    res.send(response.body);
  });
});

/**
 * Add another watch to a product
 *
 * @api protected
 */
app.post("/:product", auth.acl("user"), function(req, res, next) {
  // They are unwatching this item
  if(req.body.action === "unwatch") return next("route");

  var done = req.metric.profile("response_time", {action: "watch"});

  db.put(req.params.product, req.user.id, "1", function(err) {
    // Stop the profiling
    if(err) done({err: err.message});
    done();

    if(err) return next(err);
    res.send(204);
    app.emit("watch", id);
  });
});

/**
 * Remove a user from a watch list
 *
 * @api protected
 */
app.post("/:product", auth.acl("user"), function(req, res, next) {
  var done = req.metric.profile("response_time", {action: "unwatch"});

  db.remove(req.params.product, req.user.id, function(err) {
    // Stop the profiling
    if(err) done({err: err.message});
    done();

    if(err) return next(err);
    res.send(204);
    app.emit("unwatch", id);
  });
});
