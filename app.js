/**
 * Module dependencies.
 */
var stack = require('simple-stack-api')
  , simpleDB = require('simple-db')
  , hal = require("./lib/hal");

/**
 * Expose the app
 */
var app = module.exports = stack();

/**
 * Initialize the database
 */
app.configure("development", function() {


  function db(req, res) {
    // Noop
    var links = req.key === "links" ? {friend: {bucket: "bucket", key: "key"}} : {};
    res.emit("response", {
      metadata: {
        links: links
      },
      data: "",
      key: "key",
      error: (req.key === "error" ? new Error("test") : null)
    });
    ["this", "is", "a", "test"].forEach(function (data) {
      res.emit("data", data);
    })
    res.emit("end");
  };


  simpleDB.use("products", db);
  simpleDB.use("watchers", db);
});

app.configure("production", function() {

});

/**
 * Routes
 */
app.configure(function() {

  app.useBefore("router", function user(req, res, next) {
    // Get the user data
    req.user = {
      id: "-user-id-",
      cart: "-cart-id-",
      name: "Cameron",
      avatar: "http://placehold.it/250x250",
      cards: {
        "Discover": "kl;ajsdf0jq23rioasdf09j"
      }
    };
    next();
  });

  app.useBefore("router", "/auth", "auth", require("./lib/auth"));
  app.useBefore("router", "/cart", "cart", require("./lib/cart"));
  app.useBefore("router", "/categories", "categories", require("./lib/categories"));
  app.useBefore("router", "/products", "products", require("./lib/products"));
  app.useBefore("router", "/users", "users", require("./lib/users"));
  app.useBefore("router", "/watchers", "watchers", require("./lib/watchers"));
});

app.get("/", function(req, res, next) {

  var obj = hal()
    .link("self", req.resolve(req.url))
    .link("sales", req.resolve("categories","sales"))
    .link("cart", req.resolve("cart",req.user.cart))
    .link("categories", req.resolve("categories"))

  if(req.user.name) obj.link("profile", req.resolve("users",req.user.id));
  else obj.link("signup", req.resolve("auth/signup"));

  res.send(obj);
});
