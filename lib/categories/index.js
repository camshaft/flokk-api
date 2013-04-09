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
app.get("/", function(req, res, next) {
  // TODO get the categories from the database
  var categories = ["home", "bath", "bed"];

  var obj = hal()
    .link("self", req.resolve("."+req.url))
    .link("root", req.resolve(".."));

  categories.forEach(function(category) {
    obj.link(category, req.resolve(category));
  });

  res.send(obj);
});

app.get("/:category", function(req, res, next) {

  var obj = hal()
    .link("self", req.resolve("."+req.url))
    .link("subscribe", req.resolve("."+req.url).replace("http://", "ws://"))
    .link("root", req.resolve(".."));

  res.locals.category.items.forEach(function(product) {
    var item = hal()
      .link("self", {title: product.title, href: req.resolve("../products",product.id)})
      .link("image", product.image);

    obj.embedded("items", item);
  });

  res.send(obj);
});

app.param("category", function(req, res, next, id) {
  // TODO get the category from the database
  res.locals.category = {
    id: id,
    items: [
      {
        id: 1,
        image: "http://placehold.it/250x250",
        title: "Couch",
        sale: 99.99,
        retail: 1000,
        currency: "USD",
        end: Date.now()+10000,
        watchers: [1,2,3,4]
      }
    ],
    title: "Home"
  };
  next();
});
