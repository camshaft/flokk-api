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
 * Get cart
 */
app.get("/:cart", function(req, res, next) {

  var self = req.resolve("."+req.url);

  var cart = hal()
    .link("self", self)
    .link("root", req.resolve(".."));

  var count = 0;

  res.locals.cart.items.forEach(function(product) {
    var item = hal()
      .link("self", req.resolve("..","products",product.id))
      .link("image", product.image)
      .form("update", "POST", self, {
        quantity: {type: "number", value: product.quantity},
        product: {type: "hidden", value: product.id}
      });

    count += product.quantity;

    cart.embedded("items", item);
  });

  cart.prop("items", count);

  cart
    .form("clear", "POST", self, {
      clear: {type: "hidden", value: true}
    });

  if(req.user.cards) {
    var cards = Object.keys(req.user.cards).map(function(name) {
      return {prompt: name, value: req.user.cards[name]};
    });

    cart
      .form("checkout", "POST", req.resolve(req.params.cart,"checkout"), {
        card: {type: "select", options: cards}
      });
  }

  res.send(cart);
});

/**
 * Add item to cart
 */
// app.post("/:cart")

/**
 * Checkout the cart
 */
// app.post("/:cart/checkout")


app.param("cart", function(req, res, next, id) {
  // TOOD call the database
  res.locals.cart = {
    items: [{
      id: 1,
      quantity: 2
    }]
  };
  next();
});
