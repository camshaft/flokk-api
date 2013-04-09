/**
 * Module dependencies
 */
var express = require("express")
  , Batch = require("batch")
  , auth = require("../auth")
  , hal = require("../hal")
  , products = require("simple-db")("products")
  , watch = require("simple-db")("watchers");

/**
 * Expose the app
 */
var app = module.exports = express();

/**
 * Add a product to the system
 *
 * @api admin
 */
app.post("/", auth.acl('admin'), function(req, res, next) {
  // TODO validate input

  products
    .post("products")
    .send(req.body)
    .end(function(err, id) {
      if(err) return next();

      res.redirect(req.resolve(id));
    });
});

/**
 * Get a product product
 *
 * @api public
 */
app.get("/:product", function(req, res, next) {
  var product = res.locals.product;

  var item = hal()
    .link("self", req.resolve(req.params.product))
    .link("root", req.resolve(".."))
    .link("image", product.image)

  // TODO check if the item is in stock
  item
    .form("addToCart", "POST", req.resolve("..","cart",req.user.cart), {
      quantity: {type: "number", value: 1},
      product: {type: "hidden", value: req.params.product}
    });

  item
    .prop("title", product.title)
    .prop("description", product.description)
    .prop("sale", product.sale)
    .prop("retail", product.retail)
    .prop("currency", product.currency)
    .prop("end", product.end)
    .prop("watchers", (product.watchers || []).length);

  var watchersUrl = req.resolve("..","watchers",req.params.product);

  if(~(product.watchers || []).indexOf(req.user.id)) item.form("unwatch", "POST", watchersUrl, {action: "unwatch"});
  else item.form("watch", "POST", watchersUrl, {action: "watch"});

  if(req.user.admin) item.link("watchers", watchersUrl);

  res.send(item);
});

app.param("product", function(req, res, next, id) {
  var batch = new Batch;

  batch.push(function(done) {
    products
      .get("products", id)
      .end(function(err, response) {
        done(err, response.body);
      });
  });
  batch.push(function(done) {
    watch
      .count(id)
      .end(function(err, response) {
        done(err, response.body);
      });
  });

  batch.end(function(err, results) {
    if(err) return next(err);

    res.locals.product = results;
    next();
  });
});
