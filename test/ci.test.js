var should = require("should")
  , request = require("supertest")
  , parse = require("url").parse
  , app = require("..");

should.Assertion.prototype.links = function(test) {
  var resource = this.obj;
  should.exist(resource._links);
  resource._links.should.be.an.object;
  resource._links.should.have.ownProperty('self');
  resource._links.self.should.have.ownProperty('href');
  Array.prototype.splice.call(arguments, 0).forEach(function(link) {
    resource._links.should.have.ownProperty(link);
    // It's an array
    if(resource._links[link].length) {
      resource._links[link].forEach(function(sublink) {
        sublink.should.have.ownProperty("href");
      });
    }
    // It's an object
    else {
      resource._links[link].should.have.ownProperty('href');
    }
  });
  return this;
};

describe("api.theflokk.com", function(){

  var resources = {
    _: {_links: {"root": {href: "/"}}}
  };

  var methods = init(resources)
    , follow = methods.follow
    , followEmbedded = methods.followEmbedded
    , submit = methods.submit;

  beforeEach(follow("_", "root"));

  it("should have a correct rendering", function() {
    resources.root.should.have.links("sales", "cart", "categories", "profile");
  });

  describe("-> sales", function(){
    beforeEach(follow("root", "sales"));

    it("should have a correct rendering", function() {
      resources.sales.should.have.links("root", "subscribe");
    });
  });

  describe("-> cart", function(){
    beforeEach(follow("root", "cart"));

    it("should have a correct rendering", function() {
      resources.cart.should.have.links("root");
    });

    describe("-> clear", function(){
      beforeEach(submit("cart", "clear"));

      it("should clear the cart", function() {
        // TODO check that the cart was cleared
      });
    });

    describe("-> checkout", function(){
      beforeEach(submit("cart", "checkout"));

      it("should checkout the cart", function() {
        // TODO check that the cart was cleared
      });
    });
  });

  describe("-> categories", function(){
    beforeEach(follow("root", "categories"));

    it("should have a correct rendering", function() {
      resources.categories.should.have.links("root");
    });

    // TODO is this a good idea?
    after(function() {

      Object.keys(resources.categories._links).forEach(function(rel) {
        if(~["root", "self"].indexOf(rel)) return;
        describe("-> category."+rel, function(){

          beforeEach(follow("categories", rel));

          it("should have a correct rendering", function() {
            resources[rel].should.have.links("root", "subscribe");
          });

          after(function() {
            resources[rel]._embedded.items.forEach(function(item, idx) {

              describe("-> categories."+rel+".items."+idx, function(){
                beforeEach(followEmbedded(rel, "items", idx));

                it("should work", function() {
                  resources["items"][idx].should.have.links("root", "subscribe", "image");
                });
              });

            });
          });
        });
      });

    });
  });

  describe("-> profile", function(){
    beforeEach(follow("root", "profile"));

    it("should have a correct rendering", function() {
      resources.profile.should.have.links("root", "avatar", "history");
    });
  });

});

function init(resources) {
  function follow(parent, rel) {
    return function (done) {
      var href = parse(resources[parent]._links[rel].href).path;

      request(app)
        .get(href)
        .end(function(err, res) {
          if(err) return done(err);
          if(!res.ok) return done(new Error(res.text));
          resources[rel] = res.body;
          done();
        });
    }
  }

  function followEmbedded(parent, item, idx) {
    resources[item] = resources[item] || {};
    return function (done) {
      var href = parse(resources[parent]._embedded[item][idx]._links.self.href).path;

      request(app)
        .get(href)
        .end(function(err, res) {
          if(err) return done(err);
          if(!res.ok) return done(new Error(res.text));
          resources[item][idx] = res.body;
          done();
        });
    }
  }

  function submit(parent, form, data) {
    if(!data) data = {};

    resources[parent+"-forms"] = resources[parent+"-forms"] || {};

    return function(done) {
      var href = parse(resources[parent]._forms[form].action).path
        , method = resources[parent]._forms[form].method.toLowerCase()
        , fields = resources[parent]._forms[form].fields;

      Object.keys(fields).forEach(function(name) {
        if(!data[name]) data[name] = fields[name].value;
      });

      request(app)
        [method](href)
        .send(data)
        .end(function(err, res) {
          if(err) return done(err);
          if(!res.ok) return done(new Error(res.text));
          resources[parent+"-forms"][form] = res.body;
          done();
        });
    };
  }

  return {follow:follow, followEmbedded:followEmbedded, submit:submit};
};
