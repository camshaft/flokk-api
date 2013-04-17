
module.exports = function(state) {
  return new Hal(state);
};

function Hal (state) {
  this.state = state || {};
}

Hal.prototype.prop = function(key, value) {
  this.state[key] = value;
  return this;
};

Hal.prototype.link = function(rel, link) {
  if(!this.state._links) this.state._links = {};
  if(typeof link === "string") link = {href: link};

  if(this.state._links[rel]) {
    if (typeof this.state._links[rel].push === "undefined") {
      this.state._links[rel] = [this.state._links[rel]];
    }
    this.state._links[rel].push(link);
  }
  else {
    this.state._links[rel] = link;
  }

  return this;
};

Hal.prototype.form = function(name, method, action, fields) {
  if(!this.state._forms) this.state._forms = {};
  this.state._forms[name] = {
    method: method,
    action: action,
    fields: fields
  };
  return this;
};

Hal.prototype.embedded = function(name, obj) {
  if(!this.state._embedded) this.state._embedded = {};
  if(!this.state._embedded[name]) this.state._embedded[name] = [];
  this.state._embedded[name].push(obj);
  return this;
};

Hal.prototype.toJSON = function() {
  return this.state;
};
