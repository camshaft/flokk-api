flokk-api
=========

Concepts
--------

### Access Token

The access token uses simple-secrets to encrypt the following data in the actual key:

* Scopes for the user i.e. `["user.read", "user.update"]` with key `s`
* UserID with key `i`

We want to avoid putting more data in there because that translates to a longer access token.
