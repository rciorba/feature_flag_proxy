# an http reverse proxy using cowboy and gun

[![Build Status](https://travis-ci.org/rciorba/feature_flag_proxy.svg?branch=master)](https://travis-ci.org/rciorba/feature_flag_proxy)

This project came about while performing and incremental re-write of an old API. The
initial plan was to allow enablind/disabling proxying routes via an API. Said API never
really materialized, but routing based on HTTP method and blue-green deployments have.

# Developing

## Building

This project uses [erlang.mk](https://erlang.mk/)

```
make
```

## Running the tests

```
make tests
```

Personally I like to run the tests on a loop while developing, so `erlang.mk`'s default
behaviour of not treating warnings (like unused variables) is annoying. To disable it
export `MODE` env variable and set it to `development`. TravisCI builds without this, so
there's a safety net.

# Usage
You can use the published docker images or build the release yourself.

## Docker

The image is published to docker rciorba/feature_flag_proxy.

The image will by default read the config from `/rel/config.json`. To change that set
the `FFP_CONFIG` environment variable.

## Configuring
TODO
