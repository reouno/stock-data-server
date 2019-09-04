# stock-data-server

## Requirements

- [Haskell stack](https://docs.haskellstack.org/)

## Build

```
# clone this repo
git clone https://github.com/reouno/stock-data-server.git

# build
cd stock-data-server
stack build
```

## Run

```
stack exec stock-data-server-exe
```

### check running API

```
curl -XGET 127.0.0.1:8080
```

You get response of "Hi, I'm Stock API server.".

this is pull request test.

this is pull request test 2nd commit.
