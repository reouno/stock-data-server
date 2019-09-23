# 📈stock-data-server📉

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
# just test if the server is working
curl -XGET 127.0.0.1:8080
```

You get response of "Hi, I'm Stock API server.".

You can test also adding stock data and getting them like following examples.

```
# example of adding new stock data
curl -XPOST -H 'Accept: application/json' -H 'Content-type: application/json' -d '{"tickerSymbol":"AMZN","stockName":"Amazon.com", "priceType":"D1","priceTimestamps":["2019-09-19","2019-09-20"],"prices":[{"closePrice":1721.50,"openPrice":null,"highPrice":null,"lowPrice":null},{"closePrice":1821.50,"openPrice":1821.71,"highPrice":1830.63,"lowPrice":1780.92}]}' 127.0.0.1:8080
--> some int number which is the stock ID (brand ID)

# example of getting stock data by specifing stock ID
curl -XGET 127.0.0.1:8080/1
```

## Dependency graph

Aiming [Clean Architecture](https://www.amazon.com/Clean-Architecture-Craftsmans-Software-Structure/dp/0134494164), still under improvement.

![data/modules.png](https://github.com/reouno/stock-data-server/blob/develop/data/modules.png)
