## Invoice UI Challenge

This is a small project done as part of the process of interviewing at [IronSight](ironsight.ca).

[Live version](https://ryan1729.github.io/ironsight-invoice-challenge/)

### Assumptions made
* all users are using dollars of some sort rather than some other currency
* browser floating point accuracy is sufficient for this purpose, (AKA users won't care about rounding to the nearest penny)
* having the extra csv download feature use defaulting to 0 instead of, say, producing an error message, is fine
* if the user puts quotes in the invoice description, then if the CSV won't parse in excel etc., it's on them.

### Setup

##### First time
```sh
npm install
```

##### Filewatcher
```sh
npm run start
```
Go to http://localhost:8000/src/Main.elm

##### Release
```sh
npm run build-release
```
This overwrites the `elm.js` file in the `docs` directory
