##Invoice UI Challenge

This is a small project done as part of the process of interviewing at [IronSight](ironsight.ca).

### Assumptions made
* all users are using dollars of some sort rather than some other currency
* browser floating point accuracy is sufficient for this purpose, (AKA users won't care about rounding to the nearest penny)
* having the extra csv download feature use defaulting to 0 instead of, say, producing an error message, is fine
* if the user puts quotes in the invoice description, then if the CSV won't parse in excel etc., it's on them.
