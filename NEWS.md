# v0.2.1

- Add UTM coordinates to `yieldExtra`.
- Harvested mass in `yieldExtra` is now computed using the reported `flow` and
`cycle` variables, instead of `timelapse`.

# v0.2.0

- Add start-up message.
- Add data for year 2008, 2016-2019.
- Add column `polygon` to the boundaries data.frame to uniquely identify the
polygons inside a given watershed that conform the boundaries.
- Order `yield` and `yieldExtra` data frames by site, year, and record index.
- Fix the curation procedures of the vegetation name for the boundaries files.
Non-perennial crops are now consistenly labeled as "Row crop", whereas
previously they would be named either "Row crop" or "Row Crop".
- Fix site labels. All point-level records are assigned to a site based on their
coordinates whereas previously they would carry the site label included in the
shapefiles, which might not be correct or unique.
- New processing rules: swath is rescaled from inch to foot for years 2007,
2008, 2009, 2010, and 2012.
- New processing rules: distance is rescaled from inch to foot for years 2007,
2008, 2009, 2010, and 2012.
- New processing rules: record index is reexpresed to be always increasing for
each combination of year and site.
- New processing rules: year "2004" is labeled as "2008".
