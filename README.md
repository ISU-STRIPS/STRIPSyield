# STRIPSyield
An R package containing STRIPS yield data. To access the dataset from the PNAS paper [1], please run `pnas_data()`.

## Changelog

### v 0.2.0

__VERSION STILL NOT MERGED WITH MASTER BRANCH__

- Add shapefiles 2007-2010 for and keep shapefiles for 2012-15 (v0.1.1).
- Add vignette explaining data structure and curation protocol (see `vignette("data-curation")`).
- Rearrange the whole `data-raw` folder as explained in vignette.
- Add `data-raw/runme.R` script to read, curate, build, and save the data objects (both new and legacy).
- Keep all data objects from STRIPYield v0.1.1.
    - Rename folder `data-raw/original` -> `data-raw/legacy`.
    - Consolidate the three R scripts inside `data_raw` into one new script `data-raw/legacy.R` (no modifications).
    - Consolidate the two R scripts inside `R` into one new script `R/legacy.R` (no modifications).
    - Rename data objects `yield` -> `legacy_yield`, `raw_yield` -> `legacy_raw_yield`, `yield_conversion` -> `legacy_yield_conversion`.
    - Keep the `pnas_data()` function untouched.
    
## References
[1] Lisa A. Schulte, Jarad B. Niemi, Matthew J. Helmers, Matt Liebman, J. G. Arbuckle, David E. James,
Randall K. Kolka, Matthew E. O’Neal, Mark D. Tomer, John C. Tyndall, Heidi Asbjornsen, Pauline Drobney, Jeri Neal, Gary Van Ryswyk, and Chris Witte. (2017) “Prairie strips improve biodiversity and the delivery of
multiple ecosystem services from corn-soybean croplands” Proceedings of the National Academy of Sciences,
114(42), 11247-11252. ([url](http://www.pnas.org/content/114/42/11247.short))
