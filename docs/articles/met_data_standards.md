# Met data standards

The most basic problem is simply of agreeing on standard names for
variables. Several conventions exist, but none are widely used outside
the modelling community. Some naming conventions are listed below.

| Name | Defines | Link | Issues |
|----|----|----|----|
| Climate Forecast (CF) | Names & netCDF file format | [link](https://cfconventions.org/Data/cf-standard-names/docs/guidelines.html), and [from CEDA](https://help.ceda.ac.uk/article/4507-the-cf-metadata-convention) |  |
| ICOS | Variable names | [link](https://www.icos-etc.eu/icos/documents/instructions/bmform) |  |
| ERA5 | Variable names | [link](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=documentation) |  |
| AmeriFlux BADM | Variable names | [link](https://nerc-ceh.github.io/ameriflux.lbl.gov/data/badm/badm-standards/) | Does not including most met variables |
| Copernicus Station Exchange Format (SEF) | Variable names | [link](https://datarescue.climate.copernicus.eu/variablenames) |  |
| Copernicus Station Exchange Format (SEF) | Plain text file format   | [link](https://datarescue.climate.copernicus.eu/station-exchange-format-sef) | Only one variable per file limits use |
| CEDA BADC-CSV | Plain text file format   | [link](https://help.ceda.ac.uk/article/105-badc-csv) | Format not easily machine-readable |

These conventions are not systematically constructed, so are not easily
extended or modified to be more generally useable; several are not even
easily machine-readable. For example, the Climate Forecast (CF)
convention constructs a set of [standard
names](https://cfconventions.org/Data/cf-standard-names/docs/guidelines.html)
used widely in climate modelling. Users are able to suggest new
variables names in an ad hoc manner. The result is there are ~5250
arbitrarily defined variable names, with no system to their naming. For
example, there is no system to the naming to group together different
types of temperature measurements (such as soil temperature, surface
temperature, air temperature at 2 m or 10 m). Eleven chemical species
are defined, but arbitraryily, so no other chemical species can be
referred to without inventing new names.

Because none of these is widely used in the measurement community, we
need to build the flexibility to:

- allow easy conversion of ad hoc local naming schemes into one or more
  standards, and
- translate easily between different existing standards (e.g. ERA5 to
  CF-1).
