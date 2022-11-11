
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cc.buildr

<!-- badges: start -->
<!-- badges: end -->

The objective of cc.buildr is to help at all stages of the development
of a Curbcut instance. cc.buildr is used to build the first set of data
shared by all Curbcut instances, but also to help with the processing of
data for new Curbcut pages.

## Installation

You can install the development version of cc.buildr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MSSI-urban/cc.buildr")
```

## Usage

cc.buildr creates and works from long lists of `tibble`. For faster
building, the package frequently uses functions of the `future_*apply`
family from the `future.apply` package, for any computationally
intensive work. These implementations of the apply family function can
be solved using any backend supported by future (parallelization). We
therefore recommend defining a future backend at the beginning of the
data building script.

``` r
future::plan(future::multisession())
```

Let’s also enable the global progression handler to get a progress bar
on those more computationally intensive work.

``` r
progressr::handlers(global = TRUE)
```

## The base of a study region and its dictionaries

We start by declaring all the “regions” for which data can be
visualized. Each of the following regions (CMA, island, city, cmhc) and
scales (CSD, CT, DA, building and cmhc_zone) is a different mapbox
tileset. At this stage, the regions must be in a priority order: If the
user defaults to a region that is not available for a certain dataset,
the map will instead display the region closest to the top of the list
that has the available data.

``` r
all_tables <-
  list("CMA" = c("CSD", "CT", "DA", "building"),
       "island" = c("CSD", "CT", "DA", "building"),
       "city" = c("CSD", "CT", "DA", "building"),
       "cmhc" = c("cmhc_zone"))
```

The `master_polygon` is a single spatial feature encompassing all the
regions under study. Create a list of where geometries must be found for
every region. For any census geometries, codes from
`cancensus::list_census_regions()` can be fed: A named list of census
regions to retrieve. Links to shapefiles also work. Using
`cancensus::list_census_regions()`, retrieve the CMA code of the region
(if it is a CMA). This will be useful for the ‘Vacancy rate’ page.

``` r
cancensus_cma_code <- 24462
all_regions <- list(CMA = list(CMA = cancensus_cma_code),
                    city = list(CSD = 2466023),
                    island = "geometry/island.shp",
                    cmhc = get_cmhc_zones(list(CMA = cancensus_cma_code)))

base_polygons <- create_master_polygon(all_regions = all_regions)
crs <- base_polygons$crs
```

On one instance of Curbcut, users can move from one region to another
within the same study space. A Curbcut Montreal user can analyze a
dataset through the lens of the Montreal metropolitan area, or only
through the city of Montreal to compare boroughs. The implications are
that dynamically created text must be informed by a dictionary of
regions to correctly serve the right information using accurate wording.
Create the regions dictionary and see `?cc.buildr::regions_dictionary`
to understand the meaning of each argument.

``` r
regions_dictionary <-
  regions_dictionary(
    all_tables = all_tables,
    geo = c("CMA", "island", "city", "cmhc"),
    name = c(CMA = "Metropolitan Area",
             island = "Island of Montreal",
             city = "City of Montreal",
             cmhc = "Canada Mortgage and Housing Corporation zones"),
    to_compare = c(CMA = "in the Montreal region",
                   island = "on the island of Montreal",
                   city = "in the City of Montreal",
                   cmhc = "in the Montreal region"),
    pickable = c(CMA = TRUE,
                 island = TRUE,
                 city = TRUE,
                 cmhc = FALSE))
```

## Build scales

### Census scales

Using the output of previous retrievals, get a list of census scales.

``` r
census_scales <-
  build_census_scales(master_polygon = base_polygons$master_polygon,
                      regions = base_polygons$province_cancensus_code,
                      crs = crs)
```

Optional: In the case of Curbcut Montreal, the City of Montreal can be
split using the boroughs, which should replace the City at the CSD
scale. `cc.buildr::split_scale` performs this operation.

``` r
boroughs <- sf::st_read("arrondissements_mtl.shp")
census_scales$CSD <- split_scale(destination = census_scales$CSD,
                                 cutting_layer = boroughs,
                                 DA_table = census_scales$DA,
                                 crs = crs)
```

Create the scales dictionary for the same reason we need a regions
dictionary (correctly serve the right information using accurate
wording). In the case a feature has been split, update the according
scale in the dictionary.

``` r
scales_dictionary <- census_scales_dictionary(census_scales)
scales_dictionary[1, ] <- list(scale = "CSD",
                               sing = "borough/city",
                               plur = "boroughs or cities",
                               slider_title = "Borough/City",
                               place_heading = "{name_2} of {name}",
                               place_name = "{name}")
```

### Building scale

Spatial features of buildings are retrieved through a combination of
Open Street Map and Microsoft’s Canadian Building Footprints dataset.
Reverse geocoding is performed by a set of government public APIs and
the Open Street Map API. See the documentation to use the functions
correctly. We also add a new line to the scale dictionary.

``` r
building <- build_buildings(DA_table = census_scales$DA,
                             crs = crs,
                             MS_province = "Quebec")
building <- rev_geocode_buildings(master_polygon = base_polygons$master_polygon,
                       building = building,
                       province_code = "QC",
                       crs = crs)

scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "building",
                             sing = "dissemination area",
                             plur = "dissemination areas",
                             slider_title = "Building",
                             place_heading = "{name}",
                             place_name = "The dissemination area around {name}")
```

### CMHC scale

If the space under study is a CMA, cc.buildr creates a ready to use
Vacancy rate module. If this is the case, the regions must be properly
created and specified in the scale dictionary.

``` r
cmhc_zone <- get_cmhc_zones(list(CMA = cancensus_cma_code))
cmhc_zone <- additional_scale(additional_table = cmhc_zone,
                              DA_table = census_scales$DA,
                              ID_prefix = "cmhc",
                              name_2 = "CMHC zone",
                              crs = crs)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "cmhc_zone",
                             sing = "CMHC zone",
                             plur = "CMHC zones",
                             slider_title = "CMHC zone",
                             place_heading = "CMHC zone of {name}",
                             place_name = "{name}")
```

### Additional scale

cc.buildr lets a builder add any region or scale that is not only
census-related through the `cc.buildr::additional_scale` function. This
is an example of the collaboration with Centraide of Greater Montreal:

``` r
centraide <- sf::st_read("centraide.shp")
centraide <- additional_scale(additional_table = centraide,
                              DA_table = census_scales$DA,
                              ID_prefix = "centraide",
                              name_2 = "Centraide zone",
                              crs = crs)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "centraide",
                             sing = "centraide zone",
                             plur = "centraide zones",
                             slider_title = "Centraide zone",
                             place_heading = "Centraide zone of {name}",
                             place_name = "{name}")
```

## Consolidate scales

The way the regions and scales are organized in the following data
construction is through a list of depth 2, where the region is the first
depth and the scales are the second. The major contribution of
`cc.buildr::consolidate_scales` is that, for example, the city\$DA table
is a spatial filter between the city region and the DA table. It also
updates the naming and IDs to ensure that all scales in a region can
properly talk to each other.

``` r
all_scales <- c(census_scales,
                list(building = building),
                list(centraide = centraide),
                list(cmhc_zone = cmhc_zone))

scales_consolidated <- consolidate_scales(all_tables = all_tables,
                                          all_scales = all_scales,
                                          regions = base_polygons$regions,
                                          crs = crs)
```

## Verify dictionary conformity

``` r
verify_dictionaries(all_tables = all_tables,
                    regions_dictionary = regions_dictionary,
                    scales_dictionary = scales_dictionary)
```

## Variables and modules table

Each variable has a unique set of elements that the platform relies on
to correctly provide the predefined and precomputed information. This
ranges from the title or explanation of the variable to the pre-computed
breaks. The same is true for modules. The `module` table contains
information about the name of a module, its description, … We add empty
tables that will be filled each time a variable is added.

``` r
scales_variables_modules <-
  append_empty_variables_table(scales_consolidated = scales_consolidated)
scales_variables_modules <-
  append_empty_modules_table(scales = scales_variables_modules)
```

## The first datasets

### Census

``` r
scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 census_vectors = cc.data::census_vectors,
                 crs = crs,
                 housing_module = TRUE)
```

### Vacancy rate

``` r
scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              crs = crs, geo_uid = cancensus_cma_code)
```

### Can-BICS and Can-ALE

``` r
scales_variables_modules <-
  ru_canale(scales_variables_modules = scales_variables_modules,
            crs = crs)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             crs = crs)
```
