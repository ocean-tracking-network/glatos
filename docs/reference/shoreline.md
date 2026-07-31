# zipped polygon shapefile of Great Lakes

Polygon coastline of Great Lakes in WGS84 projection.

## Format

shapefile

## Source

<https://glatos.org/home>

## Details

Used to make
[great_lakes_polygon](https://ocean-tracking-network.github.io/glatos/reference/great_lakes_polygon.md).

Note from Todd: "This polygon layer of GL shoreline was modified by hand
to include Saginaw, Tittabawasssee, Maumee, and Sandusky rivers.
Outlines of rivers are not precise but were wide enough to allow a
continuous connection between pixels for the entire undammed river
stretch when the 'rasterize' function is used to produce a raster layer
of the GL in QGIS."

Todd's original file name was 'coastline_poly_modified_rivers'.

## Filename

shoreline.zip

## Author

Todd Hayden

## Examples

``` r

# Read polygon from shapefile

poly_file <- system.file("extdata", "shoreline.zip", package = "glatos")

poly <- sf::st_read(paste0("/vsizip/", poly_file))
#> Reading layer `shoreline' from data source 
#>   `/vsizip//tmp/RtmpUoCFFC/temp_libpath5cdd282ffe47/glatos/extdata/shoreline.zip' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 4 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -92.19876 ymin: 41.32391 xmax: -70.4547 ymax: 49.01076
#> Geodetic CRS:  WGS 84

if (FALSE) { # \dontrun{
plot(sf::st_geometry(poly))
} # }
```
