# Lab06
Chris Davis  
3/8/2017  


#Part I
##Load Plot and Shapefile

```r
library( maptools )
library( sp )
library(maps)

syr <- readShapePoly( fn="shapefiles/tl_2010_36067_tract10", proj4string=CRS("+proj=longlat +datum=WGS84") )

par( mar=c(0,0,0,0) )
plot( syr,  border="gray10" )
```

![](Lab06_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

##Add Roads

```r
roads <- readShapeLines( fn="shapefiles/tl_2015_36_prisecroads", proj4string=CRS("+proj=longlat +datum=WGS84") )

head( roads@data )
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["LINEARID"],"name":[1],"type":["fctr"],"align":["left"]},{"label":["FULLNAME"],"name":[2],"type":["fctr"],"align":["left"]},{"label":["RTTYP"],"name":[3],"type":["fctr"],"align":["left"]},{"label":["MTFCC"],"name":[4],"type":["fctr"],"align":["left"]}],"data":[{"1":"1104486676574","2":"Ny St Thruway Berkshire Exd","3":"M","4":"S1200"},{"1":"1104493254230","2":"Center St Exd","3":"M","4":"S1200"},{"1":"1104492126212","2":"Oak St Exd","3":"M","4":"S1200"},{"1":"110788145083","2":"Main St Exd","3":"M","4":"S1200"},{"1":"110788617875","2":"Gilbert St Exd","3":"M","4":"S1200"},{"1":"110496874399","2":"Forest Ave Exd","3":"M","4":"S1200"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
table( roads$RTTYP )
```

```
## 
##    C    I    M    O    S    U 
##  826  268 7009   40 2774  473
```

```r
#Extract Intersate
interstate <- roads[ roads$RTTYP == "I" , ]
```
##Look at unclipped portion

```r
par( mar=c(0,0,0,0) )
plot( syr,  border="gray80" )
plot( roads, col="steelblue", lwd=2, add=T)
plot(interstate, col = "red", add = T)
```

![](Lab06_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

##Clip

```r
library(raster)
library(rgeos)

gClip <- function(shp, bb) {
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons") 
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

#Could do just bbox(syr) for the box but that looks worse than this
xs <- c(-76.47916, -75.90119)
ys <- c(42.78893, 43.22481)

b <- bbox(syr)
b[1, ] <- xs
b[2, ] <- ys


roads_clipped <- gClip(roads, b)
interstate_clipped <- gClip(interstate, b)
```
##Plot Clipped 

```r
plot( syr,  border="gray80" )
#plot( roads_clipped, col="steelblue", lwd=2, add=T)
plot( interstate_clipped, col="red", add=T)
map.scale( metric=F, ratio=F, relwidth = 0.05, cex=0.5 )
```

![](Lab06_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

##Houses

```r
library(dplyr)
houses <- read.csv("housing.csv", stringsAsFactors = F)

plot( syr,  border="gray80" )
#plot( roads_clipped, col="steelblue", lwd=2, add=T)
plot( interstate_clipped, col="red", add=T)
map.scale( metric=F, ratio=F, relwidth = 0.05, cex=0.5 )
  
points(houses$INTPTLON, houses$INTPTLAT, pch = 19, cex=.3)
buff <- gBuffer(interstate_clipped, width = .005)
plot(buff, add=T)
```

![](Lab06_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

##Buffer it up

```r
houses <- tbl_df(houses)
lat.lon <- select(houses, INTPTLON, INTPTLAT)
names(lat.lon) <- c("lon", "lat")
lat.lon <- SpatialPoints(lat.lon, proj4string = CRS("+proj=longlat +datum=WGS84") )

overStuff <- over(lat.lon, buff)
overStuff[is.na(overStuff)] <- 0
nearHighway <- overStuff==1

houses<- mutate(houses, nearHighway)

nearHighway
```

```
##     1     2     3     4     5     6     7     8     9    10    11    12 
##  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##    13    14    15    16    17    18    19    20    21    22    23    24 
## FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE 
##    25    26    27    28    29    30    31    32    33    34    35    36 
##  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##    37    38    39    40    41    42    43    44    45    46    47    48 
## FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##    49    50    51    52    53    54    55    56    57    58    59    60 
## FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE 
##    61    62    63    64    65    66    67    68    69    70    71    72 
##  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##    73    74    75    76    77    78    79    80    81    82    83    84 
##  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE 
##    85    86    87    88    89    90    91    92    93    94    95    96 
## FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##    97    98    99   100   101   102   103   104   105   106   107   108 
## FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##   109   110   111   112   113   114   115   116   117   118   119   120 
## FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##   121   122   123   124   125   126   127   128   129   130   131   132 
## FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##   133   134   135   136   137   138   139   140   141   142   143   144 
## FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
##   145   146   147   148 
## FALSE FALSE FALSE FALSE
```

#Part II

```r
library( rgdal )
library( maptools )
library( geojsonio )
library(dplyr)
library(RColorBrewer)
library(maps)

syr <- readOGR(dsn = "shapefiles/syr_parcels.geojson")
```

```
## OGR data source with driver: GeoJSON 
## Source: "shapefiles/syr_parcels.geojson", layer: "OGRGeoJSON"
## with 41502 features
## It has 64 fields
```

```r
industrials <- syr[syr$LandUse=="Industrial", ]
notIndustrials <- syr[syr$LandUse!="Industrial", ]

plot(notIndustrials, col = "grey", border = F)

plot(industrials, add = T, col = "blue")

industrialBuff <- gBuffer(industrials, width = .004)

 overs <- over(syr, industrialBuff)

nearIndustry <- overs==1
nearIndustry[is.na(nearIndustry)]<- F

syr <- cbind(syr, nearIndustry)

plot(industrialBuff, add = T, col = rgb(44, 4, 248, alpha = 50, maxColorValue = 355))

map.scale( metric=F, ratio=F, relwidth = 0.15, cex=0.5 )
```

![](Lab06_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
schools <- syr[syr$LandUse=="Schools", ]
notSchools <- syr[syr$LandUse!="Schools", ]

plot(notSchools, col = "grey", border = F)
plot(schools, add = T, col = "red")

schoolBuff <- gBuffer(schools, width = .002)

 overs <- over(syr, schoolBuff)

nearSchools <- overs==1
nearSchools[is.na(nearSchools)]<- F

syr <- cbind(syr, nearSchools)

plot(schoolBuff, add = T, col = rgb(248, 18, 18, alpha = 50, maxColorValue = 355))

map.scale( metric=F, ratio=F, relwidth = 0.15, cex=0.5 )
```

![](Lab06_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

