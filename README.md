
<!-- README.md is generated from README.Rmd. Please edit that file -->

# downscaleLST.MDV

<!-- badges: start -->
<!-- badges: end -->

This package contains the model and necessary predictor data to
downscale MODIS 11\_L2 scenes for the McMurdo Dry Valleys of Antarctica
from 1km² to 30m².

The workflow for generating the model can be found in the GitHub
Repository <https://github.com/MLezamaValdes/downscaling_LST_MDV> and
the corresponding paper was recently submitted.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MLezamaValdes/downscaleLST.MDV")
```

## Example

This is a basic example which shows you how to downscale MODIS scene (in
this case a small extent) to 30m resolution and compare it to the MODIS
scene and - if you wish to do so - to a 30m Landsat scene captured at
the same time.

``` r
library(downscaleLST.MDV)
```

First, we’ll need to create an incidence angle raster for the time the
MODIS scene to be downscaled was captured using function make\_ia().
Next, we can downscale the MODIS scene using the supplied model and
predictor dataset.

``` r
library(raster)
library(lubridate)
library(solrad)
library(downscaleLST.MDV)


exdate <- as.POSIXct("2018-11-12 13:50:00", tz="UTC")
ex2 <- raster::stack("inst/ex_2_all_layers.grd")

##########  load the model ###############################################
load(system.file("final_model_downscaling_LST_MDV.RData", package = "downscaleLST.MDV"))

##########  make ia and put into one stack  ###############################
ia_ex2 <- make_ia(M_date = exdate, outdir = here::here("ex_output"),
                     sl=ex2$slope, as=ex2$aspect, tmplt=ex2$template)
names(ia_ex2) <- "ia"
ex2_ia <- raster::stack(ex2, ia_ex2)


dsc_ex2 <- downscaleFun(MOD=ex2_ia$Mbilin,
                        aux=ex2_ia[[c( "dem","slope","aspect","soilraster","landcoverres")]],
                        ia=ex2_ia$ia, 
                        template=ex2_ia$template, 
                        modname="MYD11_L2.A2018316.1350.006",
                        outdir = "ex_output/",
                        writeRastertoOutdir = TRUE,
                        add="_ex2",
                        outformat=".grd")

comp_ex2 <- raster::stack(ex2$Mbilin, ex2$Mngb, dsc_ex2, ex2$L)
raster::writeRaster(comp_ex2, "inst/comp_ex2.grd",overwrite=T)
```

Then we can display the downscaling result and compare to the original:

``` r
library(rasterVis)
library(sf)
library(viridis)
library(gridExtra)
library(ggplot2)

comp_ex2 <- raster::stack("inst/comp_ex2.grd") # from example for downscaleFun

aoa <- raster::raster(list.files("inst/", pattern="AOA", full.names = T))

# this is the result of applying the 
aoa_ex2 <- raster::crop(aoa, comp_ex2)
aoa_ex2[aoa_ex2 == 1] = NA # set pixels that are within AOA to NA so  they don't overlap valid LST data

ex2 <- raster::stack(comp_ex2[[2:4]], aoa_ex2)
names(ex2) <- c("MODIS_LST", "downscaled_LST","Landsat_LST", "AOA")

margin = ggplot2::theme(plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm"))

dscplot <- make_dsc_figure(LST = ex2$downscaled_LST,  LST_name = "LST",
                 aoa=ex2$AOA, ex=list(ex2), c=c("navy"), 
                 limlow = -26, limhigh = -3, legendposition = "bottom",
                 exsize = 4)

mplot <- make_dsc_figure(LST = ex2$MODIS_LST,  LST_name = "LST",
               limlow = -26, limhigh = -3,ex=list(ex2), c=c("navy"), legendposition = "bottom",
               exsize = 4)

lplot <- make_dsc_figure(LST = ex2$Landsat_LST,  LST_name = "LST",
               limlow = -26, limhigh = -3,ex=list(ex2), c=c("navy"), legendposition="bottom",
               exsize = 4)

plotlist <- list(mplot, dscplot, lplot)

gridExtra::grid.arrange(
  grobs = lapply(plotlist, "+", margin),
  layout_matrix = rbind(c(1,2,3)))
```

<div class="figure">

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="1km MODIS, 30m downscaled and 30m Landsat LST" width="100%" />
<p class="caption">
1km MODIS, 30m downscaled and 30m Landsat LST
</p>

</div>

The grey areas within the downscaled scene are pixels that are outside
of the Area of Applicability (AOA) [Meyer, Pebesma
2021](https://doi.org/10.1111/2041-210X.13650) of the model.

## Coming soon

In the future functions to download MODIS data directly and to calculate
the AOA per downscaled scene will be added.
