#' Calculate incidence angle for MODIS scene to be downscaled
#'
#' @param M_date MODIS scene capturing time in format POSIXct, UTC, just as in the MODIS filename
#' @param outdir directory the incidence angle will be written to
#' @param tmpdir directory for writing temporary raster files
#' @param sl slope in degrees, full MDV file loadable via slope <- raster(system.file("30m_slopeMDV.tif", package = "downscaleLST.MDV"))
#' @param as aspect in degrees, full MDV file loadable via aspect <- raster(system.file("30m_aspectMDV.tif", package = "downscaleLST.MDV")) 
#' @param template template file in format raster
#' @return
#' Incidence Angle raster for the time the MODIS scene to be downscaled was captured
#' @export
#'
#' @examples



#' 
#' exdate <- as.POSIXct("2018-11-12 13:50:00", tz="UTC")
#' full <- raster::stack("inst/full_size_grids_all_layers.grd")
#' ex2 <- raster::stack("inst/ex_2_all_layers.grd")
#' 
#' # for the example extent
#' ia <- make_ia(M_date = exdate, outdir = here::here("ex_day"), 
#'               sl=ex2$slope, as=ex2$aspect, tmplt=ex2$template)
#' # # for the full scene
#' # ia <- make_ia(M_date = exdate, outdir = here::here("ex_day"), 
#' #               sl=full$slope, as=full$aspect, tmplt=full$template)
#' 
#' 
make_ia <- function(M_date, outdir, tmpdir = tempdir(),
                    sl=slope, as=aspect, tmplt=template,
                    writeRastertoOutdir=TRUE){
  
  raster::rasterOptions(tmpdir=tmpdir)
  
  print(paste0("starting with ia generation for ",M_date))
  
  doy <- lubridate::yday(M_date)
  h <- lubridate::hour(M_date)
  min <- lubridate::minute(M_date)
  
  doymin <- as.numeric(doy)+((h+min/60)/24)
  
  dateaschar <- gsub(gsub(substring(as.character(M_date), 1,16), pattern="\\s", replacement="_"), 
       pattern=":", replacement="_")
  
  # check whether daylight saving applies
  MODDateNZ <- lubridate::with_tz(M_date, tzone = "Pacific/Auckland")
  if(lubridate::dst(MODDateNZ)==TRUE){
    dstnz <- 60
  } else {
    dstnz <- 0
  }
  
  #  mean latitude and longitude for whole area of interest, i.e. lat=-77.45706,lon= 161.7673
  lat <- (-77.45706)
  lon <- 161.7673
  ia <- solrad::Incidence(DOY = doymin, Lat=lat, Lon=lon, 
                          SLon=180, DS=dstnz, 
                          Slope = sl, Aspect = as)
  
  iares <- raster::resample(ia, tmplt)
  names(iares) <- "ia"
  rm(ia)
  
  if(writeRastertoOutdir==TRUE){
    
      raster::writeRaster(iares, paste0(outdir, "/ia_res", dateaschar, ".tif"), overwrite=T)
      print(paste0("wrote ia", dateaschar,"to ", paste0(outdir, "/ia_res_", dateaschar, ".tif")))
  }
  
  gc()

  return(iares)
  
}

