#' make a figure for comparing MODIS to downscaled and reference scene
#'
#' this function was adapted from ggAOA by Ludwigm6 https://github.com/Ludwigm6/mlutils/blob/b8fab319f8054df6bb4595bd8241a2d50c7753e3/R/plot_aoa.R
#' @param LST raster stack containing either MODIS and downscaled MODIS scene or the same plus a reference scene, preferably in this order, will be named by layer names
#' @param ex list containing extents that shall be plotted into the overview of the scene and displayed in a zoomed form below 
#' @param c character string with R colours names to use for smaller extent headings and boxes in main figure, should be same length as list in ex
#' @return
#' 
#' @export
#'
#' @examples

#' library(rasterVis)
#' library(sf)
#' library(viridis)
#' library(gridExtra)
#' 
#' 
#' comp_ex2 <- raster::stack("inst/comp_ex2.grd") # from example for downscaleFun
#' 
#' aoa <- raster::raster(list.files("inst/", pattern="AOA", full.names = T))
#' 
#' # this is the result of applying the 
#' aoa_ex2 <- raster::crop(aoa, comp_ex2)
#' aoa_ex2[aoa_ex2 == 1] = NA # set pixels that are within AOA to NA so  they don't overlap valid LST data
#' 
#' ex2 <- raster::stack(comp_ex2[[2:4]], aoa_ex2)
#' names(ex2) <- c("MODIS_LST", "downscaled_LST","Landsat_LST", "AOA")
#' 
#' # # stack original LST, downscaled LST and reference LST 
#' # M_dsc_L_stack <- raster::stack(full$Mngb, dsc_M, full$L, aoa)
#' 
#' # ex2 <- crop(M_dsc_L_stack, extent(dsc_ex2))
#' 
#' ############### PLOTTING ###################################################
#' # 
#' # ### full scene ################
#' # dscplot <- make_dsc_figure(LST = M_dsc_L_stack$downscaled_LST,  LST_name = "LST",
#' #                  aoa=M_dsc_L_stack$AOA, ex=list(ex2, ex4), c=c("navy","chartreuse4"), 
#' #                  limlow = -40, limhigh = 0, exsize = 1.5, 
#' #                  limitx = c(350500, 500000), limity = c(-1400000, -1200000),legendposition = "none"
#' #                  )
#' # mplot <- make_dsc_figure(LST = M_dsc_L_stack$MODIS_LST,  LST_name = "MODIS LST",
#' #                  ex=list(ex2, ex4), c=c("navy","chartreuse4"), 
#' #                  limlow = -40, limhigh = 0, exsize = 1.5, 
#' #                  limitx = c(350500, 500050), limity = c(-1400050, -1200000),legendposition = "none"
#' #                )
#' # lplot <- make_dsc_figure(LST = M_dsc_L_stack$Landsat_LST,  LST_name = "Landsat LST",
#' #                ex=list(ex2, ex4), c=c("navy","chartreuse4"), 
#' #                limlow = -40, limhigh = 0, exsize = 1.5, 
#' #                limitx = c(350500, 500050), limity = c(-1400050, -1200000),legendposition = "none"
#' # )
#' # 
#' # plotlist <- list(mplot, dscplot, lplot)
#' # 
#' # margin = theme(plot.margin = unit(c(0.7,0.7,0.4,0.4), "cm"))
#' # 
#' # # png(paste0(figurepath, "full_example_scene_with_aoa.png"),
#' # #     units="cm", width=21, height=9, res=300)
#' # grid.arrange(
#' #   grobs = lapply(plotlist, "+", margin),
#' #   layout_matrix = rbind(c(1,2,3)))
#' # # dev.off()
#' 
#' 
#' 
#' ### example 2 #################
#' library(ggplot2)
#' margin = ggplot2::theme(plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm"))
#' 
#' dscplot <- make_dsc_figure(LST = ex2$downscaled_LST,  LST_name = "LST",
#'                  aoa=ex2$AOA, ex=list(ex2), c=c("navy"), 
#'                  limlow = -26, limhigh = -3, legendposition = "none",
#'                  exsize = 4)
#' 
#' mplot <- make_dsc_figure(LST = ex2$MODIS_LST,  LST_name = "MODIS LST",
#'                limlow = -26, limhigh = -3,ex=list(ex2), c=c("navy"), legendposition = "none",
#'                exsize = 4)
#' 
#' lplot <- make_dsc_figure(LST = ex2$Landsat_LST,  LST_name = "Landsat LST",
#'                limlow = -26, limhigh = -3,ex=list(ex2), c=c("navy"), legendposition = "none",
#'                exsize = 4)
#' 
#' plotlist <- list(mplot, dscplot, lplot)
#' 
#' 
#' gridExtra::grid.arrange(
#'   grobs = lapply(plotlist, "+", margin),
#'   layout_matrix = rbind(c(1,2,3)))
#' dev.off()
#' 
#' # with legend
#' dscplot_with_legend <- make_dsc_figure(LST = ex2$downscaled_LST,  LST_name = "LST",
#'                  aoa=ex2$AOA, ex=list(ex2), c=c("navy"), 
#'                  limlow = -26, limhigh = -3,
#'                  exsize = 2)
#' 
#' dscplot_with_legend
make_dsc_figure = function(LST, aoa=NULL, LST_name = "LST", ex=NULL, c=NULL,
                 limlow=min(LST[],na.rm=T), limhigh=max(LST[],na.rm=T),
                 legendposition="right",exsize=1, limitx=NULL, limity=NULL){
  
  if(!is.null(aoa)){
    # mask aoa
    aoa[aoa == 1] = NA
    
    base <- RStoolbox::ggR(LST, geom_raster = TRUE)+
      ggplot2::scale_fill_gradientn(name = LST_name, colors = viridis::inferno(50),
                                    na.value="transparent",
                                    limits=c(limlow,limhigh))+
      ggnewscale::new_scale_fill()+
      RStoolbox::ggR(aoa, ggLayer = TRUE, forceCat = TRUE, 
                     geom_raster = TRUE, alpha = NA)+
      ggplot2::scale_fill_manual(name = "Out of AOA", 
                                 values = c("0" = "grey70"), 
                                 na.value = "transparent")+
      ggplot2::coord_equal(expand = FALSE,xlim = limitx, ylim=limity)+
      ggplot2::scale_y_continuous(n.breaks=3)+
      ggplot2::scale_x_continuous(n.breaks=3)+
      theme_map()+ggplot2::theme(legend.position=legendposition)
    
  } else {
    base <- RStoolbox::ggR(LST, geom_raster = TRUE)+
      ggplot2::scale_fill_gradientn(name = LST_name, colors = viridis::inferno(50),
                                    na.value="transparent",
                                    limits=c(limlow,limhigh))+
      ggplot2::coord_equal(expand = FALSE, xlim = limitx, ylim=limity)+
      ggplot2::scale_y_continuous(n.breaks=3)+
      ggplot2::scale_x_continuous(n.breaks=3)+
      theme_map()+ggplot2::theme(legend.position=legendposition)
  }
  

  
    if(!is.null(ex)){ # if extents are supplied, make subplot and extents 
  
    
    explotlist <- lapply(seq(ex), function(i){
      ggplot2::geom_polygon(data=data.frame(x=c(raster::extent(ex[[i]])[c(1,1)], raster::extent(ex[[i]])[c(2,2)]),
                                   y=c(raster::extent(ex[[i]])[3:4], raster::extent(ex[[i]])[c(4,3)])),
                   aes(x = x, y = y), colour = c[i], fill=NA, size = exsize)
    })
    
    base <- base+explotlist
    
    } 
  
  return(base)
}


theme_map = function(y90 = TRUE, ts=10){
  t = ggplot2::theme_void()+
    ggplot2::theme(axis.line =  ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(color = "black", size = ts),
    axis.ticks =  ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.border =  ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white"),
    legend.position = "right")
  
  if(y90){
    t = t + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))
  }
  
  
  return(t)
}

