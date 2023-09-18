##########################################################
# 
# author: Tyrone Lee & Ludwig Geistlinger 
# date: 2023- 09-15 11:49:12
# name: iSEE-spatial
# 
############################################################

options(shiny.maxRequestSize = 200*1024^2)

library(iSEE)
library(ggpubr)

# Import custom panels ---
#source("custom_plotly.R")
source("custom_img_panel.R")
source("custom_duck.R")

#get s3 credentials from system env
s3_cred <- list(
  aws_region = "us-east-1",
  access_key_id =  Sys.getenv('AWS_ACCESS_KEY_ID') ,
  secret_access_key = Sys.getenv('AWS_SECRET_ACCESS_KEY')
)

#coloring
getColorMap <- function()
{
  inv.viridis <- function(n) viridis::viridis(n, direction = -1) 
  ucsc <- function(n) ggsci::pal_ucscgb()(n) #ggpubr::get_palette("ucscgb", n)
  iSEE::ExperimentColorMap(colData = list(brain_region = ucsc),
                           global_continuous = inv.viridis)
}

#Will init ISEE with a single dataplot for cell centroids
initISEE <- function()
{

  cdatplot <- iSEE::ColumnDataPlot(YAxis = "centroid_2",
                                   XAxis = "Column data",
                                   XAxisColumnData = "centroid_1",
                                   ColorBy = "Column data",
                                   ColorByColumnData = "brain_region",
                                   #ShapeBy = "Column data",
                                   #ShapeByColumnData = "Sample",
                                   TooltipColumnData = c("brain_region","cluster_name", "centroid_1","centroid_2"),
                                   PointSize = 0.5,
                                   LegendPointSize = 2,
                                   PanelWidth = as.integer(10),
                                   PanelHeight = as.integer(600),
                                   FontSize = 1.5)
  #add on the custom functions for image and molecule data
  GENERATOR <- createCustomPlot(CUSTOM_MOLPANEL, 
                                 className = "duckMolPanel",
                                 fullName = "Duckdb Molecule Panel")
  custom.dbp <- GENERATOR(ColumnSelectionSource="ColumnDataTable1",
                          PanelWidth = as.integer(10),
                          PanelHeight = as.integer(600))
  GENERATOR2 <- createCustomPlot(CUSTOM_IMGPANEL,
                                className = "IMGMolPanel",
                                fullName = "Image viewer Panel")
  custom.vp <- GENERATOR2(ColumnSelectionSource="ColumnDataTable1",
                         PanelWidth = as.integer(10),
                         PanelHeight = as.integer(600))
  # class(custom.dp)
  class(custom.vp)

  class(custom.dbp)
  #iSEE::.addSpecificTour("ColumnDotPlot","ColumnDataPlot1", cdattour(), force= TRUE)
  panel.list <- list(cdatplot,custom.dbp, custom.vp)
  return(panel.list)
}

# MAIN
#seeing if fix works from kevin
#https://github.com/iSEE/iSEE/issues/612
setMethod(".multiSelectionResponsive", "moleculePanel", function(x, dims = character(0)) {
  return(TRUE)
})

sce60 <- readRDS("P60_L1_slice.rds")

# shinyApp(ui = ui, server = server)
iSEE::iSEE(sce60, initial = initISEE(), colormap = getColorMap())

