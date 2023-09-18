suppressPackageStartupMessages({
  library(shiny)
  library(SingleCellExperiment)
  library(aws.s3)
  library(ggplot2)
  library(magick)
  library(grid)
  library(patchwork)
  library(rlang)
})

#custom function for IMAGE PANEL
#Parameters----------
#se - summarized experiment
#rowsri - row input
#columnsci - columnselection input
#genes - gene as a list. Must be a list for dropdown box, hardcoded to use list on s3
#bucket - s3 bucket name, hardcoded
#Returns-------
#ggplot object
CUSTOM_IMGPANEL <- function(se, rowsri, columnsci, 
                            genes = s3readRDS("/sce_components/rownames.rds",
                                              bucket = "data-bucket"))
{
    # DEBUG #print(se[,unique(unlist(columnsci))])
    # get unique slice names from column
    # must unpack slice_ID from factor list (this is how its sent through panel transmission)
    slices <- levels(as.ordered(unique(unique(se[,unique(unlist(columnsci))]$Slice_ID,1))))
    
    #TODO: set error conditions in if -> or
    # -empty slices
    # -invalid slice names (how to get list on s3? might have to include txt file)
    # avoid multiple selections here?
    # print(slices)
    slice.lab <- paste("No slice data selected in ColumnDataTable.",
                       "ColumDataPlot and ImageViewerPanel work best with",
                       "selecting 1-9 slices.", sep = "\n")  

    if ( is_empty(slices) ) {
      return(
        ggplot() + theme_void() + geom_text(
          aes(x, y, label=label),
          data.frame(x = 0, y = 0, label = slice.lab),
          size = 10)
      )
    }
  
    plotimgs <- list()
    
    for (imgid in slices)
    {
      filen <- paste0("RNADistributions_Tyrone_Ludwig/",
                     imgid, "/", genes, ".png")
      try(imgread <- s3read_using(FUN = image_read,
                                  object = filen,
                                  bucket = bucket))
      imgplot <- image_ggplot(imgread, interpolate = TRUE) +
                 ggtitle(imgid) +
                 theme(plot.title = element_text(size = (15 - (length(slices) * 0.8))))
      plotimgs <- append(plotimgs, list(imgplot))
    }
  
    wrap_plots(plotimgs)
}
