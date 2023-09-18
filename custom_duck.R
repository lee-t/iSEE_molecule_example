suppressPackageStartupMessages({
  library(ggplot2)
  library(DBI)
  library(dplyr)
  library(duckdb)
  
})

#custom function for MOLECULE PANEL
#@param se - summarized experiment
#@rowsri - row input
#@columnsci - columnselection input
#@selection - hardcoded slice selection as list
#@genes - gene as a list. Must be a list for dropdown box, hardcoded to use list from directory
#@returns ggplot object
CUSTOM_MOLPANEL <- function(se, 
                            rowselect,
                            colselect,
                            selection = c("slice_L1", "slice_M1"),
                            genes = scan("genes.txt", what = "character") 
{   
    # open connection
    con = dbConnect(duckdb::duckdb(), ":memory:")
    
    # s3 bucket connection requires httpfs extension
    dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
    dbExecute(con, paste("SET s3_region='us-east-1';", 
                         "SET s3_access_key_id=accesskey;", 
                         "SET s3_secret_access_key=secretkey;"))
    
    # get gene string
    quoted_gene <- paste0("'", genes, "'")
    
    # query
    sql <- paste0("SELECT x,y FROM ",
                  "read_parquet('s3://data-bucket/molecules_parquets/*/*.parquet'",
                  ", hive_partitioning = 1) WHERE Slice_ID='",
                  selection,"' AND Gene = ", quoted_gene, ";")    
    sel_mol <- dbGetQuery(con, sql)
  
    # plot  
    ggplot(sel_mol, aes(x = x, y = y)) +
      geom_point(size = 0.5, color = "#00798c") +
      ggtitle(paste(quoted_gene, nrow(sel_mol))) + 
      theme_bw() 
       
    #dbDisconnect(con, shutdown=TRUE)
}
