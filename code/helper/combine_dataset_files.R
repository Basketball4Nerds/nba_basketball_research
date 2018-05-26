## this function reads multiple dataset files that resides in a directory
## and concatenates them into a single dataset;
## it exports the concatenated dataset as a csv file if export file path is provided;
combine_dataset_files <- function(dir_path, export_file_path=NULL) {
  
  ## get dataset file names
  raw_file_names <- list.files(dir_path)
  raw_csv_file_names <- raw_file_names[grepl('\\.csv$', raw_file_names)]
  raw_csv_file_paths <- file.path(dir_path, raw_csv_file_names)
  
  ## function to merge multiple datasets into one
  combine_csv_files <- function(csv_file_paths) {
    data_list <- lapply(csv_file_paths, function(x) {read.csv(file=x, header=T, stringsAsFactors=F)})
    df <- Reduce(function(x, y) {rbind(x, y)}, data_list)
    return(df)
  }
  
  ## load regular and post season datasets
  df <- combine_csv_files(csv_file_paths=raw_csv_file_paths)
  
  ## export if file path for exporting is provided
  if (!is.null(export_file_path))
    write.csv(df, export_file_path, row.names=FALSE)
  
  ## return
  # return(df)
}
