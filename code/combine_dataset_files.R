## this function concatenates datasets and exports into a single CSV
## by taking input and output paths as arguments
concatenate_dataset_files <- function(dir_path, export_file_path) {
  
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
  
  ## export
  write.csv(df, export_file_path, row.names=FALSE)
}



## code execution
spreads_dir_path <- './data/raw/spreads/'
totals_dir_path <- './data/raw/totals/'
moneylines_dir_path <- './data/raw/moneylines/'

spreads_export_file_path <- './data/raw/spreads_2006_to_2017.csv'
totals_export_file_path <- './data/raw/totals_2006_to_2017.csv'
moneylines_export_file_path <- './data/raw/moneylines_2006_to_2017.csv'

concatenate_dataset_files(spreads_dir_path, spreads_export_file_path)
concatenate_dataset_files(totals_dir_path, totals_export_file_path)
concatenate_dataset_files(moneylines_dir_path, moneylines_export_file_path)
