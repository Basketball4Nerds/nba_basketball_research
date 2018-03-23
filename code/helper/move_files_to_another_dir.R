## this function moves all files found in a directory to another
move_files_to_another_dir <- function(from_dir, to_dir) {
  
  ## get list of names of files to move
  file_nms <- list.files(from_dir)
  file_nms <- file_nms[grepl('\\.', file_nms)]
  
  ## move each file to a different directory
  for (file_nm in file_nms) {
    
    ## create origin file path
    orig_file_path <- file.path(from_dir, file_nm)
    
    ## create destination file path
    dest_file_path <- file.path(to_dir, file_nm)
    
    ## move file
    file.rename(from=orig_file_path, to=dest_file_path)
  }
}


