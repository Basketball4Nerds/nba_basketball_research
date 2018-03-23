## this function takes in string pattern and directory to search
## returns all files that contains the string pattern
list_files_of_pattern_str <- function(str_pattern, dir_path='./code') {
  
  ## e.g. linux string search
  # > grep -rnw '/path/to/somewhere/' -e 'pattern'
  
  ## construct linux string search command
  linux_str_search_command <- sprintf("grep -rnw '%s' -e '%s'", dir_path, str_pattern)
  
  ## run the command
  system(linux_str_search_command)
}


system("grep -rnw './code' -e 'check_equal_vectors'")

