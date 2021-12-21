
# define function for checking the existence of a directory
# and if it doesn't create it along with its parents 
dir.check <- function(path) {
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
}

# function to check whether a file is older than another
#   returns TRUE if file1 is older than file2 and FALSE otherwise
#   if any of the files dont exist, it returns FALSE
file.older.check <- function(file1, file2) {
  
  if (!(all(file.exists(file1, file2)))) {
    return(FALSE)
  }
  
  if (file.mtime(file1) < file.mtime(file2)) {
    return(TRUE)
  }
  
  return(FALSE)
}


# function for calculating a position above the range of a vector x
# muliple vectors can be supplied, again, the max result of all vectors
# is returned
ypos <- function(x, ..., frac = 0.3) {
  
  x <- c(x, ...)
  
  pos <- max(x, na.rm = TRUE) + abs(max(x, na.rm = TRUE)) * frac
  
  return(pos)
  
} 


# function for printing a log message starting with date and time
log_msg <- function(...) {
  msg <- paste0(toString(format(Sys.time())), " ", ...)
  cat(msg, "\n")
  
  # optionally log also to file, if that is given
  if (!is.null(getOption("log_file"))) {
    log_file <- getOption("log_file")
    cat(msg, fill = TRUE, file = log_file, append = TRUE, sep = "")
  }
}


# function for giving a vector of letters reflecting significance of pairwise
#   differences. Optionally respects factor ordering if provided
sig_letters <- function(ph_table, ordering = NULL, alpha = 0.05) {
  
  # set vector of lables
  lables <- LETTERS
  
  unq_lvls <- unique(as.vector(str_split(row.names(ph_table),
                                         "(?<=\\w)-(?=\\w)",
                                         simplify = TRUE)))
  
  # if no order is provided, use the order items appear in the group matrix
  if (is.null(ordering)) {
    
    ordering <- unq_lvls
    
  } else {
    
    if (!length(setdiff(ordering, unq_lvls)) == 0) {
      stop("Provided order vector does not cover all levels found in data.")
    }
    
  }
  
  n <- length(ordering)
  
  # get rows with pvals above the threshold, i.e. levels that DO NOT differ
  sel_vec <- ph_table[ , 4] > alpha
  
  # # ENSURE WE GET A MATRIX BC IN CASE OF ONLY ONE ROW RETURNING IT IS A VECTOR
  # # THIS NEITHER HAPPENS WITH 0 OR <=2 ROWS
  # non_sig_tab <- as.matrix(ph_table[ph_table[ , 4] > alpha, ])
  
  grp_mat <- str_split(rownames(ph_table)[sel_vec],
                       "(?<=\\w)-(?=\\w)",
                       simplify = TRUE)
  
  pair_mat <- matrix(0, nrow = n, ncol = n, 
                   dimnames = list(ordering, ordering))
  
  # if everything is sig. different, give out corresponding vector
  if (nrow(grp_mat) == 0) {
    return(setNames(lables[1:length(ordering)], ordering))
  }
  
  # fill pair mat, 1 will indicate a non sig diff pairing
  for (lvl in ordering) {
    
    lvl_ind <- which(ordering == lvl)
    
    for (i in 1:nrow(grp_mat)) {
      cont_vec <- grp_mat[i, ] == lvl
      
      if (any(cont_vec)) {
        # get index of the partner of current lvl in this non-sig diff
        prt_ind <- which(ordering == grp_mat[i, !cont_vec])
        
        # mark group membership in matrix
        pair_mat[lvl_ind, prt_ind] <- 1
      }
    }
  }
  
  # get list of groups by turning pair matrix into a graph, finding max cliques
  #  and extracting the vertex names 
  grp_list <- igraph::graph_from_adjacency_matrix(pair_mat,
                                     mode = "undirected") %>% 
    igraph::max_cliques() %>%
    lapply(names) %>% 
    lapply(function(x, ordering) {
      x[order(match(x, ordering))]
    }, ordering)
  
  # order list so that first vector element of each list element follows 
  #   ordering
  # get ordering vec for the list according to the order of the first element
  ord_vec <- order(match(sapply(grp_list, function(x) { x[1] }), ordering))
  grp_list <- grp_list[ord_vec]
  
    
  out_vec <- setNames(vector("character", n), ordering)
  
  for (i in 1:length(grp_list)) {
    ind_vec <- ordering %in% grp_list[[i]]
    
    out_vec[ind_vec] <- paste0(out_vec[ind_vec], lables[i])
  }
  
  return(out_vec)
}

# function to remove NAs from vector
na_rm_str <- function(x) {x[!str_detect(x, "NA")]}

# bioflor trait functions -------------------------------------------------


#function for getting traits by their regex string
get_traits <- function(text, traits, patterns) {
  
  #initiate data frame
  traits_out <- setNames(data.frame(
    matrix(ncol = length(traits),
           nrow = 1)),
    traits)
  
  #start for loop to extract each trait value
  for (i in seq(1, length(traits), 1)) {
    
    traits_out[1, i] <- stringr::str_extract_all(text, patterns[i],
                                                 simplify = TRUE) %>% 
      as.vector() %>% 
      
      # ensure levels are unique
      unique() %>% 
      
      # ensure all combinations of levels have the same structure
      sort() %>% 
      
      paste(collapse = ", ")
    
  }
  
  return(traits_out)
  
}
