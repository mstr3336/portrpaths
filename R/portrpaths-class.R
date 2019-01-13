
#' Main class for portrpaths
#'
#'
#' R6 Reference class encapsulating all portrpaths behaviour and data
#'
#'     The current implementation will provide access to, essentially, a flat
#'     Directory full of datafiles.
#'
#'     Local configuration, as well as providing an absolute root reference,
#'     MIGHT also allow overriding of global options (To be decided)
#'
#'
#' @name PortrPath-class
#' @aliases PortrPath
NULL

#' @export
PortrPath <-
R6::R6Class("PortrPath",
public = list(
  # Variables =================================================================

  # Methods ===================================================================
  initialize = function(local_config_path, shared_config_path){
    stop("Interface Stub")
  },

  print = function(){
    print(glue::glue("Root: {private$d_root}"))
    print(glue::glue("Name: {names(private$f_names)} ",
                     "Path: {private$f_paths}"))
    print(glue::glue("Parents: {private$d_parents}"))
  },
  get_file_paths = function(){
   stop("Interface Stub")
  }

),
private = list(
  # Variables =================================================================

  # The locations of the yaml configuration files

  local_config_path = NULL,
  shared_config_path = NULL,
  # Local root, set by some local config file
  d_root = NULL,
  # Information to track shared properties, on project scope
  # Subdirectories, just the path components eg(1->n) from
  # d_root / eg1 / eg2 /.../ egn /example.rds
  d_parents = NULL,
  # List of filenames in final directory
  f_names = NULL,
  f_paths = NULL,
  f_exts = NULL,

  # Methods ===================================================================
  read_config = function() {
    shared <- yaml::read_yaml(private$shared_config_path)
    local <- yaml::read_yaml(private$local_config_path)

    private$d_root <- private$handle_local_root(local$d_root)

    private$d_parents <- shared[["parent_components"]]
    private$f_names   <- shared[["file_names"]]
    private$f_exts    <- shared[["file_extensions"]]

    invisible(self)
  },


  build_whole_paths = function(){
    dir <- glue::glue_collapse(c(private$d_root, private$d_parents),
                               sep = .Platform$file.sep)


    # Delete any mappings in the current f_paths list
    private$f_paths <- private$f_names
    # Build named list from
    private$f_paths[names(private$f_names)] <-
      glue::glue("{dir}/{private$f_names}.{private$f_exts}")

    invisible(self)
  },

  # If the keyword .PROJECT_ROOT is used for the local root mapping
  # Instead set it to the local root
  handle_local_root = function(root_in){
    out <-
      switch(root_in,
             # Default
             root_in,
             .PROJECT_ROOT = here::here()
             )
    return(out)
  }
))

# Actual Implementations ======================================================

#' Inititalize A PortrPath object
#'
#' Initializes new PortrPath object from configuration files given as arguments
#'
#' @name PortrPath$new
#' @aliases PortrPath-class-new
#' @param local_config_path The location of the configuration file that define
#'     local configuration parameters
#' @param shared_config_path The location of the configuration file shared
#'     across the project
#' @section TODO:
#'     Document the input format
NULL

PortrPath$set(
  "public", "initialize",
  function(local_config_path, shared_config_path){
    private$local_config_path <- local_config_path
    private$shared_config_path <- shared_config_path

    private$read_config()

    private$build_whole_paths()

    invisible(self)
  },
  overwrite = TRUE
  )

#' Get a named list of file paths
#'
#' For files specified in the shared config, get a named list of their local
#'     paths
#'
#' @name PortrPath$get_file_paths
#' @aliases PortrPath-class-get_file_paths
#' @return A named list of file paths
NULL

PortrPath$set(
  "public", "get_file_paths",
  function(){
    return(private$f_paths)
  },
  overwrite = TRUE
)
