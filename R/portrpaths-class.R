
#' Main class for portrpaths
#'
#'
#' R6 Reference class encapsulating all portrpaths behaviour and data
#'     The current implementation will provide access to, essentially, a flat
#'     Directory full of datafiles.
#'     Local configuration, as well as providing an absolute root reference,
#'     MIGHT also allow overriding of global options (To be decided)
#'
#' @name portrpaths-class
#' @export
PortrPaths <-
R6::R6Class("PortrPaths",
public = list(
  # Variables =================================================================

  # Methods ===================================================================
  initialize = function(local_config_path, shared_config_path){
    private$local_config_path <- local_config_path
    private$shared_config_path <- shared_config_path

    private$read_config()

    private$build_whole_paths()

    invisible(self)
  },

  print = function(){
    print(glue::glue("Root: {private$d_root}"))
    print(glue::glue("Name: {names(private$f_names)} ",
                     "Path: {private$f_paths}"))
    print(glue::glue("Parents: {private$d_parents}"))
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

  # Methods ===================================================================
  read_config = function() {
    shared <- yaml::read_yaml(private$shared_config_path)
    local <- yaml::read_yaml(private$local_config_path)

    private$d_root <- local$d_root
    print(local)
    print(shared)

    private$d_parents <- shared[["parent_components"]]
    private$f_names   <- shared[["file_names"]]

    print(private$f_names)
    print(class(private$f_names))

    invisible(self)
  },


  build_whole_paths = function(){
    dir <- file.path(private$d_root, private$d_parents)

    # Delete any mappings in the current f_paths list
    private$f_paths <- private$f_names
    # Build named list from
    private$f_paths[names(private$f_names)] <-
      glue::glue("{dir}/{private$f_names}.rds")

    invisible(self)
  }
))
