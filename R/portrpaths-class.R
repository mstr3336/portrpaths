
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
  initialize = function(local_config, shared_config){
    private$local_config <- local_config
    private$shared_confg <- shared_config


  }
),
private = list(
  # Variables =================================================================

  # The locations of the yaml configuration files

  local_config = NULL,
  shared_config = NULL,
  # Local root, set by some local config file
  d_root = NULL,
  # Information to track shared properties, on project scope
  # Subdirectories, just the path components eg(1->n) from
  # d_root / eg1 / eg2 /.../ egn /example.rds
  d_subdirs = list(),
  version = NULL,
  # List of filenames in final directory
  f_names = list(),
  f_paths = list(),

  # Methods ===================================================================
  read_config = function() {
    shared <- yaml::read_yaml(private$shared_config)
    local <- yaml::read_yaml(private$local_config)
  }

  build_fpaths = function(){
    stop("Not implemented")
  }
))
