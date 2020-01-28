
#' Main class for portrpaths
#'
#' @description
#' R6 Reference class encapsulating all portrpaths behaviour and data
#'
#' Given a `yaml` formatted local configuration file, not intended to be
#' checked into VCS, the root directory for a given project's dataset may
#' be specified.
#'
#' This class also provides functionality for changing "profiles" - Swapping
#' between alternate data roots.
#'
#' For example, given a workflow that analyses some data extracted from some
#' database, with the following structure
#'
#' ```yaml
#' - root
#'   - input
#'     - patient_history.csv
#'     - diagnoses.csv
#'   - output
#'     - whatever_output.csv
#' ```
#'
#' It may be useful/neccesary to extract this data at a reduced scope, before
#' performing a full extract, or perhaps some requirement of this extraction may
#' change.
#'
#' For example, there is a test subset extract, and then a full extract, having the
#' following structure:
#'
#' ```yaml
#' - /media/mnt/SHARED_VOLUME/datasets
#'   - test_extract
#'     - input
#'       - patient_history.csv
#'       - diagnoses.csv
#'     - output
#'       - whatever_output.csv
#'   - bulk_extract
#'     - input
#'       - patient_history.csv
#'       - diagnoses.csv
#'     - output
#'       - whatever_output.csv
#' ```
#'
#' `PortrPaths` allows convenient switching between
#'  `/media/mnt/SHARED_VOLUME/datasets/bulk_extract` &
#'  `/media/mnt/SHARED_VOLUME/datasets/test_extract`.
#'
#' @export
PortrPath <-
R6::R6Class("PortrPath",
public = list(
  # Variables =================================================================

  # Methods ===================================================================

  #' @description
  #'
  #' Initializes new PortrPath object from configuration files given as arguments
  #'
  #' @param local_config_path The location of the configuration file that define
  #'     local configuration parameters
  #' @param shared_config_path __DEPRECATED__ The location of the configuration file
  #'        shared across the project
  #' @return a new PortrPath object
  #' @section TODO:
  #'     Document the input format
  initialize = function(local_config_path, shared_config_path = NULL){

    private$log <- logging::getLogger(name = "PortrPath")

    if (! is.null(shared_config_path)) {
      L$warn("Shared config is deprecated, only supply local!")
    }

    private$local_config_path <- local_config_path

    private$read_config()

    invisible(self)
  },

  #' @description
  #' Display the PortrPath object
  print = function(){
    print(glue::glue("Root: {private$d_root}"))
    print(glue::glue("Profiles:"))
    print(glue::glue("    {names(p)}: {p}",
                     p = private$profiles))
  },

  #' @description
  #' Add a named profile for quickly swapping the root.
  #'
  #' Profiles allow the user to easily switch between some favourite paths
  #'
  #' @family profiles
  #' @param name the friendly name to refer to the profile
  #' @param path the path the profile should refer to
  #' @return None
  add_profile = function(name, path) {
    private$profiles[[name]] <- path
    private$local$profiles <- private$profiles
    yaml::write_yaml(private$local, private$local_config_path)
    invisible(self)
  }
),
# ACTIVE ======================================================================
active = list(

  #' @field profile
  #' @description
  #' Get the current profile, or assign the name of an existing profile to
  #'     this to set that as the active profile
  #'
  #' @family profiles
  #' @param value the identifying string for the profile
  #' @return current profile if not being set
  #'
  #' @examples
  #' \dontrun{
  #' paths <- PortrPath$new('local.yaml', 'shared.yaml')
  #' paths$add_profile("home", "/Volumes/network_share_name01")
  #' paths$add_profile("work", "/Volumes/network_share_name02")
  #'
  #' paths$profile <- "home"
  #' paths$profile <- "work"
  #'
  #' # Alternatively:
  #' paths$set_profile("home")
  #' paths$set_profile("work")
  #'
  #' }
  profile = function(value) {
    if (missing(value)) return(private$profiles)
    if (! value %in% names(private$profiles)) stop(glue::glue("{value} is not a valid profile"))

    print(glue::glue("Setting profile to {value}"))
    self$root <- private$profiles[[value]]
    invisible(self)
  },
  #' Get or set the current root
  #'
  #' Sets the current root if assigned a value, or gets it if not
  #'
  #' @family PortrPath
  #' @family path_access
  #' @name PortrPath$root
  #' @param value the value of the root to be used. Either an absolute path
  #'     or ".PROJECT_ROOT"
  #' @return the current root, or nothing if setting
  root = function(value){
    if (missing(value)) return(private$d_root)
    private$local$d_root <- value
    yaml::write_yaml(private$local, private$local_config_path)
    private$d_root <- private$handle_local_root(value)
    print(glue::glue("Setting root to {private$d_root}"))

    invisible(self)
  }
),
# PRIVATE ======================================================================
private = list(
  # Variables =================================================================
  log = NULL,
  # The locations of the yaml configuration files

  local_config_path = NULL,
  local = NULL,
  # Local root, set by some local config file
  d_root = NULL,
  # List of filenames in final directory
  profiles = NULL,
  # Methods ===================================================================
  read_config = function() {
    private$handle_local(private$local_config_path)

    private$d_root <- private$handle_local_root(private$local$d_root)

    invisible(self)
  },

  handle_local = function(local_path){
    if (!file.exists(local_path)) {
      print(glue::glue("No config found at {local_path}"))
      private$local <- list()
    } else {
      private$local <- yaml::read_yaml(local_path)
    }

    if (! ("d_root" %in% names(private$local))){
      private$log$warn("Unable to find d_root in %s",
                   as.character(names(private$local)))

      root <- readline(prompt = "Enter data root: ")
      self$root <- root
    }

    if ("profiles" %in% names(private$local)) {
      private$profiles <- private$local$profiles
    } else {
      private$local$profiles <- list(default = private$local$d_root)
    }
    yaml::write_yaml(private$local, local_path)
  },


  # If the keyword .PROJECT_ROOT is used for the local root mapping
  # Instead set it to the local root
  handle_local_root = function(root_in){
    private$log$info("Root in: %s", root_in)
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


#' Set the active profile
#'
#' Alias of `path$profile <- name`
#' @inherit PortrPath$profile
#' @family profiles
#' @name PortrPath$set_profile
NULL
PortrPath$set(
  "public", "set_profile",
  function(name){
    self$profile <- name
    invisible(self)
  }
)

# Access root ========================


NULL

PortrPath$set(
  "active", "root",
  ,
  overwrite = TRUE
)
