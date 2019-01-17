
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
    print(glue::glue("Profiles:"))
    print(glue::glue("    {names(p)}: {p}",
                     p = private$profiles))
  },
  get_file_paths = function() stop("Interface Stub"),
  add_profile = function(name, path) stop("Interface Stub")
),
# ACTIVE ======================================================================
active = list(
  profile = function(value) stop("Interface Stub"),
  root = function(value) stop("Interface Stub"),
  files = function() stop("Interface Stub")
),
private = list(
  # Variables =================================================================
  log = NULL,
  # The locations of the yaml configuration files

  local_config_path = NULL,
  shared_config_path = NULL,
  local = NULL,
  shared = NULL,
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
  profiles = NULL,
  # Methods ===================================================================
  read_config = function() {
    private$shared <- yaml::read_yaml(private$shared_config_path)
    private$handle_local(private$local_config_path)

    private$d_root <- private$handle_local_root(private$local$d_root)

    private$d_parents <- private$shared[["parent_components"]]
    private$f_names   <- private$shared[["file_names"]]
    private$f_exts    <- private$shared[["file_extensions"]]

    invisible(self)
  },

  handle_local = function(local_path){
    if (!file.exists(local_path)) {
      root <- readline(prompt = glue::glue("No config found at {local_path}, ",
                                           "Enter data root: "))


      private$local <- list(d_root = root)
    }
    if ("profiles" %in% names(private$local)) {
      private$profiles <- private$local$profiles
    } else {
      private$local$profiles <- list(default = private$local$d_root)
    }
    yaml::write_yaml(private$local, local_path)
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

# Initialize ===========

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
    private$log <- logging::getLogger(name = "PortrPath")
    private$local_config_path <- local_config_path
    private$shared_config_path <- shared_config_path

    private$read_config()

    private$build_whole_paths()

    invisible(self)
  },
  overwrite = TRUE
  )

# get_file_paths ===========

#' @describeIn PortrPath$files
#' @section DEPRECATED
#' @name PortrPath$get_file_paths
#' @rdname PortrPath$files
#' @aliases PortrPath-class-get_file_paths
#' @return A named list of file paths
NULL

PortrPath$set(
  "public", "get_file_paths",
  function(){
    warn("Deprecated, use $files")
    return(self$files)
  },
  overwrite = TRUE
)


#' Get a named list of file paths
#'
#' For files specified in the shared config, get a named list of their local
#'     paths
#' @name files
#' @param none
#' @name PortrPath$files
#' @param value unused
#' @return a list of files belonging to the object
NULL
PortrPath$set(
  "active", "files",
  function(value) {
    if (! missing(value)) stop("Accessor only!")
    return(private$f_paths)
  },
  overwrite = TRUE
)

# Active Profile ========

#' Set the active profile
#'
#' For set the current profile according to its id
#'    Also write the d_root to file
#'
#' @param value the identifying string for the profile
#' @name PortrPath$profile
#' @return current profile if not being set
NULL

PortrPath$set(
  "active", "profile",
  function(value) {
    if (missing(value)) return(private$profiles)
    if (! value %in% names(private$profiles)) stop(glue::glue("{value} is not a valid profile"))

    print(glue::glue("Setting profile to {value}"))
    self$root <- private$profiles[[value]]

  },
  overwrite = TRUE
)

# Add Profile ===============

#' Add a profile to the list of profiles
#'
#' Profiles allow the user to easily switch between some favourite paths
#' @param name the friendly name to refer to the profile
#' @param path the path the profile should refer to
#' @return None
#' @name PortrPath$add_profile
NULL
PortrPath$set(
  "public", "add_profile",
  function(name, path) {
    private$profiles[[name]] <- path
    private$local$profiles <- private$profiles
    yaml::write_yaml(private$local, private$local_config_path)
  },
  overwrite = TRUE
)

# Access root ========================

#' Get or set the current root
#'
#' Sets the current root if assigned a value, or gets it if not
#'
#' @name PortrPath$root
#' @param value the value of the root to be used. Either an absolute path
#'     or ".PROJECT_ROOT"
#' @return the current root, or nothing if setting
NULL

PortrPath$set(
  "active", "root",
  function(value){
    if (missing(value)) return(private$d_root)
    private$local$d_root <- value
    yaml::write_yaml(private$local, private$local_config_path)
    private$d_root <- private$handle_local_root(value)
    print(glue::glue("Setting root to {private$d_root}"))
    private$build_whole_paths()
  },
  overwrite = TRUE
)
