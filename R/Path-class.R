
#' OO Path Navigator
#'
#' @importFrom rlang %||%
#' @importFrom magrittr %<>%
#' @export
Path <- R6::R6Class(
  "Path",
  public = list(
    initialize = function(path = NULL) private$abstract,
    join = function(other) private$abstact,
    climb = function(levels) private$abstract,
    print = function() print(self$show)
  ),
  active = list(
    # Active Bindings =======
    name = function(x) private$abstract(),
    show = function(x) private$abstract(),
    c = function(x) private$abstract(),
    parent = function(x) private$abstract()
  ),
  private = list(
    # Private Variables =====

    path = NULL,
    abstract = function() stop("Not written")
  )
)


# Implementations ========================

# Public ========

# intitialize ====

Path$set(
  "public", "initialize",
  function(path = NULL){
    if (is.null(path)) stop("Can't supply null path")
    private$path <- path
  },
  overwrite = TRUE
)

# join ====
Path$set(
  "public", "join",
  function(other){
    other %<>% as.character()
    self$show %>%
      pathr::file_path(other) %>%
      Path$new() %>%
    return()
  },
  overwrite = TRUE
)

# Active ============

Path$set(
  "active", "c",
  function(x){
    if (!missing(x)) stop("Dont assign to me!")

    return(private$get_children())
  },
  overwrite = TRUE
)

Path$set(
  "active", "parent",
  function(x){
    if (!missing(x)) stop("Don't assign to me!")

    out <- private$path %>% dirname() %>% Path$new()
    return(out)
  },
  overwrite = TRUE
)

Path$set(
  "active", "name",
  function(x){
    if (!missing(x)) stop("Don't assign to me!")
    out <- private$path %>%
      pathr::parse_path() %>% pathr::back()
    return(out)
  },
  overwrite = TRUE
)

Path$set(
  "active", "show",
  function(x){
    if (!missing(x)) stop("Don't assign to me!")
    out <- private$path
    return(out)
  },
  overwrite = TRUE
)

# Private =============================

# Get Children =======

Path$set(
  "private", "get_children",
  function(){
    in_dir <- base::dir(private$path)

    paths <- glue("{private$path}/{in_dir}")
    out <- list()
    out[in_dir] <- paths
    return(out)
  },
  overwrite = TRUE
)



# S3 Methods ========================

as.character.Path <- function(x){
  return(x$show)
}

#' @export
`%//%` <- function(lhs, rhs){
  lhs %<>% as.character() %>% Path$new()
  rhs %<>% as.character() %>% Path$new()
  return(lhs$join(rhs))
}
