logging::loginfo("Running setup!")
main_log <- logging::getLogger(name = "PortrPath")
test_log <- logging::getLogger(name = "testing")
test_log$setLevel("WARN")
main_log$setLevel("WARN")


setup_t1 <- function(local_path){
  local <- setup_t1_local()
  yaml::write_yaml(local, local_path)
}

setup_t1_local <- function(){
  out <- list(
    d_root = ".PROJECT_ROOT"
  )
  return(out)
}

setup_t2_local <- function(){
  local <- setup_t1_local()
  root <- here::here()
  local$d_root <- glue::glue("{root}{sep}inst",
                             sep = .Platform$file.sep)
  return(local)
}

setup_t2 <- function(local_path){
  local <- setup_t2_local()
  yaml::write_yaml(local, local_path)

}

expect_files <- function(portrpath){
  paths <- portrpath$files
  for (nm in names(paths)){
    f <- paths[[nm]]
    expect_true(file.exists(f), info = glue::glue("Can {nm}: {f} be found?"), label = nm)
  }
}

setup_profile_local <- function(){
  base_root <- system.file("test_data", package = "portrpaths")
  alts <- c("t1", "t2")
  roots <- list()
  roots[alts] <- glue::glue("{base_root}{sep}{alts}", sep = .Platform$file.sep)
  test_log$info("Roots: {roots}")
  local <- list(
    d_root = roots[["t1"]],
    profiles = list(
      default = roots[["t1"]],
      t1 = roots[["t1"]],
      t2 = roots[["t2"]]
    )
  )
  return(local)
}

setup_profile_local_expected <- function() {
  base_root <- system.file("test_data", package = "portrpaths")
  alts <- c("t1", "t2")
  roots <- list()
  roots[alts] <- glue::glue("{base_root}{sep}{alts}", sep = .Platform$file.sep)

  roots$default <- roots[["t1"]]

  return(roots)
}

setup_profile_test <- function(local_path){
  local <- setup_profile_local()

  yaml::write_yaml(local, local_path)
}
