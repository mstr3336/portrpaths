logging::loginfo("Running setup!")
main_log <- logging::getLogger(name = "PortrPath")
test_log <- logging::getLogger(name = "testing")
test_log$setLevel("WARN")
main_log$setLevel("WARN")


setup_t1 <- function(local_path, shared_path){
  local <- setup_t1_local()
  shared <- setup_t1_shared()
  yaml::write_yaml(local, local_path)
  yaml::write_yaml(shared, shared_path)
}


setup_t1_shared <- function(){
  shared <- list(
    parent_components = list(
      "inst",
      "test_data",
      "t1",
      "data"
    ),
    file_names = list(
      encs = "cardi_encs",
      diag = "cardi_diags",
      form = "cardi_forms"
    ),
    file_extensions = list(
      encs = "txt",
      diag = "txt",
      form = "made_up_ext"
    )
  )
  return(shared)
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

setup_t2_shared <- function(){
  shared <- setup_t1_shared()
  shared$parent_components <- list("test_data", "t2", "data")
  return(shared)
}

setup_t2 <- function(local_path, shared_path){
  shared <- setup_t2_shared()
  local <- setup_t2_local()
  yaml::write_yaml(shared, shared_path)
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

setup_profile_shared <- function(){
  out <- setup_t1_shared()
  out$parent_components <- c('data')
  return(out)
}

setup_profile_test <- function(local_path, shared_path){
  shared <- setup_profile_shared()
  local <- setup_profile_local()

  #print(glue::glue("{c('shared', 'local')}: {c(shared_path, local_path)}"))

  yaml::write_yaml(shared, shared_path)
  yaml::write_yaml(local, local_path)
}
