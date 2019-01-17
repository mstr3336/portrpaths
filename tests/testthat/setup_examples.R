logging::loginfo("Running setup!")
logging::setLevel("WARN", container = "PortrPath")

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
