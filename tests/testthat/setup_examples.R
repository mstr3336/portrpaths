logging::loginfo("Running setup!")

setup_t1_example <- function(local_path, shared_path){
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
