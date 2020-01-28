context("Test loading and assigning profiles works")

test_that("Can load file with profiles", {
  prof_d <- glue::glue(
    "{root}{sep}profile",
    root = system.file("test_data", package = "portrpaths"),
    sep = .Platform$file.sep)


  local_path <- glue::glue("{prof_d}{sep}local.yaml",
                                sep = .Platform$file.sep)

  setup_profile_test(local_path)

  pather <- PortrPath$new(local_path)

  expect_profiles <- setup_profile_local_expected()
  actual_profiles <- pather$profile

  profile_sets <- list(actual = actual_profiles, expect = expect_profiles)


  expect_equivalent(!!actual_profiles  %>% .[sort(names(.))], !!expect_profiles  %>% .[sort(names(.))])
})
