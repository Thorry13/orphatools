test_that("finds initial dictionaries versions", {
  df_dict = get_dict_versions()
  expect_equal(ncol(df_dict), 3)
  expect_true('internal' %in% df_dict$location)
  expect_equal(df_dict %>% filter(default==TRUE) %>% nrow(), 1)
})

test_that("add dictionary", {
  # Get existing versions
  usr_dir = tools::R_user_dir('orphatools', 'config')
  versions_path = file.path(usr_dir, 'dict_versions.csv')
  if(file.exists(versions_path))
    file.remove(versions_path)

  # Add new dictionary
  filepath = test_path('fixtures', 'dict_fr.csv')
  expect_error(add_dictionary('inexisting.file'), 'not found')
  expect_message(out <- add_dictionary(filepath), 'Adding new') %>%
    expect_message('succesfully added')
  expect_true(out)

  # Add existing dictionary
  filepath = test_path('fixtures', 'dict_en.csv')
  expect_message(out <- add_dictionary(filepath), 'Adding new') %>%
    expect_error('already exists')
  expect_true(out)

  # Check versions
  df_dict = get_dict_versions()
  expect_equal(dim(df_dict), c(2,3))
  expect_equal(df_dict %>% filter(default==TRUE) %>% nrow(), 1)
})

