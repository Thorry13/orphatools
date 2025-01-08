# Check initial versions (internal)
test_that("finds internal pack versions", {
  # include getOption verification
  df_versions = get_pack_versions()
  expect_equal(df_versions %>% ncol(), 3)
  expect_true('internal' %in% df_versions$location)
})

# Test adding new pack
test_that("adds a new pack nomenclature", {
  usr_dir = tools::R_user_dir('orphatools', 'config')
  versions_path = file.path(usr_dir, 'pack_versions.csv')
  df_versions_backup = read.csv2(versions_path)
  if(file.exists(versions_path))
    file.remove(versions_path)
  pack_zipfile = test_path('fixtures', 'Orphanet_Nomenclature_Pack_FR_short.zip')
  expect_error(add_nomenclature_pack('inexisting.file'), 'not found')
  # expect_error(add_nomenclature_pack('corrupt_zip'))
  # expect_snapshot(add_nomenclature_pack(pack_zipfile))
  expect_message(out <- add_nomenclature_pack(pack_zipfile), 'Loading and processing') %>%
    expect_message('Adding new') %>%
    expect_message('succesfully added')
  expect_true(out)

  df_versions = get_pack_versions()
  expect_equal(dim(df_versions), c(2,3))

  save_pack_versions(df_versions_backup)
})

# Test adding existing pack
test_that("adds existing nomenclature", {
  pack_zipfile = test_path('fixtures', 'Orphanet_Nomenclature_Pack_EN.zip')
  # force = FALSE
  expect_message(add_nomenclature_pack(pack_zipfile), 'Loading and processing') %>%
    expect_error('already exists')

  # what if force = TRUE for this one (see distinct in data-pack.R)
  pack_zipfile = test_path('fixtures', 'Orphanet_Nomenclature_Pack_FR_short.zip')
  # force = TRUE
  expect_message(out <- add_nomenclature_pack(pack_zipfile, force=TRUE), 'Loading and processing') %>%
    expect_warning('replaced') %>%
    expect_message('succesfully added')
  expect_true(out)

  expect_true(ncol(get_pack_versions())==3)
  expect_true(all(get_pack_versions()$location == 'internal' | file.exists(get_pack_versions()$location)))
  expect_true(sum(get_pack_versions()$default) == 1)
})

# Test adding default
test_that("set default correctly", {
  version_backup = get_pack_versions() %>% filter(default) %>% pull(version)

  pack_zipfile = test_path('fixtures', 'Orphanet_Nomenclature_Pack_FR_short.zip')
  # force = TRUE
  expect_message(out <- add_nomenclature_pack(pack_zipfile, force=TRUE, default=TRUE), 'Loading and processing') %>%
    expect_warning('replaced') %>%
    expect_message('succesfully added')
  expect_true(out)

  expect_true(ncol(get_pack_versions())==3)
  expect_true(get_pack_versions() %>% filter(default) %>% pull(location) %>% str_ends('.rda'))

  # Roll back to old default
  set_default_pack_version(version_backup)
})
