# Check initial versions (internal)
test_that("finds initial genes versions", {
  df_versions = get_genes_versions()
  expect_equal(df_versions %>% ncol(), 3)
  expect_true('internal' %in% df_versions$location)
  expect_equal(df_versions %>% filter(default==TRUE) %>% nrow(), 1)
})

# Test adding new genes file
test_that("adds a new genes associations file", {
  # Get existing versions
  usr_dir = tools::R_user_dir('orphatools', 'config')
  versions_path = file.path(usr_dir, 'genes_versions.csv')
  if(file.exists(versions_path))
    file.remove(versions_path)

  # Inexisting gile
  expect_error(add_associated_genes('inexisting.file'), 'not found')

  # Existing file but corrupted
  # ...

  # Valid existing file
  filepath = test_path('fixtures', 'genes-202306_en.xml')
  expect_message(out <- add_associated_genes(filepath), 'Loading and processing') %>%
    expect_message('Adding new') %>%
    expect_message('succesfully added')
  expect_true(out)

  # check versions
  df_versions = get_genes_versions()
  expect_equal(dim(df_versions), c(2,3))
  expect_equal(df_versions$default, c(TRUE, FALSE))
  expect_equal(df_versions %>% filter(default==TRUE) %>% nrow(), 1)
})

# Test adding existing genes file
test_that("adds existing genes file", {
  # force = FALSE
  filepath = test_path('fixtures', 'genes-202312_en.xml')
  expect_message(add_associated_genes(filepath), 'Loading and processing') %>%
    expect_error('already exists')
  expect_equal(get_genes_versions() %>% dim(), c(2,3))

  # force = TRUE
  filepath = test_path('fixtures', 'genes-202306_en.xml')
  expect_message(out <- add_associated_genes(filepath, force=TRUE), 'Loading and processing') %>%
    expect_warning('replaced') %>%
    expect_message('succesfully added')
  expect_true(out)

  # check versions
  df_versions = get_genes_versions()
  expect_equal(dim(df_versions), c(2,3))
  expect_equal(df_versions %>% filter(default==TRUE) %>% nrow(), 1)
})

# Test adding default
test_that("set default correctly", {
  expect_equal(get_genes_versions() %>% filter(default) %>% pull(location), 'internal')

  filepath = test_path('fixtures', 'genes-202306_en.xml')
  expect_message(out <- add_associated_genes(filepath, force=TRUE, default=TRUE), 'Loading and processing') %>%
    expect_warning('replaced') %>%
    expect_message('succesfully added')
  expect_true(out)

  # check versions
  df_versions = get_genes_versions()
  expect_equal(dim(df_versions), c(2,3))
  expect_true(df_versions %>% filter(default) %>% pull(location) %>% str_ends('.rda'))
  expect_equal(df_versions %>% filter(default==TRUE) %>% nrow(), 1)

  # roll back to old default
  set_default_genes_version(get_genes_versions() %>% filter(location=='internal') %>% pull(version))
})


