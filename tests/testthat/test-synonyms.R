test_that("synonyms", {
  df_synonyms = load_synonyms()
  expect_equal(ncol(df_synonyms), 3)
  expect_gt(nrow(df_synonyms), 18000)

  expect_length(get_all_labels(166100), 4)
})
