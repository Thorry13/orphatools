test_that("nomenclature", {
  df_nomenclature = load_nomenclature()

  expect_equal(ncol(df_nomenclature), 5)
  expect_gt(nrow(df_nomenclature), 10000)

  expect_equal(get_label(93928), 'Isolated epispadias')
  expect_equal(get_label('93928'), 'Isolated epispadias')
  expect_equal(get_classification_level(488437), 'Disorder')
  expect_true(stringr::str_detect(get_status(254803), 'ctive'))
  expect_true(is_active(262950) %in% c(TRUE, FALSE))
  expect_equal(get_type(269510), 'Clinical subtype')
})
