test_that("orpha_df init", {
  df_patients = data.frame(
    patient_id = c(1,1,2,3,4,5,6),
    code = c('303', '158673', '595356', '305', '79406', '79406', '595356'))
  expect_error(orpha_df(df_patients, mode='not'), "Invalide `mode` argument")
  expect_error(orpha_df(TRUE), 'not a data.frame.')
  expect_warning(orpha_df(df_patients, orpha_code_col = 'missing.col'), 'remains unchanged')
  expect_s3_class(orpha_df(df_patients, orpha_code_col= 'code'), 'orpha_df')
  expect_false(inherits(orpha_df(df_patients, orpha_code_col= 'code') %>% orpha_df(mode='off'), 'orpha_df'))
})

test_that("orpha_df stability", {
  df_patients = tibble(
    patient_id = c(1,1,2,3,4,5,6,6),
    code = c('303', '158673', '595356', '305', '79406', '79406', '306504', '595356'),
    hgnc = list(NULL, NULL, NULL, 6139, NULL, NULL, NULL, NULL))
  df_patients2 = orpha_df(df_patients, orpha_code_col = 'code')

  expect_s3_class(df_patients2, 'orpha_df')
  expect_s3_class(df_patients2 %>% setNames(c('A', 'B')), 'orpha_df')
  expect_s3_class(df_patients2 %>% filter(patient_id > 3), 'orpha_df')

  expect_s3_class(df_patients2 %>% select(code), 'orpha_df')
  expect_s3_class(df_patients2['code'], 'orpha_df')
  expect_warning(
    orpha_df(df_patients, orpha_code_col = 'code') %>% select(-code),
    'was removed')
  expect_warning(
    orpha_df(df_patients, orpha_code_col = 'code')['patient_id'],
    'was removed')

  expect_warning(df_patients2_t1 <- df_patients2 %>% mutate(code=as.numeric(code)), 'was modified')
  expect_false(inherits(df_patients2_t1, 'orpha_df'))

  # Grouping
  expect_s3_class(group_by(df_patients2), 'orpha_df')
  expect_false(inherits(group_by(df_patients2), 'grouped_orpha_df'))
})

test_that("grouped_orpha_df stability", {
  df_patients = tibble(
    patient_id = c(1,1,2,3,4,5,6,6),
    code = c('303', '158673', '595356', '305', '79406', '79406', '306504', '595356'),
    hgnc = list(NULL, NULL, NULL, 6139, NULL, NULL, NULL, NULL))
  df_grouped = orpha_df(df_patients, orpha_code_col = 'code', force_codes = 305) %>% group_by(code)

  expect_s3_class(df_grouped, 'grouped_orpha_df')
  expect_s3_class(df_grouped %>% setNames(c('A', 'B')), 'grouped_orpha_df')
  expect_s3_class(df_grouped %>% filter(patient_id > 3), 'grouped_orpha_df')
  expect_s3_class(df_grouped %>% select(code), 'grouped_orpha_df')
  expect_s3_class(df_grouped %>% select(code), 'grouped_orpha_df')
  expect_s3_class(df_grouped['code'], 'grouped_orpha_df')

  expect_warning(df_grouped_t1 <- df_grouped %>% mutate(code = as.numeric(code)), 'was modified')
  expect_false(inherits(df_grouped_t1, 'orpha_df'))
  expect_true(inherits(df_grouped_t1, 'grouped_df'))
  expect_equal(attr(df_grouped_t1, 'groups')$.rows %>% unlist(), c(1,4,5,6,2,7,3,8))

  expect_s3_class(ungroup(df_grouped), 'orpha_df')
  expect_s3_class(df_grouped %>% group_by(patient_id), 'grouped_orpha_df')
  expect_s3_class(df_grouped %>% group_by(patient_id, .add=TRUE), 'grouped_orpha_df')
})



test_that("aggregation test", {
  df_patients = data.frame(
    patient_id = c(1,1,2,3,4,5,6),
    code = c('303', '158673', '595356', '305', '79406', '79406', '595356'))
  df_grouped = orpha_df(df_patients, orpha_code_col = 'code') %>% group_by(code)

  df_counts = df_patients %>%
    orpha_df(orpha_code_col = 'code') %>%
    group_by(code) %>%
    count() %>%
    as.data.frame() %>%
    arrange(code)
  expect_equal(df_counts$n, c(1,4,3,3,2))

  df_counts = df_patients %>%
    orpha_df(orpha_code_col = 'code') %>%
    group_by(code) %>%
    summarize(n=n_distinct(patient_id)) %>%
    as.data.frame() %>%
    arrange(code)
  expect_equal(df_counts$n, c(1,3,3,3,2))

  df_patients_ext = df_patients %>%
    orpha_df(orpha_code_col = 'code') %>%
    group_by(code) %>%
    mutate(n=n_distinct(patient_id)) %>%
    as.data.frame() %>%
    arrange(code)
  expect_equal(df_patients_ext$n, c(1,3,3,3,3,2,2))
})


# Future tests ?
  # expect_s3_class(df_patients2_t1, 'orpha_df') # +check classif
  # expect_true(is.numeric(attr(df_patients2_t1, 'df_classif')$df_classif$from))
  # expect_true(is.numeric(attr(df_patients2_t1, 'df_classif')$df_classif$to))
  #
  # df_patients2_t2 = df_patients2 %>% mutate(code=paste0('ORPHA:',code))
  # expect_s3_class(df_patients2_t2, 'orpha_df') # +check classif
  # expect_true(all(attr(df_patients2_t2, 'df_classif')$df_classif$from %>% str_starts('ORPHA:')))
  # expect_true(all(attr(df_patients2_t2, 'df_classif')$df_classif$to %>% str_starts('ORPHA:')))
  #
  # df_patients2_t3 = df_patients2 %>% mutate(code=redirect_code(code, hgnc))
  # expect_s3_class(df_patients2_t3, 'orpha_df') # +classif unchanged
  #
  # df_patients2_t4 = df_patients2 %>% mutate(code=subtype_to_disorder(code))
  # expect_s3_class(df_patients2_t3, 'orpha_df') # +classif unchanged

  # expect_false(inherits(df_patients2 %>% mutate(code = 'constant'), 'orpha_df'))


#   expect_s3_class(df_grouped_t1, 'grouped_orpha_df')
#   expect_true('code' %in% group_vars(df_grouped_t1))
#   expect_true(is.numeric(attr(df_grouped_t1, 'groups')$code))
#   expect_true(is.numeric(df_grouped_t1 %>% summarize(n = n_distinct(patient_id)) %>% pull(code)))
#
#   df_grouped_t2 = df_grouped %>% mutate(code=paste0('ORPHA:',code))
#   expect_s3_class(df_grouped_t2, 'grouped_orpha_df')
#   expect_true(all(attr(df_grouped_t2, 'groups')$code %>% str_starts("ORPHA:")))
#   expect_true(all(attr(df_grouped_t2, 'df_classif')$from %>% str_starts("ORPHA:")))
#
#   df_grouped_t3 = df_grouped %>% mutate(code=redirect_code(code, hgnc))
#   expect_s3_class(df_grouped_t3, 'orpha_df')
#   expect_true(setequal(attr(df_grouped, 'groups')$code, attr(df_grouped_t3, 'groups')$code)) # It would have been FALSE without forced codes (make a test for it)
#   expect_false(all(mapply(setequal, attr(df_grouped, 'groups')$code, attr(df_grouped_t3, 'groups')$code))) # groups distribution changed
#
#   df_grouped_t4 = df_grouped %>% mutate(code=subtype_to_disorder(code))
#   expect_s3_class(df_grouped_t4, 'grouped_orpha_df')
#   expect_false(setequal(attr(df_grouped, 'groups')$code, attr(df_grouped_t4, 'groups')$code)) # subtypes disapear
#   expect_false(all(mapply(setequal, attr(df_grouped, 'groups')$code, attr(df_grouped_t4, 'groups')$code))) # groups distribution changed
#
#   df_grouped_t5 = df_grouped %>% mutate(code = 'constant')
#   expect_false(inherits(df_grouped_t5, 'orpha_df'))
#   expect_s3_class(df_grouped_t5, 'grouped_df')
