test_that("single code specification", {
  # Basic usage
  orpha_code_cmt1 = '65753'
  orpha_code_cmtX = 64747

  ## Specification possible
  expect_equal(specify_code(orpha_code_cmt1, 'MPZ', mode='symbol'), '101082') # CMT1B is the only ORPHAcode both associated with CMT1 and MPZ
  expect_equal(specify_code(orpha_code_cmt1, c('MPZ', 'POMT1'), mode='symbol'), '101082') # Same because POMT1 doesn't bring ambiguity

  ## Specification impossible
  expect_true(specify_code(orpha_code_cmtX, 'MPZ', mode='symbol') == orpha_code_cmtX)
  expect_equal(specify_code(orpha_code_cmt1, 'PMP22', mode='symbol'), orpha_code_cmt1)
  expect_equal(specify_code(orpha_code_cmt1, c('MPZ', 'PMP22'), mode='symbol'), orpha_code_cmt1)

  ## Alternatively with HGNC codes (the default mode)
  expect_equal(specify_code(orpha_code_cmt1, 7225), '101082') # CMT1B is the only ORPHAcode both associated with CMT1 and MPZ
  expect_equal(specify_code(orpha_code_cmt1, c(7225, 9202)), '101082') # CMT1B is the only ORPHAcode both associated with CMT1 and MPZ
  expect_true(specify_code(orpha_code_cmtX, 7225)==orpha_code_cmtX) # No ORPHAcode is associated both to CMTX and MPZ
  expect_equal(specify_code(orpha_code_cmt1, 9118), orpha_code_cmt1) # Several ORPHAcodes are associated both to CMT1 and PMP22 (CMT1A and CMT1E)
  expect_equal(specify_code(orpha_code_cmt1, c(7225, 9118)), orpha_code_cmt1) # Several ORPHAcodes are associated both to CMT1 and PMP22 (CMT1A and CMT1E)
})

test_that('consistencies between ORPHAcodes and genes', {
  orpha_code_cmt1 = '65753'
  orpha_code_cmtX = 64747

  ## Consistent
  expect_true(check_orpha_gene_consistency(orpha_code_cmt1, 'MPZ', mode='symbol')) # Consistent because CMT1B
  expect_true(check_orpha_gene_consistency(orpha_code_cmt1, c('MPZ', 'POMT1'), mode='symbol')) # Consistent because CMT1B
  expect_true(check_orpha_gene_consistency(orpha_code_cmt1, 'PMP22', mode='symbol')) # Consistent because CMT1A or CMT1E
  expect_true(check_orpha_gene_consistency(orpha_code_cmt1, c('MPZ', 'PMP22'), mode='symbol')) # Consistent because CMT1A or CMT1B or CMT1E ? Or unconsistent because unexpected list of mutated genes ?

  ## Unconsistent
  expect_false(check_orpha_gene_consistency(orpha_code_cmtX, 'MPZ', mode='symbol'))

  ## Alternatively with HGNC codes (the default mode)
  expect_true(check_orpha_gene_consistency(orpha_code_cmt1, 7225)) # Consistent because CMT1B
  expect_true(check_orpha_gene_consistency(orpha_code_cmt1, c(7225, 9202))) # Consistent because CMT1B
  expect_true(check_orpha_gene_consistency(orpha_code_cmt1, 9118)) # Consistent because CMT1A or CMT1E
  expect_true(check_orpha_gene_consistency(orpha_code_cmt1, c(7225, 9118))) # Consistent because CMT1A or CMT1B or CMT1E ? Or unconsistent because unexpected list of mutated genes ?
  expect_false(check_orpha_gene_consistency(orpha_code_cmtX, 7225)) # Unconsistent

})


test_that("multiple codes specifications", {
  df = tibble(
    patient_id=c('A', 'A', 'B', 'C', 'D', 'D'),
    initial_orpha_code = c("65753", "65753", "903", "65753", "65753", "65753"), # CMT1 and von Willebrand
    symbol = c("MPZ", "LITAF", "VWF", "LITAF", "MPZ", "VWF")) # MPZ, VWF and LITAF

  # External gene as argument
  df_spec1 = df %>% mutate(assigned_orpha_code = specify_code(initial_orpha_code, genes='MPZ', mode='symbol'))
  expect_equal(df_spec1$assigned_orpha_code, c('101082', '101082', '903', '101082', '101082', '101082'))

  # Use gene in column
  df_spec2 = df %>% mutate(assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol'))
  expect_equal(df_spec2$assigned_orpha_code, c('101082', '101083', '903', '101083', '101082', '65753'))

  # Consider all genes as belonging to the same patient (default considers each row as independent)
  df_nogroup = df %>% mutate(assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol', .by=NULL))
  expect_equal(df_nogroup$assigned_orpha_code, c('65753', '65753', '903', '65753', '65753', '65753'))

  # Combine genes for each patient if multiple mutations were observed
  df_patient_groups = df %>% mutate(assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol', .by=patient_id))
  expect_equal(df_patient_groups$assigned_orpha_code, c('65753', '65753', '903', '101083', '101082', '101082'))
})


test_that("specification with a list-column for genes", {
  df = tibble(
    patient_id=c('A', 'B', 'C', 'D'),
    initial_orpha_code = c("65753", "903", "65753", "65753"), # CMT1 and von Willebrand
    symbol = list(c("MPZ", "LITAF"), "VWF", "LITAF", c("MPZ", "VWF")))

  # This method is an alternative to the last above
  df_old = df %>% unnest(symbol)
  df_spec_old = df_old %>% mutate(assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol', .by=patient_id)) %>% tidyr::chop(symbol)
  df_spec = df %>% mutate(assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol'))
  expect_equal(df_spec$assigned_orpha_code, df_spec_old$assigned_orpha_code)

  # The method is intended for data already grouped by patients
  df2 = tibble(
    patient_id=c('A', 'A', 'B', 'C', 'D'),
    initial_orpha_code = c("65753", "65753", "903", "65753", "65753"), # CMT1 and von Willebrand
    symbol = list(c("MPZ", "VWF"), c("LITAF", "VWF"), "VWF", "LITAF", c("MPZ", "VWF")))
  df2_old = df2 %>% mutate(i = row_number()) %>% unnest(symbol)
  df2_old_spec = df2_old %>% mutate(assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol', .by=patient_id)) %>% tidyr::chop(symbol)

  # Incorrect
  df2_spec1 = df2 %>% mutate(assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol'))
  expect_false(all(df2_spec1$assigned_orpha_code == df2_old_spec$assigned_orpha_code))

  # Corrected
  df2_spec2 = df2 %>% mutate(assigned_orpha_code = specify_code(initial_orpha_code, genes=symbol, mode='symbol', .by=patient_id))
  expect_equal(df2_spec2$assigned_orpha_code, df2_old_spec$assigned_orpha_code)
})

library(tictoc)
test_that('efficiency', {
  df_active = load_raw_nomenclature() %>% filter(status %in% c(1,129,513))
  df_classif = load_all_classifications()
  df_pref_symbols = load_genes_synonyms() %>%
    distinct(gene_id, pref_symbol)
  df_parents_children = load_associated_genes() %>% # Get all ORPHAcode-gene pairs
    semi_join(df_active, by='orpha_code') %>% # Keep active ORPHAcodes only
    left_join(df_classif, by=c('orpha_code'='to'), relationship = 'many-to-many') %>% # Join their parents
    left_join(df_pref_symbols, by='gene_id') %>% # Join pref symbols
    rename(parent_code=from) %>%
    group_by(orpha_code) %>%
    mutate(parent_code=first(parent_code)) %>% # Keep one parent
    ungroup() %>%
    distinct()

  # There are more than 8000 parents to respecify
  tic()
  expect_warning(
    A <- df_parents_children %>%
      mutate(reassigned_code = specify_code(parent_code, genes=HGNC, mode='HGNC')),
    'greater than 10'
  )
  t1 = toc()
  tic()
  expect_warning(
    B <- df_parents_children %>%
      mutate(reassigned_code = specify_code(parent_code, genes=pref_symbol, mode='symbol')),
    'greater than 10')
  t2 = toc()

  tic()
  expect_warning(
    C <- df_parents_children %>%
      mutate(reassigned_code = specify_code(parent_code, genes=pref_symbol, mode='symbol', .by=(1:nrow(df_parents_children))%%2)),
    'greater than 10')
  t3 = toc()

  expect_lt(t1$toc-t1$tic, 40)
  expect_lt(t2$toc-t2$tic, 40)
  expect_lt(t3$toc-t3$tic, 60)

  expect_true(any(A$reassigned_code == A$orpha_code))
  expect_true(any(B$reassigned_code == A$orpha_code))
  expect_true(any(C$reassigned_code == A$orpha_code))

})
