test_that('global classification', {
  all_classif = load_classifications()
  df_classif = bind_rows(all_classif) %>% distinct()
  G = igraph::graph_from_data_frame(df_classif)

  expect_gt(length(all_classif), 30) # More than 30 classification heads
  expect_equal(names(df_classif), c('from', 'to'))
  expect_true(abs(length(find_roots(G)) - length(all_classif)) < 5) # classification heads after merging the whole classification
})

library(tictoc)
test_that("basic genealogy", {
  expect_setequal(get_parents(303), as.character(c(139027, 79361)))
  expect_setequal(get_children(631), as.character(c(629,231662,231671,231679,231692)))
  expect_setequal(get_ancestors(139027), as.character(c(93890,183530,98053)))
  tic();expect_equal(get_descendants(281241), as.character(c(2671,583607,583612,583602,2697,585,85212,66631)));t=toc()
  expect_lt(t$toc - t$tic, 0.2) # execution should be quick
  expect_equal(get_siblings(308552), '420429')

  # change output
  expect_equal(get_parents(303, output='edgelist') %>% names(), c('from', 'to'))
  expect_equal(get_children(631, output='edgelist') %>% names(), c('from', 'to'))
  expect_equal(get_ancestors(139027, output='edgelist') %>% names(), c('from', 'to'))
  expect_equal(get_descendants(281241, output='edgelist') %>% names(), c('from', 'to'))
  expect_equal(get_siblings(308552, output='edgelist') %>% names(), c('from', 'to'))

  expect_s3_class(get_parents(303, output='graph'), 'igraph')
  expect_s3_class(get_children(631, output='graph'), 'igraph')
  expect_s3_class(get_ancestors(139027, output='graph'), 'igraph')
  expect_s3_class(get_descendants(281241, output='graph'), 'igraph')
  expect_s3_class(get_siblings(308552, output='graph'), 'igraph')

  expect_lte(get_ancestors(139027, max_depth = 2, output='graph') %>% igraph::diameter(), 3)
  expect_lte(get_descendants(139027, max_depth = 2, output='graph') %>% igraph::diameter(), 3)

  # check errors
  expect_error(get_ancestors(139027, max_depth = -1), 'must be NULL or greater than 0')
  expect_error(get_descendants(139027, max_depth = -1), 'must be NULL or greater than 0')

  # check warnings
  expect_warning(get_parents(11111111), 'not belong')
  expect_warning(get_children(11111111), 'not belong')
  expect_warning(get_descendants(11111111), 'not belong')
  expect_warning(get_ancestors(11111111), 'not belong')
  expect_warning(get_siblings(11111111), 'not belong')

})

test_that('advanced genealogy', {
  # merge branches
  expect_gt(get_descendants(c(139027, 79361)) %>% length, 200)
  expect_equal(get_ancestors(c(139027, 79361), output='edgelist') %>% names(), c('from', 'to'))
  expect_s3_class(get_ancestors(c(139027, 79361), output='graph'), 'igraph')

  # complete family
  expected_res = c("303","595356","79408","79409","79411","89842","89843","231568","79410","158673","158676")
  expect_setequal(complete_family(c(158676, 158673), max_depth=2), expected_res)
  expect_equal(complete_family(c(158676, 158673), max_depth=2, output='edgelist') %>% names(), c('from', 'to'))
  expect_s3_class(complete_family(c(158676, 158673), max_depth=2, output='graph'), 'igraph')

  # LCAs
  expect_setequal(get_LCAs(c(303,305,595356)), c('139027', '79361'))
})

test_that('classification levels', {
  library(tictoc)

  # Subtypes to disorder
  df_nomenclature = load_raw_nomenclature()
  some_subtypes = df_nomenclature %>% filter(status %in% c(1,129,513), level == 36554) %>% pull(orpha_code) %>% sample(100)

  tic()
  df_disorders = data.frame(subtype_code = some_subtypes) %>%
    mutate(associated_disorder = subtype_to_disorder(subtype_code)) %>%
    unnest(associated_disorder, keep_empty = TRUE) %>%
    left_join(df_nomenclature, c('associated_disorder'='orpha_code'))
  t = toc()

  expect_true(all(df_disorders$level == 36547))
  expect_lt(t$toc - t$tic, 10)

  # Closest groups
  expect_equal(get_lowest_groups(orpha_code='158676'), '303')
})

test_that('merge graphs', {
  G_ancestors = get_ancestors(c(303,304,305), output='graph')
  G_descendants = get_descendants(c(303,304,305), output='graph')
  G_merged = merge_graphs(list(G_ancestors, G_descendants))

  E_ancestors = igraph::E(G_ancestors)
  E_global = igraph::E(G_merged)

  expect_true(all(names(igraph::V(G_ancestors)) %in% names(igraph::V(G_merged))))
  expect_true(all(names(igraph::V(G_descendants)) %in% names(igraph::V(G_merged))))
  expect_true(all(igraph::E(G_ancestors) %in% igraph::E(G_merged)))
  expect_true(all(igraph::E(G_descendants) %in% igraph::E(G_merged)))
})
