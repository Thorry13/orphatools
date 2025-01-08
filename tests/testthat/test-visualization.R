test_that("indentation", {
  orpha_codes = c(303, get_descendants('303'))
  df = data.frame(orpha_code=orpha_codes) %>%
    orpha_df(orpha_code_col='orpha_code') %>%
    left_join(load_nomenclature(), by='orpha_code')
  df_indent = apply_orpha_indent(df, indented_cols='label', prefix='Label_')

  expect_named(df_indent, c('orpha_code','level','status','type','Label_1','Label_2','Label_3'))
  expect_s3_class(df_indent, 'orpha_df')
})


test_that("plots", {
  G = get_descendants(c(303,304,305), output='graph')

  expect_s3_class(G, 'igraph')

  p1 = plot(G)
  vdiffr::expect_doppelganger("default plot", p1)

  p2 = plot(G, layout=layout_tree)
  vdiffr::expect_doppelganger("layout_tree plot", p2)

  ip1 = interactive_plot(G)
  expect_gt(nrow(ip1$x$nodes), 30)
  expect_equal(names(ip1$x$nodes), c("id", "label"))
  expect_gt(nrow(ip1$x$edges), 30)
  expect_equal(names(ip1$x$edges), c('from', 'to'))

  ip2 = interactive_plot(G, layout_tree=TRUE)
  expect_equal(names(ip2$x$nodes), c("id", "label", "x", "y"))
})


test_that("coloration", {
  G = get_descendants(c(303,304,305), output='graph')

  Gc = color_codes(G, c(303,304,305)) %>%
    color_class_levels()
  vertex_attrs = igraph::as_data_frame(Gc, what='vertices')

  expect_true(any(!is.na(vertex_attrs$label.color)))
  expect_true(any(!is.na(vertex_attrs$frame.color)))
  expect_true(any(!is.na(vertex_attrs$color)))

  pc = plot(Gc)
  vdiffr::expect_doppelganger("colored plot", pc)


  ipc = interactive_plot(Gc)
  expect_gt(nrow(ipc$x$nodes), 30)
  expect_equal(names(ipc$x$nodes),
               c("id", "label.color", "frame.color", "classLevel",
                 "label", "color.background", "color.highlight",
                 "color.border", "font.color"))
})
