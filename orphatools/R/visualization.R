#' Color graphs
#'
#' @description
#' `color_codes` colors the given ORPHAcodes to emphasize them among the others.
#'
#' `color_classif_levels` colors all vertices of the given graph to distinguish classification levels.
#'
#' @param graph The graph to color.
#' @param orpha_codes The nodes to color.
#'
#' @return The colored graph
#'
#' @import magrittr
#' @importFrom igraph set_vertex_attr V
#'
#' @export
#'
#' @examples
#' # Build graph
#' init_codes = c(303, 305)
#' graph = merge_branches(init_codes, direction='down', output='graph')
#' plot(graph)
#'
#' # Emphasize some specific ORPHAcodes
#' graph = color_codes(graph, init_codes)
#' plot(graph)
#'
#' # Distinguish classification levels
#' graph = color_class_levels(graph)
#' plot(graph)
#' @name color-graphs

#' @rdname color-graphs
#' @export
color_codes = function(graph, orpha_codes){
    nodes = V(graph) %>% names
    graph = set_vertex_attr(graph, 'label.color', index=nodes %in% orpha_codes, '#CD0000') # red3
    graph = set_vertex_attr(graph, 'frame.color', index=nodes %in% orpha_codes, '#FF0000') # red
}


#' @rdname color-graphs
#' @export
color_class_levels = function(graph){
  # Determine classification levels for each ORPHAcodes
  class_levels = get_classification_level(V(graph) %>% names())

  # Color nodes
  graph = set_vertex_attr(graph, 'classLevel', index=class_levels == 'Group', 'Group')
  graph = set_vertex_attr(graph, 'color', index=class_levels == 'Group', '#4169E1') # royalblue1
  graph = set_vertex_attr(graph, 'classLevel', index=class_levels == 'Disorder', 'Disorder')
  graph = set_vertex_attr(graph, 'color', index=class_levels == 'Disorder', '#7CCD7C') # palegreen3
  graph = set_vertex_attr(graph, 'classLevel', index=class_levels == 'Subtype', 'Subtype')
  graph = set_vertex_attr(graph, 'color', index=class_levels == 'Subtype', '#CD96CD') # plum3
}


#' Plot using a tree-like layout
#'
#' @description
#' This layout function was specifically designed to display Orphanet classifications.
#' Indeed, it only contains from/to relationships, so the latter must be oriented from top to bottom (or bottom to top if `reverse_y` is `TRUE`).
#'
#' The function was designed to be used with [plot.igraph()] (`layout` argument).
#'
#' The associated vertical positions are obtained using `vertical_positions`.
#'
#' @param graph The graph to display.
#' @param reverse_y If `TRUE`, display from top to bottom.
#'
#' @return An adapted layout to plot nodes from Orpha classifications
#' @import magrittr
#' @importFrom dplyr group_by summarize filter select full_join n_distinct pull
#' @importFrom igraph as_data_frame
#' @export
#'
#' @examples
#' graph = get_ancestors('303', output='graph')
#'
#' plot(graph)
#' plot(graph, layout=igraph::layout_as_tree)
#' plot(graph, layout=layout_tree)
#'
#' df_y = vertical_positions(graph)
layout_tree = function(graph, reverse_y=TRUE)
{
  options(dplyr.summarise.inform=F)

  # Y positions
  df_y = vertical_positions(graph)
  df_y$y = (-1)^reverse_y * df_y$y

  # X positions
  h_size = horizontal_sizes(graph, df_y)
  df_x = horizontal_positions(graph, df_y, h_size)

  df_nodes = full_join(df_x, df_y, by='name')
  if(n_distinct(df_nodes$x) > 1)
    df_nodes$x = scale(df_nodes$x)

  # Reorder
  v = graph %>% as_data_frame(what='vertices')
  df_nodes = df_nodes[match(v$name, df_nodes$name),c('x','y')]

  return(df_nodes %>% as.matrix())
}


#' @rdname layout_tree
#' @export
vertical_positions = function(graph)
{
  # Initialization
  df_nodes = as_data_frame(graph, what = 'vertices')
  df_edges = as_data_frame(graph, what = 'edges')

  df_y = data.frame(name = unique(df_nodes$name), y = as.numeric(NA))
  i = 1
  roots = find_roots(graph)
  df_y$y[df_y$name %in% roots] = i

  # Calculate depth
  current_nodes = df_edges %>%
    filter(from %in% roots) %>%
    pull(to)
  while(length(current_nodes))
  {
    i = i+1
    df_y$y[df_y$name %in% current_nodes] = i
    current_nodes = df_edges %>%
      filter(from %in% current_nodes) %>%
      pull(to)
  }

  return(df_y)
}


#' Calculate horizontal coordinates for each node of the given graph
#'
#' @param graph The graph from which nodes positions (x axis) will be calculated.
#' @param df_y Y coordinates which are useful to simplify the graph.
#' @param h_size Horizontal widths at each graph level are needed to compute X positions.
#' @param root_node `root_node` is the reference to calculate the X coordinates of
#' its children. It is a dataframe with a name and a relative_position columns. If NULL,
#' it considers a SuperNode above the graph roots with relative_position=0.
#'
#' @import magrittr
#' @importFrom dplyr left_join group_by summarize bind_rows
#' @importFrom igraph as_data_frame graph_from_data_frame
horizontal_positions = function(graph, df_y, h_size, root_node=NULL)
{
  if(is.null(root_node))
  {
    root_nodes = find_roots(graph)

    # Add SuperNode on top
    df_edges = as_data_frame(graph, what='edges')
    df_edges = bind_rows(data.frame(from='SuperNode',
                                    to=root_nodes),
                         df_edges) %>%
      left_join(df_y, by=c('to'='name')) %>%
      group_by(to) %>%
      summarize(from=from[which.max(y)]) %>%
      as.data.frame()
    h_size = bind_rows(data.frame(name='SuperNode',
                                  size=sum(h_size$size[h_size$name %in% root_nodes])),
                       h_size)
    ext_graph = graph_from_data_frame(df_edges[,c('from', 'to')])

    # Recursively calculate X positions of each node of the graph
    # SuperNode is supposed to be centered
    df_x = horizontal_positions(ext_graph,
                                df_y,
                                h_size,
                                root_node=list(name='SuperNode',
                                               relative_position=0))
    # Remove "SuperNode" line
    df_x = df_x[2:nrow(df_x),]

    return(df_x)
  }
  else
  {
    # Get edges
    df_edges = as_data_frame(graph, what='edges')

    # Check if current node is a leaf
    if(!root_node$name %in% df_edges$from)
    {
      return(data.frame(name=root_node$name,
                        x=root_node$relative_position))
    }
    else
    {
      # Find children of the current node
      children = df_edges %>% filter(from==root_node$name)
      N = length(children$to)

      # Get their sizes
      children_sizes = h_size$size[match(children$to, h_size$name)]
      root_size = h_size$size[h_size$name==root_node$name]

      # Calculate X position of the current node
      children_relative_pos =
        root_node$relative_position -
        (root_size+children_sizes)/2 +
        cumsum(children_sizes)

      # Recursively calculate positions of descendant nodes
      df_children_pos = 1:N %>%
        lapply(function(i) horizontal_positions(graph,
                                                df_y,
                                                h_size,
                                                root_node=list(name=children$to[i],
                                                               relative_position=children_relative_pos[i]))) %>%
        bind_rows()

      return(rbind(data.frame(name=root_node$name,
                              x=root_node$relative_position),
                   df_children_pos))
    }
  }
}


#' Compute how large should be the graph at each depth
#'
#' @param graph The graph from which nodes positions (x axis) will be calculated.
#' @param df_y Y coordinates which are useful to simplify the graph.
#' @param root_node `root_node` is the reference to calculate the X coordinates of
#' its children. It is a dataframe with a name and a relative_position columns. If NULL,
#' it appplies the function recursively on each root of the graph.
#'
#' @import magrittr
#' @importFrom dplyr left_join group_by summarize bind_rows filter select
#' @importFrom igraph as_data_frame
#' @importFrom stringr str_split_1 str_length
horizontal_sizes = function(graph, df_y, root_node=NULL)
{
  # if no root_node is provided, take roots of the graph instead
  if(is.null(root_node))
  {
    root_nodes = find_roots(graph)
    sizes = root_nodes %>%
      lapply(function(node) horizontal_sizes(graph, df_y, node)) %>%
      bind_rows()
    return(sizes)
  }
  else
  {
    # Remove some edges to keep a descendant architecture
    df_edges = igraph::as_data_frame(graph, what='edges') %>%
      left_join(df_y, by=c('to'='name')) %>%
      group_by(to) %>%
      summarize(from=from[which.max(y)]) %>%
      as.data.frame()

    df_nodes = igraph::as_data_frame(graph, what='vertices')

    # Provided root_node may be a leaf of the graph
    if(!root_node %in% df_edges$from)
    {
      max_length = root_node %>% str_split_1('\n') %>% str_length() %>% max()
      size = max(10, max_length)
      return(data.frame(name=root_node, size=size))
    }
    else
    {
      # Apply the function recursively on each child
      children = df_edges %>%
        filter(from==root_node) %>%
        pull(to)
      df_children_sizes = children %>%
        lapply(function(node) horizontal_sizes(graph, df_y, node)) %>%
        bind_rows()

      # Once children sizes are known, root_size can be calculated
      root_size = df_children_sizes %>%
        filter(name %in% children) %>%
        select(size) %>%
        sum()
      return(rbind(data.frame(name=root_node,
                              size=root_size),
                   df_children_sizes))
    }
  }
}


#' Dynamic plot
#'
#' @description
#' This function uses `visNetwork` library for a better visualization of larger graphs.
#'
#' @param graph The graph to visualize.
#' @param layout_tree If `TRUE`, use [layout_tree()] to set initial nodes position.
#'
#' @import magrittr
#' @import visNetwork
#' @importFrom dplyr mutate rename
#' @importFrom igraph as_data_frame
#' @export
#'
#' @examples
#' \dontrun{
#' graph = get_descendants('68346', output='graph')
#' interactive_plot(graph)
#' }
interactive_plot = function(graph, layout_tree = FALSE)
{
  df_nodes = graph %>%
    as_data_frame(what='vertices') %>%
    rename(id=name) %>%
    mutate(label=id)


  # Rename color parameters if they are provided
  if(all(c('color', 'frame.color', 'label.color') %in%
                names(df_nodes))){
  df_nodes = df_nodes %>%
    mutate(color.background = color,
           color.highlight = color,
           color.border = ifelse(
             is.na(frame.color),
             color,
             frame.color
             ),
           font.color = label.color) %>%
    select(-color)
  }

  df_edges = graph %>% as_data_frame(what='edges')

  vis = visNetwork(df_nodes, df_edges) %>%
    visEdges(arrows = "to",
             color = '#848484',
             width = 2)

  if(layout_tree)
    vis = vis %>% visIgraphLayout(layout = 'layout_tree', reverse_y = FALSE)

  return(vis)
}



#' Indentation
#'
#' @description
#' In order to make it easier to visualize which ORPHAcodes contain the others in a data.frame,
#' this function apply indentation, which means the associated ORPHAcode is one level lower in Orphanet classification than the ORPHAcode above.
#'
#' ORPHAcodes may appear in multiple rows because they can be child of several parents.
#'
#' @param df An [orpha_df()] instantiation.
#' @param df_classif The classification data to consider. If NULL, find all classifications containing the given ORPHAcode.
#' @param indented_cols The columns that need to be shifted.
#' @param prefix The prefix of the indented columns. Default is `"indent"`.
#'
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom stringr str_count str_replace
#'
#' @return A matrix with the right indentations applied on the requested columns
#' @export
#'
#' @examples
#' library(dplyr)
#' orpha_codes = get_descendants('307711', output='codes_only')
#'
#' df = orpha_df(data.frame(orpha_code=orpha_codes), orpha_code_col='orpha_code') %>%
#'   left_join(load_nomenclature(), by='orpha_code')
#' df_indented = apply_orpha_indent(df, indented_cols='label')
apply_orpha_indent = function(df, df_classif=NULL, indented_cols=NULL, prefix='indent')
{
  if(!'orpha_df' %in% class(df))
    stop(simpleError('`apply_orpha_indent` needs an `orpha_df` instantiation to work.'))

  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  if(is.null(indented_cols))
    indented_cols = names(df)

  # Prune classification
  code_col = attr(df, 'orpha_code_col')
  df_classif = df_classif %>% filter(from %in% df[[code_col]] | from %in% df[[code_col]])

  # Calculate index for each ORPHAcode
  df_index = df_classif %>% assign_indent_index()

  # Join index
  df = df %>%
    left_join(df_index, by='orpha_code' %>% setNames(code_col)) %>%
    mutate(n_indent = str_count(index, '\\.'))

  # Apply indentation
  M = matrix('', nrow = nrow(df), ncol = max(df$n_indent, na.rm = T) + length(indented_cols) - 1)
  for(i in 1:nrow(df))
  {
    j = df$n_indent[i]
    M[i,j:(j+length(indented_cols)-1)] = df[i,indented_cols] %>% as.matrix()
  }
  df_indent = as.data.frame(M) %>% rename_with(~str_replace(.x, 'V', prefix))

  df_final = df %>%
    select(-any_of(indented_cols), -index, -n_indent) %>%
    bind_cols(df_indent)

  return(df_final)
}


assign_indent_index = function(df, current_code = NULL, current_index = '')
{
  # First check if recursivity just started or is about to finish.
  graph = graph_from_data_frame(df)
  if(is.null(current_code))
    current_children = find_roots(graph)
  else if(current_code %in% find_leaves(graph))
    return(NULL)
  else
    current_children = df %>% filter(from == current_code) %>% pull(to)

  # Compute index for each child (and their own children by recursivity) and compile results
  N_children = length(current_children)
  n_digits = floor(log10(N_children)) + 1
  conv_spec = paste0('%0',n_digits, 'd')
  children_index = paste(current_index, sprintf(conv_spec, 1:N_children), sep='.')

  df_index = lapply(1:N_children,
                    function(i) assign_indent_index(df, current_children[i], children_index[i])) %>%
    bind_rows() %>%
    rbind(data.frame(orpha_code = current_children,
                     index = children_index))

  # Return the sorted data.frame
  return(df_index %>% arrange(index))
}
