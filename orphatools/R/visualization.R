#' Calculate nodes position for a fancy layout
#'
#' @param graph The graph to display
#' @param reverse_y If TRUE, display from top to bottom
#' @param factor (default 5) This parameter tells how far the nodes should be from each other
#'
#' @return An adapted layout to plot nodes from Orpha classifications
#' @import magrittr
#' @importFrom dplyr group_by summarize filter select
#' @importFrom igraph as_data_frame
#' @export
#'
#' @examples
#' df_ancestors = get_ancestors('303')
#' graph = igraph::graph_from_data_frame(df_ancestors)
#' plot(graph)
#' plot(graph, layout=igraph::layout_as_tree)
#' plot(graph, layout=layout_tree)
layout_tree = function(graph, reverse_y=TRUE, factor=5)
{
  options(dplyr.summarise.inform=F)

  # X positions
  h_size = horizontal_sizes(graph)
  df_nodes = horizontal_positions(graph, h_size)

  # Y positions
  root_nodes = find_roots(graph)
  df_edges = as_data_frame(graph, what='edges')
  df_edges = rbind(data.frame(from='SuperNode', to=root_nodes, depth=min(df_edges$depth)-1),
                   df_edges)
  df_edges = df_edges %>%
    group_by(to) %>% summarize(from=from[which.max(depth)],
                               depth=depth[which.max(depth)]) %>%
    as.data.frame()
  df_nodes$y = df_nodes$node %>% sapply(function(node) df_edges %>%
                                          filter(to==node) %>%
                                          select(depth) %>% as.numeric()
                                        )

  # Reorder
  v = graph %>% as_data_frame(what='vertices')
  df_nodes = df_nodes[match(v$name, df_nodes$node),c('x','y')]

  df_nodes$y = (-1)^reverse_y * df_nodes$y

  df_nodes$x = scale(df_nodes$x)*factor
  df_nodes$y = scale(df_nodes$y)*factor

  return(df_nodes %>% as.matrix())
}

horizontal_positions = function(graph, h_size, root_node=NULL)
{
  if(is.null(root_node))
  {
    root_nodes = find_roots(graph)
    df_edges = igraph::as_data_frame(graph, what='edges')
    df_edges = rbind(data.frame(from='SuperNode', to=root_nodes, depth=0),
                     df_edges)
    h_size = rbind(data.frame(node='SuperNode', size=sum(h_size$size[h_size$node %in% root_nodes])),
                   h_size)
    ext_graph = graph_from_data_frame(df_edges)
    df_x = horizontal_positions(ext_graph,
                                h_size,
                                root_node=list(name='SuperNode',
                                               relative_position=0))
    df_x = df_x[2:nrow(df_x),] # Remove "SuperNode" line
    return(df_x)
  }
  else
  {
    df_edges = igraph::as_data_frame(graph, what='edges') %>%
      group_by(to) %>% summarize(from=from[which.max(depth)]) %>%
      as.data.frame()
    if(!root_node$name %in% df_edges$from)
    {
      return(data.frame(node=root_node$name, x=root_node$relative_position))
    }
    else
    {
      children = df_edges %>% filter(from==root_node$name)
      N = length(children$to)
      children_sizes = h_size$size[match(children$to, h_size$node)]
      root_size = h_size$size[h_size$node==root_node$name]
      children_relative_pos = root_node$relative_position - (root_size+children_sizes)/2 + cumsum(children_sizes)
      df_children_pos = 1:N %>%
        lapply(function(i) horizontal_positions(graph,
                                                h_size,
                                                root_node=list(name=children$to[i],
                                                               relative_position=children_relative_pos[i]))) %>%
        bind_rows()
      return(rbind(data.frame(node=root_node$name, x=root_node$relative_position), df_children_pos))
    }
  }
}

horizontal_sizes = function(graph, root_node=NULL)
{
  if(is.null(root_node))
  {
    root_nodes = find_roots(graph)
    sizes = root_nodes %>%
      lapply(function(node) horizontal_sizes(graph, node)) %>%
      bind_rows()
    return(sizes)
  }
  else
  {
    df_edges = igraph::as_data_frame(graph, what='edges') %>%
      group_by(to) %>% summarize(from=from[which.max(depth)]) %>%
      as.data.frame()
    df_nodes = igraph::as_data_frame(graph, what='vertices')

    if(!root_node %in% df_edges$from)
    {
      return(data.frame(node=root_node, size=1))
    }
    else
    {
      children = df_edges %>% filter(from==root_node)
      children = children$to # %>% as.numeric()
      df_children_sizes = children %>%
        lapply(function(node) horizontal_sizes(graph, node)) %>%
        bind_rows()
      root_size = df_children_sizes %>%
        filter(node %in% children) %>%
        select(size) %>% sum()
      return(rbind(data.frame(node=root_node, size=root_size), df_children_sizes))
    }
  }
}


#' This function was designed for a better visualization of large graphs.
#'
#' @param graph An igraph object to visualize
#'
#' @import magrittr
#' @import visNetwork
#' @importFrom dplyr mutate rename
#' @importFrom igraph as_data_frame
#' @export
#'
#' @examples
#' D = get_descendants('68346')
#' graph = igraph::graph_from_data_frame(D)
#' interactive_plot(graph)
interactive_plot = function(graph)
{
  visNetwork(graph %>%
               as_data_frame(what='vertices') %>%
               mutate(label=name) %>%
               rename(id=name),
             graph %>% as_data_frame(what='edges'))
}


#' Use ORPHA classification to build hierarchical style representation of a given ORPHA tree.
#'
#' @param df The given ORPHA tree. It is supposed to be a from/to data.frame object.
#' @param current_code An ORPHA code to analyse the children from. Used for recursivity.
#' @param current_index The index of the given ORPHA code. It will be expanded for each child. Used for recursivity.
#'
#' @import magrittr
#' @importFrom dplyr filter pull bind_rows arrange
#'
#' @return A sorted data.frame containing ORPHA codes present in the given ORPHA tree and their associated index.
#' This makes appear the hierarchical structure of the classification as one can see
#' on www.orpha.net.
#' @export
#'
#' @examples
#' df = get_descendants('307711')
#' df_index = assign_indent_index(df)
assign_indent_index = function(df, current_code = NULL, current_index = '')
{
  # First check if recursivity just started or is about to finish.
  if(is.null(current_code))
    current_children = find_roots(df)
  else if(current_code %in% find_leaves(df))
    return(NULL)
  else
    current_children = df %>% filter(from == current_code) %>% pull(to)

  # Compute index for each child (and their own children by recursivity) and compile results
  N_children = length(current_children)
  children_index = paste(current_index, 1:N_children %>% as.character(), sep='.')

  df_index = lapply(1:N_children,
                    function(i) assign_indent_index(df, current_children[i], children_index[i])) %>%
    bind_rows() %>%
    rbind(data.frame(code = current_children,
                     index = children_index))

  # Return the sorted data.frame
  return(df_index %>% arrange(index))
}


#' Shift some rows (indentation) on the base of the given index to make some hierarchical structure appear.
#'
#' @param df A sorted data.frame with ORPHA codes and their corresponding index (see *assign_indent_index* function)
#' @param cols_to_display The columns to keep and to shift
#'
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom stringr str_count
#'
#' @return A matrix with the right indentations applied on the requested columns
#' @export
#'
#' @examples
#' df = get_descendants('307711')
#' df_index = assign_indent_index(df)
#' df_test = df_index
#' df_test$N = 1:nrow(df_test)
#' M_shifted = apply_indent(df_test, cols_to_display = c('code', 'N'))
apply_indent = function(df, cols_to_display)
{
  df = df %>% mutate(n_indent = str_count(index, '\\.'))
  M = matrix('', nrow = nrow(df), ncol = max(df$n_indent, na.rm = T) + length(cols_to_display) - 1)

  for(i in 1:nrow(df))
  {
    j = df$n_indent[i]
    M[i,j:(j+length(cols_to_display)-1)] = df[i,cols_to_display] %>% as.matrix()
  }
  return(M)
}


#' Color vertices of a graph to emphasize some specific nodes or make classification levels appear
#'
#' @param graph, The graph to color
#' @param emphasize_nodes, List of codes to emphasize in the graph (red color).
#' @param display_class_levels If TRUE, color vertex frames with the associated
#' color (blue for groups, green for disorders and purple for subtypes)
#'
#' @import magrittr
#' @importFrom dplyr bind_rows
#' @importFrom igraph set_vertex_attr V
#'
#' @return
#' @export
#'
#' @examples
#' code = 303
#' codes_list = c(303, 79361, 158676, 89843)
#' graph = get_common_graph(code, what='both')
#' graph = color_graph()
#' graph = color_graph(emphasize_nodes = codes_list)
#' graph = color_graph(emphasize_nodes = codes_list, display_class_levels = FALSE)
color_graph = function(graph, emphasize_nodes=NULL, display_class_levels=TRUE)
{
  if(!is.null(emphasize_nodes))
  {
    nodes = V(graph) %>% names
    graph = set_vertex_attr(graph, 'label.color', index=nodes %in% emphasize_nodes, 'red3')
    # graph = set_vertex_attr(graph, 'label.font', index=nodes %in% emphasize_nodes, 2)
    # graph = set_vertex_attr(graph, 'frame.width', index=nodes %in% emphasize_nodes, 3)
    graph = set_vertex_attr(graph, 'frame.color', index=nodes %in% emphasize_nodes, 'red')
  }

  if(display_class_levels)
  {
    nodes = V(graph) %>% names
    props = nodes %>%
      lapply(function(node) get_code_properties(node, literal_values = TRUE)) %>%
      bind_rows()

    graph = set_vertex_attr(graph, 'color', index=props$classLevel == 'Group', 'royalblue1')
    graph = set_vertex_attr(graph, 'color', index=props$classLevel == 'Disorder', 'palegreen3')
    graph = set_vertex_attr(graph, 'color', index=props$classLevel == 'Subtype', 'plum3')
  }

  return(graph)
}
