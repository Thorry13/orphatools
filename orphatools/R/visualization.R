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
#' df_ancestors = get_ancestors(303)
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
                                          select(depth) %>%
                                          as.numeric())

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
      children = children$to %>% as.numeric()
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
#' graph = get_descendants(68346)
#' interactive_plot(graph)
interactive_plot = function(graph)
{
  visNetwork(graph %>%
               as_data_frame(what='vertices') %>%
               mutate(label=name) %>%
               rename(id=name),
             graph %>% as_data_frame(what='edges'))
}
