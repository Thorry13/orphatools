#' Calculate nodes position for a fancy layout
#'
#' @param graph The graph to display
#' @param reverse_y If TRUE, display from top to bottom
#' @param factor (default 5) This parameter tells how far the nodes should be from each other
#'
#' @return An adapted layout to plot nodes from Orpha classifications
#' @import magrittr
#' @importFrom dplyr group_by summarize filter select full_join n_distinct
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

  # Y positions
  df_y = vertical_positions(graph)
  df_y$y = (-1)^reverse_y * df_y$y

  # X positions
  h_size = horizontal_sizes(graph, df_y)
  df_x = horizontal_positions(graph, df_y, h_size)

  df_nodes = full_join(df_x, df_y, by='name')
  if(n_distinct(df_nodes$x) > 1)
    df_nodes$x = scale(df_nodes$x)*factor
  # if(n_distinct(df_nodes$y) > 1)
  #   df_nodes$y = scale(df_nodes$y)*factor

  # Reorder
  v = graph %>% as_data_frame(what='vertices')
  df_nodes = df_nodes[match(v$name, df_nodes$name),c('x','y')]

  return(df_nodes %>% as.matrix())
}

#' Calculate vertical coordinates for each node of the given graph
#'
#' @param graph The graph from which nodes positions (y axis) will be calculated
#'
#' @import magrittr
#' @importFrom dplyr filter pull
#' @importFrom igraph as_data_frame
#'
#' @return
#' @export
#'
#' @examples
#' df_edges = data.frame(from=c('A', 'B', 'C'), to = c('B', 'C', 'D'), depth = c(-3,-2,-1))
#' g = igraph::graph_from_data_frame(df_edges)
#' df_y = vertical_positions(g)
vertical_positions = function(graph)
{
  # Initialization
  df_nodes = as_data_frame(graph, what = 'vertices')
  df_edges = as_data_frame(graph, what = 'edges')

  df_y = data.frame(name = unique(df_nodes$name),
                    y = as.numeric(NA))
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
#' @param graph The graph from which nodes positions (x axis) will be calculated
#' @param df_y Y coordinates which are useful to simplify the graph
#' @param h_size Horizontal widths at each graph level are needed to compute X positions
#' @param root_node `root_node` is the reference to calculate the X coordinates of
#' its children. It is a dataframe with a name and a relative_position columns. If NULL,
#' it considers a SuperNode above the graph roots with relative_position=0.
#'
#' @import magrittr
#' @importFrom dplyr left_join group_by summarize bind_rows
#' @importFrom igraph as_data_frame graph_from_data_frame
#'
#' @return
#' @export
#'
#' @examples
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
#' @param graph The graph from which nodes positions (x axis) will be calculated
#' @param df_y Y coordinates which are useful to simplify the graph
#' @param root_node `root_node` is the reference to calculate the X coordinates of
#' its children. It is a dataframe with a name and a relative_position columns. If NULL,
#' it appplies the function recursively on each root of the graph.
#'
#' @import magrittr
#' @importFrom dplyr left_join group_by summarize bind_rows filter select
#' @importFrom igraph as_data_frame
#' @importFrom stringr str_split_1 str_length
#'

#' @return
#'
#' @examples
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


#' This function was designed for a better visualization of large graphs.
#'
#' @param graph An igraph object to visualize
#' @param layout_tree if TRUE, use the layout function above to set initial nodes position
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


#' Use ORPHA classification to build hierarchical style representation of a given ORPHA tree.
#'
#' @param df The given ORPHA tree. It is supposed to be a from/to data.frame object.
#' @param current_code An ORPHA code to analyse the children from. Used for recursivity.
#' @param current_index The index of the given ORPHA code. It will be expanded for each child. Used for recursivity.
#'
#' @import magrittr
#' @importFrom dplyr filter pull bind_rows arrange
#' @importFrom igraph graph_from_data_frame
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
  graph = graph_from_data_frame(df)
  if(is.null(current_code))
    current_children = find_roots(graph)
  else if(current_code %in% find_leaves(graph))
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
#' graph = color_graph(graph)
#' graph = color_graph(graph, emphasize_nodes = codes_list)
#' graph = color_graph(graph, emphasize_nodes = codes_list, display_class_levels = FALSE)
color_graph = function(graph, emphasize_nodes=NULL, display_class_levels=TRUE)
{
  if(!is.null(emphasize_nodes))
  {
    nodes = V(graph) %>% names
    graph = set_vertex_attr(graph, 'label.color', index=nodes %in% emphasize_nodes, '#CD0000') # red3
    graph = set_vertex_attr(graph, 'frame.color', index=nodes %in% emphasize_nodes, '#FF0000') # red
  }

  if(display_class_levels)
  {
    # Find associated properties
    nom_data = load_nomenclature() %>%
      translate_properties()
    class_levels = data.frame(orphaCode = V(graph) %>% names) %>%
      left_join(nom_data, by='orphaCode') %>%
      pull(classLevel)

    # Color nodes
    graph = set_vertex_attr(graph, 'color', index=class_levels == 'Group', '#4169E1') # royalblue1
    graph = set_vertex_attr(graph, 'color', index=class_levels == 'Disorder', '#7CCD7C') # palegreen3
    graph = set_vertex_attr(graph, 'color', index=class_levels == 'Subtype', '#CD96CD') # plum3
  }

  return(graph)
}
