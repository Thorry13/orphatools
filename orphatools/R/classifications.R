#' Load the 33 classifications given in the Orphanet nomenclature pack
#'
#' @return All classifications as xml2 objects
#' @import magrittr
#' @importFrom xml2 read_xml
#' @export
#'
#' @examples
load_classifications = function()
{
  # Load data
  pack_nomenclature_path = system.file('extdata', 'Orphanet_Nomenclature_Pack_FR', package='orphatools')
  orpha_classifications_path = file.path(pack_nomenclature_path, 'Classifications_fr')
  class_list = list.files(orpha_classifications_path) %>% as.list()
  all_class = list()
  all_class = lapply(class_list,
                     function(class_file) read_xml(file.path(orpha_classifications_path, class_file))) %>%
    setNames(class_list %>% sapply(tools::file_path_sans_ext))

  return(all_class)
}


#' Convert a numerical Orpha code as an xml2 "ClassificationNode" object
#'
#' @param orphaCode The numerical Orpha code
#' @param class_data The classification data to consider
#' @param all If TRUE, returns all classification nodes associated to the given code
#'
#' @return An xml2 "ClassificationNode" object, as it is described in Orphanet nomenclature pack classifications
#' @importFrom xml2 xml_find_first xml_find_all
#' @export
#'
#' @examples
code2classNode = function(orphaCode, class_data, all=FALSE)
{
  # Find XPath to the relevant "ClassificationNode" objects in the xml data
  xpath = sprintf('//ClassificationNode[child::Disorder/OrphaCode=%s]', orphaCode)

  # Compute first or all classification nodes associated to the code
  if(all)
    class_node = xml_find_all(class_data, xpath)
  else
    class_node = xml_find_first(class_data, xpath)
  return(class_node)
}


#' Convert an xml2 "ClassificationNode" object as a numerical Orpha code
#'
#' @param classNode The xml2 "ClassificationNode" object
#'
#' @return The associated Orpha code
#' @import magrittr
#' @importFrom xml2 xml_child xml_text
#' @export
#'
#' @examples
classNode2code = function(classNode)
{
  # The "code "ClassificationNode" object just needs to be inspect to find its related Orpha code
  classNode %>% xml_child() %>% xml_child() %>% xml_text() %>% as.numeric()
}


#' Find all ancestors of a given Orpha code as it is in the Orphanet classification data
#'
#' @param orphaCode The numerical Orpha code
#' @param class_data The classification data to consider. If NULL, find all classifications containing the given Orpha code. Ancestors may differ depending on the chosen classification.
#' @param codes_only If FALSE, returns a data.frame object (columns "from" and "to") ready to be converted into a graph
#'
#' @return A from/to data.frame ready to be converted into a graph, or codes only if asked so
#' @import magrittr
#' @importFrom xml2 xml_parents xml_name
#' @importFrom dplyr group_by bind_rows distinct lag
#' @importFrom purrr is_empty
#' @export
#'
#' @examples
get_ancestors = function(orphaCode, class_data=NULL, codes_only=FALSE, full_connections=FALSE)
{
  get_ancestors_from_class = function(orphaCode, class_data)
  {
    # Find main classification nodes
    nodes = code2classNode(orphaCode, class_data, all=TRUE)
    if(is_empty(nodes))
      return(NULL)

    # Find all parents nodes up to the root (ancestors) for each node in the xml data
    nodes_ancestors = lapply(nodes, xml_parents)


    # Keep "ClassificationNode" nodes only and retrieve their associated Orpha codes
    nodes_ancestors =
      lapply(nodes_ancestors,
             function(node_ancestors) Filter(function(parent) xml_name(parent)=='ClassificationNode',
                                             node_ancestors) %>% sapply(classNode2code))
    if(is_empty(nodes_ancestors[[1]]))
      return(NULL)

    # Build a from/to data frame based on the ancestors data (ancestors already are in the correct order)
    if(full_connections)
    {
      df_list =
        lapply(nodes_ancestors,
               function(node_ancestors) lapply(seq_len(length(node_ancestors)-1),
                                               function(i) data.frame(from = node_ancestors[(1+i):length(node_ancestors)],
                                                                      to = lag(node_ancestors,i) %>% na.omit(),
                                                                      depth = (-1-i):-length(node_ancestors))) %>%
                 append(list(data.frame(from = node_ancestors, to = orphaCode, depth = (-1:-length(node_ancestors))))) %>%
                 bind_rows())
    }
    else
    {
      df_list =
        lapply(nodes_ancestors,
               function(node_ancestors) data.frame(from = node_ancestors,
                                                   to = c(orphaCode,
                                                          na.omit(lag(node_ancestors,1))),
                                                   depth = -1:-length(node_ancestors)))
    }

    # Merge the different found branches
    df_ancestors = bind_rows(df_list)
    df_ancestors = df_ancestors %>% group_by(from, to) %>% summarize(depth = min(depth))

    return(df_ancestors)
  }

  # Use specified classification or all of them if NULL was given
  if(is.null(class_data))
    all_class = load_classifications()
  else
    all_class = list(class_data)

  # Find ancestors in each classification
  df_ancestors_list = all_class %>%
    lapply(function(current_class) get_ancestors_from_class(orphaCode, current_class))

  # Merge all found ancestors
  df_ancestors = df_ancestors_list %>% bind_rows() %>% distinct() %>% as.data.frame()

  # Returns the data.frame object ready to be converted into a graph or Orpha codes only
  if(codes_only)
    return(unique(df_ancestors$from))
  else
    return(df_ancestors)
}


#' Get descendants recusevily from a given Orpha code
#'
#' @param orphaCode The numerical Orpha code
#' @param class_data The classification data to consider. If NULL, find one automatically. Descendance is independant from classification.
#' @param codes_only If FALSE, returns a data.frame object (columns "from" and "to") ready to be converted into a graph
#' @param ... # Additional parameters (edge_from_leaves, depth, return_depth) :
#' - edge_from_leaves : The leaves (last descendants) also belong to the "from" column
#' - depth : Depth information to transmit and increment recursevily
#' - return_depth : Return depth for each edge in the final data.frame
#'
#' @return A from/to data.frame ready to be converted into a graph, or codes only if asked so
#' @import magrittr
#' @importFrom xml2 xml_find_first xml_text xml_child xml_children xml_attr
#' @importFrom dplyr bind_rows distinct
#' @export
#'
#' @examples
get_descendants = function(orphaCode, class_data=NULL, codes_only=FALSE, ...)
{
  # If no classification was specified, find one containing the given Orpha code
  if(is.null(class_data))
  {
    all_class = load_classifications()
    for(class_data in all_class)
    {
      if(!is.na(code2classNode(orphaCode, class_data)))
        break
    }
  }

  # Check if the code belongs to the given classification
  if(is.na(code2classNode(orphaCode, class_data)))
    return(NULL)

  # Read additional parameters
  args = list(...)
  if(!'depth' %in% names(args))
    args$depth = 0

  # If the Orpha code is not entered, then choose the classification head code
  if(is.null(orphaCode))
    orphaCode = xml_find_first(class_data, '//OrphaCode') %>% xml_text() %>% as.numeric()

  # Find main classification node (one is enough as the others would have the same descendants)
  node = code2classNode(orphaCode, class_data, all=FALSE)

  # Calculate the number of direct children
  n_children = node %>% xml_child('ClassificationNodeChildList') %>% xml_attr('count') %>% as.numeric()

  # Ending condition for recursion. Having no children means being a leaf
  if(n_children==0)
  {
    # To make leaves appear in the "from" column. The NULL value is attributed in the corresponding "to" column
    if('edge_from_leaves' %in% names(args) && args$edge_from_leaves)
    {
      df = data.frame(from=orphaCode, to=NaN)
      if('depth' %in% names(args))
        df$depth = args$depth
      return(df)
    }
    else
      return(NULL)
  }
  else
  {
    # Find the existing children
    childrenCodes = node %>%
      xml_child('ClassificationNodeChildList') %>%
      xml_children() %>%
      sapply(classNode2code)

    # Build a from/to data frame and increment depth if necessary
    df_children = data.frame(from=orphaCode, to=childrenCodes)
    if('depth' %in% names(args))
    {
      df_children$depth = args$depth
      args$depth = args$depth + 1
    }

    # Call get_descendants recursively to find further descendants for each child
    df_further_descendants =
      lapply(childrenCodes, function(childCode) do.call(get_descendants,
                                                        c(list(orphaCode=childCode,
                                                               class_data=class_data,
                                                               codes_only=FALSE),
                                                        args) %>% as.list()))

    # Merge descendants of all children
    df_descendants =
      bind_rows(df_children, df_further_descendants) %>% distinct()

    # Return
    if(codes_only)
      return(unique(df_descendants$to))
    else if('return_depth' %in% names(args) && !args$return_depth)
      return(df_descendants[,c('from', 'to')])
    else
      return(df_descendants)
  }
}


#' Get a base graph from a codes list, which is basically a graph including the relevant ancestors and the given codes.
#'
#' @param codes_list The codes to build the base graph from
#' @param class_data The Orpha classification to consider to find the ancestors.
#' If NULL, automatically search a classification for each code
#'
#' @return The built base graph
#' @import magrittr
#' @importFrom igraph graph_from_data_frame as_data_frame set_vertex_attr V
#' @importFrom dplyr bind_rows distinct
#' @importFrom stats na.omit
#' @importFrom purrr keep is_empty
#' @export
#'
#' @examples
#' all_class = load_classifications()
#' graph = get_common_graph(c(303,304,305), all_class[[6]])
get_common_graph = function(codes_list, class_data=NULL, colored=FALSE, what = 'both', full_connections = FALSE)
{
  if(what=='both')
    what = c('ancestors', 'descendants')

  if('ancestors' %in% what)
  {
    df_ancestors_list = codes_list %>%
      lapply(function(code) get_ancestors(code, class_data, full_connections = full_connections)) %>%
      keep(function(df) !is_empty(df))
    graph_ancestors = df_ancestors_list %>%
      lapply(graph_from_data_frame) %>%
      merge_graphs()
  }
  else
    graph_ancestors = NULL

  if('descendants' %in% what)
  {
    graph_descendants = NULL
    for(current_code in codes_list)
    {
      if(is.null(graph_descendants) ||
         !current_code %in% as.numeric(names(V(graph_descendants))))
      {
        df_descendants = get_descendants(current_code, class_data)
        if(!is_empty(df_descendants))
          graph_descendants = merge_graphs(list(graph_descendants,
                                                graph_from_data_frame(df_descendants)))
      }
    }
  }
  else
    graph_descendants = NULL

  graph = merge_graphs(graphs_list = list(graph_ancestors, graph_descendants))

  if(colored)
  {
    nodes = V(graph) %>% names %>% as.numeric()
    graph = set_vertex_attr(graph, 'color', index=nodes %in% codes_list, 'green')
    graph = set_vertex_attr(graph, 'color', index=!nodes %in% codes_list, 'tomato')
  }

  return(graph)
}


#' Analyze ancestors to find the lowest group containing the given code
#'
#' @param orphaCode The code to find the lowest group from
#' @param class_data The classification data to consider.
#' If NULL, find all classifications containing the given Orpha code.
#' Ancestors may differ depending on the chosen classification.
#'
#' @return The lowest group containing the code, or the code itself if it is a group of disorders already
#' @import magrittr
#' @importFrom dplyr filter pull
#' @importFrom purrr keep
#' @importFrom igraph all_simple_paths
#' @export
#'
#' @examples
get_lowest_groups = function(orphaCode, class_data=NULL)
{
  keep_lowest_groups = function(code, df_ancestors)
  {
    df_select = df_ancestors %>% filter(to == code)
    df_select$classLevel = df_select$from %>%
      sapply(function(code) get_code_properties(code, nom_data)['classLevel'])

    groups_supp = NULL
    if(sum(df_select$classLevel != 36540))
      groups_supp = df_select %>%
        filter(classLevel != 36540) %>%
        pull(from) %>%
        sapply(keep_lowest_groups, df_ancestors) %>%
        unique()

    lowest_groups = unique(c(df_select %>% filter(classLevel == 36540) %>% pull(from),
                             groups_supp))
    return(lowest_groups)
  }
  nom_data = load_nomenclature()

  # Check if it isn't a group already
  props = get_code_properties(orphaCode)
  if(props['classLevel'] == 36540)
    return(orphaCode)

  # Find groups above the code
  df_ancestors = get_ancestors(orphaCode, class_data)
  group_ancestors = keep_lowest_groups(orphaCode, df_ancestors)

  # Check if a parent is not an ancestor of another
  graph = get_common_graph(group_ancestors, what='ancestors')
  combs = expand.grid(group_ancestors, group_ancestors)
  to_rem = combs %>%
    apply(1, function(x) all_simple_paths(graph,
                                          as.character(x[1]),
                                          as.character(x[2]))) %>% # Try to find a path between two ancestors
    keep(function(path) length(path) > 0) %>% # If a path is found, ancestor is discarded
    sapply(function(path) path[[1]][1]) %>%
    names %>% as.numeric() %>% unique() # Clean results

  # Remove ancestors to be discarded to keep the lowest only
  lowest_groups = setdiff(group_ancestors, to_rem) %>% as.numeric()
  return(lowest_groups)
}


#' Find the root or the roots of a given graph.
#'
#' @param graph The graph to find the root from
#' @param first_only If TRUE, return one root only
#'
#' @return The root(s) of the given graph as a numeric code
#' @import magrittr
#' @importFrom igraph degree
#' @export
#'
#' @examples
#' all_class = load_classifications()
#' graph = get_common_graph(c(303,304,305), all_class[[6]])
#' root = find_roots(graph)
find_roots = function(graph, first_only=FALSE)
{
  # Roots are the only nodes having no arcs going in
  D = degree(graph, mode='in')
  D = D[D==0]
  roots = names(D) %>% as.numeric()
  if(first_only)
    return(roots[1])
  else
    return(roots)
}


#' Find the leaves of a given graph.
#'
#' @param graph The graph to find the leaves from
#'
#' @return The leaves of the given graph as numeric codes
#' @import magrittr
#' @importFrom igraph degree
#' @export
#'
#' @examples
#' all_class = load_classifications()
#' graph = get_common_graph(c(303,304,305), all_class[[6]])
#' leaves = find_leaves(graph)
find_leaves = function(graph)
{
  # Leaves are the only nodes having no arcs going out
  d = degree(graph, mode='out')
  d = d[d==0]
  leaves = names(d) %>% as.numeric()
  return(leaves)
}


#' Extract the lowest common ancestors (LCAs) of a codes list in a given graph which was previously calculated.
#' Discussion : When there are two independent LCAs, we don't compare how "high" there are compared to the given nodes.
#'
#' @param codes_list The codes list to extract the LCAs from
#' @param class_data The Orpha classification to consider to find the ancestors.
#' If NULL, automatically search a classification for each code
#'
#' @return The lowest common ancestors of the given codes
#' @import magrittr
#' @importFrom igraph all_simple_paths
#' @importFrom purrr keep
#'
#' @export
#'
#' @examples
get_LCAs = function(codes_list, class_data=NULL)
{
  get_paths_LCA = function(pathsPair)
  {
    pathsPair = lapply(pathsPair, unlist) %>% unname()
    inter = Reduce(intersect, pathsPair) # Find all common nodes (ancestors) for both paths
    index = max(match(inter, pathsPair[[1]]), na.rm=TRUE) # Find the LCA among the common ancestors
    if(is.na(index))
      return(NULL)
    else
      return(pathsPair[[1]][index])
  }

  # codes_list = as.character(codes_list) # Convert codes into character vectors
  graph = get_common_graph(codes_list, class_data) %>% add_superNode()

  # Find all paths going from the root to the codes in the given list
  all_paths = codes_list %>%
    as.character() %>%
    lapply(function(current_to) all_simple_paths(graph, from='SuperNode', to=current_to) %>%
                                      lapply(function(path) path %>% names()))

  # Find all possible pairs for the calculated paths
  combs = expand.grid(all_paths)

  # Find potential LCAs by computing the apparent LCA for each pair of paths
  LCAs = apply(combs, 1, get_paths_LCA) %>% unique()
  LCAs_combs = expand.grid(LCAs, LCAs)

  # Remove common ancestors which are not LCAs
  to_rem = LCAs_combs %>%
    apply(1, function(x) all_simple_paths(graph, x[1], x[2])) %>% # Try to find a path between two ancestors
    keep(function(path) length(path) > 0) %>% # If a path is found, parent is discarded
    sapply(function(path) path[[1]][1]) %>% names %>% unique() # Clean results

  # Remove ancestors to be discarded to keep LCAs only
  LCAs = setdiff(LCAs, c(to_rem, 'SuperNode')) %>% as.numeric()
  return(LCAs)
}


#' Find the associated disorder Orpha code for a given subtype code
#'
#' @param subtypeCode The Orpha code of the subtype of disorder
#' @param class_data The classification to consider.
#' If NULL, automatically search a classification for each code
#'
#' @return The Orpha code of the associated disorder
#' @import magrittr
#' @importFrom dplyr filter pull
#' @export
#'
#' @examples
subtype_to_disorder = function(subtypeCode, class_data=NULL)
{
  # If no classification was specified, find one containing the given Orpha code
  if(is.null(class_data))
  {
    all_class = load_classifications()
    for(class_data in all_class)
    {
      if(!is.na(code2classNode(subtypeCode, class_data)))
        break
      else
        class_data = NULL
    }
  }

  nom_data = load_nomenclature()
  ancestors = get_ancestors(subtypeCode, class_data = class_data, codes_only = TRUE)
  disorderCode = sapply(ancestors, get_code_properties, nom_data) %>%
    t() %>%
    as.data.frame() %>%
    filter(classLevel == 36547) %>%
    pull(orphaCode)

  return(disorderCode)
}


#' Find relationships between the given codes and tries to extend it with relevant ones.
#'
#' @param codes_list The codes list to extend
#' @param class_data The classification to consider
#' @param base_graph The graph to base the calculation on
#' @param colored If TRUE, nodes are colored with green for the given codes and with red for the codes found as relatives
#' @param children_only If TRUE, no ancestors are searched and the given codes are considered as the graph roots.
#' If FALSE, roots are the LCAs of the given codes. When the classification head is found as LCA, user may experiment calculation issues.
#'
#' @return The given codes and their relevant relatives according to the classification
#' @import magrittr
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom stats na.omit
#' @importFrom igraph set_vertex_attr graph_from_data_frame
#' @export
get_relatives = function(codes_list, class_data=NULL, colored=TRUE, children_only=FALSE)
{
  # Find descendants from common ancestors or from the given codes
  if(children_only)
    top_codes = codes_list
  else
  {
    common_ancestors = get_LCAs(codes_list, class_data)
    top_codes = common_ancestors
  }
  df_edges = lapply(top_codes,
                    function(code) get_descendants(code,
                                                   class_data,
                                                   codes_only=FALSE,
                                                   edge_from_leaves=TRUE,
                                                   depth=1,
                                                   return_depth=TRUE)) %>%
    bind_rows() %>%
    group_by(from, to) %>%
    summarize(depth=max(depth)) %>% as.data.frame()

  # Extract nodes
  nodes = unique(c(df_edges$from, df_edges$to))
  nodes = na.omit(nodes)
  df_nodes = data.frame(nodes)

  # Extract edges
  df_edges = df_edges %>% na.omit()

  # Build the associated graph and color it if necessary
  graph = graph_from_data_frame(df_edges, vertices = df_nodes)
  if(colored)
  {
    graph = set_vertex_attr(graph, 'color', index=nodes %in% codes_list, 'green')
    graph = set_vertex_attr(graph, 'color', index=!nodes %in% codes_list, 'tomato')
  }
  return(graph)
}


#' Convert a list of graphs into a single merged graph
#'
#' @param graphs_list The graphs to merge
#'
#' @return The merged graph
#' @import magrittr
#' @importFrom igraph as_data_frame graph_from_data_frame
#' @importFrom dplyr bind_rows distinct group_by summarize
#'
#' @export
merge_graphs = function(graphs_list)
{
  graphs_list = graphs_list[!sapply(graphs_list, is.null)]

  if(!is_empty(graphs_list))
    {
    # Merge nodes
    df_nodes = graphs_list %>%
      lapply(function(graph) as_data_frame(graph, what='vertices')) %>%
      bind_rows() %>%
      distinct()

    # Merge edges
    df_edges = graphs_list %>%
      lapply(function(graph) as_data_frame(graph, what='edges')) %>%
      bind_rows()

    # Manage additional columns
    if('depth' %in% colnames(df_edges)){
      df_edges = df_edges %>%
        group_by(from, to) %>%
        summarize(depth=max(depth, na.rm = TRUE))
      df_edges$depth = df_edges$depth %>%
        replace(which(is.infinite(df_edges$depth)), NA)}
    else
      df_edges = df_edges %>% distinct()

    # Merge graphs
    merged_graph = graph_from_data_frame(df_edges, vertices=df_nodes)
    return(merged_graph)
  }
}


#' Add an extra node above the roots of the graph
#'
#' @param graph The graph to add the extra node to
#'
#' @return The new graph with the added node
#' @import magrittr
#' @importFrom igraph add_vertices add_edges
#' @export
#'
#' @examples
add_superNode = function(graph)
{
  roots = find_roots(graph)
  graph = graph %>% add_vertices(1, name='SuperNode')

  for(root in roots)
    graph = graph %>% add_edges(c('SuperNode', root))

  return(graph)
}

