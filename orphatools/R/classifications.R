#' Load the 33 classifications given in the Orphanet nomenclature pack
#'
#' @return All classifications as dataframes
#' @import magrittr
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' all_class = load_classifications()
load_classifications = function()
{
  extdata_path = system.file('extdata', package='orphatools')
  all_class_path = file.path(extdata_path, 'all_class.RDS')

  if(file.exists(all_class_path))
    all_class = readRDS(file.path(extdata_path, 'all_class.RDS'))
  else
    stop(simpleError(
'Loading of classifications failed. Internal files might be broken.
See `upload_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(all_class)
}



#' Checks if a given Orpha code belongs to a given classification.
#'
#' @param orphaCode The given Orpha code
#' @param class_data One of the 33 Orpha classifications
#'
#' @return
#' @export
#'
#' @examples
#' all_class = load_classifications()
#' print(is_in_classif('303', all_class[[1]]))
#' print(is_in_classif('303', all_class[[6]]))
is_in_classif = function(orphaCode, class_data)
{
  all_codes = unique(c(class_data$from, class_data$to))
  return(orphaCode %in% all_codes)
}



#' Find the root or the roots of a given graph.
#'
#' @param graph The graph (igraph object) to find the roots from
#' @param first_only If TRUE, return one root only
#'
#' @return The root(s) of the given graph as a numeric code
#' @import magrittr
#' @importFrom igraph degree
#' @export
#'
#' @examples
#' edgelist = get_ancestors('303')
#' graph = igraph::graph_from_data_frame(edgelist)
#' roots = find_roots(graph)
#'
find_roots = function(graph, first_only=FALSE)
{
  # Roots are the only nodes having no arcs going in
  D = degree(graph, mode='in')
  D = D[D==0]
  roots = names(D) # %>% as.numeric()
  if(first_only)
    return(roots[1])
  else
    return(roots)
}



#' Find the leaves of a given graph.
#'
#' @param graph The graph (igraph object) to find the leaves from
#'
#' @return The leaves of the given graph as numeric codes
#' @import magrittr
#' @importFrom igraph degree
#' @export
#'
#' @examples
#' edgelist = get_descendants('303')
#' graph = igraph::graph_from_data_frame(edgelist)
#' leaves = find_leaves(graph)
#'
find_leaves = function(graph)
{
  # Leaves are the only nodes having no arcs going out
  d = degree(graph, mode='out')
  d = d[d==0]
  leaves = names(d) # %>% as.numeric()

  return(leaves)
}



#' Find all ancestors of a given Orpha code as it is in the Orphanet classification data
#'
#' @param orphaCode The numerical Orpha code
#' @param class_data The classification data to consider. If NULL, find all classifications containing the given Orpha code. Ancestors may differ depending on the chosen classification.
#' @param codes_only If FALSE, returns a data.frame object (columns "from" and "to") ready to be converted into a graph
#' @param shortcuts (default FALSE) if TRUE, add an edge from every found ancestors to the given Orpha code
#'
#' @return A from/to data.frame ready to be converted into a graph, or codes only if asked so
#' @import magrittr
#' @importFrom dplyr bind_rows distinct filter
#' @importFrom purrr is_empty
#' @importFrom igraph all_simple_paths graph_from_data_frame
#' @export
#'
#' @examples
#' code = '303'
#' ancestors_codes = get_ancestors(code, codes_only=TRUE)
#' df_ancestors = get_ancestors(code)
#'
#' all_class = load_classifications()
#' df_ancestors = get_ancestors(code, class_data = all_class[['ORPHAclassification_156_rare_genetic_diseases_fr']])
#' df_ancestors = get_ancestors(code, class_data = all_class[['ORPHAclassification_146_rare_cardiac_diseases_fr']])
get_ancestors = function(orphaCode, class_data=NULL, codes_only=FALSE, shortcuts=FALSE)
{
  orphaCode = as.character(orphaCode)
  # Use specified classification or all of them if NULL was given
  if(is.null(class_data))
    class_data = load_classifications() %>% bind_rows() %>% distinct()

  # Find ancestors in classification
  if(!is_in_classif(orphaCode, class_data))
      return(NULL)

  class_graph = graph_from_data_frame(class_data)
  roots = find_roots(class_graph)
  ancestors =
    lapply(roots,function(root) all_simple_paths(class_graph, root, orphaCode)) %>%
    unlist() %>% names() %>% unique() %>% setdiff(orphaCode)

  if(length(ancestors) == 0)
    return(NULL)

  if(codes_only)
    return(ancestors)
  else
  {
    df_ancestors = class_data %>%
      filter(from %in% ancestors, to %in% c(ancestors, orphaCode))
    # Shortcuts are useful aggregated information from descendants to ancestors
    if(shortcuts)
    {
      df_shortcuts = data.frame(from=ancestors,
                                to=orphaCode)
      df_ancestors = bind_rows(df_ancestors, df_shortcuts) %>% distinct()
    }

    return(df_ancestors)
  }
}


#' Get descendants recusevily from a given Orpha code
#'
#' @param orphaCode The numerical Orpha code
#' @param class_data The classification data to consider. If NULL, find one automatically. Descendance is independant from classification.
#' @param codes_only If FALSE, returns a data.frame object (columns "from" and "to") ready to be converted into a graph
#' @param shortcuts (default FALSE) if TRUE, add an edge from the given Orpha code to every found descendants
#'
#' @return A from/to data.frame ready to be converted into a graph, or codes only if asked so
#' @import magrittr
#' @importFrom dplyr bind_rows distinct filter
#' @importFrom igraph graph_from_data_frame all_simple_paths
#' @export
#'
#' @examples
#' code = '303'
#' descendants_codes = get_descendants(code, codes_only=TRUE)
#' df_descendants = get_descendants(code)
get_descendants = function(orphaCode, class_data=NULL, codes_only=FALSE, shortcuts=FALSE)
{
  orphaCode = as.character(orphaCode)
  # If no classification was specified, find one containing the given Orpha code
  if(is.null(class_data))
  {
    all_class = load_classifications()
    for(class_data in all_class)
    {
      if(is_in_classif(orphaCode, class_data))
        break
    }
  }

  # Check if the code belongs to the given classification
  if(!is_in_classif(orphaCode, class_data))
    return(NULL)

  descendants = class_data %>%
    graph_from_data_frame() %>%
    all_simple_paths(orphaCode) %>%
    unlist() %>% names() %>% unique() %>% setdiff(orphaCode)
    # lapply(function(path0)
    #   make_empty_graph(directed=T) %>%
    #     add_vertices(length(path0), attr = list(name=names(path0))) +
    #     path(names(path0))
    #   ) %>%
    # lapply(as_data_frame, what='edges') %>%
    # bind_rows() %>%
    # distinct()
  # descendants = unique(df_descendants$to)

  if(length(descendants) == 0)
    return(NULL)

  # Return
  if(codes_only)
    return(descendants)
  else
  {
    df_descendants = class_data %>%
      filter(to %in% descendants, from %in% c(orphaCode, descendants))
    # Shortcuts are useful to aggregate information from descendants to ancestors
    if(shortcuts)
    {
      df_shortcuts = data.frame(from=orphaCode,
                                to=descendants)
      df_descendants = bind_rows(df_descendants, df_shortcuts) %>% distinct()
    }

    return(df_descendants)
  }
}


#' Get siblings of an Orpha code.
#'
#' @param orphaCode The numerical Orpha code
#' @param class_data The classification data to consider. If NULL, find one automatically. Descendance is independant from classification.
#' @param codes_only If FALSE, returns a data.frame object (columns "from" and "to") ready to be converted into a graph
#'
#' @return A from/to data.frame ready to be converted into a graph, or codes only if asked so
#' @import magrittr
#' @importFrom dplyr bind_rows distinct filter pull
#' @export
#'
#' @examples
#' code = '303'
#' siblings = get_siblings(code)
get_siblings = function(orphaCode, class_data=NULL, codes_only=T)
{
  if(is.null(class_data))
    class_data = load_classifications() %>% bind_rows() %>% distinct()

  parents = class_data %>% filter(to==orphaCode) %>% pull(from)

  df_siblings = class_data %>% filter(from %in% parents, to != orphaCode)

  if(codes_only)
    return(setdiff(unique(df_siblings$to), orphaCode))
  else
    return(df_siblings)
}


#' Get a base graph from a codes list, which is basically a graph including the relevant ancestors and the given codes.
#'
#' @param codes_list The codes to build the base graph from
#' @param class_data The Orpha classification to consider to find the ancestors.
#' If NULL, automatically search a classification for each code
#' @param what Specify if you want 'ancestors', 'descendants' or 'both'
#'
#' @return The built base graph
#' @import magrittr
#' @importFrom igraph graph_from_data_frame V induced_subgraph all_simple_paths
#' @importFrom dplyr bind_rows distinct arrange desc filter first
#' @importFrom stats na.omit
#' @importFrom purrr keep is_empty
#' @export
#'
#' @examples
#' all_class = load_classifications()
#' codes_list = c('303', '305', '595356')
#'
#' common_graph = get_common_graph(codes_list)
#' common_graph = get_common_graph(codes_list, class_data = all_class[[1]])
#' common_graph = get_common_graph(codes_list, what = 'ancestors')
#'
get_common_graph = function(codes_list, class_data=NULL, what='both'){
  codes_list = na.omit(codes_list)
  if(what=='both')
    what = c('ancestors', 'descendants')
  if(is.null(class_data))
    class_data = load_classifications() %>% bind_rows() %>% distinct()

  class_graph = graph_from_data_frame(class_data)

  df_y = class_graph %>%
    vertical_positions() %>%
    filter(name %in% codes_list)

  all_ancestors = NULL
  all_descendants = NULL
  if('ancestors' %in% what){
    if(length(codes_list) < 100){
      df_y_copy = df_y %>% arrange(desc(y))
      # for(orphaCode in df_y$name){
      while(nrow(df_y_copy)){
        orphaCode = first(df_y_copy$name)
        ancestors = get_ancestors(orphaCode, class_data, codes_only=T)
        all_ancestors = unique(c(all_ancestors, ancestors))
        df_y_copy = df_y_copy %>% filter(!name %in% c(orphaCode, all_ancestors))
      }
    }else{
      roots = find_roots(class_graph)
      all_ancestors = roots %>%
        lapply(function(root){
                # all_paths = all_simple_paths(class_graph, from = root)
                # all_leaves = all_paths %>% sapply(function(x) x %>% names() %>% last()) %>% unique()
                # index = which(all_leaves %in% codes_list)
                # selected_paths = all_paths[index]
                selected_paths =
                  all_simple_paths(class_graph, from=root,
                                   to=intersect(codes_list, names(V(class_graph))))
                all_ancestors = selected_paths %>% unlist() %>% names() %>%
                  unique() %>% setdiff(codes_list)
                }) %>%
        unlist() %>% unique()
    }
  }
  if('descendants' %in% what){
    df_y_copy = df_y %>% arrange(y)
    # for(orphaCode in df_y$name){
    while(nrow(df_y_copy)){
      orphaCode = first(df_y_copy$name)
      # descendants = get_descendants(orphaCode, class_data, codes_only=T)
      descendants = all_simple_paths(class_graph, from=orphaCode) %>%
        unlist() %>% names() %>% unique() %>% setdiff(codes_list)
      all_descendants = unique(c(all_descendants, descendants))
      df_y_copy = df_y_copy %>% filter(!name %in% c(orphaCode, all_descendants))
    }
  }

  all_codes = unique(c(all_ancestors, codes_list, all_descendants))
  v_index = which(names(V(class_graph)) %in% all_codes)
  graph_induced = induced_subgraph(class_graph, vids = v_index)

  return(graph_induced)
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
#' codes_list = c('303', '305', '595356')
#' all_class = load_classifications()
#'
#' get_LCAs(codes_list)
#' get_LCAs(codes_list, class_data = all_class[['ORPHAclassification_187_rare_skin_diseases_fr']])
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
  LCAs = setdiff(LCAs, c(to_rem, 'SuperNode')) # %>% as.numeric()
  return(LCAs)
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
#' codes_list = c('303', '305', '595356')
#' get_lowest_groups(orphaCode = '158676')
get_lowest_groups = function(orphaCode, class_data=NULL)
{
  keep_lowest_groups = function(code, df_ancestors, nom_data)
  {
    df_select = df_ancestors %>%
      filter(to == code) %>%
      left_join(nom_data[,c('orphaCode', 'classLevel')],
                by=c('from'='orphaCode'))

    # Find upper groups recursively if a disorder or a subtype of disorder was found as parent
    groups_supp = NULL
    if(sum(df_select$classLevel != '36540'))
      groups_supp = df_select %>%
        filter(classLevel != '36540') %>%
        pull(from) %>%
        sapply(keep_lowest_groups, df_ancestors, nom_data) %>%
        unique()

    lowest_groups = unique(c(df_select %>% filter(classLevel == '36540') %>% pull(from),
                             groups_supp))
    return(lowest_groups)
  }

  nom_data = load_nomenclature()

  # Check if it isn't a group already
  code = orphaCode
  classLevel = nom_data %>% filter(orphaCode == code) %>% pull(classLevel)
  if(classLevel == '36540')
    return(orphaCode)

  # Find groups above the code
  df_ancestors = get_ancestors(orphaCode, class_data)
  group_ancestors = keep_lowest_groups(orphaCode, df_ancestors, nom_data)

  # Check if a parent is not an ancestor of another
  graph = get_common_graph(group_ancestors, what='ancestors')
  combs = expand.grid(group_ancestors, group_ancestors)
  to_rem = combs %>%
    apply(1, function(x)
      # Try to find a path between two ancestors
      all_simple_paths(graph, x[1], x[2])) %>%

    # If a path is found, ancestor is discarded
    keep(function(path) length(path) > 0) %>%
    sapply(function(path) path[[1]][1]) %>%
    names %>%
    unique() # Clean results

  # Remove ancestors to be discarded to keep the lowest only
  lowest_groups = setdiff(group_ancestors, to_rem) # %>% as.numeric()
  return(lowest_groups)
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
#' subtype_to_disorder(subtypeCode = '158676') # 158676 is a subtype of disorder
#' subtype_to_disorder(subtypeCode = '303') # 303 is a group of disorder
subtype_to_disorder = function(subtypeCode, class_data=NULL)
{
  nom_data = load_nomenclature()
  ancestors = get_ancestors(subtypeCode, class_data = class_data, codes_only = TRUE)
  disorder_code = nom_data %>%
    filter(orphaCode %in% ancestors,
           classLevel == 36547) %>%
    pull(orphaCode)
    # data.frame(orphaCode = ancestors) %>%
    # left_join(nom_data, by='orphaCode') %>%
    # filter(classLevel == 36547) %>%
    # pull(orphaCode)

  return(disorder_code)
}



#' This function basically finds the siblings of each provided code before building
#' the graph of the completed family. It is recommended to use the `color_graph`
#' function afterwards to visualize which codes were added.
#'
#' @param codes_list The codes list to extend
#' @param class_data The classification to consider
#'
#' @return The given codes and their relevant relatives according to the classification
#' @import magrittr
#' @importFrom dplyr bind_rows group_by summarize filter
#' @importFrom stats na.omit
#' @importFrom igraph graph_from_data_frame as_data_frame delete_vertices
#' @export
#'
#' @examples
#' codes_list = codes_list = c('303', '305', '595356')
#' all_class = load_classifications()
#'
#' new_graph = complete_family(codes_list)
#' new_graph = complete_family(codes_list, class_data = all_class[['ORPHAclassification_187_rare_skin_diseases_fr']])
complete_family = function(codes_list, class_data=NULL, include_ancestors = T)
{
  if(is.null(class_data))
    class_data = load_classifications() %>% bind_rows() %>% distinct()

  what = ifelse(include_ancestors, 'both', 'descendants')
  all_siblings = lapply(codes_list,
                       get_siblings,
                       class_data=class_data,
                       codes_only=TRUE) %>%
    unlist() %>% unique()
  all_parents = class_data %>% filter(to %in% codes_list) %>% pull(from) %>% unique()
  new_codes_list = unique(c(codes_list, all_parents, all_siblings))

  graph = get_common_graph(new_codes_list, class_data=class_data, what = what)
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
#' @examples
#' code = '303'
#' df_descendants = get_descendants(code)
#' df_ancestors = get_ancestors(code)
#'
#' graph_d = igraph::graph_from_data_frame(df_descendants)
#' graph_a = igraph::graph_from_data_frame(df_ancestors)
#'
#' merged_graph = merge_graphs(list(graph_d, graph_a))
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

    # # Manage additional columns
    # if('depth' %in% colnames(df_edges)){
    #   df_edges = df_edges %>%
    #     group_by(from, to) %>%
    #     summarize(depth=max(depth, na.rm = TRUE))
    #   df_edges$depth = df_edges$depth %>%
    #     replace(which(is.infinite(df_edges$depth)), NA)
    #
    #   }
    # else
    df_edges = df_edges %>%
      select(from, to) %>%
      distinct()

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
add_superNode = function(graph)
{
  roots = find_roots(graph)
  graph = graph %>% add_vertices(1, name='SuperNode')

  for(root in roots)
    graph = graph %>% add_edges(c('SuperNode', root))

  return(graph)
}

