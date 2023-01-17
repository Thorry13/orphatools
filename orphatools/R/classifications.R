#' Read class returns a from/to dataframe associated to a classification
#'
#' @param path Path to classification file
#' @param load Load a dumped output. It the output does not exist, it is recalculated
#' @param save Dump the output for later readings
#'
#' @return A from/to dataframe representing the edges between the classification entities
#' @import magrittr
#' @importFrom dplyr bind_rows
#' @importFrom xml2 read_xml xml_find_first xml_find_all
#' @export
read_class = function(path, load=TRUE, save=TRUE)
{
  class_data = read_xml(path)

  # Find classification nodes
  classification_nodes = xml_find_all(class_data, '//ClassificationNode')
  df_class = classification_nodes %>%
    lapply( function(class_node){
      from = class_node %>% xml_find_first('Disorder/OrphaCode') %>% xml_text() # %>% as.numeric()
      to = class_node %>% xml_find_all('ClassificationNodeChildList/ClassificationNode/Disorder/OrphaCode') %>% xml_text() # %>% as.numeric()
      df_temp = NULL
      if(length(to) != 0)
        df_temp = list(from=from, to=to)
      return(df_temp)
    }) %>% bind_rows() %>% as.data.frame()
}



#' Load the 33 classifications given in the Orphanet nomenclature pack
#'
#' @param load Load a dumped output. It the output does not exist, it is recalculated.
#' @param save Dump the output for later readings
#'
#' @return All classifications as xml2 objects
#' @import magrittr
#' @importFrom dplyr bind_rows
#' @importFrom xml2 read_xml
#' @export
#'
#' @examples
#' all_class = load_classifications()
load_classifications = function(load=TRUE, save=TRUE)
{
  extdata_path = system.file('extdata', package='orphatools')

  # Load data
  if(load && file.exists(file.path(extdata_path, 'all_class.RDS')))
    all_class = readRDS(file.path(extdata_path, 'all_class.RDS'))
  else
  {
    orpha_classifications_path = file.path(extdata_path, 'Orphanet_Nomenclature_Pack_FR', 'Classifications_fr')
    class_list = list.files(orpha_classifications_path) %>% as.list()
    all_class = list()
    all_class = lapply(class_list,
                       function(class_file) read_class(file.path(orpha_classifications_path, class_file))) %>%
      setNames(class_list %>% sapply(tools::file_path_sans_ext))
    if(save)
      saveRDS(all_class, file.path(extdata_path, 'all_class.RDS'))
  }

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
#' @param X The graph (igraph object or edgelist as a dataframe) to find the roots from
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
#'
#' roots = find_roots(edgelist)
#' roots = find_roots(graph)
find_roots = function(X, first_only=FALSE)
{
  # Roots are the only nodes having no arcs going in
  if('data.frame' %in% class(X))
    roots = setdiff(X$from, X$to)
  else if(class(X)=='igraph')
  {
    D = degree(X, mode='in')
    D = D[D==0]
    roots = names(D) # %>% as.numeric()
  }
  if(first_only)
    return(roots[1])
  else
    return(roots)
}



#' Find the leaves of a given graph.
#'
#' @param X The graph (igraph object or edgelist as a dataframe) to find the leaves from
#'
#' @return The leaves of the given graph as numeric codes
#' @import magrittr
#' @importFrom igraph degree
#' @export
#'
#' @examples
#' edgelist = get_descendants('303')
#' graph = igraph::graph_from_data_frame(edgelist)
#'
#' leaves = find_leaves(edgelist)
#' leaves = find_leaves(graph)
find_leaves = function(X)
{
  # Leaves are the only nodes having no arcs going out
  if('data.frame' %in% class(X))
    leaves = setdiff(X$to, X$from)
  else if(class(X)=='igraph')
  {
    d = degree(X, mode='out')
    d = d[d==0]
    leaves = names(d) # %>% as.numeric()
  }
  return(leaves)
}



#' Find all ancestors of a given Orpha code as it is in the Orphanet classification data
#'
#' @param orphaCode The numerical Orpha code
#' @param class_data The classification data to consider. If NULL, find all classifications containing the given Orpha code. Ancestors may differ depending on the chosen classification.
#' @param gen Number of generations to take into account. If Inf, the default, all generations are included.
#' @param codes_only If FALSE, returns a data.frame object (columns "from" and "to") ready to be converted into a graph
#' @param shortcuts (default FALSE) if TRUE, add an edge from every found ancestors to the given Orpha code
#'
#' @return A from/to data.frame ready to be converted into a graph, or codes only if asked so
#' @import magrittr
#' @importFrom xml2 xml_parents xml_name
#' @importFrom dplyr mutate group_by ungroup bind_rows distinct lag rename left_join
#' @importFrom purrr is_empty
#' @importFrom ggenealogy getAncestors
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
get_ancestors = function(orphaCode, class_data=NULL, gen=Inf, codes_only=FALSE, shortcuts=FALSE)
{
  get_ancestors_from_class = function(orphaCode, class_data)
  {
    if(!is_in_classif(orphaCode, class_data))
      return(NULL)

    class_data = class_data %>% rename(child=to, parent=from)

    # Find all parents nodes up to the root (ancestors) for each node in the xml data
    ancestors = getAncestors(orphaCode, class_data, gen=gen)

    return(ancestors)
  }

  # Use specified classification or all of them if NULL was given
  if(is.null(class_data))
    all_class = load_classifications()
  else
    all_class = list(class_data)

  # Find ancestors in each classification
  ancestors = all_class %>%
    lapply(function(current_class) get_ancestors_from_class(orphaCode, current_class)) %>%
    bind_rows()

  if(length(ancestors) == 0)
    return(NULL)

  if(codes_only)
    return(unique(ancestors$label))
  else
  {
    all_codes = c(ancestors$label, orphaCode)

    # Shortcuts are useful aggregate information from descendants to ancestors
    if(shortcuts)
    {
      df_shortcuts = data.frame(from=ancestors$label,
                                to=orphaCode)
      all_edges = bind_rows(c(all_class, list(df_shortcuts)))
    }
    else
      all_edges = bind_rows(all_class) %>%
        left_join(ancestors, by=c('from'='label'))

    # Cleaning
    df_ancestors = all_edges %>%
      select(from, to) %>%
      filter(from %in% all_codes & to %in% all_codes) %>%
      distinct() %>%
      as.data.frame()

    return(df_ancestors)
  }
}



#' Get descendants recusevily from a given Orpha code
#'
#' @param orphaCode The numerical Orpha code
#' @param class_data The classification data to consider. If NULL, find one automatically. Descendance is independant from classification.
#' @param gen Number of generations to take into account. If Inf, the default, all generations are included.
#' @param codes_only If FALSE, returns a data.frame object (columns "from" and "to") ready to be converted into a graph
#' @param shortcuts (default FALSE) if TRUE, add an edge from the given Orpha code to every found descendants
#'
#' @return A from/to data.frame ready to be converted into a graph, or codes only if asked so
#' @import magrittr
#' @importFrom xml2 xml_find_first xml_text xml_child xml_children xml_attr
#' @importFrom dplyr bind_rows distinct
#' @importFrom ggenealogy getDescendants
#' @export
#'
#' @examples
#' code = '303'
#' descendants_codes = get_descendants(code, codes_only=TRUE)
#' df_descendants = get_descendants(code)
get_descendants = function(orphaCode, class_data=NULL, gen=Inf, codes_only=FALSE, shortcuts=FALSE)
{
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

  descendants = getDescendants(orphaCode,
                               class_data %>% rename(parent=from, child=to),
                               gen=gen)

  if(length(descendants) == 0)
    return(NULL)

  # Return
  if(codes_only)
    return(unique(descendants$label))
  else
  {
    all_codes = c(descendants$label, orphaCode)

    # Shortcuts are useful to aggregate information from descendants to ancestors
    if(shortcuts)
    {
      df_shortcuts = data.frame(from=orphaCode,
                                to=descendants$label)
      all_edges = bind_rows(c(all_class, list(df_shortcuts)))
    }
    else
      all_edges = bind_rows(all_class) %>%
        left_join(descendants, by=c('to'='label'))

    # Cleaning
    df_descendants = all_edges %>%
      select(from, to) %>%
      filter(from %in% all_codes & to %in% all_codes) %>%
      distinct() %>%
      as.data.frame()

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
#' @importFrom xml2 xml_find_first xml_text xml_child xml_children xml_attr
#' @importFrom dplyr bind_rows distinct
#' @importFrom ggenealogy getDescendants
#' @export
#'
#' @examples
#' code = '303'
#' descendants_codes = get_descendants(code, codes_only=TRUE)
#' df_descendants = get_descendants(code)
get_siblings = function(orphaCode, class_data=NULL, codes_only=FALSE)
{
  parents = get_ancestors(orphaCode,
                             class_data=class_data,
                             gen=1,
                             codes_only=TRUE)

  df_siblings = lapply(parents,
                       get_descendants,
                       class_data=class_data,
                       gen=1) %>%
    bind_rows() %>%
    distinct() %>%
    as.data.frame()

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
#' @param shortcuts Add shortcuts when looking for ancestors and descendants.
#' A shortcut is a direct link between an ancestor and its descendant.
#'
#' @return The built base graph
#' @import magrittr
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr bind_rows distinct
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
#' common_graph = get_common_graph(codes_list, shortcuts = TRUE)
#'
get_common_graph = function(codes_list, class_data=NULL, what='both', shortcuts=FALSE)
{
  if(what=='both')
    what = c('ancestors', 'descendants')

  if('ancestors' %in% what)
  {
    df_ancestors_list = codes_list %>%
      lapply(function(code) get_ancestors(code,
                                          class_data,
                                          shortcuts=shortcuts)) %>%
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
         # !current_code %in% as.numeric(names(V(graph_descendants))))
         !current_code %in% names(V(graph_descendants)))
      {
        df_descendants = get_descendants(current_code,
                                         class_data,
                                         shortcuts=shortcuts)
        if(!is_empty(df_descendants))
          graph_descendants = merge_graphs(list(graph_descendants,
                                                graph_from_data_frame(df_descendants)))
      }
    }
  }
  else
    graph_descendants = NULL

  graph = merge_graphs(graphs_list = list(graph_ancestors, graph_descendants))

  return(graph)
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
  # If no classification was specified, find one containing the given Orpha code
  if(is.null(class_data))
  {
    all_class = load_classifications()
    for(class_data in all_class)
    {
      if(is_in_classif(subtypeCode, class_data))
        break
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
complete_family = function(codes_list, class_data=NULL)
{
  graph_ancestors = get_common_graph(codes_list,
                                  class_data=class_data,
                                  what = 'ancestors')
  graph_siblings = lapply(codes_list,
                       get_siblings,
                       class_data=class_data) %>%
    bind_rows() %>%
    graph_from_data_frame()

  graph = merge_graphs(list(graph_ancestors, graph_siblings))

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

