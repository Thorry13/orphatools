#' Orphanet classifications
#'
#' @description
#' The functions presented below analyze relationships between ORPHAcodes and returns
#' results in a convenient format for further analysis or display.
#'
#' @details
#' Orphanet organized its classification system around 34 different hierarchies,
#' which are not completely distinct.
#'
#' For each hierarchy is listed a set of from/to relationships between ORPHAcodes.
#' A child is always more specific than its parent.
#'
#' `load_classifications`
#' Mind using `bind_rows` followed by `distinct` to merge all classification.
#'
#' The classification system will change according to the chosen `"orphatools_nomenclature"`
#' option, that you can set via the [orphatools_options()] interface. Add a new available option
#' with [add_nomenclature_pack()].
#'
#' @seealso [add_nomenclature_pack()], [orphatools_options()]
#' @examples
#' library(dplyr)
#'
#' all_classif = load_classifications()
#' df_all_classif = load_classifications() %>% bind_rows() %>% distinct()
#'
#' is_in_classif(303, all_classif[[1]])
#' is_in_classif(303, all_classif[[6]])
#'
#' @name classifications
NULL

#' @rdname classifications
#' @return All classifications as a list of data.frame
#' @export
load_classifications = function()
{
    v = getOption('orphatools_nomenclature', default_pack_version())
  nomenclature_path = get_pack_versions() %>% filter(version==v) %>% pull(location)

  #internal pack_data is silently loaded
  if(file.exists(nomenclature_path))
    load(nomenclature_path) # Load other pack_data
  else if(nomenclature_path != 'internal')
    stop(simpleError(
    'Loading of classifications failed. Internal files might be broken.
    See `orphatools_options`, `add_nomenclature_pack` or consider reisntalling orphatools package.'))

  return(pack_data$classifications)
}


#' @rdname classifications
#' @param orpha_code An ORPHAcode.
#' @param df_classif A from/to data.frame providing relationships between ORPHAcodes.
#'
#' @return
#' Returns TRUE if a given ORPHAcode belongs to a given classification, FALSE else.
#' @export
is_in_classif = function(orpha_code, df_classif)
{
  all_codes = unique(c(df_classif$from, df_classif$to)) %>% as.character()
  return(orpha_code %in% all_codes)
}


#' Analyze ORPHA classifications
#'
#' @description
#' The Orphanet classification system can be seen as a wide genealogy tree, that's why
#' the following terms will refer to this field.
#'
#' There two classes of functions:
#'
#' - Functions applied to a single ORPHAcode:
#'   - `get_parents`
#'   - `get_children`
#'   - `get_ancestors`
#'   - `get_descendants`
#'   - `get_siblings`
#'
#' - Functions applied to a set of ORPHAcodes:
#'   - `merge_branches`
#'   - `complete_family`
#'   - `get_LCAs`
#'
#' When you don't want to analyze to whole Orphanet classification system, `df_classif` should be set.
#'
#' When only ORPHAcodes are needed, without the edges information, set `output='codes_only'`.
#' Alternatively, if you wish to analyze or visualize ORPHAcodes interactions,
#' set `output='edgelist'` or `output='graph'`.
#'
#' @param orpha_code The ORPHAcode to start from.
#' @param orpha_codes A vector of ORPHAcodes used to perform operations.
#' @param output A value specifying the output format. Should be in `c("codes_only", "edgelist", "graph")`.
#' @param max_depth The maximum reached depth starting from the given ORPHAcode.
#' @param df_classif The classification data to consider. If NULL, load the whole Orphanet classification.
#' @param direction This gives the ancestors and/or descendants direction to complete the family. Should be in `c("up", "down", "both")`.
#'
#' @return
#' Results are returned in the format specified in `output` argument :
#' `'codes_only'`, `'edgelist'`, `'graph'`.
#'
#' @import dplyr
#' @importFrom igraph graph_from_data_frame as_data_frame all_simple_paths induced_subgraph V
#' @importFrom stats na.omit
#' @importFrom purrr keep is_empty
#' @importFrom stringr str_equal
#'
#' @examples
#' library(dplyr)
#' all_classif = load_classifications()
#' orpha_codes = c('303', '305', '595356')
#' orpha_code = '303'
#'
#' # ---
#' # Ancestors
#'
#' # Get ancestors ORPHAcodes only
#' ancestors_codes = get_ancestors(orpha_code)
#'
#' # Change output format
#' df_ancestors = get_ancestors(orpha_code, output='edgelist')
#' graph_ancestors = get_ancestors(orpha_code, output='graph')
#'
#' # Get parents only
#' parents = get_ancestors(orpha_code, max_depth=1)
#' parents = get_parents(orpha_code)
#'
#' # Select a specific classification tree
#' classif1 = all_classif[['ORPHAclassification_156_rare_genetic_diseases_fr']]
#' ancestors = get_ancestors(orpha_code, df_classif=classif1)
#'
#' classif2 = all_classif[['ORPHAclassification_146_rare_cardiac_diseases_fr']]
#' ancestors = get_ancestors(orpha_code, df_classif=classif2)
#'
#' # ---
#' # Descendants
#'
#' # Get desendants ORPHAcodes only
#' descendants_codes = get_descendants(orpha_code)
#'
#' # Change output format
#' df_descendants = get_descendants(orpha_code, output='edgelist')
#' graph_descendants = get_descendants(orpha_code, output='graph')
#'
#' # Get children only
#' df_children = get_descendants(orpha_code, max_depth=1)
#' df_childen = get_children(orpha_code)
#'
#' # ---
#' # Siblings
#'
#' siblings = get_siblings(orpha_code)
#' df_siblings = get_siblings(orpha_code, output='edgelist')
#' graph_siblings = get_siblings(orpha_code, output='graph')
#'
#' # ---
#' # Lowest common ancestors
#'
#' lcas = get_LCAs(orpha_codes)
#'
#' classif = all_classif[['ORPHAclassification_187_rare_skin_diseases_fr']]
#' lcas = get_LCAs(orpha_codes, df_classif = classif)
#'
#' # ---
#' # Merge branches
#'
#' family_codes = merge_branches(orpha_codes)
#' family_codes = merge_branches(orpha_codes, df_classif = all_classif[[1]])
#' family_codes = merge_branches(orpha_codes, direction='up')
#' family_codes = merge_branches(orpha_codes, direction='down')
#' df_family = merge_branches(orpha_codes, output='edgelist')
#' graph_family = merge_branches(orpha_codes, output='graph')
#'
#' # ---
#' # Complete family
#' family_codes = complete_family(orpha_codes)
#' family_codes = complete_family(orpha_codes, df_classif=all_classif[[1]])
#' family_codes = complete_family(orpha_codes, max_depth=0)
#' df_family = complete_family(orpha_codes, output = 'edgelist')
#' graph_family = complete_family(orpha_codes, output='graph')
#'
#' @name analyze-genealogy
NULL

#' @rdname analyze-genealogy
#' @export
get_parents = function(orpha_code, output='codes_only', df_classif=NULL){
  # Use specified classification or all of them if NULL was given
  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  # Search parents
  df_parents = df_classif %>% filter(to == orpha_code)
  parents = df_parents %>% pull(from) %>% unique()

  # Return results in the specified format
  if(length(parents) == 0)
    return(NULL)
  if(output == 'codes_only')
    return(parents)
  else if(output == 'edgelist')
    return(df_parents)
  else if(output == 'graph')
    return(graph_from_data_frame(df_parents))
  else{
    warning('No valid output format was given. `output` value should be in `c("codes_only", "edgelist", "graph")`')
    return(NULL)
  }
}


#' @rdname analyze-genealogy
#' @export
get_children = function(orpha_code, output='codes_only', df_classif=NULL){
  # Use specified classification or all of them if NULL was given
  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  # Search children
  df_children = df_classif %>% filter(from == orpha_code)
  children = df_children %>% pull(to) %>% unique()

  # Return results in the specified format
  if(length(children) == 0)
    return(NULL)
  if(output == 'codes_only')
    return(children)
  else if(output == 'edgelist')
    return(df_children)
  else if(output == 'graph')
    return(graph_from_data_frame(df_children))
  else{
    warning('No valid output format was given. `output` value should be in `c("codes_only", "edgelist", "graph")`')
    return(NULL)
  }
}


#' @rdname analyze-genealogy
#' @export
get_ancestors = function(orpha_code, output='codes_only', max_depth=NULL, df_classif=NULL)
{
  if(is.null(max_depth))
    max_depth=-1

  orpha_code = as.character(orpha_code)
  # Use specified classification or all of them if NULL was given
  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  # Check if the given ORPHAcode is found in classification
  if(!is_in_classif(orpha_code, df_classif))
      return(NULL)

  # Find all paths from the given ORPHAcode and extract elements
  ancestors = df_classif %>%
    graph_from_data_frame() %>%
    all_simple_paths(orpha_code, mode='in', cutoff=max_depth) %>%
    unlist() %>% names() %>% unique() %>% setdiff(orpha_code)

  # Build edgelist
  df_ancestors = df_classif %>%
    filter(from %in% ancestors, to %in% c(ancestors, orpha_code))

  # Return results in the specified format
  if(length(ancestors) == 0)
    return(NULL)
  else if(output=='codes_only')
    return(ancestors)
  else if(output=='edgelist')
    return(df_ancestors)
  else if(output=='graph')
    return(graph_from_data_frame(df_ancestors))
  else{
    warning('No valid output format was given. `output` value should be in `c("codes_only", "edgelist", "graph")`')
    return(NULL)
  }
}


#' @rdname analyze-genealogy
#' @export
get_descendants = function(orpha_code, output='codes_only', max_depth=NULL, df_classif=NULL){
  orpha_code = as.character(orpha_code)

  if(is.null(max_depth))
    max_depth=-1

  # Use specified classification or all of them if NULL was given
  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  # Check if the code belongs to the given classification
  if(!is_in_classif(orpha_code, df_classif))
    return(NULL)

  # Find all paths from the given ORPHAcode and extract elements
  descendants = df_classif %>%
    graph_from_data_frame() %>%
    all_simple_paths(orpha_code, mode='out', cutoff=max_depth) %>%
    unlist() %>% names() %>% unique() %>% setdiff(orpha_code)

  # Build edgelist
  df_descendants = df_classif %>%
      filter(to %in% descendants, from %in% c(orpha_code, descendants))

  # Return results in the specified format
  if(length(descendants) == 0)
    return(NULL)
  if(output == 'codes_only')
    return(descendants)
  else if(output == 'edgelist')
    return(df_descendants)
  else if(output == 'graph')
    return(graph_from_data_frame(df_descendants))
  else{
    warning('No valid output format was given. `output` value should be in `c("codes_only", "edgelist", "graph")`')
    return(NULL)
  }
}


#' @rdname analyze-genealogy
#' @export
get_siblings = function(orpha_code, output='codes_only', df_classif=NULL)
{
  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  # Find parents
  parents = get_parents(orpha_code, df_classif, output='codes_only')

  # Find the children of parents (=siblings)
  df_siblings = df_classif %>% filter(from %in% parents, to != orpha_code)
  siblings = setdiff(unique(df_siblings$to), orpha_code)

  # Return results in the specified format
  if(length(siblings) == 0)
    return(NULL)
  if(output == 'codes_only')
    return(siblings)
  else if(output == 'edgelist')
    return(df_siblings)
  else if(output == 'graph')
    return(graph_from_data_frame(df_siblings))
  else{
    warning('No valid output format was given. `output` value should be in `c("codes_only", "edgelist", "graph")`')
    return(NULL)
  }
}


#' @rdname analyze-genealogy
#' @export
merge_branches = function(orpha_codes, output='codes_only', df_classif=NULL, direction=c('up', 'down', 'both')){
  orpha_codes = na.omit(orpha_codes)

  if('both' %in% direction)
    direction = c('up', 'down')

  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  graph_classif = graph_from_data_frame(df_classif)

  df_y = graph_classif %>%
    vertical_positions() %>%
    filter(name %in% orpha_codes)

  all_ancestors = NULL
  all_descendants = NULL

  # Search ancestors
  if('up' %in% direction){
    roots = find_roots(graph_classif)
    all_ancestors = roots %>%
      lapply(function(root){
              selected_paths =
                all_simple_paths(graph_classif, from=root,
                                 to=intersect(orpha_codes, names(V(graph_classif))))
              all_ancestors = selected_paths %>% unlist() %>% names() %>%
                unique() %>% setdiff(orpha_codes)
              }) %>%
      unlist() %>% unique()
  }

  # Search descendants
  if('down' %in% direction){
    df_y_copy = df_y %>% arrange(y)
    while(nrow(df_y_copy)){
      orpha_code = first(df_y_copy$name)
      descendants = all_simple_paths(graph_classif, from=orpha_code) %>%
        unlist() %>% names() %>% unique() %>% setdiff(orpha_codes)
      all_descendants = unique(c(all_descendants, descendants))
      df_y_copy = df_y_copy %>% filter(!name %in% c(orpha_code, all_descendants))
    }
  }

  # Use the found ORPHAcodes to induce the original graph
  all_codes = unique(c(all_ancestors, orpha_codes, all_descendants))
  v_index = which(names(V(graph_classif)) %in% all_codes)
  graph_induced = induced_subgraph(graph_classif, vids = v_index)

  # Return results in the expected format
  if(length(V(graph_induced)) == 0)
    return(NULL)
  if(output == 'codes_only')
    return(names(V(graph_induced)))
  else if(output == 'edgelist')
    return(as_data_frame(graph_induced))
  else if(output == 'graph')
    return(return(graph_induced))
  else{
    warning('No valid output format was given. `output` value should be in `c("codes_only", "edgelist", "graph")`')
    return(NULL)
  }
}


#' @rdname analyze-genealogy
#' @export
complete_family = function(orpha_codes, output='codes_only', df_classif=NULL, max_depth=1)
{
  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  # Build graph from the found ancestors
  if(max_depth == 1)
    ancestors = df_classif %>% filter(to %in% orpha_codes) %>% pull(from) %>% unique()
  else if(is.null(max_depth))
    ancestors = merge_branches(orpha_codes, df_classif = df_classif, direction = 'up')
  else
    ancestors = lapply(
      orpha_codes,
      \(x) get_ancestors(x, df_classif=df_classif, max_depth=max_depth)) %>%
      unname() %>% unlist() %>% unique()

  new_orpha_codes = unique(c(orpha_codes, ancestors))
  graph_family = merge_branches(new_orpha_codes, output='graph', df_classif=df_classif, direction='down')

  # Return results in the expected format
  if(is.null(graph_family))
    return(NULL)
  else if(output == 'codes_only')
    return(V(graph_family) %>% names())
  else if(output == 'edgelist')
    return(as_data_frame(graph_family, what='edges'))
  else if(output == 'graph')
    return(graph_family)
  else{
    warning('No valid output format was given. `output` value should be in `c("codes_only", "edgelist", "graph")`')
    return(NULL)
  }
}


#' @rdname analyze-genealogy
#' @export
get_LCAs = function(orpha_codes, df_classif=NULL)
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

  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  graph = merge_branches(orpha_codes, output='graph', df_classif=df_classif) %>% add_superNode()

  # Find all paths going from the root to the codes in the given list
  all_paths = orpha_codes %>%
    as.character() %>%
    lapply(function(current_child) all_simple_paths(graph, from='SuperNode', to=current_child) %>%
                                      lapply(function(path) path %>% names()))

  # Find all possible pairs for the calculated paths
  combs = expand.grid(all_paths)

  # Find potential LCAs by computing the apparent LCA for each pair of paths
  LCAs = apply(combs, 1, get_paths_LCA) %>% unique()
  LCAs_combs = expand.grid(LCAs, LCAs)

  # Remove common ancestors which are not LCAs
  to_rem = LCAs_combs %>%
    # Try to find a path between two ancestors
    apply(1, function(x) all_simple_paths(graph, x[1], x[2])) %>%

    # If a path is found, parent is discarded
    keep(function(path) length(path) > 0) %>%

    # Clean results
    sapply(function(path) path[[1]][1]) %>% names %>% unique()

  # Remove ancestors to be discarded to keep LCAs only
  LCAs = setdiff(LCAs, c(to_rem, 'SuperNode'))
  return(LCAs)
}


#' Find specific ancestors
#'
#' @description
#' These functions helps you to find key ORPHAcodes which are located above the given ORPHAcode
#' in the classification system.
#'
#' @param orpha_code A vector of ORPHAcodes.
#' @param df_classif The classification to consider. If NULL, loads the whole Orphanet classification.
#'
#' @return
#' `subtype_to_disorder` returns the associated disorder, or the ORPHAcode itself if it
#' is already a disorder, or NULL if it is a group of disorders.
#' If a vector of ORPHAcodes is provided, function is applied on each element,
#' and the associated vector is returned.
#'
#' `get_lowest_groups` returns the closest groups from the given ORPHAcode,
#' or the ORPHAcode itself if it is a group of disorders already.
#'
#' @import dplyr
#' @importFrom igraph graph_from_data_frame
#' @importFrom purrr keep
#'
#' @examples
#' subtype_to_disorder(orpha_code = '158676')
#' # ORPHA:158676 is a subtype of disorder
#'
#' subtype_to_disorder(orpha_code = '303')
#' # ORPHA:303 is a group of disorder
#'
#' get_lowest_groups(orpha_code = '158676')
#'
#' @name upper-classification-levels

#' @rdname upper-classification-levels
#' @export
subtype_to_disorder = function(orpha_code, df_classif=NULL)
{
  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  df_nomenclature = load_raw_nomenclature()

  # If several ORPHAcodes are given, apply function on each element
  if(length(orpha_code) > 1)
    return(sapply(orpha_code, subtype_to_disorder, df_classif, USE.NAMES = F))

  # Find ancestors of the subtype code
  ancestors = get_ancestors(orpha_code, output='codes_only', df_classif=df_classif)

  # Find the disorder among ancestors (there should be only one)
  disorder_code = df_nomenclature %>%
    filter(orpha_code %in% ancestors,
           level == 36547) %>%
    pull(orpha_code)

  return(disorder_code)
}


#' @rdname upper-classification-levels
#' @export
get_lowest_groups = function(orpha_code, df_classif=NULL)
{
  if(is.null(df_classif))
    df_classif = load_classifications() %>% bind_rows() %>% distinct()

  df_nomenclature = load_raw_nomenclature()

  if(length(orpha_code) > 1)
    stop(simpleError('`get_lowest_groups` accepts only single ORPHAcode as an input.'))

  # Check if it isn't a group already
  classLevel = df_nomenclature %>% filter(orpha_code == .env$orpha_code) %>% pull(level)
  if(classLevel == '36540')
    return(orpha_code)

  # Find groups of disorders containing the given ORPHAcode
  ancestors = get_ancestors(orpha_code, output='codes_only', df_classif=df_classif)
  df_ancestors = get_ancestors(orpha_code, output='edgelist', df_classif=df_classif)
  df_groups = data.frame(orpha_code=ancestors) %>%
    left_join(df_nomenclature, by='orpha_code') %>%
    filter(level == '36540')

  # Get the lowest
  lowest_groups = df_ancestors %>%
    semi_join(df_groups, by=c('to'='orpha_code')) %>%
    graph_from_data_frame() %>%
    find_leaves()

  return(lowest_groups)
}


#' Merge graphs
#'
#' @description
#' `r lifecycle::badge('questioning')`
#'
#' Convert a list of graphs into a single merged graph
#'
#' @param graphs_list The graphs to merge.
#'
#' @return The merged graph
#' @import magrittr
#' @importFrom igraph as_data_frame graph_from_data_frame
#' @importFrom dplyr bind_rows distinct group_by summarize
#'
#' @export
#' @examples
#' code = '303'
#' graph_descendants = get_descendants(code, output='graph')
#' graph_ancestors = get_ancestors(code, output='graph')
#'
#' merged_graph = merge_graphs(list(graph_descendants, graph_ancestors))
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
      bind_rows() %>%
      distinct(from, to)

    # Merge graphs
    merged_graph = graph_from_data_frame(df_edges, vertices=df_nodes)
    return(merged_graph)
  }
}


#' Add an extra node above the roots of the graph
#'
#' @param graph The graph to which the extra node should be added.
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


#' Operations on graphs
#'
#' @description
#' Orphanet classification system can be depicted by oriented graphs as it is a wide list of
#' parent/child relationships.
#'
#' The `igraph` package provides useful tools for graphs analysis that we can use for the
#' study of rare diseases.
#'
#' The roots are the ORPHAcodes without any parent,
#' like the heads of classification. The leaves are the ORPHAcodes without any children.
#' They are usually disorders or subtypes of disorders (see [get_classification_level()]).
#'
#' @param graph An igraph object.
#'
#' @import magrittr
#' @importFrom igraph degree
#'
#' @return The extrem nodes of a graph.
#'
#' @examples
#' graph = get_ancestors('303', output='graph')
#'
#' roots = find_roots(graph)
#' leaves = find_leaves(graph)
#'
#' @name graph-operations
NULL

#' @rdname graph-operations
#' @export
find_roots = function(graph)
{
  # Roots are the only nodes having no arcs going in
  D = degree(graph, mode='in')
  D = D[D==0]
  roots = names(D)
  return(roots)
}


#' @rdname graph-operations
#' @export
find_leaves = function(graph)
{
  # Leaves are the only nodes having no arcs going out
  d = degree(graph, mode='out')
  d = d[d==0]
  leaves = names(d)

  return(leaves)
}
