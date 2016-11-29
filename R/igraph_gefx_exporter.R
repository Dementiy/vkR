#' Converts the given igraph object to GEXF format and saves it at the given filepath location
#'
#' @param g Input igraph object to be converted to gexf format
#' @param filepath File location where the output gexf file should be saved
#' @author Gopalakrishna Palem, \email{Gopalakrishna.Palem@@Yahoo.com}
#' @export
saveAsGEXF <- function(g, filepath="converted_graph.gexf") {
  if (!requireNamespace("igraph", quietly = TRUE))
    stop("The package igraph was not installed")
  if (!requireNamespace("rgexf", quietly = TRUE))
    stop("The package rgexf was not installed")

  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  if(is.null(igraph::V(g)$label))
    igraph::V(g)$label <- as.character(igraph::V(g))

  # similarily if edges does not have weight, add default 1 weight
  if(is.null(igraph::E(g)$weight))
    igraph::E(g)$weight <- rep.int(1, igraph::ecount(g))

  nodes <- data.frame(cbind(igraph::V(g), igraph::V(g)$label))
  edges <- t(Vectorize(igraph::get.edge, vectorize.args='id')(g, 1:igraph::ecount(g)))

  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(igraph::list.vertex.attributes(g), "label")
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&#038;", igraph::get.vertex.attribute(g, attr))))

  # combine all edge attributes into a matrix (and take care of & for xml)
  eAttrNames <- setdiff(igraph::list.edge.attributes(g), "weight")
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&#038;", igraph::get.edge.attribute(g, attr))))

  # combine all graph attributes into a meta-data
  graphAtt <- sapply(igraph::list.graph.attributes(g), function(attr) sub("&", "&#038;", igraph::get.graph.attribute(g, attr)))

  # generate the gexf object
  output <- rgexf::write.gexf(nodes, edges,
                       edgesWeight=igraph::E(g)$weight,
                       edgesAtt = edgesAtt,
                       nodesAtt = nodesAtt,
                       meta=c(list(creator="Gopalakrishna Palem", description="igraph -> gexf converted file", keywords="igraph, gexf, R, rgexf"), graphAtt))

  print(output, filepath, replace=T)
}
