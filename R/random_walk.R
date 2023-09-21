#' Random walk simulation
#'
#' Computes a random walk path for a given client.
#'
#' @param v The initial vertex of the input graph.
#' @param g The input graph. It should be a transactional graph with the amount as the attribute of each edge. The vertices must be the clients IDs.
#' @param data Dataframe with information of the clients. It should include a column with the clients IDs named "customer_id" and the alert label named "sar_flag" that must be a boolean variable.
#'
#' @return A vector with the number of steps taken in the random walk and the total transactioned amount in it.
#' @export
#'
#' @examples
#' g <- igraph::graph_from_data_frame(d = transactions_small_example[, 1:3], directed = TRUE)
#' v <- transactions_small_example[1, 1]
#' rw_client(v, g, data = clients_small_example)
#' @import igraph
#' @references Eddin, A. N., Bono, J., Aparício, D., Polido, D., Ascensao, J. T., Bizarro, P., and Ribeiro, P. (2021). Anti-money laundering alert optimization using machine learning with graphs. arXiv preprint arXiv:2112.07508.

rw_client <- function(v, g, data) {
  datac <- data
  edges_out <- igraph::incident(g, v, mode = "out")
  comp <- 0
  tot_amount <- 0
  if (length(edges_out) == 0) {
    comp <- 0
    tot_amount <- 0
  } else {
    edge <- sample(edges_out, 1)
    v2 <- igraph::ends(g, edge)[2]
    comp <- 1
    tot_amount <- tot_amount + as.numeric(igraph::edge_attr(g, "amount", edge))
    while ((datac[datac$customer_id == v2, "sar_flag"]) != 1) {
      edges_out <- igraph::incident(g, v2, mode = "out")
      if (length(edges_out) == 0) {
        break
      }
      edge <- sample(edges_out, 1)
      v2 <- igraph::ends(g, edge)[2]
      comp <- comp + 1
      tot_amount <- tot_amount + as.numeric(igraph::edge_attr(g, "amount", edge))
    }
  }
  c <- vector()
  c[1] <- comp
  c[2] <- tot_amount
  return(c)
}
