#' Metrics for multiple random walks
#'
#' Computes metrics for 50 generated random walks using the function 'rw_client'.
#'
#' @param v The initial vertex of the input graph.
#' @param g The input graph. It should be a transactional graph with the amount as the attribute of each edge. The vertices must be the clients IDs.
#' @param data Dataframe with information of the clients. It should include a column with the clients IDs named "customer_id" and the alert label named "sar_flag" that must be a boolean variable.
#'
#' @return A vector with the minimum, mean and maximum for both the number of steps and total transactioned amount in the random walks calculated.
#' @export
#'
#' @examples
#' g <- igraph::graph_from_data_frame(d = transactions_small_example[, 1:3], directed = TRUE)
#' v <- transactions_small_example[1, 1]
#' mean_rw_client(v, g, data = clients_small_example)
#'
#' @import igraph
#' @references Eddin, A. N., Bono, J., Aparício, D., Polido, D., Ascensao, J. T., Bizarro, P., and Ribeiro, P. (2021). Anti-money laundering alert optimization using machine learning with graphs. arXiv preprint arXiv:2112.07508.

mean_rw_client <- function(v, g, data) {
  datac <- data
  c <- vector()
  m <- vector()
  for (i in 1:50) {
    c[i] <- rw_client(v, g, datac)[1]
    m[i] <- rw_client(v, g, datac)[2]
  }
  final <- vector()
  final[1] <- min(c)
  final[2] <- mean(c)
  final[3] <- max(c)
  final[4] <- min(m)
  final[5] <- mean(m)
  final[6] <- max(m)
  return(final)
}
