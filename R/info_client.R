#' Random walk metrics for each client
#'
#' Computes the metrics of the generated random walks for every client in the dataframe using the function 'mean_rw_client'.
#'
#' @param g The input graph. Transactional graph containing the amount (in monetary unit) as the attribute of each edge. The vertices must be the clients IDs.
#' @param data Dataframe with information of the clients. It should include a column with the clients IDs named "customer_id" and the alert label named "sar_flag" that must be a boolean variable.
#'
#' @return A dataframe with the clients IDs and the computed metrics (minimum, mean and maximum for both the number of steps and total transactioned amount) for the random walks starting in each client.
#' @export
#'
#' @examples
#' g <- igraph::graph_from_data_frame(d = transactions_small_example[, 1:3], directed = TRUE)
#' info_client(g, data = clients_small_example)
#'
#' @import igraph
#' @references Eddin, A. N., Bono, J., Aparício, D., Polido, D., Ascensao, J. T., Bizarro, P., and Ribeiro, P. (2021). Anti-money laundering alert optimization using machine learning with graphs. arXiv preprint arXiv:2112.07508.

info_client <- function(g, data) {
  datac <- data
  n <- length(datac$customer_id)
  min_comp <- vector()
  mean_comp <- vector()
  max_comp <- vector()
  min_amount <- vector()
  mean_amount <- vector()
  max_amount <- vector()
  for (i in 1:n) {
    v1 <- datac$customer_id[i]
    rand_walk <- mean_rw_client(v1, g, datac)
    min_comp[i] <- rand_walk[1]
    mean_comp[i] <- rand_walk[2]
    max_comp[i] <- rand_walk[3]
    min_amount[i] <- rand_walk[4]
    mean_amount[i] <- rand_walk[5]
    max_amount[i] <- rand_walk[6]
  }
  df <- data.frame(
    datac$customer_id, min_comp, mean_comp, max_comp, min_amount,
    mean_amount, max_amount
  )
  return(df)
}
