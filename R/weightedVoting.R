
# Traverse tree function
traverse_tree <- function(rf, tree, data_row) {
  node_id <- 1  # Start from the root node
  
  while (TRUE) {
    node <- tree[node_id, ]
    
    # Check if the node is a terminal node
    if (node['status'] == -1) {
      return(node['prediction'])  # Return the prediction for terminal node
    }
    
    # Get split information
    split_var_index <- node['split var']
    term_labels <- attr(rf[["terms"]], "term.labels")
    split_var <- term_labels[split_var_index]
    split_point <- node['split point']
    
    # Determine the next node based on the split
    if (data_row[[split_var]] <= split_point) {
      node_id <- node['left daughter']
    } else {
      node_id <- node['right daughter']
    }
  }
}

# Prediction function using a single tree
predict_single_tree <- function(rf, tree, data) {
  predictions <- sapply(1:nrow(data), function(i) {
    traverse_tree(rf, tree, data[i, ])
  })
  return(predictions)
}

normalize <- function(x) {
  # Check if the input vector has more than one unique value to avoid division by zero
  if (length(unique(x)) == 1) {
    return(rep(0.5, length(x)))  # If all values are the same, return 0.5 for all
  }
  
  # Normalize the values to the range [0, 1]
  return((x - min(x)) / (max(x) - min(x)))
}


weighted_voting <- function(rf, dt_in, t = 100, size = 500, weight.target = c(1, 1)){
  # Check if proximity and inbag attributes are available
  if(is.null(rf$inbag)){
    stop("Inbag tracking must be true for weighted voting approach.")
  }
  
  # Convert dt_in to data.table
  setDT(dt_in)
  
  n <- nrow(dt_in)
  dt_pred <- as.data.table(matrix(NA, nrow = n, ncol = rf$ntree))
  initial_times <- numeric(5)
  
  for (k in 1:5) {
    start_time <- Sys.time()
    tree <- getTree(rf, k)
    predict_single_tree(rf, tree, dt_in)
    end_time <- Sys.time()
    initial_times[k] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }
  
  # Estimate total time
  avg_time <- mean(initial_times)
  estimated_total_time <- avg_time * rf$ntree
  cat(sprintf("Estimated total time: %.2f seconds\n", estimated_total_time))
  
  predictions_list <- lapply(1:rf$ntree, function(k) {
    tree <- getTree(rf, k)
    predict_single_tree(rf, tree, data = dt_in)
  })
  dt_pred <- as.data.table(do.call(cbind, predictions_list))
  
  setnames(dt_pred, paste0("V", 1:rf$ntree))
  
  # Identify the target variable
  target <- as.character(attr(rf[["terms"]], "variables")[[2]])
  
  
  if(!is.numeric(class(dt_in[[target]]))){
    dt_in[, (target) := lapply(.SD, as.numeric), .SDcols = target]
  }
  if( any(sapply(dt_pred, class) != "numeric") ){
    dt_pred[, names(dt_pred) := lapply(.SD, as.numeric), .SDcols = names(dt_pred)]
    
  }
  # Calculate proportions for sampling
  prop <- dt_in[, .N, by = target][, N := pmin(N, floor(weight.target * (N/sum(N)) * size))]
  
  # Add an id column to dt_in
  dt_in[, id := 1:n]
  
  # Sample similar instances for each class
  dt_sampsf <- rbind(
    dt_in[get(target) == prop[1, ][[target]]][sample(.N, prop[1, ]$N, replace = FALSE)],
    dt_in[get(target) == prop[2, ][[target]]][sample(.N, prop[2, ]$N, replace = FALSE)]
  )
  
  if(is.null(rf$proximity)){
    proximity <- 
      predict(rf, newdata = dt_sampsf, proximity = TRUE)$proximity
  } else {
    proximity <- rf$proximity
  }
  
  # Initialize weights
  weight <- rep(0, rf$ntree)
  
  dt_oob <- as.data.table(rf$inbag == 0)
  dt_target <-
    data.table::as.data.table(matrix(
      rep(dt_in[, get(target)], times = rf$ntree),
      ncol = rf$ntree,
      byrow = FALSE
    ))
  
  dt_target[!dt_oob] <- NA
  
  dt_pred[!dt_oob] <- NA
  
  # Initialize weight variable
  weight <- numeric(rf$ntree)
  
  # Add progress bar
  pb <- txtProgressBar(min = 0, max = nrow(dt_sampsf), style = 3)
  
  # Loop through each row of dt_sampsf
  for(i in 1:nrow(dt_sampsf)){
    
    j <- dt_sampsf[i, "id"][[1]]
    if( is.null(rf$proximity) ) prox <- proximity[i, ]
    if( !is.null(rf$proximity) ) prox <- proximity[j, ]
    prox_ind <- order(prox, decreasing = TRUE)
    
    if(prox[prox_ind[t]] == 1){
      sim_ind <- sample(as.numeric(names(prox[prox == 1])), t, replace = FALSE)
    } else {
      sim_ind <- prox_ind[1:t]
    }
    sim_ind <- sim_ind[prox[sim_ind] > 0]
    
    dt_tmp <- dt_target[sim_ind, ]
    
    if(any(colSums(dt_tmp, na.rm = TRUE) == 0)){
      warning(paste("At least one tree has had for entry: ", j, " no Out-Of_Bag cases, \n and therefore will be skipped."), noBreaks. = TRUE)
      next
    }
    
    dt_count <- as.data.table(t(rbind(
      colSums(dt_tmp == 1, na.rm = TRUE),
      colSums(dt_tmp == 2, na.rm = TRUE)
    )))
    
    dt_count[, maj := ifelse(.SD[[1]] > .SD[[2]], 1, 2), .SDcols = c("V1", "V2")]
    
    dt_tmp_pred <- dt_pred[sim_ind, ]
    dt_count_pred <- as.data.table(t(rbind(
      colSums(dt_tmp_pred == 1, na.rm = TRUE),
      colSums(dt_tmp_pred == 2, na.rm = TRUE)
    )))
    dt_count_pred[, maj := ifelse(.SD[[1]] > .SD[[2]], 1, 2), .SDcols = c("V1", "V2")]
    
    dt_maj <- as.data.table(matrix(
      rep(dt_count[["maj"]], times = length(sim_ind)),
      ncol = rf$ntree,
      byrow = TRUE
    ))
    
    weight <- weight + pmax((
      colSums(dt_tmp_pred == dt_maj & dt_tmp == dt_tmp_pred, na.rm = TRUE) - 
        colSums(dt_tmp_pred != dt_maj & dt_tmp == dt_tmp_pred, na.rm = TRUE)
    ) / colSums(!is.na(dt_tmp)), 0)
    

  }
  
  
  
  # Normalize weights
  weight  <- weight / nrow(dt_sampsf)
  
  weight <- normalize(weight)
  
  return(weight)
}

