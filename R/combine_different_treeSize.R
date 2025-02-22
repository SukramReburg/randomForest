combineDS <- function(...) {
  pad0 <- function(x, len) c(x, rep(0, len-length(x)))
  padm0 <- function(x, len) rbind(x, matrix(0, nrow=len-nrow(x),
                                            ncol=ncol(x)))
  rflist <- list(...)
  areForest <- sapply(rflist, function(x) inherits(x, "randomForest")) 
  if (any(!areForest)) stop("Argument must be a list of randomForest objects")
  importance_rownames <- lapply(rflist, function(x) rownames(x$importance))
  if (!sapply(importance_rownames[-1], 
             function(x) all(x %in% importance_rownames[[1]])))
    stop("Predictor set of the the second randomForest object must be subset of 
         the first randomForest object.")
  ## Use the first component as a template
  rf <- rflist[[1]]
  classRF <- rf$type == "classification"
  trees <- sapply(rflist, function(x) x$ntree)
  ntree <- sum(trees)
  rf$ntree <- ntree
  nforest <- length(rflist)
  haveTest <- ! any(sapply(rflist, function(x) is.null(x$test)))
  ## Check if predictor variables are identical.
  vlist <- lapply(rflist, function(x) rownames(x$importance))
  numvars <- sapply(vlist, length)
  ## Combine the forest component, if any
  mapping_table <- matrix(c(0,
    1:length(rownames(rflist[[2]]$importance)),
    0,which(rownames(rf$importance) %in% rownames(rflist[[2]]$importance))),
    ncol = 2, byrow = FALSE)
  colnames(mapping_table) <- c("original", "mapped")
  mapping_vector <- setNames(mapping_table[, 2], mapping_table[, 1])
  
  # Use the mapping vector to replace values in the original matrix
  rflist[[2]]$forest$bestvar <-
    matrix(
      mapping_vector[as.character(rflist[[2]]$forest$bestvar)],
      nrow = nrow(rflist[[2]]$forest$bestvar),
      ncol = ncol(rflist[[2]]$forest$bestvar)
    )
  
  
  
  
  haveForest <- sapply(rflist, function(x) !is.null(x$forest))
  if (all(haveForest)) {
    nrnodes <- max(sapply(rflist, function(x) x$forest$nrnodes))
    rf$forest$nrnodes <- nrnodes
    rf$forest$ndbigtree <-
      unlist(sapply(rflist, function(x) x$forest$ndbigtree))
    rf$forest$nodestatus <-
      do.call("cbind", lapply(rflist, function(x)
        padm0(x$forest$nodestatus, nrnodes)))
    rf$forest $bestvar <-
      do.call("cbind",
              lapply(rflist, function(x)
                padm0(x$forest$bestvar, nrnodes)))
    rf$forest$xbestsplit <-
      do.call("cbind",
              lapply(rflist, function(x)
                padm0(x$forest$xbestsplit, nrnodes)))
    rf$forest$nodepred <-
      do.call("cbind", lapply(rflist, function(x)
        padm0(x$forest$nodepred, nrnodes)))
    tree.dim <- dim(rf$forest$treemap)
    if (classRF) {
      rf$forest$treemap <-
        array(unlist(lapply(rflist, function(x) apply(x$forest$treemap, 2:3,
                                                      pad0, nrnodes))),
              c(nrnodes, 2, ntree))
    } else {
      rf$forest$leftDaughter <-
        do.call("cbind",
                lapply(rflist, function(x)
                  padm0(x$forest$leftDaughter, nrnodes)))
      rf$forest$rightDaughter <-
        do.call("cbind",
                lapply(rflist, function(x)
                  padm0(x$forest$rightDaughter, nrnodes)))
    }
    rf$forest$ntree <- ntree
    if (classRF) rf$forest$cutoff <- rflist[[1]]$forest$cutoff
  } else {
    rf$forest <- NULL
  }
  
  if (classRF) {
    ## Combine the votes matrix: 
    rf$votes <- 0
    rf$oob.times <- 0
    areVotes <- all(sapply(rflist, function(x) any(x$votes > 1, na.rm=TRUE)))
    if (areVotes) {
      for(i in 1:nforest) {
        rf$oob.times <- rf$oob.times + rflist[[i]]$oob.times
        rf$votes <- rf$votes +
          ifelse(is.na(rflist[[i]]$votes), 0, rflist[[i]]$votes)
      }
    } else {
      for(i in 1:nforest) {
        rf$oob.times <- rf$oob.times + rflist[[i]]$oob.times            
        rf$votes <- rf$votes +
          ifelse(is.na(rflist[[i]]$votes), 0, rflist[[i]]$votes) *
          rflist[[i]]$oob.times
      }
      rf$votes <- rf$votes / rf$oob.times
    }
    rf$predicted <- factor(colnames(rf$votes)[max.col(rf$votes)],
                           levels=levels(rf$predicted))
    if(haveTest) {
      rf$test$votes <- 0
      if (any(rf$test$votes > 1)) {
        for(i in 1:nforest)
          rf$test$votes <- rf$test$votes + rflist[[i]]$test$votes
      } else {
        for (i in 1:nforest)
          rf$test$votes <- rf$test$votes +
            rflist[[i]]$test$votes * rflist[[i]]$ntree
      }
      rf$test$predicted <-
        factor(colnames(rf$test$votes)[max.col(rf$test$votes)],
               levels=levels(rf$test$predicted))
    }
  } else {
    
    # for (i in 1:nforest) rf$predicted <- rf$predicted +
    #     rflist[[i]]$predicted * rflist[[i]]$ntree
    # rf$predicted <- rf$predicted / ntree
    if (haveTest) {
      rf$test$predicted <- 0
      for (i in 1:nforest) rf$test$predicted <- rf$test$predicted +
          rflist[[i]]$test$predicted * rflist[[i]]$ntree
      rf$test$predicted <- rf$test$predicted / ntree
    }
  }
  
  rf$predicted <- 0
  rf$predicted <-
    factor(colnames(rf$votes)[max.col(rf$votes)],
           levels=rf$classes)
  
  
  tib <- table(rf$y, rf$predicted)
  rf$confusion <- cbind(tib, tib[, 2]/rowSums(tib))
  
  rf$err.rate <- rbind(rflist[[1]]$err.rate, rflist[[2]]$err.rate)
  
  
  ## If variable importance is in all of them, compute the average
  ## (weighted by the number of trees in each forest)
  have.imp <- !any(sapply(rflist, function(x) is.null(x$importance)))
  if (have.imp) {
    
    importance_comb <- matrix(
      c(
        rflist[[1]]$importance[, 1],  # First forest's importance values
        sapply(
          rownames(rflist[[1]]$importance), 
          function(x) {
            if (x %in% rownames(rflist[[2]]$importance)) {
              rflist[[2]]$importance[x, 1]  # Use the correct row name for indexing
            } else {
              0  # Fill with zero if the variable is not in the second forest
            }
          }
        )
      ),
      ncol = 2, byrow = FALSE
    )
    rownames(importance_comb) <- rownames(rflist[[1]]$importance)
    
    importanceSD_comb <- matrix(
      c(
        rflist[[1]]$importance[, 1],  # First forest's importance values
        sapply(
          rownames(rflist[[1]]$importance), 
          function(x) {
            if (x %in% rownames(rflist[[2]]$importance)) {
              rflist[[2]]$importance[x, 1]  # Use the correct row name for indexing
            } else {
              0  # Fill with zero if the variable is not in the second forest
            }
          }
        )
      ),
      ncol = 2, byrow = FALSE
    )
    rownames(importanceSD_comb) <- rownames(rflist[[1]]$importance)

    
    
    rf$importance <- rf$importanceSD <- 0
    
    rf$importance <- 
      matrix(importance_comb[, 1]*rflist[[1]]$ntree +
      importance_comb[, 2]*rflist[[2]]$ntree)
    
    rownames(rf$importance) <-  rownames(importance_comb)
    
    rf$importanceSD <- 
      matrix(importanceSD_comb[, 1]*rflist[[1]]$ntree +
               importanceSD_comb[, 2]*rflist[[2]]$ntree)
    
    rownames(rf$importanceSD) <-  rownames(importanceSD_comb)
    
    rf$importance <- rf$importance / ntree
    rf$importanceSD <- sqrt(rf$importanceSD / ntree)
    haveCaseImp <- !any(sapply(rflist, function(x)
      is.null(x$localImportance)))
    ## Average casewise importance
    if (haveCaseImp) {
      rf$localImportance <- 0
      for (i in 1:nforest) {
        rf$localImportance <- rf$localImportance +
          rflist[[i]]$localImportance * rflist[[i]]$ntree
      }
      rf$localImportance <- rf$localImportance / ntree
    }
  }
  
  map_vector <- function(vector, mapping) {
    mapped_vector <- rep(NA, max(mapping))
    mapped_vector[mapping_vector[2:length(mapping_vector)]] <- vector
    return(mapped_vector)
  }
  
  mapped_rf2 <- lapply(rflist[[2]]$omega, map_vector, mapping = mapping_vector)
  
  rf$omega <- c(rf$omega, mapped_rf2)
  names(rf$omega) <- seq_along(rf$omega)
  
  
  ## If proximity is in all of them, compute the average
  ## (weighted by the number of trees in each forest)
  have.prox <- !any(sapply(rflist, function(x) is.null(x$proximity)))
  if (have.prox) {
    rf$proximity <- 0
    for(i in 1:nforest)
      rf$proximity <- rf$proximity + rflist[[i]]$proximity * rflist[[i]]$ntree
    rf$proximity <- rf$proximity / ntree
  }
  
  ## if there are inbag matrices, combine them as well.
  hasInBag <- all(sapply(rflist, function(x) !is.null(x$inbag)))
  if (hasInBag) rf$inbag <- do.call(cbind, lapply(rflist, "[[", "inbag"))
  rf
}
