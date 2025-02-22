find_numberofTrees <- function(rf, data, 
                               features.next = NULL, 
                               imp_f.next = NULL, unimp_f.next = NULL,
                               reconstruct_all){
  
  term_labels <- attr(rf[["terms"]], "term.labels")
  term_indices <- match(term_labels, names(data))
  target_label <- attr(rf[["terms"]], "variables")[[2]]
  
  DT <-
    data.table(matrix(unlist(rf[["omega"]]), nrow = rf$ntree, byrow = TRUE))
  DT[is.na(DT)] = 0
  
  gamma <- (1/rf[["err.rate"]][, 1])/(1/min(rf[["err.rate"]][, 1]))
  
  DT <- as.matrix(DT)
  colnames(DT) <- term_labels
  
  weight_f <- t(DT) %*% gamma/(max(t(DT) %*% gamma))
  
  if (is.null(features.next)) {
    f = floor(sqrt(length(term_labels)))
    u = floor(sqrt(length(term_labels)))
    v = length(term_labels) - u
    features <- term_labels[order(t(DT) %*% gamma, decreasing = T)]
    imp_f <- features[1:u]
    unimp_f <- features[u + 1:v]
  } else {
    f = floor(sqrt(length(features.next)))
    u = length(imp_f.next)
    v = length(unimp_f.next)
    features = features.next
    imp_f = imp_f.next
    unimp_f = unimp_f.next
  }
  
  thr <- max(
    mean(weight_f[unimp_f, ]) - 2* sd(weight_f[unimp_f, ]),
    0)
  if (length(unimp_f[weight_f[unimp_f, ] < thr]) != 0) {
    R <- unimp_f[weight_f[unimp_f, ] < thr]
  } else {
    R <- unimp_f[min(weight_f[unimp_f, ]) == weight_f[unimp_f, ]]
  }
  
  A <- unimp_f[weight_f[unimp_f, ] >= mean(weight_f[imp_f, ])]
  
  imp_f.next = unique(c(imp_f, A))
  unimp_f.next <- setdiff(unimp_f, c(A, R))
  
  delta_u = length(imp_f.next) - u
  delta_v = length(unimp_f.next) - v
  features.next <- c(imp_f.next, unimp_f.next)
  
  
  q = 1 - choose(length(unimp_f), f)/choose(length(imp_f) + length(unimp_f), f) 
  
  # N_av = round(mean(rf[["forest"]][["ndbigtree"]]))
  # N_av = round(mean(treesize(rf)))
  N_av = 6
  l   = rf$ntree * N_av * q ** (N_av-1) * (1-q ** N_av) ** (rf$ntree-1) 
  qu  = (factorial(v) / factorial(u+v)) * (f * factorial(u+v-1-f))/factorial(v-f)
  qv  = - factorial(v-1) / (factorial(u+v-1)) * (factorial(u+v-1-f) * u * f) / (factorial(v-f) * (u+v))
  roh = (1 - choose(u+v-f, f) / choose(u+v, f)) ** N_av
  w   = ((1-roh)**(round(rf$ntree/2))) / 2 * log(1-roh) - (1-q**N_av)**(rf$ntree)*log(1-q**N_av) 
  if (w > 0) {
    delta_B = ceiling(abs(l * (qu * delta_u + qv * delta_v) / w))
  } else{
    delta_B = 1
  }
  if (reconstruct_all == T)  {
    ntree.next = delta_B + rf$ntree
  } else {
    ntree.next = delta_B
  }
  
  return(list("number of trees" = ntree.next, "features" = features.next, 
              "target" = target_label, "important" = imp_f.next, "unimportant" = unimp_f.next))
  
}

addTrees <- function(data, ntree, features, target){
  form_red = as.formula(paste(target, "~", paste(features, collapse = " + ")))
  rf <- randomForest(
    formula = form_red,
    data = data,
    ntree = ntree,
    attrEval = 1,
    isSizeopt = TRUE,
    keep.inbag = TRUE
    )
  return(rf)
}

optnumTree_alg <- function(rf_start, data, reconstruct_all = T){
  
  numberofTrees <- find_numberofTrees(rf = rf_start, data = data, 
                                      features.next = NULL, 
                                      imp_f.next = NULL, unimp_f.next = NULL, 
                                      reconstruct_all)
  
  # Replace the old forest as a whole and calculate new forest with # of Trees
  #   new set of features. Ohterwise only add delta # of trees to the already 
  #   existing ensenmble.
  if (reconstruct_all == T) {
    rf_add <-
      addTrees(
        data = data,
        ntree = numberofTrees[["number of trees"]],
        features = numberofTrees[["features"]][order(match(numberofTrees[["features"]], colnames(data)))],
        target = numberofTrees[["target"]]
      )
  } else {
    rf_tmp <-  addTrees(
      data = data,
      ntree = numberofTrees[["number of trees"]],
      features = numberofTrees[["features"]][order(match(numberofTrees[["features"]], colnames(data)))],
      target = numberofTrees[["target"]]
    )
    
    rf_add <- combineDS(rf_start, rf_tmp)
  }
    counter = 0
  while(length(numberofTrees[["unimportant"]]) > 
        floor(sqrt(length(numberofTrees[["features"]]))) & counter < 20){
    counter = counter + 1
    cat("\n", counter, "-th iteration of adding Trees!", "\n")
    numberofTrees.next <- find_numberofTrees(rf = rf_add, data = data,
                                        features.next = numberofTrees[["features"]][order(match(numberofTrees[["features"]], colnames(data)))],
                                        imp_f.next = numberofTrees[["important"]], 
                                        unimp_f.next = numberofTrees[["unimportant"]],
                                        reconstruct_all)
    cat(paste0(" \n Number of trees to add: ", 
               numberofTrees.next[["number of trees"]], ". \n"))
    if(numberofTrees.next[["number of trees"]] > 600){
      cat("\n Next step with #", 
          numberofTrees.next[["number of trees"]], 
          "number of Trees aborted, due to calculation time issue. \n",
          "Previous RF returned")
      return(rf_add)
    } else{
      if(reconstruct_all == T){
      rf_add.next <- 
        addTrees(
          data = data,
          ntree = numberofTrees.next[["number of trees"]],
          features = numberofTrees.next[["features"]][order(match(numberofTrees.next[["features"]], colnames(data)))],
          target = numberofTrees.next[["target"]]
        )
      cat(paste0(" \n New tree has : ", 
                 rf_add.next$ntree, " trees. \n"))
      } else {
        rf_tmp <-  addTrees(
          data = data,
          ntree = numberofTrees.next[["number of trees"]],
          features = numberofTrees.next[["features"]][order(match(numberofTrees.next[["features"]], colnames(data)))],
          target = numberofTrees.next[["target"]]
        )
        
        rf_add.next <- combineDS(rf_add, rf_tmp)
      }
    }
    
    cat("\n Trees to add: ", numberofTrees.next[["number of trees"]])
    
    numberofTrees <- numberofTrees.next
    rf_add <- rf_add.next
    
    rm(numberofTrees.next, rf_add.next)
  }
  return(rf_add)
}





