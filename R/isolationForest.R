library(data.tree)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

buildTree <- function(node, data, remainingDepth, actualDepth) {
  ##print(data[1:10,])
  node$obsCount <- nrow(data)
  ##print(paste0("remainingDepth: ", remainingDepth))
  if (node$obsCount == 0) {
    child <- node$AddChild('no_objects')
    child$feature <- ' '
    child$obsCount <- 0
    child$depth <- actualDepth
    child$leaf <- 1
    
  }
  else if (node$obsCount == 1) {
    #construct leaf when 1 object in node
    child <- node$AddChild('one_object')
    node$feature <- tail(names(data), 1)
    child$obsCount <- 1
    child$feature <- ''
    child$depth <- actualDepth
    child$leaf <- 1
  }
  else if (remainingDepth == 0){
    #construct leaf because max size of tree is reached
    child <- node$AddChild('max_tree_size')
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
    child$depth <- actualDepth
    child$leaf <- 1

  }
  else {
    
    numberOfFeatures <- length(names(data))
    # set.seed(123)
    whichFeature <- sample(1 : numberOfFeatures, 1, replace = TRUE)
    feature <- names(data)[whichFeature]
    vector <- data[, whichFeature, drop = FALSE]
    if(var(vector, na.rm = TRUE) == 0) {
      #create leaf if for a given feature there is only 1 value
      child <- node$AddChild('same_values')
      node$feature <- tail(names(data), 1)
      child$obsCount <- nrow(data)
      child$feature <- ''
      child$depth <- actualDepth
      child$leaf <- 1
    }
    else {
      remainingDepth <- remainingDepth - 1
      actualDepth <- actualDepth + 1
      node$feature <- feature
      minValue <- min(vector, na.rm = TRUE)
      maxValue <- max(vector, na.rm = TRUE)
      # set.seed(123)
      splitPoint <- runif(1, min=minValue, max=maxValue)
      node$splitPoint <- splitPoint
      childObs <- split(data, 
                        data[ ,feature] > splitPoint, 
                        drop = TRUE)
      for(i in 1:length(childObs)) {
        #construct a child having the name of that feature value (e.g. 'red')
        name <- paste(feature, toString(splitPoint), "greater", names(childObs)[i], sep = " ")
        child <- node$AddChild(name)
        child$leaf <- 0
        #print("Children")
        #print(node$children)
        #call the algorithm recursively on the child and the subset
        #print(paste0("Building node with condition: ", name))
        buildTree(child, childObs[[i]], remainingDepth, actualDepth)
      }
      
    }
  }
}

buildIsolationForest <- function(data, numberOfTrees = 25, subsetSize = 256) {
  
  maxTreeDepth = log(subsetSize, 2)
  if(subsetSize > nrow(data)) {
    subsetSize = nrow(data)
    warning("Subset size bigger than number of data rows, 
            changing subset size to number of data rows")
  }
  for(i in 1:numberOfTrees){
    tree <- Node$new("mammography")
    subSet <- data[sample(nrow(data), subsetSize), ]
    #print(paste0("Building tree: ", i))
    buildTree(tree, subSet, maxTreeDepth, 0)
    ##print("children in building forest")
    ##print(tree$children)
    trees = append(trees, tree)
    # #print(trees)
  }
  model = structure(list(trees = trees), class = "isolationForestClass")
  # return(trees)
  return(model)
}

pathLength <- function(tree, row) {
  #print(tree$children)
  if (tree$children[[1]]$leaf == 1) {
    if(tree$children[[1]]$obsCount == 1) {
      return(tree$children[[1]]$depth)
    } else {
      return(tree$children[[1]]$depth + computeC(tree$children[[1]]$obsCount))
    }
    
  }
  #print("searching tree")
  feature <- tree$feature
  splitPoint <- tree$splitPoint
  if(row[1, feature] > splitPoint) {
    name <- paste(feature, toString(splitPoint), "greater", "TRUE", sep = " ")
  }
  else {
    name <- paste(feature, toString(splitPoint), "greater", "FALSE", sep = " ")
  }
  child <- tree$children[[name]]
  return(pathLength(child, row))
}

predict.isolationForestClass = function(modellObject, data, subsetSize = 256) {
  
  if(subsetSize > nrow(data)) {
    subsetSize = nrow(data)
    warning("Subset size bigger than number of data rows, 
            changing subset size to number of data rows")
  }
  
  predictions = list()
  n = subsetSize
  c_const = computeC(n)

  for(i in 1:nrow(data)) {
    averageDepth <- 0
    for(j in 4:length(modellObject$trees)) {
      depth <- pathLength(modellObject$trees[[j]], data[i,])
      averageDepth <- averageDepth + depth
    }
    averageDepth <- averageDepth / (length(modellObject$trees) - 3)
    
    if(2^(-averageDepth/c_const) > 0.5){
      tmpPrediction = 1
    } else {
      tmpPrediction = 0
    }
    # predictions <- append(predictions, 2^(-averageDepth/c_const))
    predictions <- append(predictions, tmpPrediction)
  }
  
  return(unlist(predictions))
}

computeC <- function(n) {
  return(2*(log(n - 1) + 0.5772156649) - (2*(n - 1)/n))
}


