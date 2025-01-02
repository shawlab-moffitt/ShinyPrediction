









loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


date_to_gene <- function(vec,mouse = FALSE) {
  if (is.null(vec)) exit("Please provide vector argument")
  if (!mouse) {
    vec <- gsub('(\\d+)-(Mar)','MARCH\\1',vec)
    vec <- gsub('(\\d+)-(Sep)','SEPT\\1',vec)
    vec <- gsub('(\\d+)-(Dec)','DEC\\1',vec)
  } else {
    vec <- gsub('(\\d+)-(Mar)','March\\1',vec)
    vec <- gsub('(\\d+)-(Sep)','Sept\\1',vec)
    vec <- gsub('(\\d+)-(Dec)','Dec\\1',vec)
  }
  return(vec)
}

detect_species <- function(genes) {
  # check the capitalization pattern
  is_human_gene <- function(gene) {
    return(grepl("^[A-Z]+$", gene))
  }
  is_mouse_gene <- function(gene) {
    return(grepl("^[A-Z][a-z]*$", gene))
  }
  # Count the number of human and mouse gene patterns
  human_count <- sum(sapply(genes, is_human_gene))
  mouse_count <- sum(sapply(genes, is_mouse_gene))
  # Determine the majority match
  if (human_count > mouse_count) {
    return("human")
  } else if (mouse_count > human_count) {
    return("mouse")
  } else {
    return("undetermined") # If counts are equal or if there are no matches
  }
}

filter_expression_matrix <- function(data,criteria,proportion) {
  # Extract gene symbols
  gene_symbols <- data[, 1]
  # Extract the numerical data part of the matrix
  numeric_data <- data[, -1]
  # Calculate the number of columns with numerical data
  num_cols <- ncol(numeric_data)
  # Determine the threshold for the number of columns with values < 1
  threshold <- (proportion/100) * num_cols
  # Filter the rows based on the threshold
  keep_rows <- apply(numeric_data, 1, function(row) {
    sum(row < criteria) <= threshold
  })
  # Combine the gene symbols with the filtered numeric data
  filtered_matrix <- cbind(gene_symbols[keep_rows], numeric_data[keep_rows, ])
  colnames(filtered_matrix)[1] <- colnames(data)[1]
  return(filtered_matrix)
}






#lrClusterPlot <- function(data,kmax = 11,nstart = 10) {
#  wss <- sapply(1:kmax,function(k){kmeans(data,k,nstart=nstart)$tot.withinss})
#  plot(1:kmax,wss,typ="b",pch=19,frame=F,xlab="number of clusters",
#       ylab="Total with clusters sum of squares")
#  p <- recordPlot()
#  dev.off()
#  return(p)
#}

lrFeaturePredict <- function(data_train = NULL,meta_train = NULL,data_test = NULL,meta_test = NULL,
                             feature = NULL,reference = NULL,model = NULL,usek = TRUE,k = 7,kmax = 11,nstart = 10) {

  require(nnet)
  res <- list()
  if (is.null(k)) k <- length(unique(c(meta_train[,feature],meta_test[,feature])))

  if (!is.null(data_test) & is.null(meta_test)) {
    meta_test <- data.frame(SampleID = colnames(data_test),
                            Feature = NA)
    rownames(meta_test) <- meta_test[,1]
    if (!is.null(meta_train)) {
      colnames(meta_test)[1] <- colnames(meta_train)[1]
    }
    colnames(meta_test)[2] <- feature
  }

  if (!is.null(meta_train)) {meta_train[which(is.na(meta_train[,feature])),feature] <- "NA"}
  #if (!is.null(meta_test)) {meta_test[which(is.na(meta_test[,feature])),feature] <- "NA"}
  if (!feature %in% colnames(meta_test)) {meta_test[,feature] <- "NA"}
  rownames(meta_train) <- meta_train[,1]
  rownames(meta_test) <- meta_test[,1]
  res[["data"]][["data_train"]] <- data_train
  res[["data"]][["meta_train"]] <- meta_train
  res[["data"]][["data_test"]] <- data_test
  res[["data"]][["meta_test"]] <- meta_test
  res[["data"]][["feature"]] <- feature
  res[["data"]][["reference"]] <- reference

  if (!is.null(model)) {
    test <- model
    model_fitted <- fitted(test)
    CategoricalSubgroup <- test$lev
    reference <- test$lev[1]
    if (length(unique(CategoricalSubgroup)) == 2) {
      model_fitted <- as.data.frame(model_fitted)
      model_fitted[, as.character(reference)] <- 1-model_fitted[,1]
      colnames(model_fitted)[1] <- unique(CategoricalSubgroup)[which(unique(CategoricalSubgroup) != reference)]
    }
    res[["Model Fitted"]] <- as.data.frame(model_fitted)

    #members <- seq(length(test$lev))
    #names(members) <- test$lev
    #k <- length(members)

    members <- seq(length(test$coefnames[-1]))
    names(members) <- test$coefnames[-1]
    k <- length(members)
    res[["clusters"]] <- members

    groupCenterTest <- NULL
    for (j in 1:k){
      idx<-which(members==j)
      z<-data_test[idx,]
      if (length(idx) == 1) {
        meanScore = z
      } else {
        meanScore <- apply(z,2,mean)
      }
      groupCenterTest<-rbind(groupCenterTest,meanScore)
    }
    rownames(groupCenterTest) <- paste("Group",1:k,sep="")

    groupCenterTestT <- t(groupCenterTest)
    predicted_full <- as.data.frame(predict(test,groupCenterTestT,type="probs"))

    CategoricalSubgroup <- test$lev
    reference <- test$lev[1]
    if (length(unique(CategoricalSubgroup)) == 2) {
      predicted_full[, as.character(reference)] <- 1-predicted_full[,1]
      colnames(predicted_full)[1] <- unique(CategoricalSubgroup)[which(unique(CategoricalSubgroup) != reference)]
    }
    res[["Group Center Testing"]] <- as.data.frame(groupCenterTestT)

  } else {
    totalTest <- ncol(data_train) + ncol(data_test)
    meta_train <- meta_train[order(meta_train[,feature]),]

    data_train <- data_train[,rownames(meta_train)]
    CategoricalSubgroup <- meta_train[colnames(data_train),feature]
    totalTrain <- ncol(data_train)

    data_full <- cbind(data_train,cbind(data_train,data_test))

    geneSet <- rownames(data_train)


    if (usek) {
      result <- kmeans(data_train,k,nstart=nstart)
      members <- result$cluster
    } else {
      members <- seq(nrow(data_train))
      names(members) <- rownames(data_train)
      k <- length(members)
    }
    res[["clusters"]] <- members

    groupCenter <- NULL
    for (j in 1:k){
      idx<-which(members==j)
      z<-data_train[idx,]
      if (length(idx) == 1) {
        meanScore = z
      } else {
        meanScore <- apply(z,2,mean)
      }
      groupCenter<-rbind(groupCenter,meanScore)
    }
    rownames(groupCenter) <- paste("Group",1:k,sep="")

    groupCenterT <- t(groupCenter)
    groupCenterT <- data.frame(CategoricalSubgroup,groupCenterT)
    colnames(groupCenterT)[1] <- feature
    groupCenterT[,feature] <- factor(groupCenterT[,feature])
    groupCenterT[,feature] <- relevel(groupCenterT[,feature],ref=reference)
    res[["Group Center Training"]] <- groupCenterT

    feature <- sprintf(ifelse((grepl(" ", feature) | !is.na(suppressWarnings(as.numeric(substring(feature, 1, 1))))), "`%s`", "%s"), feature)
    test <- nnet::multinom(as.formula(paste(feature,"~ .")),data=groupCenterT)

    #test <- nnet::multinom(CategoricalSubgroup ~ .,data=groupCenterT)
    res[["Regression Model"]] <- test
    model_fitted <- fitted(test)
    if (length(unique(CategoricalSubgroup)) == 2) {
      model_fitted <- as.data.frame(model_fitted)
      model_fitted[, as.character(reference)] <- 1-model_fitted[,1]
      colnames(model_fitted)[1] <- unique(CategoricalSubgroup)[which(unique(CategoricalSubgroup) != reference)]
    }
    res[["Model Fitted"]] <- as.data.frame(model_fitted)

    groupCenterTest <- NULL
    for (j in 1:k){
      idx<-which(members==j)
      z<-data_test[idx,]
      if (length(idx) == 1) {
        meanScore = z
      } else {
        meanScore <- apply(z,2,mean)
      }
      groupCenterTest<-rbind(groupCenterTest,meanScore)
    }
    rownames(groupCenterTest) <- paste("Group",1:k,sep="")

    groupCenterTestT <- t(groupCenterTest)
    res[["Group Center Testing"]] <- as.data.frame(groupCenterTestT)

    data_test_full <- data_full[,(totalTrain+1):(totalTrain+totalTest)]

    groupMean_full <- NULL
    for (j in 1:k){
      idx<-which(members==j)
      z_full <- data_test_full[idx,]
      if (length(idx) == 1) {
        meanScore_full = z_full
      } else {
        meanScore_full <- apply(z_full,2,mean)
      }
      groupMean_full <- rbind(groupMean_full,meanScore_full)

    }
    rownames(groupMean_full)<-paste("Group",1:k,sep="")

    groupMean_fullT <- t(groupMean_full)

    predicted_full <- as.data.frame(predict(test,groupMean_fullT,type="probs"))
    if (length(unique(CategoricalSubgroup)) == 2) {
      predicted_full[, as.character(reference)] <- 1-predicted_full[,1]
      colnames(predicted_full)[1] <- unique(CategoricalSubgroup)[which(unique(CategoricalSubgroup) != reference)]
    }

    groupMean_fullT <- as.data.frame(groupMean_fullT)
    groupMean_fullT$Training_Testing <- ifelse(rownames(groupMean_fullT) %in% colnames(data_train),"Training","Testing")
    groupMean_fullT <- cbind(Training_Testing = groupMean_fullT$Training_Testing,
                             groupMean_fullT[,c(1:(ncol(groupMean_fullT)-1))])
    res[["Group Mean Train and Test"]] <- groupMean_fullT
  }


  Predicted_full_col <- apply(predicted_full,1,function(x) {
    which(x == max(x,na.rm = T))
  })
  predicted_full_cols <- seq(ncol(predicted_full))
  names(predicted_full_cols) <- colnames(predicted_full)
  predicted_full$Predicted_col <- unname(unlist(Predicted_full_col))
  predicted_full$Prediction <- names(predicted_full_cols)[match(predicted_full$Predicted_col, predicted_full_cols)]
  predicted_full <- predicted_full[,which(colnames(predicted_full) != "Predicted_col")]
  if (!is.null(meta_train)) {
    predicted_full[,"SampleID"] <- rownames(predicted_full)
    colnames(meta_train)[1] <- "SampleID"
    colnames(meta_test)[1] <- "SampleID"
    predicted_full <- merge(meta_train[,c(colnames(meta_train)[1],feature)],predicted_full,all.y = T)
    predicted_full <- merge(meta_test[,c(colnames(meta_test)[1],feature)],predicted_full,all.y = T)
    predicted_full <- cbind(predicted_full[,c(1,2)],
                            Prediction = predicted_full$Prediction,
                            predicted_full[,c(3:(ncol(predicted_full)-1))])
    rownames(predicted_full) <- predicted_full[,1]

    predicted_training <- predicted_full[colnames(data_train),]
    res[["Prediction Training"]] <- predicted_training

    predicted_testing <- predicted_full[colnames(data_test),]
    res[["Prediction Testing"]] <- predicted_testing

    predicted_full$Training_Testing <- ifelse(predicted_full[,1] %in% colnames(data_train),"Training","Testing")
    predicted_full <- cbind(predicted_full[,1, drop = F],
                            Training_Testing = predicted_full$Training_Testing,
                            predicted_full[,c(2:(ncol(predicted_full)-1))])
    res[["Prediction Training and Testing"]] <- predicted_full
  } else {
    predicted_full[,colnames(meta_test)[1]] <- rownames(predicted_full)
    predicted_full <- merge(meta_test[,c(colnames(meta_test)[1],feature)],predicted_full,all.y = T)
    predicted_full <- cbind(predicted_full[,c(1,2)],
                            Prediction = predicted_full$Prediction,
                            predicted_full[,c(3:(ncol(predicted_full)-1))])
    rownames(predicted_full) <- predicted_full[,1]

    predicted_testing <- predicted_full[colnames(data_test),]
    res[["Prediction Testing"]] <- predicted_testing
  }

  return(res)

}



multi_class_rates <- function(confusion_matrix) {
  true_positives  <- diag(confusion_matrix)
  false_positives <- colSums(confusion_matrix) - true_positives
  false_negatives <- rowSums(confusion_matrix) - true_positives
  true_negatives  <- sum(confusion_matrix) - true_positives -
    false_positives - false_negatives
  return(data.frame(true_positives, false_positives, true_negatives,
                    false_negatives, row.names = names(true_positives)))
}



make_boxplot <- function(data = NULL,group = NULL,feature = NULL,
                         title = "Box Plot", x_title = NULL, y_title = NULL,
                         title_font = 18, x_title_font = 16, y_title_font = 16, x_tick_font = 14, y_tick_font = 14,
                         y_lim = NULL, violin = FALSE, flip = FALSE, stat = "none", dots = TRUE, dot_size = 2,
                         axis_orient = 45, hjust_orient = 1, vjust_orient = 1, x_order = "Ascending") {


  if (x_order == "Descending"){
    barp <- ggplot(data, aes(x=reorder(!!sym(group),-!!sym(feature), FUN = median),y=!!sym(feature), fill=!!sym(group)))
    plotdf_dots <- data
    plotdf_dots$Group <- reorder(plotdf_dots[,group],-plotdf_dots[,feature], FUN = median)
    plotdf_dots$xj <- jitter(as.numeric(factor(plotdf_dots$Group)))
  }
  if (x_order == "Ascending"){
    barp <- ggplot(data, aes(x=reorder(!!sym(group),!!sym(feature), FUN = median),y=!!sym(feature), fill=!!sym(group)))
    plotdf_dots <- data
    plotdf_dots$Group <- reorder(plotdf_dots[,group],plotdf_dots[,feature], FUN = median)
    plotdf_dots$xj <- jitter(as.numeric(factor(plotdf_dots$Group)))
  }
  if (x_order == "Not Specificed"){
    barp <- ggplot(data, aes(x=!!sym(group),y=!!sym(feature), fill=!!sym(group)))
    plotdf_dots <- data
    plotdf_dots$xj <- jitter(as.numeric(factor(plotdf_dots$Group)))
  }
  if (violin) {
    barp <- barp + geom_violin() +
      stat_summary(fun=median, geom="crossbar", width=0.5, color="black")
  } else {
    barp <- barp + geom_boxplot(width = 0.5, lwd = 1)
  }
  barp <- barp +
    theme_minimal() +
    labs(title = title,
         x = x_title, y = y_title) +
    theme(axis.text.x = element_text(size = x_tick_font,angle = axis_orient, hjust = hjust_orient, vjust = vjust_orient),
          axis.title.x = element_text(size = x_title_font),
          axis.text.y = element_text(size = y_tick_font),
          axis.title.y = element_text(size = y_title_font),
          title = element_text(size = title_font),
          legend.position = "none")
  if (!is.null(y_lim)) {
    barp <- barp +
      ylim(paste0(as.numeric(y_lim[1]),as.numeric(y_lim[2])))
  }
  if (stat != "none") {
    barp <- barp + ggpubr::stat_compare_means(method = stat)
  }
  if (dots) {
    barp <- barp + geom_point(data = plotdf_dots, aes(x=xj), col="grey14", size=dot_size)
  }
  if (flip) {
    barp <- barp + coord_flip()
  }
  return(barp)
}































