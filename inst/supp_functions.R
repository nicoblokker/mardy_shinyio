coloring <- function(g, data = data){
          V(g)$party <- NA
          for(i in 1:igraph::vcount(g)){
                    idx <- which(data$name == V(g)$name[i])
                    if(length(idx) > 0){
                              V(g)$party[i] <- unique(unlist(data$parties[idx]))[1]
                    } else {
                              V(g)$party[i] <- "claim"
                    }
          }
          V(g)$party[is.na(V(g)$party)] <- "without"
          party_colors <- c("AfD" = "darkblue",
                            "CDU" = "black",
                            "CSU" = "deepskyblue",
                            "SPD" = "red",
                            "Green" = "forestgreen",
                            "Left" = "darkorchid",
                            "FDP" = "yellow",
                            "NPD" = "orange",
                            "without" = "slategrey",
                            "claim" = "grey")
          V(g)$color <- unname(party_colors[V(g)$party])
          V(g)$color
}


two2one <- function(df, ret = "graph", iso = TRUE){
          df[, 3] <- ifelse(df[, 3] < 0, -1, 1)
          colnames(df) <- c("from","to","weight")
          mpos <- mneg <- m <- stats::xtabs(weight ~ from + to, data = df)
          mpos[mpos < 0] <- 0
          mneg[mneg > 0] <- 0
          prj <-  function(matrix1, matrix2 = NULL, iso = iso){
                    if(is.null(matrix2) & sum(matrix1) > 0){
                              matrix_trans <- tcrossprod(matrix1)
                              comment(matrix_trans) <- "pos"
                    }
                    if(is.null(matrix2) & sum(matrix1) <= 0){
                              matrix_trans <- tcrossprod(matrix1)
                              comment(matrix_trans) <- "neg"
                    }
                    if(!is.null(matrix2)){
                              matrix_trans <- (tcrossprod(matrix1, matrix2) + tcrossprod(matrix2, matrix1))*-1
                              comment(matrix_trans) <- "conf"
                    }
                    graph <- igraph::graph.adjacency(matrix_trans, diag = iso, weighted = T, mode = "upper")
                    E(graph)$coalition <- attributes(matrix_trans)$comment
                    E(graph)$cpos <- c("pos" = 1, "neg" = -1, "conf" = 2)[E(graph)$coalition]
                    E(graph)$color <- c("pos" = "green", "neg" = "red", "conf" = "grey")[E(graph)$coalition]
                    projection <- igraph::get.data.frame(graph)
                    return(projection)
          }
          elist <- purrr::map2_df(list(mpos, mneg, mpos), list(NULL, NULL, mneg), prj, iso = iso)
          if(ret == "graph"){
                    graph_prj <- graph_from_data_frame(elist, directed = F)
                    graph_prj <- simplify(graph_prj, remove.multiple = F, remove.loops = T)
                    return(graph_prj)
          } else {
                    return(elist)
          }
}


lookup_codes <- function(code, language = "english"){
          find_c <- function(code){
                    label <- migration_codebook_english$description[which(migration_codebook_english$sub == code)]
                    if(length(label) == 1){
                              names(label) <- code
                              return(label)
                    } else {
                              label <- NA
                              names(label) <- code
                              warning(paste0("Category '", code, "' does not exist, returning NA"), call. = FALSE)
                              return(label)
                    }
          }
          sapply(code, find_c, USE.NAMES = FALSE)
}
