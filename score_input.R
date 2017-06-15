
calSimilarityDT <- function(x, updateProgress = NULL) {
  # initialize similarity matrix
  colCount <- ncol(x)
  cos <- matrix(0, nrow = colCount, ncol = colCount)
  rownames(cos) <- colnames(x)
  colnames(cos) <- colnames(x)
  
  if (is.function(updateProgress)) {
    for(i in 1:ncol(x)) {
      for(j in i:ncol(x)) {
        text <- paste0(colnames(i), " vs ", colnames(j))
        updateProgress(detail = text, deno = colCount)
        cos[i,j] = cosine_similarity(x[[i]],x[[j]])
        cos[j,i] = cos[i,j]  
      }
    }
    
  }
  return(cos)
}

cosine_similarity <- function(x,y){
  c <- crossprod(x,y)/sqrt(crossprod(x) * crossprod(y))
  return(c)
}

getCosineSimilarity <- function(dt, dt_exclusions, topN, updateProgress = NULL) {
  
  names(dt) = c('id', 'value', 'product_views', 'cart_adds', 'orders')
  num_class <- length(unique(dt$value))
  
  if(num_class < 3) return(NULL)
  
  #Remove exclusion criteria classes
  if(!is.null(dt_exclusions())) {
    dt_exclusions <- data.table(dt_exclusions())
    names(dt_exclusions) <- c('value')
    dt$exclude <- match(dt$value,dt_exclusions$value)
    dt <- dt[is.na(dt$exclude),]  
  }
  
  dt <- dt[, list (
    value
    ,product_views
    ,cart_adds
    ,orders
    , min_pv = min(product_views)
    , max_pv = max(product_views)
    , min_ca = min(cart_adds)
    , max_ca = max(cart_adds)
    , min_or = min(orders)
    , max_or = max(orders)
  )
  , by=c('id')]
  
  dt <- dt[, list (
    value
    , pv_score = ifelse(max_pv == 0, 0, ifelse(min_pv == max_pv, 0, (product_views - min_pv)/(max_pv - min_pv)))
    , ca_score = ifelse(max_ca == 0, 0, ifelse(min_ca == max_ca, 0, (cart_adds - min_ca)/(max_ca - min_ca)))
    , or_score = ifelse(max_or == 0, 0, ifelse(min_or == max_or, 0, (orders - min_or)/(max_or - min_or)))
  )
  , by=c('id')]
  
  # Add Cust Class Score as sum of pv_score, ca_score & or_score
  dt[,score:=pv_score + ca_score + or_score]
  
  # Drop columns from df_res
  drop_cols <- c("pv_score", "ca_score", "or_score")
  dt[, drop_cols] = NULL
  
  mt <- data.table::dcast(dt, id ~ value, fun.aggregate=sum, value.var = 'score')
  
  mt$id = NULL
  mt <- calSimilarityDT(mt, updateProgress)
  
  sim_r <- as.data.table(as.table(mt))
  names(sim_r) <- c("similar_value","value","score")
  
  sim_r <- sim_r[order(value,-score),head(.SD,topN),by=(value)]
  sim_r <- sim_r[sim_r$value != sim_r$similar_value]
  
  return(sim_r)
  #sim_r = sim_r[order(value,-score),head(.SD,topN),by=(value)]
}