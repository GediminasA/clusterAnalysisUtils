
#' @title clusterDataParser
#'
#' @description
#' `clusterDataParser` reads in UC format clustering infor and makes convenient
#' data strucrures for cluster merging and representative - cluster members 
#' mapping. 
#'
#' @importFrom R6 R6Class
#' @importFrom data.table  fread fwrite data.table as.data.table
#' @importFrom dplyr filter select  left_join bind_rows bind_cols 
#' @importFrom dtplyr lazy_dt
#' @export 
clusterDataParser <- R6Class("clustersclusterDataParser", 
    public = list(
      #' @field df (`data.table`)\cr
      #' Stores clustering data - two columns - representative and members
      df = NULL,
      
      #' @field seq_rep (`list`)\cr
      #' names list maping a seqence ID to the reresentative  ID
      seq_rep = NULL,
      
      #' @description
      #' Creates a new instance of this [R6][R6::R6Class] class.
      #'
      #' @param fileName (`character(1)`)\cr
      #'   An a clustering file file (like those produced by Vsearch tools).
      #' Read in the UC file and parse the produced df
      #' 
      #' @param remove_sizes (`bool(1)`)\cr
      #' Removes from id part with size info ex. "<id>;size=12" 
      #' 
      #' @param format (`character(1)`)\cr
      #'  Should indicate the type. Defaults to UC
      #' 
      initialize = function(fileName,remove_sizes=T, format="UC") {    
        private$.fileName = fileName
        private$.remove_sizes = remove_sizes
        if (format =="UC") {
          private$.parseUC()
        }
        if (format =="MT") {
          private$.parseMT()
        }
        private$.get_seq_rep()
      },
      
      #' @description
      #' Returns a list of cluster menbers IDs given the  representative ID.
      #' @param repr_id (`character(1)`)\cr
      #'   A ccluster representative sequence id
      get_members = function(repr_id){
          out <- self$df %>%
              lazy_dt() %>%
              filter(Cluster == repr_id) %>%
              as.data.table()
          return(unlist(out$Member))
      }
    ),
    
      private = list(
        #' @field .fileName (`character(1)`)\cr
        #'   An a clustering file file (like those produced by Vsearch tools).
        .fileName = NULL,
  
        #' @field .remove_sizes (`bool(1)`)\cr
        #' Is it needed to removes from id part the info ex. "<id>;size=12"
        .remove_sizes = NULL,
        
        #' @description
        #' Parse data frame and get member to cluster map
        .get_seq_rep = function() {
          df <- self$df %>%
            as.data.table()
          out <- df$Cluster
          names(out) <- df$Member
          self$seq_rep <- out
        },
        
        #' @description 
        #' Parse UC format
        .parseUC = function() {    
          df <- fread(private$.fileName,header=F)
          
          df1 <- df %>%  
            lazy_dt() %>%
            filter(V1 == "S") %>%
            select(Member = V9, Cluster = V10) %>%
            as.data.table()
          
          df2 <- df %>%
            lazy_dt() %>%
            filter(V1 == "H") %>%
            select(Member = V9, Cluster = V10) %>%
            as.data.table()
          df1$Cluster <- df1$Member
          self$df <- data.table(bind_rows(df1,df2))
          if (private$.remove_sizes){
            self$df$Cluster <- unlist(lapply(self$df$Cluster,function(x){
              out = strsplit(x,split=";",fixed = T)[[1]][[1]]
              return(out)
            }))
            self$df$Member <- unlist(lapply(self$df$Member,function(x){
              out = strsplit(x,split=";",fixed = T)[[1]][[1]]
              return(out)
            })) 
          }
        },
        
        #' @description 
        #' Parse Mothur format - as outputted by swarm with option '-r' 
        .parseMT = function() {
          dd <- fread(private$.fileName,header=F, sep = "\t") %>% 
            select(-V1,-V2) %>%
            as.data.frame()
          dd <- unlist(dd[1,], use.names = F)
          
          clusters <- list()
          members <- list()
          ct = 0
          for (d in dd) {
            d2 = unlist(strsplit(d,split = ",",fixed=T)[[1]])
            if (private$.remove_sizes){
              d2 = unlist(lapply(d2,function(s) {
                strsplit(s,split=";",fixed=T)[[1]][[1]]
              }))
            }
            Cluster = d2[[1]]
            for(Member in d2) {
              ct = ct + 1
              clusters[[ct]] <- Cluster
              members[[ct]] <- Member
            }
          }
          
          df = data.table(Member = unlist(members),Cluster = unlist(clusters))
          self$df <- df
          
        }
    )
)

