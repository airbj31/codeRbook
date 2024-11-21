#' simple Exploratory data analysis on data frame
#'
#' show df's missing ratio, number of variables and column types..
#' @name describe_df
#' @author Byungju Kim (bjk@inu.ac.kr)
#' @param x a data.frame or tibble object.
#' @param desc column description file (under-development)
#' @param n_var number of values which used to distinguish factor and character.
#' @param show_freq calculate frequency
#' @param keepValues keep original numeric values for further analysis in codebook app.
#' @import tidyverse
#' @examples
#' describe_df(billboard)
#' @export
describe_df <- function(df,desc,n_var=7,show_freq=F,keepValues=F) {
#  stopifnot(exists(as.character(substitute(df))))
  stopifnot(inherits(df,what="data.frame"))
  x <- df
  dimension <- dim(x)
  row_FMISS <- x %>% is.na() %>% rowSums()/dimension[2]
  nRow    <- dimension[1]
  nColumn <- dimension[2]
  x[] <- lapply(x, function(x) {if (inherits(x, "POSIXt")) as.Date(x) else x}) ## change POSIXt to date class
  K<-tibble(columns=colnames(x),col_type=unlist(sapply(x,class)))
  K2<-lapply(x,BKtbl)
  K3<-lapply(K2,length)
  K4<-lapply(x,is.na)
  K4<-lapply(K4,sum)
  K5<-lapply(x, function(x) {x %in% c(0,1,NA)})
  K5<-lapply(K5, sum)
  if(isFALSE(show_freq)) {
    K<-K %>% mutate(n_uniq_Val=unlist(K3),nMISS=unlist(K4),nLogic=unlist(K5)) %>%
      mutate(col_type=case_when(col_type   == "list"     ~ "list",
                                nLogic     == nRow       ~ "logic",
                                n_uniq_Val <= n_var      ~ "factor",
                                TRUE       ~ col_type)) %>% select(-nLogic)
  } else {
    K<-K %>% mutate(n_uniq_Val=unlist(K3),nMISS=unlist(K4),fMISS=unlist(K4)/nRow,nLogic=unlist(K5)) %>%
      mutate(col_type=case_when(col_type   == "list"     ~ "list",
                                nLogic     == nRow      ~ "logic",
                                n_uniq_Val <= n_var     ~ "factor",
                                TRUE              ~ col_type)) %>% select(-nLogic)
  }


  numidx <- K %>% mutate(index=row.names(K)) %>% dplyr::filter(col_type %in% c("numeric","integer","double")) %>% pull(index) %>% as.numeric()
  catidx <- K %>% mutate(index=row.names(K)) %>% dplyr::filter(col_type %in% c("factor","logic")) %>% pull(index) %>% as.numeric()
#  others <- K %>% mutate(index=row.names(K)) %>% dplyr::filter(!col_type %in% c("factor","logic","numeric","integer","double")) %>% pull(index) %>% as.numeric()

  ## characters

  ## categorical value summary
  if(length(catidx)==0) {cvs=NA} else {
    cvs<-lapply(x[,as.numeric(catidx)],table,useNA="always")
    cvs<-lapply(cvs,tbl2df)
  }

  ## numeric value summary
  if(length(numidx)==0) {nvs=NA} else {
    nvs<-sapply(x[,as.numeric(numidx)],summary)

    if(is.matrix(nvs)) {nvs <- nvs %>% t() %>% as.data.frame()} else if(is.list(nvs)) { nvs <- nvs %>% t() %>%  bind_rows() }
    nvs <- nvs %>% mutate(column=K$columns[numidx],idx=numidx)

    if("NA's" %in% colnames(nvs)) {
      nvs <- nvs %>% as_tibble() %>% select(idx,column,"Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","NA's") %>% rename("QT1"="1st Qu.","QT3"="3rd Qu.","nMISS"="NA's")
    } else {
      nvs <- nvs %>% as_tibble() %>% select(idx,column,"Min.","1st Qu.","Median","Mean","3rd Qu.","Max.") %>% rename("QT1"="1st Qu.","QT3"="3rd Qu.") |> mutate(nMISS=0)
    }
    if(isTRUE(keepValues)) {
      CN<-colnames(x)[numidx]
      nvs <- nvs %>% left_join( x %>% select(all_of(CN)) %>% gather(column,distribution,all_of(CN)) %>% group_by(column) %>% summarize(distribution=list(distribution)))
    }
  }

  Desc <- structure(list(dimension=dimension,
                         df_summary=K,
                         obs_fMISS=row_FMISS,
#                        character_column_summary=character_columns,
                         category_column_summary=cvs,
                         numeric_column_summary=nvs,
                         col_type_string=paste(substr(K$col_type,1,1),collapse="")),
                    class = "df_desc")
  return(Desc)
}
