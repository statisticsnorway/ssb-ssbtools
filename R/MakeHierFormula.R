
#' Finding hierarchical variable groups
#'
#' As HierarchicalGroups() with eachName = TRUE, but output belonging to same mainName are combined.
#'
#' @param x Matrix or data frame containing the variables
#'
#' @return  List containing the groups.
#' @keywords internal
#' @export
#' @author Øyvind Langsrud
#'
HierarchicalGroups2 <- function(x){
  a <- SSBtools::HierarchicalGroups(x,eachName = TRUE)
  b <- a[!duplicated(names(a))]
  for(i in 1:length(b)) b[[i]] = unique(unlist(a[names(a)==names(b)[i]]))
  b
}

#' Finding hierarchical variable groups
#'
#' As HierarchicalGroups() with eachName = FALSE, but output belonging to same mainName are combined.
#'
#' @param x Matrix or data frame containing the variables
#'
#' @return  List containing the groups.
#' @keywords internal
#' @export
#' @author Øyvind Langsrud
#'
HierarchicalGroups3 <- function(x){
  a <- SSBtools::HierarchicalGroups(x,eachName = FALSE)
  b <- a[!duplicated(names(a))]
  for(i in 1:length(b)) b[[i]] = unique(unlist(a[names(a)==names(b)[i]]))
  b
}


#' Make model formula from data taking into account hierarchical variables
#'
#' @encoding UTF8
#'
#' @param data data frame
#' @param hGroups Output from HierarchicalGroups2()
#' @param n Interaction level or 0 (all levels)
#' @param sim Include "~" when TRUE
#'
#' @return Formula as character string
#' @export
#' @author Øyvind Langsrud
#'
#' @examples
#' x <- SSBtoolsData("sprt_emp_withEU")[, -4]
#' MakeHierFormula(x)
#' MakeHierFormula(x, n = 2)
#' MakeHierFormula(x, n = 0)
MakeHierFormula <- function(data=NULL,hGroups=HierarchicalGroups2(data),n=length(hGroups),sim=TRUE){
  if(n==0)
    sepS = ":"
  else
    sepS = "*"
  n = min(n,length(hGroups))
  m = AllNCombinations(sapply(hGroups,length),n)
  n = NROW(m)
  k = NCOL(m)
  x0 = rep("",k)
  z=rep("",n)
  for(i in seq_len(n)){
    mi = m[i,]
    x = x0
    for(j in seq_len(k))
      if(mi[j]) x[j] = hGroups[[j]][mi[j]]
    x = x[mi!=0]
    s = x[1]
    for(t in seq_len(length(x)-1))
      s = paste(s,x[t+1],sep=sepS)
    z[i] = s
  }
  if(!sim)
    return(paste(z,collapse=" + "))
  paste("~",paste(z,collapse=" + "),sep=" ")
}





AllCombinations <- function(x = c(3,1,2),with0=TRUE,m=matrix(0,1,0)){
  if(!length(x)) return(m)
  nm <- NROW(m)
  AllCombinations(x[-1],with0,cbind(m[rep(seq_len(nm),x[1]+with0),],sort(rep(seq_len(x[1]+with0),nm))-with0))
}


AllNCombinations <- function(x = c(3,1,2),n=0,returnSorted=TRUE,returnList=FALSE){
  m = AllCombinations(x)
  rS = rowSums(m>0)
  if(n) return(m[rS==n, ,drop=FALSE])
  if(returnList){
    a <- vector("list",max(rS))
    for(i in seq_len(max(rS))) a[[i]] = m[rS==i, ,drop=FALSE]
    return(a)
  }
  m = m[!rS==0, ,drop=FALSE]
  rS = rS[!rS==0]
  if(returnSorted) return(m[order(rS), ,drop=FALSE])
  m
}

