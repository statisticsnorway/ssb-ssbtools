#' Function that returns a dataset
#'
#' 
#' @param dataset Name of data set within the SSBtools package
#'
#' @return data frame
#' 
#' @details 
#' \strong{FIFA2018ABCD:} A hierarchy table based on
#' countries within groups A-D in the football championship, 2018 FIFA World Cup.
#' 
#' \strong{sprt_emp:} Employment in sport in thousand persons. Data from Eurostat database.
#'  
#' \strong{sprt_emp_geoHier:}  Country hierarchy for the employment in sport data.
#' 
#' \strong{sprt_emp_ageHier:}  Age hierarchy for the employment in sport data.
#' 
#' \strong{sprt_emp_withEU:} The data set sprt_emp extended with a EU variable.
#' 
#' \strong{sosialFiktiv, z1, z1w, z2, z2w, z3, z3w, z3wb:} See \code{\link{sosialFiktiv}}.
#' 
#' @export
#' @importFrom utils data
#' @author Øyvind Langsrud
#'
#' @examples
#' SSBtoolsData("FIFA2018ABCD")
#' SSBtoolsData("sprt_emp")
#' SSBtoolsData("sprt_emp_geoHier")
#' SSBtoolsData("sprt_emp_ageHier")
#' SSBtoolsData("sprt_emp_withEU")
#' SSBtoolsData("z1w")
SSBtoolsData <- function(dataset) {
  if (dataset == "FIFA2018ABCD") {
    return(data.frame(stringsAsFactors = FALSE, mapsFrom = c("Australia", "Iran", "Saudi Arabia", "Egypt", "Morocco", "Nigeria", "Argentina", "Peru", "Uruguay", "Croatia", "Denmark", "France", "Iceland", "Portugal", "Russia", "Spain", "Iceland", "Russia", "Russia", "Croatia", "Europe", "nonEU", "Europe", "nonSchengen"), 
                      mapsTo = c("Oceania", rep("Asia", 2), rep("Africa", 3), rep("America", 3), rep("Europe", 7), rep("nonEU", 2), rep("nonSchengen", 2), rep("EU", 2), rep("Schengen", 2)), 
                      sign = c(rep(1, 21), -1, 1, -1), level = c(rep(1, 20), c(rep(2, 4)))))
    
  }
  if (dataset == "sprt_emp") {
    # Employment in sport , _age http://ec.europa.eu/eurostat/web/sport/employment-in-sport/data/database Employment in sport in thousand persons
    ths <- c(51.1, 1.8, 7.3, 96.4, 1.6, 16.1, 55, 1.7, 6.9, 103.8, 1.7, 14.8, 63.6, 1.9, 10.5, 99.4, 1.6, 17.6, 66.9, 1.8, 11.6, 120.3, 1.5, 20.2, 63.4, 1.9, 14.2, 119.6, 1.6, 24.3, 69.1, 1.9, 12.7, 122.1, 1.9, 25.8)
    x <- data.frame(age = c(rep("Y15-29", 3), rep("Y30-64", 3)), geo = c("Spain", "Iceland", "Portugal"), year = as.character(rep(2011:2016, each = 6)), ths_per = ths, stringsAsFactors = FALSE)
    x <- x[x$year %in% as.character(2014:2016), ]
    rownames(x) <- NULL
    return(x)
  }
  if (dataset == "sprt_emp_ageHier") {
    return(data.frame(stringsAsFactors = FALSE, mapsFrom = c("Y15-29", "Y30-64"), mapsTo = "Y15-64", sign = 1, level = 1))
  }
  if (dataset == "sprt_emp_geoHier") {
    h <- SSBtoolsData("FIFA2018ABCD")
    h <- h[h$mapsFrom %in% c("Spain", "Iceland", "Portugal", "Europe", "nonEU") & h$mapsTo != "Schengen", ]
    rownames(h) <- NULL
    return(h)
  }
  if (dataset == "sprt_emp_withEU") {
    x <- SSBtoolsData("sprt_emp")
    x$eu = with(x, c("EU", "nonEU")[1+(geo=="Iceland")]) 
    return(x)
  }
  
  if (dataset %in% c(if (dataset %in% c("sosialFiktiv", "z1", "z1w", "z2", "z2w", "z3", "z3w", "z3wb")) {
    return(SSBtoolsData_(dataset))
  })) {
    return(SSBtoolsData_(dataset))
  }
  
  stop(paste("No data with dataset =", dataset))
}




# stackoverflow questions 30357330
pkgEnvSSBtoolsData <- new.env(parent=emptyenv())


SSBtoolsData_ <- function(dataset) {
  if (!exists(dataset, pkgEnvSSBtoolsData))
    data(list = dataset, package = "SSBtools", envir = pkgEnvSSBtoolsData)
  return(pkgEnvSSBtoolsData[[dataset]])
  return(NULL)
}


#' Fictitious datasets returned by SSBtoolsData()
#' 
#' The most comprehensive dataset, \code{sosialFiktiv}, contains three dimensions. The first dimension is 'region' which is grouped in two ways, 'fylke' and  'kostragr'. The other two are 'hovedint' and 'mnd'. In 'mnd2' two of the three categories in 'mnd' are merged.
#' The other datasets (\code{z1}, \code{z1w}, \code{z2}, \code{z2w}, \code{z3}, \code{z3w}, \code{z3wb}) are smaller subdatasets.
#' Datasets marked with '\code{w}' are unstacked and several variables are holding counts.
#'
#' @docType data
#' @keywords datasets internal
#' @name sosialFiktiv
NULL

#' @rdname sosialFiktiv
#' @name z1
NULL

#' @rdname sosialFiktiv
#' @name z1micro
NULL

#' @rdname sosialFiktiv
#' @name z1w
NULL

#' @rdname sosialFiktiv
#' @name z2
NULL

#' @rdname sosialFiktiv
#' @name z2w
NULL

#' @rdname sosialFiktiv
#' @name z3
NULL

#' @rdname sosialFiktiv
#' @name z3w
NULL

#' @rdname sosialFiktiv
#' @name z3wb
NULL
















