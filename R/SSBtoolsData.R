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
#' \strong{sp_emp_withEU:} As `sprt_emp_withEU`, but coded differently.
#' 
#' \strong{example1} Example data similar to `sp_emp_withEU`.
#' 
#' \strong{magnitude1:} Example data for magnitude tabulation. Same countries as above.  
#' 
#' \strong{my_km2:} Fictitious grid data. 
#' 
#' \strong{mun_accidents:} Fictitious traffic accident by municipality data.
#' 
#' \strong{sosialFiktiv, z1, z1w, z2, z2w, z3, z3w, z3wb:} See \code{\link{sosialFiktiv}}.
#' 
#' \strong{d4, d1, d1w, d2, d2w, d3, d3w, d3wb:} English translation of the datasets above.
#' 
#' \strong{d2s, d2ws:} `d2` and `d2w` modified to smaller/easier data.
#' 
#' \strong{power10to1, power10to2, \eqn{\ldots}:} `power10to`\eqn{i} is hierarchical data with \eqn{10^i} rows and \eqn{2*i} columns. 
#'         Tip: Try `FindDimLists(SSBtoolsData("power10to3"))`  
#' 
#' @export
#' @importFrom utils data
#' @author Øyvind Langsrud and Daniel Lupp
#'
#' @examples
#' SSBtoolsData("FIFA2018ABCD")
#' SSBtoolsData("sprt_emp")
#' SSBtoolsData("sprt_emp_geoHier")
#' SSBtoolsData("sprt_emp_ageHier")
#' SSBtoolsData("sprt_emp_withEU")
#' SSBtoolsData("d1w")
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
  if (dataset == "sp_emp_withEU") {
    x <- SSBtoolsData("sprt_emp_withEU")[,c(1,2,5,3,4)] 
    names(x)[5] <- "value"
    x$age[x$age == "Y15-29"] <- "young"
    x$age[x$age == "Y30-64"] <- "old"
    return(x)
  }
   
  if (dataset == "my_km2") { # my_km² not allowed, Portable packages must use only ASCII characters in their R code,
    x <- data.frame(Square1000m  = c(rep("my_km",10), rep("another_km",8)),
                    Square250m   = c(rep("500_000",3), rep("750_250",4), rep("750_500",3), rep("another_500_000",4), rep("another_750_250",4)),
                    Municipality = c(rep("Oslo",3), rep("Nittedal",6), rep("Oslo",9)),
                    Age = c("15_to_65", "15_to_65", "under_15", "15_to_65", "15_to_65", "under_15", "under_15", "65_and_over", "65_and_over", "15_to_65", 
                            "15_to_65", "15_to_65", "under_15", "under_15", "15_to_65", "15_to_65", "65_and_over", "65_and_over"),
                    Sex = c("female", "male", "male", "female", "male", "female", "male", "female", "male", "female", "female", "male", "female", "male", 
                            "female", "male", "female", "male"),
                    freq = c(3L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 26L, 21L, 31L, 34L, 2L, 1L, 10L, 13L))
    return(x)
  }
  
  if (dataset == "mun_accidents") {
    mun <- c("k1", "k2", "k3", "k4", "k5", "k6")
    inj <- c("serious", "light", "none", "unknown")
    x <- expand.grid(mun, inj, stringsAsFactors = FALSE)
    names(x) <- c("mun", "inj")
    x$freq <- c(4L, 5L, 3L, 4L, 1L, 6L, 0L, 0L, 2L, 1L, 0L, 0L, 0L, 1L, 1L, 
                4L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
    return(x)
  }

  socialDataset <- c(d4 = "sosialFiktiv", 
                      d1 = "z1", d1w = "z1w", 
                      d2 = "z2", d2w = "z2w", 
                      d3 = "z3", d3w = "z3w", d3wb = "z3wb")
  
  if (dataset %in% socialDataset) {
    return(SSBtoolsData_(dataset))
  }

  if (dataset %in% names(socialDataset)) {
    z <- SSBtoolsData_(socialDataset[dataset])
    if("hovedint" %in% names(z)){
      z$hovedint <- SocialReCode(z$hovedint)
    }
    names(z) <- SocialReCode(names(z)) 
    for(i in which(sapply(z, is.numeric))){
      z[[i]] <- as.integer(z[[i]])
    }
    row.names(z) <- NULL
    return(z)
  }

  if (dataset == "d2s") {
    return(Make_d2small(SSBtoolsData("d2")))
  }
  if (dataset == "d2ws") {
    return(Make_d2small(SSBtoolsData("d2w"), freq = "other"))
  }
  
  if (dataset == "magnitude1") {
    q <- data.frame(geo = rep(c("Iceland", "Portugal", "Spain"), each = 4), 
                    eu = "EU", 
                    sector4 = c("Agriculture", "Entertainment", "Governmental", "Industry"), 
                    sector2 = "private")[-c(1, 3), ]
    q$eu[1:2] <- "nonEU"
    q$sector2[c(5, 9)] <- "public"
    q <- q[c(2, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 8, 9, 9, 10), ]
    q$company <- c("B", "B", "C", "D", "A", "B", "B", "D", "B", "D", 
                   "B", "D", "A", "B", "A", "B", "C", "C", "D", "C")
    
    q$value <- c(9.6, 16.8, 8.8, 1.9, 75.9, 24.5, 7.1, 2.3, 21.6, 2, 
                 25.7, 3.4, 96.6, 43.2, 77.4, 11.5, 16.4, 6.5, 2.7, 8.4)
    q <- SortRows(q[c(3, 4, 1, 2, 5, 6)])
    rownames(q) <- NULL
    return(q)
  }
  
  if (dataset == "example1") {
    q <- SSBtoolsData("sprt_emp_withEU")[, c(1, 2, 5, 3)]
    q$age[q$age == "Y15-29"] <- "young"
    q$age[q$age == "Y30-64"] <- "old"
    q$freq <- c(5, 2, 0, 6, 3, 4, 5, 0, 0, 6, 3, 4, 7, 1, 1, 5, 4, 3)
    rownames(q) <- NULL
    return(q)
  }
  
  if (substr(dataset, 1, 9) == "power10to") {
    n <- as.integer(substr(dataset, 10, 20))
    if (n > 26) {
      stop("Not enough letters for coding")
    }
    return(Power10toN(n))
  }
  
  stop(paste("No data with dataset =", dataset))
}

P10 <- function(nam = c("a", "A")) {
  z <- data.frame(1:10, c(100, 100, 200, 200, 200, rep(300, 5)))
  names(z) <- nam
  for (i in seq_along(z)) {
    z[[i]] <- paste0(names(z)[i], z[[i]])
  }
  z
}

Power10toN <- function(n) {
  z <- P10()
  for (i in SeqInc(2, n)) {
    z <- CrossCodeFrames(z, P10(c(letters[i], LETTERS[i])))
  }
  z
}


Make_d2small <- function(data, freq = "freq") {  # Modify to smaller/easier data
  sreg <- c("A", "B", "C", "I", "J", "K")
  data <- data[data$region %in% sreg, ]
  names(data)[names(data) == "k_group"] <- "size"
  data$size[data$size == "300"] <- "BIG"
  data$size[data$size == "400"] <- "small"
  data$county[data$county == 1] <- "county-1"
  data$county[data$county == 4] <- "county-2"
  data$county[data$county == 5] <- "county-2"
  data$county[data$county == 10] <- "county-3"
  data$region[data$region == "I"] <- "D"
  data$region[data$region == "J"] <- "E"
  data$region[data$region == "K"] <- "F"
  data[[freq]][1:2] <- 2:3
  rownames(data) <- NULL
  data
}


#Based on Kostra::CharacterReCode
SocialReCode <- function(x,oldLevels = names(newLevels), 
                            newLevels = 
  # # generated by
  # z3 <- SSBtoolsData("z3")
  # z3w <- SSBtoolsData("z3w")
  # z3wb <- SSBtoolsData("z3wb")
  # a <- unique(c(names(z3), z3$hovedint, names(z3w), names(z3wb)))
  # names(a) <- a
  # # See https://www.ssb.no/en/statbank/table/12203
  # a <- gsub("fylke", "county", a)
  # a <- gsub("kostragr", "k_group", a)  # KOSTRA-groups
  # a <- gsub("hovedint", "main_income", a)  # main source of income
  # a <- gsub("mnd", "months", a)
  # a <- gsub("ant", "freq", a)
  # a <- gsub("annet", "other", a)
  # a <- gsub("arbeid", "wages", a)  # Norwegian: arbeidsinntekt 
  # a <- gsub("soshjelp", "assistance", a)  # social assistance
  # a <- gsub("trygd", "pensions", a)  # Norwegian: trygd/pensjon, English: just pensions
  # dput(a)
  c(region = "region", fylke = "county", kostragr = "k_group", 
    hovedint = "main_income", mnd = "months", mnd2 = "months2", ant = "freq", 
    annet = "other", arbeid = "wages", soshjelp = "assistance", trygd = "pensions", 
    annet_m01m05 = "other_m01m05", arbeid_m01m05 = "wages_m01m05", 
    soshjelp_m01m05 = "assistance_m01m05", trygd_m01m05 = "pensions_m01m05", 
    annet_m10m12 = "other_m10m12", arbeid_m10m12 = "wages_m10m12", 
    soshjelp_m10m12 = "assistance_m10m12", trygd_m10m12 = "pensions_m10m12", 
    annet_m06m09 = "other_m06m09", arbeid_m06m09 = "wages_m06m09", 
    soshjelp_m06m09 = "assistance_m06m09", trygd_m06m09 = "pensions_m06m09", 
    annet_m01m05_M01M05 = "other_m01m05_M01M05", arbeid_m01m05_M01M05 = "wages_m01m05_M01M05", 
    soshjelp_m01m05_M01M05 = "assistance_m01m05_M01M05", trygd_m01m05_M01M05 = "pensions_m01m05_M01M05", 
    annet_m10m12_M06M12 = "other_m10m12_M06M12", arbeid_m10m12_M06M12 = "wages_m10m12_M06M12", 
    soshjelp_m10m12_M06M12 = "assistance_m10m12_M06M12", trygd_m10m12_M06M12 = "pensions_m10m12_M06M12", 
    annet_m06m09_M06M12 = "other_m06m09_M06M12", arbeid_m06m09_M06M12 = "wages_m06m09_M06M12", 
    soshjelp_m06m09_M06M12 = "assistance_m06m09_M06M12", trygd_m06m09_M06M12 = "pensions_m06m09_M06M12")){
  x <- as.factor(x)
  le <- levels(x)
  ma <- match(le, oldLevels)
  isMatch <- !is.na(ma)
  le[isMatch] <- newLevels[ma[isMatch]]
  levels(x) <- le
  as.character(x)
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
#' The most comprehensive dataset, \code{sosialFiktiv}, contains three dimensions. 
#' The first dimension is 'region' which is grouped in two ways, 'fylke' and  
#' 'kostragr'. The other two are 'hovedint' and 'mnd'. In 'mnd2' two of the 
#' three categories in 'mnd' are merged.
#' The other datasets (\code{z1}, \code{z1w}, \code{z2}, \code{z2w}, \code{z3},
#'  \code{z3w}, \code{z3wb}) are smaller subdatasets.
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
















