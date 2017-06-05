#' Read
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
read_data <- function(filepath) {
  readr::read_delim(file = filepath, delim = ";")
}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
add_test_cols <- function(tibble1) {
  tibble1 %>%
    dplyr::mutate(type = stringr::str_extract(post_title_1, ": .*"),
           test = stringr::str_extract(post_title_1, ".*:")) %>%
    dplyr::mutate(type = stringr::str_extract(type, "[A-Z].*"))

}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
filt_sct <- function(d, sch = "All", classn = "All",testn = "All") {
  if (any(sch != "All")) {
    d <- d %>%
      dplyr::filter(school %in% sch)
  }
  if (any(classn != "All")) {
    d <- d %>%
      dplyr::filter(class %in% classn)
  }
  if (any(testn != "All")) {
    d <- d %>%
      dplyr::filter(test %in% testn)
  }
  d %>%
    dplyr::select(user_id, school, class, test, type, percentage)
}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
filter_type <- function(tibble1, type_v) {
  tibble1 %>%
    dplyr::filter(type %in% type_v)
}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
get_pairs <- function(data) {
  filt_pre <- filter_type(data, type_v = "Pre Test")
  filt_post <- filter_type(data, type_v = "Post Test")

  d <- dplyr::inner_join(filt_pre, filt_post, by = c("user_id", "school", "class", "test"))
  e <- d[!duplicated(d), ]
  e
}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
paired_t <- function(group1, group2) {

  tt <- stats::t.test(group1, group2, paired = TRUE, conf.level = 0.95)
  Metric <- names(unlist(tt))
  dplyr::tibble(Metric, Value = unlist(tt))

}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
cohenTibble <- function(group1, group2) {

  cohD <- lsr::cohensD(group1, group2)
  dplyr::tibble(Metric = "Cohen's d", Value = cohD)

}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
combTestStats <- function(group1, group2) {

  rbind(paired_t(group1, group2), cohenTibble(group1, group2))

}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
prog_measure <- function(group1, group2, ceiling = 100, threshold = 0.05) {

  diffP <- group2 - group1
  labels <- c("Regressed", "No Change", "Progressed")
  RNOP <- cut(diffP, breaks = c(-Inf, -1e-5, 1e-5, Inf),
              labels = labels)
  dplyr::tibble(Value =  labels, Count = summary(RNOP))

}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
mean_group <- function(tibble1, grp.var1 = "school", grp.var2 = "school", grp.var3 = "school") {

  dots <- list(~mean(percentage))

  tibble1 %>%
    dplyr::group_by_(grp.var1, grp.var2,grp.var3) %>%
    dplyr::summarise_(total = ~n(), .dots = dots)
}

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
input_return <- function(input) {
  if(is.null(input)) {
    "All"
  }
  else {
    as.character(paste0(input, collapse = ", "))
  }
}
