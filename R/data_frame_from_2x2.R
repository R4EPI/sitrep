#' create a 2x4 matrix from a 2x2x2 array
#'
#' Here, we are assuming the following:
#'
#' 1. the array has three dimensions
#' 2. the first value for any dimension is TRUE and the second value is FALSE
#' 3. there are a total of 8 cells whose contents represent counts.
#' 4. the dimensions are orderd as exposure, outcome, and stratifier
#'
#' @param x a 2x2x2 array
#'
#' @return a 2x4 matrix with the third dimension of the array in rows.
#' @noRd
case_matrix <- function(x, total = FALSE) {

  # The incoming table will be a 3D array that has this pattern:
  # x <- array(1:8, 
  #       dim = c(2, 2, 2), 
  #       dimnames = list(exposure = 1:2, outcome = 1:2, strata = 1:2)
  #      )
  # x
  # , , strata = 1
  # 
  #         outcome
  # exposure 1 2
  #        1 1 3
  #        2 2 4
  # 
  # , , strata = 2
  # 
  #         outcome
  # exposure 1 2
  #        1 5 7
  #        2 6 8
  #
  # Dimension 1: exposure (1 = exposed, 2 = unexposed)
  # Dimension 2: outcome  (1 = case,    2 = control)
  # Dimension 3 (optional): stratifier (e.g. age group)

  # we subset things differently if we have a cube (3D) or square (2D).
  #
  # For the cube, we result in 3 rows because we take the sum of all the strata
  # to create the crude.
  #
  # For the square, we only need to produce one row.
  ndim <- length(dim(x))
  if (ndim == 3L) { 
    A <- x[1, 1, ]
    B <- x[1, 2, ]
    C <- x[2, 1, ]
    D <- x[2, 2, ] 
    res <- c(c(sum(A), A), 
             c(sum(B), B), 
             c(sum(C), C), 
             c(sum(D), D)
            ) 
    nr     <- dim(x)[ndim] + 1L
    rnames <- dimnames(x)[[3]]
  } else {
    A <- x[1, 1]
    B <- x[1, 2]
    C <- x[2, 1]
    D <- x[2, 2] 
    res    <- c(A, B, C, D)
    nr     <- 1L
    rnames <- NULL
  }
  res <- matrix(res, 
                nrow = nr,
                dimnames = list(strata = c("crude", rnames),
                                c(A = "exp_cases",    
                                  B = "exp_controls", 
                                  C = "unexp_cases",
                                  D = "unexp_controls")
                )
  )
  if (ndim == 3) {
    res[if (total) 1:3 else 2:3, ]
  } else {
    res
  }
}

#' create a data frame from a 2x2 matrix
#'
#' @param x a 2x2 matrix or 3D array with exposure variable in rows and outcome
#'   in columns
#'
#' @return a data frame with the important combinations:
#'  - A_exp_cases
#'  - B_exp_controls
#'  - C_unexp_cases
#'  - D_unexp_controls
#'  - total_cases (A + B)
#'  - total_controls (C + D)
#'  - total_exposed (A + C)
#'  - total_unexposed (B + D)
#'  - total (A + B + C + D)
#' @export
#' @examples
#' arr <- c(10, 35, 90, 465, 36, 25, 164, 175)
#' arr <- array(arr, dim = c(2, 2, 2),
#'              dimnames = list(risk = c(TRUE, FALSE),
#'                              outcome = c(TRUE, FALSE),
#'                              old = c(FALSE, TRUE))
#'        )
#' arr
#' data_frame_from_2x2(arr)
data_frame_from_2x2 <- function(x) {

  cmat <- case_matrix(x, total = TRUE)
  A_exp_cases      <- cmat[, "exp_cases"]
  C_unexp_cases    <- cmat[, "unexp_cases"]
  B_exp_controls   <- cmat[, "exp_controls"]
  D_unexp_controls <- cmat[, "unexp_controls"]

  data.frame(
    A_exp_cases      = A_exp_cases,
    B_exp_controls   = B_exp_controls,
    C_unexp_cases    = C_unexp_cases,
    D_unexp_controls = D_unexp_controls,

    total_cases      = A_exp_cases    + C_unexp_cases,
    total_controls   = B_exp_controls + D_unexp_controls,

    total_exposed    = A_exp_cases    + B_exp_controls,
    total_unexposed  = C_unexp_cases  + D_unexp_controls,

    total            = A_exp_cases    + C_unexp_cases +
                       B_exp_controls + D_unexp_controls
  )

}


