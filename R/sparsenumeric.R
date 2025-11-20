#' sparse numeric vector class
#'
#' represents sparse numeric vectors by storing only nonzero positions and values
#'
#' @slot value numeric vector of values for nonzero elements in sparse numeric
#' @slot pos integer vector of positions for nonzero elements in sparse numeric
#' @slot length total integer length of sparse numeric vector, including all zeros
#'
#' @details value and pos vectors must be of the same length, they correspond to each other
#'
#' @exportClass sparse_numeric
setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

setValidity(
  Class = "sparse_numeric",
  method = function(object) {
    if (length(object@value) != length(object@pos)) {
      return("value and position vectors must be same length")
    }
    if (sum(object@pos > object@length) > 0)
    {
      return("positions of values must be less than length of vector")
    }
    if (sum(duplicated(object@pos)) > 0)
    {
      return("cannot have multiple values in the same position")
    }
    TRUE
  }
)

setAs(
  from = "numeric",
  to = "sparse_numeric",
  def = function(from) {
    pos = which(from != 0)
    value = from[which(from != 0)]
    leng = length(from)
    methods::new("sparse_numeric", value=value, pos=pos, length=leng)
  }
)

setAs(
  from = "sparse_numeric",
  to = "numeric",
  def = function(from) {
    vec = numeric(from@length)
    vec[from@pos] = from@value
    vec
  }
)

#' adds together two sparse numeric vectors
#'
#' @param object the first sparse numeric vector object
#' @param object2 the second sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a sparse numeric vector object, the sum of the two passed in
#' @export
setGeneric(
  "sparse_add",
  function(object,object2,...){
    standardGeneric("sparse_add")
  }
)

#' adds together two sparse numeric vectors
#'
#' @param object the first sparse numeric vector object
#' @param object2 the second sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a sparse numeric vector object, the sum of the two passed in
#' @exportMethod sparse_add
setMethod(
  "sparse_add",
  signature(object = "sparse_numeric", object2 = "sparse_numeric"),
  function(object, object2,...){
    if (object@length != object2@length){
      stop('vectors must have the same length')
    }
    #sorting by position
    pos1 = object@pos[order(object@pos)]
    val1 = object@value[order(object@pos)]
    pos2 = object2@pos[order(object2@pos)]
    val2 = object2@value[order(object2@pos)]

    #matching ones
    matches1 = which(pos1 %in% pos2)
    matches2 = which(pos2 %in% pos1)
    value = val1[matches1] + val2[matches2]
    pos = pos1[matches1]

    #nonmatches
    nomatch1 = !(pos1 %in% pos2)
    nomatch2 = !(pos2 %in% pos1)
    value = c(value, val1[nomatch1])
    value = c(value, val2[nomatch2])
    pos = c(pos, pos1[nomatch1])
    pos = c(pos, pos2[nomatch2])

    #resorting
    sortorder = order(pos)
    pos = pos[sortorder]
    value = value[sortorder]

    #removing zero values
    nonzero = (value != 0)
    pos = pos[nonzero]
    value = value[nonzero]
    methods::new("sparse_numeric", value=value, pos=pos, length=object@length)
  }
)

#' adds together two sparse numeric vectors
#'
#' @param e1 the first sparse numeric vector object
#' @param e2 the second sparse numeric vector object
#'
#' @return a sparse numeric vector object, the sum of the two passed in
#' @exportMethod +
setMethod(
  "+",
  signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
  function(e1, e2) {
    sparse_add(e1, e2)
  }
)

#' multiplies together two sparse numeric vectors
#'
#' @param object the first sparse numeric vector object
#' @param object2 the second sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a sparse numeric vector object, the product of the two passed in
#' @export
setGeneric(
  "sparse_mult",
  function(object,object2,...){
    standardGeneric("sparse_mult")
  }
)

#' multiplies together two sparse numeric vectors
#'
#' @param object the first sparse numeric vector object
#' @param object2 the second sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a sparse numeric vector object, the product of the two passed in
#' @exportMethod sparse_mult
setMethod(
  "sparse_mult",
  signature(object = "sparse_numeric", object2 = "sparse_numeric"),
  function(object, object2,...){
    if (object@length != object2@length){
      stop('vectors must have the same length')
    }
    #sorting by position
    pos1 = object@pos[order(object@pos)]
    val1 = object@value[order(object@pos)]
    pos2 = object2@pos[order(object2@pos)]
    val2 = object2@value[order(object2@pos)]

    #matching ones
    matches1 = which(pos1 %in% pos2)
    matches2 = which(pos2 %in% pos1)
    value = val1[matches1] * val2[matches2]
    pos = pos1[matches1]

    #resorting
    sortorder = order(pos)
    pos = pos[sortorder]
    value = value[sortorder]
    methods::new("sparse_numeric", value=value, pos=pos, length=object@length)
  }
)

#' multiplies together two sparse numeric vectors
#'
#' @param e1 the first sparse numeric vector object
#' @param e2 the second sparse numeric vector object
#'
#' @return a sparse numeric vector object, the product of the two passed in
#' @exportMethod *
setMethod(
  "*",
  signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
  function(e1, e2) {
    sparse_mult(e1, e2)
  }
)

#' subtracts one sparse numeric vector from another
#'
#' @param object the sparse numeric vector object to be subtracted from
#' @param object2 the sparse numeric vector object to subtract
#' @param ... additional arguments
#'
#' @return a sparse numeric vector object, result of object - object2
#' @export
setGeneric(
  "sparse_sub",
  function(object,object2,...){
    standardGeneric("sparse_sub")
  }
)

#' subtracts one sparse numeric vector from another
#'
#' @param object the sparse numeric vector object to be subtracted from
#' @param object2 the sparse numeric vector object to subtract
#' @param ... additional arguments
#'
#' @return a sparse numeric vector object, result of object - object2
#' @exportMethod sparse_sub
setMethod(
  "sparse_sub",
  signature(object = "sparse_numeric", object2 = "sparse_numeric"),
  function(object, object2,...){
    if (object@length != object2@length){
      stop('vectors must have the same length')
    }
    #sorting by position
    pos1 = object@pos[order(object@pos)]
    val1 = object@value[order(object@pos)]
    pos2 = object2@pos[order(object2@pos)]
    val2 = object2@value[order(object2@pos)]

    #matching ones
    matches1 = which(pos1 %in% pos2)
    matches2 = which(pos2 %in% pos1)
    value = val1[matches1] - val2[matches2]
    pos = pos1[matches1]

    #nonmatches
    nomatch1 = !(pos1 %in% pos2)
    nomatch2 = !(pos2 %in% pos1)
    value = c(value, val1[nomatch1])
    value = c(value, -1*val2[nomatch2])
    pos = c(pos, pos1[nomatch1])
    pos = c(pos, pos2[nomatch2])

    #resorting
    sortorder = order(pos)
    pos = pos[sortorder]
    value = value[sortorder]

    #removing zero values
    nonzero = (value != 0)
    pos = pos[nonzero]
    value = value[nonzero]
    methods::new("sparse_numeric", value=value, pos=pos, length=object@length)
  }
)

#' subtracts one sparse numeric vector from another
#'
#' @param e1 the sparse numeric vector object to be subtracted from
#' @param e2 the sparse numeric vector object to subtract
#'
#' @return a sparse numeric vector object, result of object - object2
#' @exportMethod -
setMethod(
  "-",
  signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
  function(e1, e2) {
    sparse_sub(e1, e2)
  }
)

#' gets the cross product of two sparse numeric vectors
#'
#' @param object the first sparse numeric vector object
#' @param object2 the second sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a numeric, the cross product of the two vectors
#' @export
setGeneric(
  "sparse_crossprod",
  function(object,object2,...){
    standardGeneric("sparse_crossprod")
  }
)

#' gets the cross product of two sparse numeric vectors
#'
#' @param object the first sparse numeric vector object
#' @param object2 the second sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a numeric, the cross product of the two vectors
#' @exportMethod sparse_crossprod
setMethod(
  "sparse_crossprod",
  signature(object = "sparse_numeric", object2 = "sparse_numeric"),
  function(object, object2,...){
    if (object@length != object2@length){
      stop('vectors must have the same length')
    }
    objectmult = sparse_mult(object, object2)
    sum(objectmult@value)
  }
)

#' prints out attributes of sparse numeric vector object
#' (positions of nonzero elements, values of nonzero elements, length of vector)
#'
#' @param object the sparse numeric vector object
#'
#' @importFrom methods show
#' @export
setMethod(
  "show",
  signature(object = "sparse_numeric"),
  function(object){
    cat('nonzero positions: ', object@pos, "\n")
    cat('nonzero values: ', object@value, "\n")
    cat('total length: ', object@length, "\n")
  }
)

#' creates scatterplot of overlapping nonzero positions between two sparse numeric vectors
#'
#' @param x the first sparse numeric vector object
#' @param y the second sparse numeric vector object
#' @param ... additional arguments
#'
#' @exportMethod plot
setMethod(
  "plot",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y,...){
    if (x@length != y@length){
      stop('vectors must have the same length')
    }
    #sorting by position
    pos1 = x@pos[order(x@pos)]
    val1 = x@value[order(x@pos)]
    pos2 = y@pos[order(y@pos)]
    val2 = y@value[order(y@pos)]

    #matching ones
    matches1 = which(pos1 %in% pos2)
    matches2 = which(pos2 %in% pos1)

    if(length(matches1) == 0){
      stop('no overlapping nonzero positions to plot')
    }
    plot(val1[matches1], val2[matches2],
         main = 'Scatterplot of Overlapping Nonzero Positions')
  }
)

#' creates numeric vector with all zeros removed from sparse numeric object
#'
#' @param object the sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a numeric vector
#' @export
setGeneric(
  "unsparse",
  function(object,...){
    standardGeneric("unsparse")
  }
)

#' creates numeric vector with all zeros removed from sparse numeric object
#'
#' @param object the sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a numeric vector, no longer sparse and no 0s
#' @exportMethod unsparse
setMethod(
  "unsparse",
  signature(object = "sparse_numeric"),
  function(object,...){
    sortorder = order(object@pos)
    object@value[sortorder]
  }
)

#' calculates mean of sparse numeric vector object
#'
#' @param x the sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a numeric, the mean of the sparse numeric
#' @exportMethod mean
setMethod(
  "mean",
  signature(x = "sparse_numeric"),
  function(x,...){
    sum(x@value) / x@length
  }
)

#' gets squared norm of vector (sqrt of sum of squared elements)
#'
#' @param object the sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a numeric, the norm of the sparse numeric
#' @export
setGeneric(
  "norm",
  function(object,...){
    standardGeneric("norm")
  }
)

#' gets squared norm of vector (sqrt of sum of squared elements)
#'
#' @param object the sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a numeric, the norm of the sparse numeric
#' @exportMethod norm
setMethod(
  "norm",
  signature(object = "sparse_numeric"),
  function(object,...){
    sqrt(sum(object@value ^2))
  }
)

#' gets standardized version of sparse numeric vector object, in dense form
#' takes each element of vector and subtracts vector mean & divides by vector st. dev
#'
#' @param object the sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a dense vector, the standardized version of the sparse numeric
#' @export
setGeneric(
  "standardize",
  function(object,...){
    standardGeneric("standardize")
  }
)

#' gets standardized version of sparse numeric vector object, in dense form
#' takes each element of vector and subtracts vector mean & divides by vector st. dev
#'
#' @param object the sparse numeric vector object
#' @param ... additional arguments
#'
#' @return a dense vector, the standardized version of the sparse numeric
#' @exportMethod standardize
setMethod(
  "standardize",
  signature(object = "sparse_numeric"),
  function(object,...){
    dense = methods::as(object, "numeric")
    vmean = base::mean(dense)
    vstd = stats::sd(dense)
    (dense - vmean) / vstd
  }
)
