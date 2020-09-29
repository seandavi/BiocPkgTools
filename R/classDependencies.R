
#' Retrieve Class relationships
#'
#' As the title says it should do something with class relationships
#'
#' @param class a single \code{character} value defining a \sQuote{S4} class
#'   name
#' @param includeUnions \code{TRUE} or \code{FALSE}: Should union definitions
#'   included in the result? (default: \code{FALSE})
#'
#' @name class-dependencies
#'
#' @importFrom methods getClass is
#' @importFrom data.tree as.Node
#'
#' @export
#'
#' @examples
#' depData <- classDepData("RangedSummarizedExperiment")
#' classDepData("RangedSummarizedExperiment")
classDepTree <- function(class, includeUnions = FALSE) {
    # input check
    if(!is.logical(includeUnions) && length(includeUnions) != 1L &&
       !is.na(includeUnions)){
        stop("'includeUnions' must be TRUE or FALSE.", call. = FALSE)
    }
    if(!is.character(class) && length(class) != 1L &&
       !is.na(class)){
        stop("'class' must be a single character value.", call. = FALSE)
    }
    #
    data <- classDepData(class = class, includeUnions = includeUnions)
    dt <- as.Node(data, mode="network")
    dt
}

#' @rdname class-dependencies
#'
#' @export
classDepData <- function(class, includeUnions = FALSE) {
    # input check
    if(!is.logical(includeUnions) && length(includeUnions) != 1L &&
       !is.na(includeUnions)){
        stop("'includeUnions' must be TRUE or FALSE.", call. = FALSE)
    }
    if(!is.character(class) && length(class) != 1L &&
       !is.na(class)){
        stop("'class' must be a single character value.", call. = FALSE)
    }
    #
    classData <- .get_s_class_data(class)
    slotClassData <- .get_s_slot_class_data(class)
    data <- rbind(classData,slotClassData)
    if(!includeUnions){
        data <- data[data$childUnion != FALSE,]
    }
    data
}

.get_s_class_data <- function(class){
    classDef <- getClass(class)
    classData <- .get_s_extends_data(classDef@contains)
    classData <- classData[!vapply(classData,is.null,logical(1))]
    if(!is.null(classData)){
        classData$type <- "object"
    }
    classData
}

.get_s_slot_class_data <- function(class){
    classDef <- getClass(class)
    slotClassDefs <- lapply(classDef@slots, getClass)
    # check which slot is a union
    isUnion <- vapply(slotClassDefs,is,logical(1),"ClassUnionRepresentation")
    #
    notUnionSlotClassDefs <- lapply(slotClassDefs[!isUnion],
                                    function(scd){ scd@contains })
    FUN <- function(scd){
        scd <- getClass(scd)
        scd <- scd@subclasses
        scd <- scd[vapply(scd,function(e){e@distance == 1L},logical(1))]
        scd <- lapply(names(scd), getClass)
        scd <- scd[!vapply(scd,function(e){e@className == "NULL"},logical(1))]
        lapply(scd,function(e){e@contains})
    }
    unionSlotClassDefs <- lapply(classDef@slots[isUnion], FUN)
    slotClassDefs <- c(notUnionSlotClassDefs,
                       do.call(c,unionSlotClassDefs))
    # create class -> slot link
    child <- vapply(slotClassDefs,
                    function(scd){
                        scd[[1L]]@subClass
                    }, character(1))
    childVirtual <- .is_s_virtual(child)
    parentUnion <- is(classDef,"ClassUnionRepresentation")
    childUnion <- vapply(slotClassDefs,is,logical(1),"ClassUnionRepresentation")
    classData <- .get_s_extends_data.frame(class, child,
                                           parentVirtual = classDef@virtual,
                                           parentUnion = parentUnion,
                                           childVirtual = childVirtual,
                                           childUnion = childUnion)
    classData <- unique(classData)
    # get slot class data
    slotClassData <- lapply(slotClassDefs,.get_s_extends_data)
    # merge results
    slotClassData <- do.call(rbind,c(list(classData),slotClassData))
    rownames(slotClassData) <- NULL
    slotClassData$type <- "slot"
    slotClassData
}

.get_s_extends_data <- function(ext){
    if(length(ext) == 0L){
        return(NULL)
    }
    ext <- ext[vapply(ext,function(e){e@distance == 1L},logical(1))]
    if(length(ext) > 0L){
        res <- .get_s_extends_element(ext)
        res <- do.call(rbind,
                       c(list(res),
                         lapply(res$child, .get_s_class_data)))
    } else {
        res <- data.frame(parent = c(), child = c(), type = c(),
                          parentVirtual = c(), childVirtual = c(),
                          union = c())
    }
    res
}

.is_s_virtual <- function(x){
    unname(vapply(x,
                  function(e){
                      getClass(e)@virtual
                  },logical(1)))
}
.is_s_union <- function(x){
    unname(vapply(x,
                  function(e){
                      is(getClass(e),"ClassUnionRepresentation")
                  },logical(1)))
}

.get_s_extends_element <- function(ext){
    parent <- unname(vapply(ext,function(e){as.character(e@subClass)},character(1)))
    child <- unname(vapply(ext,function(e){as.character(e@superClass)},character(1)))
    parentVirtual <- .is_s_virtual(parent)
    childVirtual <- .is_s_virtual(child)
    parentUnion <- .is_s_union(parent)
    childUnion <- .is_s_union(child)
    .get_s_extends_data.frame(parent = parent, child = child,
                              parentVirtual = parentVirtual,
                              parentUnion = parentUnion,
                              childVirtual = childVirtual,
                              childUnion = childUnion)
}

.get_s_extends_data.frame <- function(parent = "", child = "", type = "",
                                      parentVirtual = FALSE,
                                      parentUnion = FALSE,
                                      childVirtual = FALSE,
                                      childUnion = FALSE){
    data.frame(parent = parent,
               child = child,
               type = type,
               parentVirtual = parentVirtual,
               parentUnion = parentUnion,
               childVirtual = childVirtual,
               childUnion = childUnion)
}

#' @rdname class-dependencies
#'
#' @importFrom data.tree GetDefaultTooltip SetGraphStyle SetNodeStyle Traverse
#'
#' @export
classDepData <- function(class, includeUnions = FALSE) {
    # input check
    if(!is.logical(includeUnions) && length(includeUnions) != 1L &&
       !is.na(includeUnions)){
        stop("'includeUnions' must be TRUE or FALSE.", call. = FALSE)
    }
    if(!is.character(class) && length(class) != 1L &&
       !is.na(class)){
        stop("'class' must be a single character value.", call. = FALSE)
    }
    #
    dt <- classDepTree(class = class, includeUnions = includeUnions)
    # basic styles
    SetGraphStyle(dt,
                  rankdir = "TB")
    SetNodeStyle(dt,
                 style = "filled,rounded",
                 shape = "box",
                 fillcolor = "#87B13F",
                 tooltip = GetDefaultTooltip)
    # color
    FUN_node_blue <- function(node){
        SetNodeStyle(node, fillcolor = "#1A81C2", inherit = FALSE)
    }
    Do(Traverse(dt,
                filterFun = function(x) {
                    ans <- x$Get("type")
                    !is.na(ans) & ans %in% "slot"
                }),
       FUN_node_blue)
    #
    plot(dt)
}
