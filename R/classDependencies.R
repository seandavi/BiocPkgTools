
#' Retrieve Class relationships
#'
#' As the title says it should do something with class relationships
#'
#' @param class a single \code{character} value defining a \sQuote{S4} class
#'   name
#' @param includeUnions \code{TRUE} or \code{FALSE}: Should union definitions
#'   included in the result? (default: \code{FALSE})
#' @param data a \code{data.frame} with compatible columns. See output of
#'   \code{buildClassDepData}
#' @param g an \code{igraph} object with compatible edge attributes. See output
#'   of \code{buildClassDepGraph}
#' @param pkg a single \code{character} value defining a package name
#'
#' @name class-dependencies
#'
#' @importFrom methods getClass is
#' @importFrom igraph graph_from_data_frame
#'
#' @export
#'
#' @examples
#' library("SummarizedExperiment")
#' depData <- buildClassDepData("RangedSummarizedExperiment")
#' depData
#' g <- buildClassDepGraph("RangedSummarizedExperiment")
#' plotClassDepGraph(g)
buildClassDepGraph <- function(class, includeUnions = FALSE) {
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
    data <- buildClassDepData(class = class, includeUnions = includeUnions)
    g <- igraph::graph_from_data_frame(data)
    g
}

#' @rdname class-dependencies
#'
#' @export
buildClassDepData <- function(class, includeUnions = FALSE) {
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
        data <- data[data$childUnion == FALSE,]
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
    if(length(slotClassDefs) == 0L){
        return(NULL)
    }
    # create class -> slot link
    child <- vapply(slotClassDefs,
                    function(scd){
                        if(length(scd) == 0L){
                            return(NA_character_)
                        }
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
                      if(is.na(e)){
                          return(FALSE)
                      }
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
#' @export
buildClassDepFromPackage <- function(pkg, includeUnions = FALSE){
    exports <- getNamespaceExports(pkg)
    classNames <- exports[grepl(".__C__",exports)]
    classNames <- gsub(".__C__","",classNames)
    data <- lapply(classNames, buildClassDepData, includeUnions = includeUnions)
    names(data) <- classNames
    data
}

#' @rdname class-dependencies
#' @export
plotClassDep <- function(class, includeUnions = FALSE) {
    plotClassDepGraph(buildClassDepGraph(class, includeUnions = includeUnions))
}
#' @rdname class-dependencies
#'
#' @importFrom igraph graph_from_data_frame
#'
#' @export
plotClassDepData <- function(data) {
    plotClassDepGraph(igraph::graph_from_data_frame(data))
}
#' @rdname class-dependencies
#'
#' @importFrom igraph E V degree layout_with_kk edge_attr V<- E<- 
#' @importFrom graphics strheight strwidth
#' @importFrom grDevices extendrange
#'
#' @export
plotClassDepGraph <- function(g) {
    # input check
    if(!all(require_edge_attr %in% names(edge_attr(g)))){
        stop("'g' must have the following edge attributes: '",
             paste(require_edge_attr, collapse = "', '"),"'")
    }
    # common
    V(g)$size <- nchar(names(V(g)))
    V(g)$label.family <- "sans"
    V(g)$label <- V(g)$name
    # set vertex color
    from <- function(){}
    color <- vapply(names(V(g)),
                    function(name){
                        virtual <- unique(E(g)[from(name)]$parentVirtual)
                        if(length(virtual) == 0 || virtual) {
                            "#1a81c2"
                        } else {
                            "#87b13f"
                        }
                    },
                    character(1))
    V(g)$color <- color
    label.color <- color
    label.color[label.color == "#1a81c2"] <- "white"
    label.color[label.color == "#87b13f"] <- "black"
    V(g)$label.color <- label.color
    # set edge color
    color <- E(g)$type
    color[color == "object"] <- "orange"
    color[color == "slot"] <- "gray"
    E(g)$color <- color
    # set edge type
    lty <- E(g)$type
    lty[lty == "object"] <- "solid"
    lty[lty == "slot"] <- "dotted"
    E(g)$lty <- lty
    # other edge settings
    E(g)$width <- 2
    # root
    root <- which(degree(g, v = V(g), mode = "in")==0, useNames = T)
    V(g)[root]$color <- "red"
    # plot
    co <- layout_with_kk(g)
    plot(0, type="n", ann=FALSE, axes=FALSE, xlim = extendrange(co[,1]),
         ylim=extendrange(co[,2]))
    plot(g, layout = co, rescale=FALSE, add=TRUE,
         vertex.shape="rectangle",
         vertex.size = (strwidth(V(g)$label) + strwidth("oo")) * 100,
         vertex.size2 = strheight("I") * 2 * 100)
}

require_edge_attr <- c("type",
                      "parentVirtual", "parentUnion",
                      "childVirtual", "childUnion")
