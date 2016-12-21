
#' List of data.frames to single data.frame conversion
#' @author Krunoslav Juraic
#' @description conversion of list of data.frames to single data frame.
#'              Can be added aditional column with data.frame names
#' @param lst list of data.frames for conversion
#' @param nms names of data.frames that will be added as additional column
#'            By default sets names(lst)
#' @return joined data.frame with additional column
#' @examples
#'  df1 <- data.frame(a = c(0,1,2), b = c(10,11,12))
#'  df2 <- data.frame(a = c(110,111,112), b = c(1110,1111,1112))
#'  lst <- list()
#'  lst[[1]] <- df1
#'  lst[[2]] <- df2
#'  names(lst) <- c("DF1", "DF2")
#'  lst2df(lst, names(lst))
lst2df <- function(lst, nms = names(lst)){
  dat.df <- data.table::rbindlist(Map(cbind, lst, name=nms))
  return(dat.df)
}

#' read multiple tabular files to list of data.frames
#'
#' @author K. Juraic
#' @description Read tabular data from multiple files in list of data.frames
#' @details Function is based on read.table function. If the list of file.name
#'          is not set it's open tk_file_dialog to set multiple files with data.
#'          It's using read.table function. Additional parameters for read.table
#'          function can be simply added to function call like skip, header.
#'          Return list of data.frames. Names of list elements set to base file
#'          names with data.
#' @param fnms list of file names
#' @param ... additional parameters for read.table function
#'
#' @return list of data.frames with readed data
#'
#' @examples
#'        \dontrun{read_tables(skip=26, header=TRUE)}
read_tables <- function(fnms = tk_choose.files(), ...){
  dat_lst <- alply(.data = fnms,
                   .margins = 1,
                   .fun = read.table,
                   .progress = "tk",
                   ...)
  names(dat_lst) <- basename(fnms)
  print(fnms)
  return(dat_lst)
}


#' preview plot of single data.frame in list
#'
#' @author K. Juraic
#' @description Use manipulate to preview plot data from list of data.frames.
#'              (X,Y) pair of column shoud be specified. Additional graphic
#'              parametrs for plot function can be specified.
#'              Usually this function is used in combination wity function
#'              read_tables().
#'              As plot main title it's used data.frame name.
#' @param lst list of data.frames for preview
#' @param ... additional parameters transfered to plot function
#' @return None
#' @examples
#'    \dontrun{
#'             dat <- read_tables()
#'             plot_tables(dat, col = 2)
#'    }
plot_tables <- function(lst, ...){
  manipulate({
    plot(lst[[id]][,col_x], lst[[id]][,col_y],
         main = names(lst)[id],
         type = 'o', pch = 16, cex = .5, ...
    )
  },
  id = slider(min = 1, max = length(dat_lst), initial = 1, label = "Sample_ID")
  )
}
