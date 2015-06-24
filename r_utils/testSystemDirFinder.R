findArgs <- function(env, pattern) {
    nms <- ls(envir = as.environment(env))
    nms <- nms[is.na(match(nms, c("F","T")))] # <-- work around "checking hack"
    aa <- sapply(nms, function(.) { o <- get(.)
                                    if(is.function(o)) names(formals(o)) })
    iw <- sapply(aa, function(a) any(grepl(pattern, a, ignore.case=TRUE)))
    aa[iw]
}
findArgs("package:base", "warn")

getImageData = function(dFolder = getwd(), currentDate = Sys.Date(), token = ''){
    #if path is not available find the first folder for the recent close month
    currentDate = as.Date(currentDate)
    currentMonth = as.Date(cut(currentDate, 'month'))
    closeMonth = as.Date(cut(currentMonth-1, 'month'))
    dir.list = dir(path = dFolder, pattern = paste("results_",format(closeMonth, '%B'), '*',token, sep = ''))
    file.list = dir(dir.list, pattern = '*.[rR][dD]ata')
}

# test cases

a = getImageData(getwd(), '2014-10-01', token = 'SoR')
b = getImageData(getwd(), '2014-10-01', token = 'All')
