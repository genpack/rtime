# Header
# Filename:      tshourly.R
# Description:   Contains a class for working with multi-variate hourly time series supporting prediction and visualization
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    14 September 2018
# Last Revision: 14 September 2018
# Version:       0.1.0

# Version   Date                  Action
# --------------------------------------
# 0.1.0     14 September 2018     initial issue converted from tsdaily.R

#' @import gener


valid.addables.classes = c('numeric', 'integer', 'timeDate', 'POSIXlt', 'Date', 'double')

valid.arima.methods    = c('CSS-ML', 'ML', 'CSS')


# Definition of TIME.SERIES class
#' A Reference Class representing a time series.
#'
#' @field N.int An integer representing the number of time intervals in the time series
#' @field ctn An integer representing the current time interval number
#' @field stn An integer representing the starting time interval number of the control window
#' @field etn An integer representing the starting time interval number of the control window
#' @field data A data.frame with the same number of rows as fields \code{time} containing the time series data
#'
#' @export TS.HOURLY
TS.HOURLY <- setRefClass("TS.HOURLY",
                         fields = list(
                           ID        = "character",
                           name      = "character",
                           N.int     = "integer",
                           ctn       = "integer",
                           stn       = "integer",
                           etn       = "integer",
                           data      = "data.frame"
                         ),

                         methods = list(
                           initialize = function(from = NULL, until = NULL, ID = NULL, name = NULL, ...){
                             callSuper(...)

                             name <<- name %>% verify('character', default = '')
                             ID   <<- ID %>% verify('character', default = '')

                             if(!is.null(from) & !is.null(until)){
                               data <<- data.frame(time = seq(from = from %>% as.time %>% cut(breaks = 'hour') %>% as.POSIXct, to = until %>% as.time %>% cut(breaks = 'hour') %>% as.POSIXct, by = 3600)) %>%
                                 as_tibble

                               N.int <<- nrow(data)
                               ctn   <<- as.integer(N.int)
                               stn   <<- as.integer(1)
                               etn   <<- as.integer(N.int)
                             }
                           },

                           sortByTime = function(decreasing = F){
                             "Sorts the data based on time"
                             data <<- data[order(data$time, decreasing = decreasing), ]
                           },

                           # converts time series to regular periodic basis and returns the periodic time series
                           as.daily = function(...){
                             TS.DAILY(from = min(data$time), until = max(data$time)) %>% feedData(data, time_col = 'time', ...) # ... passes aggregator functions list
                           },

                           reset = function(){stn <<- ctn},

                           append.WeekStart = function(colname = 'weekStart', labels = NULL, custom_starts = NULL, ...){
                             data[, colname] <<- cut(data$time, breaks = 'week')
                           },

                           append.DayStart = function(colname = 'dayStart', labels = NULL, custom_starts = NULL, ...){
                             data[, colname] <<- cut(data$time, breaks = 'day')
                           },

                           feedData = function(dataset, hour_col = NULL){
                             dataset %<>% nameColumns(columns = list(time = hour_col), classes = list(time = 'POSIXct'))
                             data <<- data %>% left_join(dataset, by = 'time')
                           },

                           feedEventLog = function(dataset, time_col, aggrigators = NULL){
                             dataset %<>% nameColumns(columns = list(time = time_col), classes = list(time = 'POSIXct'))

                             mixed = data %>% mutate(time = time %>% setTZ('GMT')) %>% bind_rows(dataset) %>%
                               mutate(time = cut(time, breaks = 'day') %>% as.POSIXct) %>% as.data.frame

                             aggrigators %<>% verify('list', default = list())
                             figures = names(mixed) %-% 'time'
                             for(coln in figures){
                               # determine default aggregator function:
                               if (inherits(mixed[, coln], valid.numeric.classes)){defunc = 'sum'} else {defunc = 'most.common'}

                               aggrigators[[coln]] %<>% verify('character', lengths = 1, default = defunc)
                             }

                             funcnames = aggrigators %>% list.extract(figures) %>% unlist
                             scr = "mixed %>% dplyr::group_by(time) %>% dplyr::summarise(" %++%
                               (paste0(figures, " = ",
                                       funcnames %>% paste0("(", figures, ifelse(funcnames %in%  c('mean', 'sum', 'median', 'sd'), ", na.rm = T",""), ")")
                               ) %>%
                                 paste(collapse = ', ')) %++% ")"

                             data <<- eval(parse(text = scr))
                           },

                           feedEventLog.molten = function(dataset, time_col, var_col, value_col, aggrigator = NULL){
                             "
                             feeds additional data to the time series in molten format.

                             "
                             dataset %<>% nameColumns(columns = list(time = time_col, variable = varCol, value = valueCol), classes = list(time = 'POSIXct', variable = 'character', value = 'integer')) %>%
                               reshape2::dcast(time~variable, value.var = 'value', fun.aggregate = aggrigator)

                             feedData(dataset, time_col = 'time')
                           },

                           removeFigures = function(figures){
                             figures = verify(figures, 'character', domain = names(data) %-% 'time', fix = T, default = names(data) %-% 'time')
                             NC = which(colnames(data) %in%  figures)
                             if (length(NC) > 0){data <<- data[, - NC, drop = F]}
                           },

                           catFigs  = function(){return(nominals(data))},

                           numFigs  = function(){return(numerics(data))},

                           timeNumber = function(time){
                             if (inherits(time, c('integer', 'numeric'))){
                               if (time > N.int){time = N.int}
                               if (time < 1){time = 1}
                               return(as.integer(time))
                             } else {time = as.time(time, target_class = 'POSIXct')}

                             if (time > max(data$time)){time = max(data$time)}
                             if (time < min(data$time)){time = min(data$time)}
                             return(which(data$time == time)[1])
                           },

                           goto = function(time){
                             ctn <<- timeNumber(time)
                             if (stn > ctn){stn <<- ctn}
                           },

                           jump = function(N = 1){
                             goto(ctn + N)
                           },

                           now = function(){
                             return(data$time[ctn])
                           },

                           # Returns the moving average of the figure
                           # todo: put high.pass.mean threshold in the settings default is NA which means all values are accepted
                           mov.avr = function(figure, ...){
                             return(high.pass.moving.mean(v = data[sequence(ctn), figure], ...))
                           },

                           get.cumulative = function(){
                             out       <- new('TS.HOURLY')
                             out$data  <- data.frame(time = data$time, data %>% select(-time) %>% cumulative)
                             out$N.int <- nrow(out$data)
                             out$ctn   <- as.integer(out$N.int)
                             out$stn   <- as.integer(1)
                             out$etn   <- as.integer(out$N.int)
                             return(out)
                           },


                           average = function(period = stn:ctn, figures = numFigs()){
                             colMeans(data[period, figures, drop = F], na.rm = T)
                           },

                           break.hod = function(figure, hod_format = c('%H', '%h'), transpose = F){
                             hod_format = match.arg(hod_format)
                             tbl = data

                             tbl$Day = tbl$time %>% format('%Y-%m-%d')
                             tbl$HOD = tbl$time %>% format(hod_format)
                             tbl %<>% reshape2::dcast(Day ~ HOD, value.var = figure)
                             if(transpose){tbl %<>% column2Rownames('Day') %>% as.matrix %>% t %>% as.data.frame %>% rownames2Column('HOD')}
                             return(tbl)
                           },

                           mov.sd = function(figure, ...){
                             return(high.pass.moving.sd(data[sequence(ctn), figure], ...))
                           },

                           current = function(figure){
                             return( data[ctn, figure])
                           },

                           initial = function(figure){
                             return( data[1, figure])
                           },

                           last = function(figure){
                             return( data[N.int, figure])
                           },

                           aggregate.seasonal = function(period = sequence(ctn), figures = numFigs(), seasonality = 'dow', func = mean, centralize = F, replace.missing = NA, rem.seas.col = T){
                             # Verifications
                             assert(seasonality %in% c('dow', 'moy', 'doy', 'dof'), err_msg = "Unknown value for argument 'seasonality'. Must be in c('dow', 'moy', 'doy')", match.call()[[1]])

                             dataset = data[period, figures, drop = F]
                             timeset = time[period]
                             dataset[is.na(dataset)] <- replace.missing
                             switch(seasonality,
                                    'dow' = {
                                      S   <- aggregate(dataset, by = list(dayOfWeek(timeset)), FUN = func)
                                      S[,1] = factor(S[,1], levels = names(wdlabel), labels = wdlabel)
                                    },
                                    'dof' = {
                                      S   <- aggregate(dataset, by = list(fortday(timeset)), FUN = func)
                                      S[,1] = factor(S[,1], levels = fdlabel, labels = fdlabel)
                                    },
                                    'moy' = {
                                      mlb   = mntlabel[months(timeset)]
                                      S     = aggregate(dataset, by = list(mlb), FUN = func)
                                      S[,1] = factor(S[,1], levels = mntlabel)
                                    },
                                    'doy' = {
                                      tt    = as.POSIXlt(timeset)
                                      dylb  = paste(tt$mday, mntlabel[tt$mon + 1])
                                      S     = aggregate(dataset, by = list(dylb), FUN = func)
                                      S[,1] = factor(S[,1], levels = dylb)
                                      # todo: dylb must be sorted to be set as levels
                                    })
                             if (centralize) {
                               if (length(figures) > 1){S[, figures] = apply(S[, figures], 2, function(x) x - mean(x))}
                               else {
                                 x = S[, figures]
                                 S[, figures] <- x - mean(x)}
                             }

                             rownames(S) = as.character(S[, 1])

                             if (rem.seas.col){S = S[,-1, drop = F]} else {colnames(S)[1] <- seasonality}
                             return(S)
                           },

                           plot.motion = function(figures = numFigs(), config = NULL, ...){
                             assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
                             stateSettings <-'
                             {"colorOption":"_UNIQUE_COLOR", "showTrails":false, "nonSelectedAlpha":0, "xAxisOption":"_ALPHABETICAL"}
                             '

                             num.figs = numFigs()
                             figures  = verify(figures, 'character', domain = num.figs, default = num.figs, varname = 'figures')
                             U = data[, figures] %>% mutate(timeVar = as.POSIXct(time)) %>%
                               reshape2::melt(id = 'timeVar', variable.name = idVarName)
                             gvisMotionChart(U, idvar = idVarName, timevar = 'timeVar', sizevar = 'value', options = list(state = stateSettings))
                           }, # Regular Motioan Chart, remember googleVis motionchart only accepts time or numeric as time

                           plot.seasonality = function(period  = sequence(ctn), figures = numFigs(), seasonality = 'dow', func = mean, centralize = F, replace.missing = NA,
                                                       package = 'googleVis', type = 'bar', click_input_id = NULL, config = NULL, ...){
                             verify(type, 'character', domain = 'bar', varname = 'type')
                             verify(package, 'character', domain = c('googleVis', 'plotly'), varname = 'package')
                             assert(require(package, character.only = T), "Package " %++% package %++% " is not installed!", err_src = match.call()[[1]])
                             assert(require('niravis'), "Package niravis is not installed!", err_src = match.call()[[1]])
                             S = aggregate.seasonal(period = period, figures = figures, seasonality = seasonality, func = func, centralize = centralize,
                                                    replace.missing = replace.missing, rem.seas.col = F)
                             S = S[order(S[,1]),]
                             switch(package,
                                    'googleVis' = {switch(type,
                                                          'bar' = {
                                                            if (is.null(config)){config = gglvis.column.settings}
                                                            if (!is.null(click_input_id)){config$gvis.listener.jscode = gglvis.click.jscript(click_input_id)}
                                                            g = googleVis.bar(S, x = 'dow', y = figures, func = NULL, options = config, ...)
                                                          })},
                                    'plotly' = {switch(type,
                                                       'bar' = {
                                                         g = plotly.bar(S, x = 'dow', y = figures, func = NULL)
                                                       })}
                             )
                             return(g)
                           },

                           plot.calendar = function(period = stn:ctn, figure = numFigs()[1], package = 'googleVis', type = 'calheat', click_input_id = NULL, config = NULL, ...){
                             type = tolower(type)
                             verify(type, 'character', domain = 'calheat', varname = 'type')
                             verify(package, 'character', domain = 'googleVis', varname = 'package')
                             assert(require(package, character.only = T), "Package " %++% package %++% " is not installed!", err_src = match.call()[[1]])
                             assert(require('niravis'), "Package niravis is not installed!", err_src = match.call()[[1]])
                             switch(package,
                                    'googleVis' = {switch(type,
                                                          'calheat' = {
                                                            if (is.null(config)){config = gglvis.calendar.settings}
                                                            if (config$height == "auto"){
                                                              maxt = as.POSIXlt(max(time[period]))
                                                              mint = as.POSIXlt(min(time[period]))
                                                              h = 100*(maxt$year - mint$year + 1) + 20
                                                            } else {h = gglvis.calendar.settings$height}

                                                            config = list(
                                                              title    = config$title,
                                                              height   = h,
                                                              calendar = list2Json(config, fields_remove = 'height')
                                                            )
                                                            if (!is.null(click_input_id)){config$gvis.listener.jscode = gglvis.click.jscript(click_input_id)}

                                                            gvisCalendar(to.data.frame(period, figure, time_class = 'POSIXct'), timevar = "time", numvar  = figure, options = config, ...)
                                                          })})
                           },

                           plot.history = function(period = stn:ctn, figures = numFigs(), plotter = 'dygraphs', type = 'tsline', config = NULL, ...){
                             verify(plotter, 'character', lengths = 1, varname = 'plotter')
                             verify(figures, 'character', domain = c(numerics(data), 'total', 'average'), varname = "figures", null_allowed = F)
                             assert(require('niravis'), "Package niravis is not installed!", err_src = match.call()[[1]])
                             data[period, ] %>% niraPlot(x = 'time', y = figures %>% as.list, type = type, plotter = plotter, config = config, ...)
                           },

                           plot.value = function(period = NULL, figure = numFigs()[1], package = 'rAmCharts', type = 'gauge', levels = NULL, percentage = FALSE, config = NULL, ...){
                             # Verifications:
                             verify(type, 'character', 'gauge', varname = 'type')
                             verify(package, 'character', c('googleVis', 'rAmCharts'), varname = 'package')
                             assert(require(package, character.only = T), "Package " %++% package %++% " is not installed!", err_src = match.call()[[1]])
                             assert(require('niravis'), "Package niravis is not installed!", err_src = match.call()[[1]])
                             period  = verify(period, c('integer', 'numeric'), domain = c(1, N.int), default = ctn, varname = 'period')

                             lgnd = list(min = min(data[, figure], na.rm = T), max = max(data[, figure], na.rm = T))
                             switch(package,
                                    'rAmCharts' = {rAmCharts.gauge(theta = mean(data[period, figure], na.rm = T), legend = lgnd)},
                                    'googleVis' = {
                                      tbl = data.frame(label = figure, value = colMeans(data[period, figure], na.rm = T))
                                      googleVis.gauge(tbl, label = 'label', theta = 'value', legend = lgnd)}
                             )

                           },

                           plot.timeBreak.yoy = function(figure, x.axis, years, labels = NULL, year.start = '01-01', func = mean, package = 'dygraphs', type = 'line', ...){
                             # todo: should add more packages and types + add verifications
                             if      (x.axis == 'doy'){D  = timeBreak.doy(years = years, labels = labels, year.start = year.start, figure = figure, pretty = T, sort.rows = T)}
                             else if (x.axis == 'moy'){D  = timeBreak.moy(years = years, labels = labels, year.start = year.start, figure = figure, func = func)}
                             else if (x.axis == 'woy'){D  = timeBreak.woy(years = years, labels = labels, year.start = year.start, figure = figure, func = func)}
                             else {stop("\n Unsupported value for 'x.axis' argument! \n")}

                             assert(require(dygraphs), "Package 'dygraphs' is not installed!", err_src = match.call()[[1]])
                             dygraphs.line(D, x = x.axis, ...)
                           }
                         ))

# Generic Functions:
setMethod("names",   "TS.HOURLY", function(x) names(x$data))
setMethod("head",    "TS.HOURLY", function(x, ...) head(x$data, ...))
setMethod("tail",    "TS.HOURLY", function(x, ...) tail(x$data, ...))
setMethod("dim",     "TS.HOURLY", function(x) dim(x$data))
setMethod("colSums", "TS.HOURLY", function(x) colSums(x$data))
setMethod("rowSums", "TS.HOURLY", function(x) rowSums(x$data))
setMethod("length",  "TS.HOURLY", function(x) length(x$time))
setMethod("show",    "TS.HOURLY", function(object) show(object$data))

setGeneric("duration", function(x) standardGeneric("duration"))
setMethod("duration", "TS.HOURLY", function(x) max(x$data$time) - min(x$data$time))

# Functions working with TIME.SERIES objects:
#' @export
plot.TS.HOURLY = function(obj, figures = 1, period = obj$stn:obj$ctn, type = 'o', ...){
  if (class(figures) %in% c('numeric', 'integer')){figures = names(obj)[figures]}

  plot.new()
  N = length(figures)
  par(mfrow=c(1 , N))
  for (i in sequence(N)){
    if (nchar(obj$name) == 0){mainStr = figures[i]} else {mainStr = paste(obj$name,':',figures[i])}
    plot(obj$time[period], obj$data[period, figures[i]],  main = mainStr, type = type,  ...)
  }
  # todo: set y axis and x axis labels
}


#' @export
summary.TS.HOURLY = function(obj){
  summary(obj$data)
}


#' @export
'[.TS.HOURLY'   = function(obj, period = sequence(obj$N.int), figures = colnames(obj$data)){
  x = obj$copy()
  x$data  = x$data[period, figures, drop = F]
  return(x)
}

#' @export
'names<-.TS.HOURLY' = function(obj, value){
  colnames(obj$data)  <- value
  return(obj)
}

#' @export
'[<-.TS.HOURLY' = function(obj, value, ...){
  obj$data[...] <- value
  return(obj)
}
