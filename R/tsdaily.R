

# Header
# Filename:      tsdaily.R
# Description:   Contains a class for working with multi-variate daily time series supporting prediction and visualization
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    25 June 2018
# Last Revision: 03 July 2018
# Version:       0.1.2

# Version   Date               Action

# -----------------------------------
# 0.1.0     25 June 2018     initial issue
# 0.1.2     03 July 2018     Method feedEventLog() and feedEventLog.molten added()

#' @import gener

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
#' @export TS.DAILY
TS.DAILY <- setRefClass("TS.DAILY",
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
                              until %<>% as.time('Date')

                              data <<- data.frame(date = seq(from = from %>% as.time('Date'), to = until %>% as.time('Date'), by = 1)) %>%
                                as_tibble

                              N.int <<- nrow(data)
                              ctn   <<- as.integer(N.int)
                              stn   <<- as.integer(1)
                              etn   <<- as.integer(N.int)
                            }

                          },

                          sortByDate = function(decreasing = F){
                            "Sorts the data based on date"
                            data <<- data[order(data$date, decreasing = decreasing), ]
                          },

                          # converts time series to regular periodic basis and returns the periodic hourly time series
                          as.weekly = function(...){
                            TS.WEEKLY(from = min(data$date), until = max(data$date)) %>% feedData(data, time_col = 'date', ...) # ... passes aggregator functions list
                          },

                          reset = function(){stn <<- ctn},

                          append.WeekStart = function(colname = 'weekStart', labels = NULL, custom_starts = NULL, ...){
                            data[, colname] <<- cut(data$date, breaks = 'week')
                          },

                          feedData = function(dataset, date_col = NULL){
                            dataset %<>% nameColumns(columns = list(date = date_col), classes = list(date = 'Date'))
                            data <<- data %>% left_join(dataset, by = 'date') %>% na2zero
                          },

                          feedEventLog = function(dataset, time_col, aggrFuncNames = NULL){
                            dataset %<>% nameColumns(columns = list(time = time_col), classes = list(time = 'POSIXct'))

                            mixed = data %>% mutate(time = date %>% setTZ('GMT')) %>% bind_rows(dataset) %>%
                              mutate(date = cut(time, breaks = 'day') %>% as.Date) %>% select(-time) %>% as.data.frame

                            aggrFuncNames %<>% verify('list', default = list())
                            figures = names(mixed) %-% 'date'
                            for(coln in figures){
                              # determine default aggregator function:
                              if (inherits(mixed[, coln], valid.numeric.classes)){defunc = 'sum'} else {defunc = 'most.common'}

                              aggrFuncNames[[coln]] %<>% verify('character', lengths = 1, default = defunc)
                            }

                            funcnames = aggrFuncNames %>% list.extract(figures) %>% unlist
                            scr = "mixed %>% dplyr::group_by(date) %>% dplyr::summarise(" %++%
                              (paste0(figures, " = ",
                                      funcnames %>% paste0("(", figures, ifelse(funcnames %in%  c('mean', 'sum', 'median', 'sd'), ", na.rm = T",""), ")")
                              ) %>%
                                paste(collapse = ', ')) %++% ")"

                            data <<- eval(parse(text = scr))
                          },

                          feedEventLog.molten = function(dataset, time_col, var_col, value_col, aggrFunctions = NULL){
                            "
                            feeds additional data to the time series in molten format.

                            "
                            dataset %<>% nameColumns(columns = list(time = time_col, variable = var_col, value = value_col), classes = list(date = 'Date', variable = 'character', value = 'integer')) %>%
                              reshape2::dcast(time~variable, value.var = 'value')

                            feedData(dataset, time_col = 'time')
                          },

                          removeFigures = function(figures){
                            figures = verify(figures, 'character', domain = names(data) %-% 'date', fix = T, default = names(data) %-% 'time')
                            NC = which(colnames(data) %in%  figures)
                            if (length(NC) > 0){data <<- data[, - NC, drop = F]}
                          },

                          catFigs  = function(){return(nominals(data))},

                          numFigs  = function(){return(numerics(data))},

                          dateNumber = function(date){
                            if (inherits(date, c('integer', 'numeric'))){
                              if (date > N.int){date = N.int}
                              if (date < 1){date = 1}
                              return(as.integer(date))
                            } else {date = as.time(date, target_class = 'Date')}

                            if (date > max(data$date)){date = max(data$date)}
                            if (date < min(data$date)){date = min(data$date)}
                            return(which(data$date == date)[1])
                          },

                          goto = function(date){
                            ctn <<- dateNumber(date)
                            if (stn > ctn){stn <<- ctn}
                          },

                          jump = function(N = 1){
                            goto(ctn + N)
                          },

                          now = function(){
                            return(data$date[ctn])
                          },

                          # Returns the moving average of the figure
                          # todo: put high.pass.mean threshold in the settings default is NA which means all values are accepted
                          mov.avr = function(figure, ...){
                            return(high.pass.moving.mean(v = data[sequence(ctn), figure], ...))
                          },

                          # breaks the time series for one figure into a table: Shows values of one figure in a matrix where each row represents a day of year (from 1 to 365) and each column represents a year
                          break.doy = function(figure, doy_format = c('%m-%d', '%b %d', '%B %d')){

                            doy_format = match.arg(doy_format)
                            tbl = data

                            tbl$Year = tbl$date %>% format('%Y')
                            tbl$DOY  = tbl$date %>% format('%m-%d')
                            tbl %>% reshape2::dcast(Year ~ DOY, value.var = figure)
                          },

                          break.woy = function(figure, aggregator = sum){
                            tbl = data

                            tbl$Year = tbl$date %>% format('%Y')
                            tbl$WOY  = tbl$date %>% lubridate::week()
                            tbl %>% reshape2::dcast(Year ~ WOY, value.var = figure, fun.aggregate = aggregator)
                          },

                          break.moy = function(figure, moy_format = c('%B', '%b', '%m'), aggregator = sum){
                            moy_format = match.arg(moy_format)
                            tbl = data

                            tbl$Year = tbl$date %>% format('%Y')
                            tbl$MOY  = tbl$date %>% format(moy_format)
                            tbl %>% reshape2::dcast(Year ~ MOY, value.var = figure, fun.aggregate = aggregator)
                          },

                          average = function(period = stn:ctn, figures = numFigs()){
                            colMeans(data[period, figures, drop = F], na.rm = T)
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

                          aggregate.seasonal = function(period = sequence(ctn), figures = numFigs(), seasonality = 'dow', aggregator = mean, centralize = F, replace.missing = NA, rem.seas.col = T){
                            # Verifications
                            assert(seasonality %in% c('dow', 'moy', 'doy', 'dof'), err_msg = "Unknown value for argument 'seasonality'. Must be in c('dow', 'moy', 'doy')", match.call()[[1]])

                            dataset = data[period, figures, drop = F]
                            timeset = data$date[period]
                            dataset[is.na(dataset)] <- replace.missing
                            switch(seasonality,
                                   'dow' = {
                                     S   <- aggregate(dataset, by = list(weekdays(timeset)), FUN = aggregator)
                                   },
                                   'dof' = {
                                     S   <- aggregate(dataset, by = list(fortday(timeset)), FUN = aggregator)
                                     S[,1] = factor(S[,1], levels = fdlabel, labels = fdlabel)
                                   },
                                   'moy' = {
                                     mlb   = months(timeset)
                                     S     = aggregate(dataset, by = list(mlb), FUN = aggregator)
                                     S[,1] = factor(S[,1], levels = mntlabel)
                                   },
                                   'doy' = {
                                     tt    = as.POSIXlt(timeset)
                                     dylb  = paste(tt$mday, mntlabel[tt$mon + 1])
                                     S     = aggregate(dataset, by = list(dylb), FUN = aggregator)
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
                            U = data[, figures] %>% mutate(dateTimeVar = as.Date(time)) %>%
                              reshape2::melt(id = 'dateTimeVar', variable.name = idVarName)
                            gvisMotionChart(U, idvar = idVarName, timevar = 'dateTimeVar', sizevar = 'value', options = list(state = stateSettings))
                          }, # Regular Motioan Chart, remember googleVis motionchart only accepts date or numeric as time

                          plot.seasonality = function(period  = sequence(ctn), figures = numFigs(), seasonality = 'dow', aggregator = mean, centralize = F, replace.missing = NA,
                                                      package = 'plotly', type = 'bar', click_input_id = NULL, config = NULL, ...){
                            verify(type, 'character', domain = 'bar', varname = 'type')
                            verify(package, 'character', domain = c('googleVis', 'plotly'), varname = 'package')
                            assert(require(package, character.only = T), "Package " %++% package %++% " is not installed!", err_src = match.call()[[1]])
                            assert(require('viser'), "Package viser is not installed!", err_src = match.call()[[1]])
                            S = aggregate.seasonal(period = period, figures = figures, seasonality = seasonality, aggregator = aggregator, centralize = centralize,
                                                   replace.missing = replace.missing, rem.seas.col = F)
                            S = S[order(S[,1]),]
                            switch(package,
                                   'googleVis' = {switch(type,
                                                         'bar' = {
                                                           if (is.null(config)){config = gglvis.column.settings}
                                                           if (!is.null(click_input_id)){config$gvis.listener.jscode = gglvis.click.jscript(click_input_id)}
                                                           g = googleVis.bar(S, x = 'dow', y = figures, aggregator = NULL, options = config, ...)
                                                         })},
                                   'plotly' = {switch(type,
                                                      'bar' = {
                                                        g = plotly.bar(S, x = 'dow', y = figures, aggregator = NULL)
                                                      })}
                            )
                            return(g)
                          },

                          plot.calendar = function(period = stn:ctn, figure = numFigs()[1], package = 'googleVis', type = 'calheat', click_input_id = NULL, config = NULL, ...){
                            type = tolower(type)
                            verify(type, 'character', domain = 'calheat', varname = 'type')
                            verify(package, 'character', domain = 'googleVis', varname = 'package')
                            assert(require(package, character.only = T), "Package " %++% package %++% " is not installed!", err_src = match.call()[[1]])
                            assert(require('viser'), "Package viser is not installed!", err_src = match.call()[[1]])
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

                                                           gvisCalendar(to.data.frame(period, figure, time_class = 'Date'), datevar = "time", numvar  = figure, options = config, ...)
                                                         })})
                          },

                          plot.history = function(period = stn:ctn, figures = numFigs(), plotter = 'dygraphs', type = 'tsline', config = NULL, ...){
                            verify(plotter, 'character', lengths = 1, varname = 'plotter')
                            verify(figures, 'character', domain = c(numerics(data), 'total', 'average'), varname = "figures", null_allowed = F)
                            assert(require('viser'), "Package viser is not installed!", err_src = match.call()[[1]])
                            data[period, ] %>% viserPlot(x = 'date', y = figures %>% as.list, type = type, plotter = plotter, config = config, ...)
                          },

                          plot.value = function(period = NULL, figure = numFigs()[1], package = 'rAmCharts', type = 'gauge', levels = NULL, percentage = FALSE, config = NULL, ...){
                            # Verifications:
                            verify(type, 'character', 'gauge', varname = 'type')
                            verify(package, 'character', c('googleVis', 'rAmCharts'), varname = 'package')
                            assert(require(package, character.only = T), "Package " %++% package %++% " is not installed!", err_src = match.call()[[1]])
                            assert(require('viser'), "Package viser is not installed!", err_src = match.call()[[1]])
                            period  = verify(period, c('integer', 'numeric'), domain = c(1, N.int), default = ctn, varname = 'period')

                            lgnd = list(min = min(data[, figure], na.rm = T), max = max(data[, figure], na.rm = T))
                            switch(package,
                                   'rAmCharts' = {rAmCharts.gauge(theta = mean(data[period, figure], na.rm = T), legend = lgnd)},
                                   'googleVis' = {
                                     tbl = data.frame(label = figure, value = colMeans(data[period, figure], na.rm = T))
                                     googleVis.gauge(tbl, label = 'label', theta = 'value', legend = lgnd)}
                            )

                          },

                          plot.timeBreak.yoy = function(figure, x.axis, years, labels = NULL, year.start = '01-01', aggregator = mean, package = 'dygraphs', type = 'line', ...){
                            # todo: should add more packages and types + add verifications
                            if      (x.axis == 'doy'){D  = timeBreak.doy(years = years, labels = labels, year.start = year.start, figure = figure, pretty = T, sort.rows = T)}
                            else if (x.axis == 'moy'){D  = timeBreak.moy(years = years, labels = labels, year.start = year.start, figure = figure, aggregator = aggregator)}
                            else if (x.axis == 'woy'){D  = timeBreak.woy(years = years, labels = labels, year.start = year.start, figure = figure, aggregator = aggregator)}
                            else {stop("\n Unsupported value for 'x.axis' argument! \n")}

                            assert(require(dygraphs), "Package 'dygraphs' is not installed!", err_src = match.call()[[1]])
                            dygraphs.line(D, x = x.axis, ...)
                          }
                        ))

# Generic Functions:
# setMethod("names",   "TS.DAILY", function(x) names(x$data))
# setMethod("head",    "TS.DAILY", function(x, ...) head(x$data, ...))
# setMethod("tail",    "TS.DAILY", function(x, ...) tail(x$data, ...))
# setMethod("dim",     "TS.DAILY", function(x) dim(x$data))
# setMethod("colSums", "TS.DAILY", function(x) colSums(x$data))
# setMethod("rowSums", "TS.DAILY", function(x) rowSums(x$data))
# setMethod("length",  "TS.DAILY", function(x) length(x$time))
# setMethod("show",    "TS.DAILY", function(object) show(object$data))
# 
# setGeneric("duration", function(x) standardGeneric("duration"))
# setMethod("duration", "TS.DAILY", function(x) max(x$data$date) - min(x$data$date))

# Functions working with TIME.SERIES objects:
#' @export
plot.TS.DAILY = function(obj, figures = 1, period = obj$stn:obj$ctn, type = 'o', ...){
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
summary.TS.DAILY = function(obj){
  summary(obj$data)
}


#' @export
'[.TS.DAILY'   = function(obj, period = sequence(obj$N.int), figures = colnames(obj$data)){
  x = obj$copy()
  x$data  = x$data[period, figures, drop = F]
  return(x)
}

#' @export
'names<-.TS.DAILY' = function(obj, value){
  colnames(obj$data)  <- value
  return(obj)
}

#' @export
'[<-.TS.DAILY' = function(obj, value, ...){
  obj$data[...] <- value
  return(obj)
}

# fix it
as.TS.DAILY = function(x, start = Sys.Date(), freq = 'day', ...){
  if (inherits(x, c('numeric'))){
    start = as.time(start)
    tt    = timeSequence(from = start, by = freq, length.out = length(x), ...)
    return(TIME.SERIES(timeset = tt, dataset = x))
  } else if (inherits(x, 'ts')){
    assert(require(timeSeries), "Package 'timeSeries' is not installed!", err_src = match.call()[[1]])
    y  = as.timeSeries(x)
    tt = time(y)
    if (inherits(tt, 'timeDate')){
      return(new('TIME.SERIES', timeset = tt, dataset = as.data.frame(y)))
    } else (return(as.TIME.SERIES(as.numeric(x), start = start, freq = freq, ...)))
  } else if (inherits(x, 'timeSeries')){
    return(new('TIME.SERIES', timeset = time(x), dataset = as.data.frame(x), ...))
  } else if (inherits(x, 'data.frame')){new('TIME.SERIES', dataset = x, ...)}
}


