
# Header
# Filename:      timeseries.R
# Description:   Contains a class for working with multi-channel (multi-variate) time series supporting prediction and visualization
# Author:        Nicolas Berta
# Email :        nicolas.berta@gmail.com
# Start Date:    19 January 2016
# Last Revision: 29 March 2017
# Version:       2.5.1

# Version   Date               Action
# -----------------------------------
# 2.1.0     05 July 2016       in method feed(), 'names' changed to 'colnames' so that the input argument can accept matrix objects as well as data.frames
# 2.2.0     26 August 2016     'regression' has been added as a prediction model, taking into account seasonalities
# 2.2.1     26 August 2016     Method updateForecast() added to class TIME.SERIES
# 2.2.2     26 August 2016     Method predictNext() added to class TIME.SERIES
# 2.3.0     07 September 2016  Method initialize() modified: If time_col is not specified, rownames of dataset is considered as timeset
# 2.3.1     08 September 2016  Methods timeBreak.doy.year(), timeBreak.woy.year() and timeBreak.moy.year() added. These methods return data.frames containing values of a selected figure aggregates and broken in time intervals. Test them to see how they work!
# 2.3.2     08 September 2016  Function build.arima() modified: checks and returns NULL if arima model is not made for any reason.
# 2.3.3     09 September 2016  Argument 'years' added to methods timeBreak.doy.year(), timeBreak.woy.year() and timeBreak.moy.year()
# 2.3.4     28 September 2016  Function build.arima() modified: Returns null if length of history data is leass than twice the given frequency. Because in this case, Arima predictor is prone to return Nan standard errors.
# 2.3.5     28 September 2016  Method predict() in class TSP.MODEL changed: If Nans are returned for fserr, standard deviation of predicted values will be used as fserr. It happens very rarely!
# 2.3.6     29 September 2016  Methods numerics() and categoricals() modified: Calls functions numerics() and nominals() from package gener (ver 1.3.4)
# 2.3.7     29 September 2016  Package niravis is now required for all plot methods
# 2.3.8     15 November 2016   Methods numerics() and categoricals() renamed to: numFigs() and catFigs()
# 2.4.0     30 November 2016   Methods plot*() modified: Calls appropriate functions from niraVis.
# 2.4.1     01 December 2016   Argument 'range' renamed to 'domain' when function gener::verify() is called.
# 2.4.2     06 December 2016   Method time.number modified: Calls function as.time() from gener.
# 2.4.3     19 December 2016   Method as.TIME.SERIES() added. Package 'timeSeries' is now a requirement for 'timser'.
# 2.4.4     21 December 2016   Default properties 'name' and 'ID' changed to blank character ''. Method plot() modified to avoid putting ':' in the plot main string when property 'name' is blank.
# 2.4.5     02 February 2017   Method timeBreak.moy.year() renamed to timeBreak.moy() and modified: returns a data.frame with rownames including all months sorted. Methods timeBreak.doy() and timeBreak.woy() renamed accordingly.
# 2.4.6     02 February 2017   Method plot.timeBreak.yoy() added. Currently supporting only package 'dygraphs' and type 'ts.line'
# 2.4.7     16 February 2017   Method plot.timeBreak.yoy() modified: Bug rectified: argument 'func' is transferred to methods timeBreak.moy() and timeBreak.woy()
# 2.4.8     17 February 2017   Method plot.motion() added.
# 2.4.9     22 February 2017   Method append.interval.start() modified: Argument 'interval' supports vlaues 'custom' and 'fortnight'. Also arguments 'custom_starts' and 'labels' added.
# 2.4.10    23 February 2017   Methods timeBreak.doy(), timeBreak.woy() and timeBreak.moy() modified: Arguments 'lables' and 'year.start' added. (Customized year start for fiscal year)
# 2.5.0     23 February 2017   Method plot.timeBreak.yoy() modified: Arguments 'lables' and 'year.start' added. (Customized year start for fiscal year)
# 2.5.1     29 March 2017      Method update() of class TSP.MODEL modified: argument passed to function Arima() changed from x to y in the new version of forecast and caused problem. Argument name removed!. Also a warning addded when call is returned with error!

#' @import gener
#' @import timeDate

Agg.Rule = function(){
  agr = data.frame(figures = character(), functions = character(), stringsAsFactors = F)
  class(agr) <- append(class(agr), "Agg.Rule")
  return(agr)
}
# todo: use aggregate() and apply() methods in the base to do all aggregations

#' @export
valid.addables.classes = c('numeric', 'integer', 'timeDate', 'POSIXlt', 'Date', 'double')
#' @export
valid.arima.methods    = c('CSS-ML', 'ML', 'CSS')

#' @export
build.arima = function(x, freq = 14, auto = F, ...){
  verify(x, allowed = c('numeric', 'integer'), varname = 'x')
  if (length(x) < 2*freq){return(NULL)}
  ar = try('a' - 'b', silent = T)
  i  = 0
  x  = ts(x, frequency = freq, start = c(1,1))
  if(auto){
    ar = try(auto.arima(x), silent = T)
    if (inherits(ar, 'ARIMA')){return(ar)}
  }
  while (inherits(ar, 'try-error') & (length(x) > freq)){
    i = i + 1
    if (i > 3){
      i = 1
      N = max(freq, length(x) - freq)
      x = x[sequence(N)]
    }
    md = valid.arima.methods[i]
    ar = try(Arima(x, order = c(3,1,3), seasonal = list(order = c(1,1,1), period = freq), method = md, ...), silent = TRUE)
    if (inherits(ar, 'try-error')){
      ar = try(Arima(x, order = c(3,1,3), seasonal = list(order = c(1,0,1), period = freq), method = md, ...), silent = TRUE)
    }
  }

  if (inherits(ar, 'Arima')){return(ar)} else {return(NULL)}
}


#' Reference Class TSP.MODEL is combination of properties and methods aimed to train multiple various time-series forecasting
#' model and generate predictions.
#'
#' @field train.time Vector of \code{timeDate} objects containing the time stamps of the training data
#' @field train.data Vector of numerics (same length as \code{time}) containing values of the training data
#' @field seasonalities Vector of characters specifying what seasonalities should be applied
#'        Valid values are:
#'        * \code{'dow'}
#'        * \code{'moy'}
#'        * \code{'doy'}
#' @field model A list of models. List elements can be different objects depending on the model type.
#'        For example Arima models are instances of class \linkS4class{forecast::Arima}
#' @field pred A named vector of numerics containing the predicted values.
#'        Vector names represent the date/time stamp of the predicted value.
#' @field serr A named vector of numerics containing the estimated standard error for the predicted values.
#'        Vector names represent the date/time stamp of the predicted value.
#'
#' @export TSP.MODEL
#' @exportClass TSP.MODEL
TSP.MODEL <- setRefClass("TSP.MODEL",
                         fields = list(
                           train.time    = "timeDate",
                           train.data    = "numeric",
                           seasonalities = 'character',
                           model         = "list",
                           pred          = "numeric",
                           serr          = "numeric",
                           period        = "numeric",
                           DOW           = "data.frame",
                           MOY           = "data.frame",
                           DOY           = "data.frame"),
                         methods = list(
                           initialize = function(train.time = timeDate(), period = 14, seasonalities = c('dow', 'moy', 'doy'), ...){
                             "Class constructor"
                             callSuper(...)
                             train.time    <<- train.time
                             seasonalities <<- seasonalities
                             period        <<- period
                           },

                           train      = function(time, data, model_type = 'arima', vf = T){
                             "
                             Trains a time-series forecast model for using the given time and dataset. \n
                             \n
                             Arguments: \n
                             time: Vector of 'timeDate' objects containing the time stamps of the training data. \n
                             data: Vector of numerics (same length as argument 'time' containing values of the training data. \n
                             vf:   A logical: Should verification be done before implementation? \n
                             Returns: Nothing \n
                             The documentation of this method is under construction
                             "

                             # Verifications:
                             model_type = tolower(model_type)
                             assert(model_type %in% c('mean', 'arima', 'moving.average', 'regression', 'arimareg'), "Unknown model!", match.call()[[1]])
                             if (model_type == 'arima'){
                               assert(require(forecast), "Package forecast is not installed!", err_src = match.call()[[1]])
                             }
                             assert(length(time) == length(data), "time and data must have the same lengths", match.call()[[1]])
                             # todo: treat missing values
                             # todo: sort time

                             N = length(time)
                             train.time    <<- time
                             train.data    <<- data
                             seasonalities <<- seasonalities
                             # period        <<- as.numeric(difftime(time[N], time[N - 1], units = 'sec')) # todo: mean(time - vect.lag(time), na.rm = T)

                             if ('doy' %in% seasonalities){
                               DOY  <<- extract.seasonality(time, data, 'doy', centralize = (model_type == 'arima'))
                               doy = distribute.seasonality(time, DOY, 'doy')
                               if (model_type %in% c('arima', 'mean')){data = data - doy}
                             }

                             if ('dow' %in% seasonalities){
                               DOW <<- extract.seasonality(time, data, 'dow', centralize = (model_type == 'arima'))
                               dow = distribute.seasonality(time, DOW, 'dow')
                               if (model_type %in% c('arima', 'mean')){data = data - dow}
                             }

                             if ('moy' %in% seasonalities){
                               MOY  <<- extract.seasonality(time, data, 'moy', centralize = (model_type == 'arima'))
                               moy = distribute.seasonality(time, MOY, 'moy')
                               if (model_type %in% c('arima', 'mean')){data = data - moy}
                             }

                             switch(model_type,
                                    "mean"       = {model[[model_type]] <<- list(mean = mean(data), sd = sd(data))},
                                    "regression" = {
                                      TrainData = data.frame(doy, dow, moy, Y = data)
                                      regmodel  = lm(Y ~ doy + dow + moy, data = TrainData)
                                      model[[model_type]] <<- regmodel
                                    },
                                    "arima"= {
                                      ar       = build.arima(data, auto = F, freq = period, optim.control = list(maxit = 1000))
                                      if (is.null(ar)){
                                        cat('warning: Arima model failed! Simple mean replaced!')
                                        model[[model_type]] <<- list(mean = mean(data), sd = sd(data))
                                      } else {model[[model_type]] <<- ar}
                                    },
                                    "arimareg"= {
                                      ar        = build.arima(data, freq = period, optim.control = list(maxit = 1000))
                                      TrainData = data.frame(arfit = ar$x + ar$residuals, doy, dow, moy, Y = data)
                                      reg       = lm(Y ~ arfit + doy + dow + moy, data = TrainData)
                                      model[[model_type]] <<- list(armodel = ar, regmodel = reg)
                                    },

                                    "moving.average"= {model[[model_type]] <<- list(mean = high.pass.moving.mean(train.data), sd = high.pass.moving.sd(train.data))})
                           },

                           predict = function(time, model_type = 'arima'){
                             time  = sort(time)
                             tstr  = time2Char(time)

                             assert(model_type %in% names(model), paste("Given model",model_type,"has not been trained!"), match.call()[[1]])
                             assert(time[1] > train.time[length(train.time)], "Given time for prediction must be after the last training time", match.call()[[1]])

                             if ('doy' %in% seasonalities){doy   = distribute.seasonality(time, DOY, 'doy')} else {doy = 0}
                             if ('dow' %in% seasonalities){dow   = distribute.seasonality(time, DOW, 'dow')} else {dow = 0}
                             if ('moy' %in% seasonalities){moy   = distribute.seasonality(time, MOY, 'moy')} else {moy = 0}

                             if (model_type %in% c('mean', 'moving.average')){
                               pred[tstr] <<- model[[model_type]]$mean + dow + moy + doy
                               serr[tstr] <<- model[[model_type]]$sd
                             }
                             else if(model_type == 'regression'){
                               pred[tstr] <<- model[[model_type]]$coefficients[1] +
                                 model[[model_type]]$coefficients['doy']*doy +
                                 model[[model_type]]$coefficients['dow']*dow +
                                 model[[model_type]]$coefficients['moy']*moy
                               serr[tstr] <<- sd(model[[model_type]]$residuals)
                             }
                             else if (model_type == 'arima'){
                               if (inherits(model[[model_type]], 'ARIMA')){
                                 # ext  = data.frame(doy, dow, moy)
                                 assert(require(forecast), "Package forecast is not installed!", err_src = match.call()[[1]])
                                 options(warn = -1)
                                 res  = forecast::forecast(model[[model_type]], h = length(time))
                                 options(warn  = 0)
                                 pred[tstr] <<- res$mean + dow + moy + doy
                                 serr[tstr] <<- (res$upper[,2]- res$lower[,2])/3.92
                                 if (is.na(sum(serr[tstr]))){serr[tstr] <<- sd(pred[tstr], na.rm = T)}
                               } else {
                                 pred[tstr] <<- model[[model_type]]$mean + dow + moy + doy
                                 serr[tstr] <<- model[[model_type]]$sd
                               }
                             }
                             else if(model_type == 'arimareg'){
                               arfit  = as.numeric(forecast(model[[model_type]]$armodel, h = length(time))$mean)
                               pred[tstr] <<- model[[model_type]]$regmodel$coefficients[1] +
                                 model[[model_type]]$regmodel$coefficients['arfit']*arfit +
                                 model[[model_type]]$regmodel$coefficients['doy']*doy +
                                 model[[model_type]]$regmodel$coefficients['dow']*dow +
                                 model[[model_type]]$regmodel$coefficients['moy']*moy
                               serr[tstr] <<- sd(model[[model_type]]$regmodel$residuals)
                             }

                             out = list(time = time, pred = pred[tstr], serr = serr[tstr])
                             return(out)
                           },

                           reset = function(){
                             pred <<- numeric()
                             serr <<- numeric()},

                           update  = function(new_time, new_data, model_type = 'arima'){
                             # Currently, only updates for ARIMA models. For other models, training is refreshed.
                             N = length(new_time)
                             assert(length(new_data) == N, "Given arguments 'new_time' and 'new_data' must have equal lengths")
                             assert(min(new_time) > max(train.time), "New time must be after the latest training time")
                             # new_ts = ts(new_data,frequency = 14,start=c(ceiling((length(train.data))/14),(length(train.data)%%14 + 1)))
                             train.time <<- c(train.time, new_time)
                             train.data <<- c(train.data, new_data)
                             if (model_type == 'arima'){
                               # ar = try(Arima(x = new_ts, model = model[['arima']]), silent = T)
                               ar = try(Arima(train.data, model = model[['arima']]), silent = T)
                               if (!inherits(ar, 'try-error')) {model[['arima']] <<- ar} else {
                                 cat('\n', 'WARNING: Arima Model failed to update! Training a new model...')
                                 train(time = train.time, data = train.data, model_type = model_type)
                               }
                             } else {
                               train(time = train.time, data = train.data, model_type = model_type)
                             }
                           },

                           evaluate = function(time, data, model_type = 'mean'){
                             assert(length(time) == length(data), "time and data must have the same lengths", match.call()[[1]])
                             res = predict(time, model_type = model_type)
                             list(estimated.error = max(res$serr, na.rm = T), actual.error = sqrt(mean((res$pred - data)^2)))
                           }
                             ))

# Definition of TIME.SERIES class
#' A Reference Class representing a time series.
#'
#' @field N.int An integer representing the number of time intervals in the time series
#' @field ctn An integer representing the current time interval number
#' @field ctn An integer representing the current time interval number
#' @field stn An integer representing the starting time interval number of the control window
#' @field time Vector of class timeDate containing the time stamps of the time series
#' @field data A data.frame with the same number of rows as fields \code{time} containing the time series data
#'
#' @export TIME.SERIES
#' @exportClass TIME.SERIES
TIME.SERIES <- setRefClass("TIME.SERIES",
                           fields = list(
                             ID        = "character",
                             name      = "character",
                             N.int     = "integer",
                             ctn       = "integer",
                             stn       = "integer",
                             etn       = "integer",
                             zone      = "character",
                             center    = "character",
                             time      = "timeDate",
                             data      = "data.frame",
                             forecast  = "list",
                             agg.rule  = "data.frame"
                           ),

                           methods = list(
                             initialize = function(dataset = NULL, time_col = NULL, timeset = NULL, name = '', ID = '',
                                                   format = NULL, zone = "GMT", center = "GMT", set_time_labels = T, sort_time = F, ...){
                               # format = "%d/%m/%Y"
                               callSuper(...)

                               # Error messages:
                               err.source = match.call()[[1]]
                               err.msg.1 = make.err.msg("Argument 'dataset' cannot be converted to a data.frame", err.source)
                               err.msg.2 = make.err.msg("Argument 'timeset' cannot be converted to a timeDate", err.source)
                               err.msg.3 = make.err.msg("Dimentionality mismatch: Arguments 'timeset' and 'dataset'", err.source)

                               name <<- name
                               ID   <<- ID

                               dataset = try(as.data.frame(dataset))
                               verify(dataset, err_msg = err.msg.1)

                               # Manage argument timeset if not specified:
                               if (is.null(timeset)){
                                 # if 'timeset' is not specified,
                                 # Don't we have anything in dataset?
                                 if (dim(dataset)[2] == 0){
                                   # when both time and data are NULL, a blank object is created!
                                   time <<- timeDate()
                                   data <<- data.frame()
                                 } else if (is.null(time_col)){
                                   # When timeset is NULL, and time_col is NULL, rownames of dataset is considered as timeset
                                   timeset = rownames(dataset)
                                   dataset = dataset
                                 } else {
                                   # When timeset is NULL, the column specified by time_col considered as timeset
                                   tcoln   = which(colnames(dataset) == time_col)
                                   assert(length(tcoln) == 1, "TIME.SERIES$initialize(): Given time_col should refer to a single column in dataset" )
                                   timeset = dataset[,  tcoln]
                                   dataset = dataset[,- tcoln, drop = F]
                                 }
                               }

                               tt = as.time(timeset, target_class = "timeDate", format = format, zone = zone)
                               # tt = as.time(timeset, target_class = "timeDate", format = format)
                               finCenter(tt) <- center

                               time  <<- tt
                               N.int <<- length(time)

                               if (is.empty(dataset)){dataset = as.data.frame(matrix(nrow = N.int, ncol = 0))}
                               else {assert(dim(dataset)[1] == N.int, err.msg.3)}
                               # rownames(dataset) <- as.character(time, zone = zone, FinCenter = center)

                               data <<- dataset

                               ctn <<- as.integer(1)
                               stn <<- as.integer(1)
                               etn <<- N.int

                               zone   <<- zone
                               center <<- center

                               if (set_time_labels){set.time.labels()}

                               agg.rule <<- data.frame(figures = colnames(data), functions = rep('mean',dim(data)[2]), stringsAsFactors = F)
                               if (!is.empty(agg.rule)){
                                 rownames(agg.rule) <<- paste0(agg.rule$functions, '.', agg.rule$figures)
                               }

                               if (sort_time){sort.by.time()}
                             },

                             # todo: add sort.by.figure()
                             sort.by.time = function(){
                               "Sorts the data ascendingly based on time"
                               sorted <- order(time)
                               time  <<- time[sorted]
                               if (!is.empty(data)) {data  <<- data[sorted,, drop = F]}
                             },

                             # This function removes rows associated with duplicated times in a TIME.SERIES object
                             remove.duplicated = function(){
                               x   = as.character(time)
                               tbd = duplicated(x) # tbd: to be deleted
                               time <<- time[!tbd]
                               data <<- data[!tbd, ]
                             },

                             # removes missing values from time series
                             # if give_missing is TRUE, then returns a boolean flag indicating which rows are deleted
                             remove.missing = function(figures = names(data), give_missing = F){
                               ISNA     = is.na(data[, figures, drop = F])
                               X        = rowSums(ISNA)
                               misindex = (X > 0)
                               data  <<- data[!misindex, ]
                               time  <<- time[!misindex, ]
                               fix()
                               if (give_missing){return(misindex)}
                             },

                             # converts time series to regular periodic basis and returns the periodic hourly time series
                             as.periodic = function(interval = "weeks", ...){
                               obj = .self$copy()
                               obj$append.interval.start(interval = interval, field_name = 'time')
                               D = obj$aggregate.cat(category = 'time', ...)
                               TIME.SERIES(dataset = D)
                             },

                             reset = function(){stn <<- ctn},

                             append.interval.start = function(interval = 'week', field_name = interval %++% '.Start', labels = NULL, custom_starts = NULL, ...){
                               verify(interval, 'character', domain = c("sec", "min", "hour", "day", "DSTday", "week", "fortnight", "month", "quarter", "year", "custom"), varname = "interval", null_allowed = F)

                               tt = as.POSIXct(time)
                               if (interval == 'custom'){
                                 custom_starts = as.time(verify(custom_starts, valid.time.classes, null_allowed = F, varname = 'custom_starts'), target_class = 'POSIXlt')
                                 labels        = verify(labels, 'character', lengths(length(custom_starts)), default = custom_starts, varname = 'labels')
                                 fint = findInterval(tt, custom_starts)
                                 fint[fint == 0] <- NA
                                 append(labels[fint], field_names = field_name)
                               } else {append(cut(tt, breaks = interval, labels = labels, ...), field_names = field_name)}
                             },

                             append = function(fields, field_names = NULL){
                               "
                               Appends Vector or data.frame or matrix to the data
                               Arguments:
                               - field Vector or data.frame. length or dim must match
                               - field.names [Optional] Vector of character strings containing the column names to be appended
                               "
                               # todo: add verifications
                               old.ns = names(data)
                               data <<- cbind(data, fields)
                               if (!is.null(field_names)){names(data) <<- c(old.ns, field_names)}
                             },

                             feed = function(dataset){
                               dates = intersect(rownames(dataset), rownames(data))
                               for (i in colnames(dataset)){data[dates, i] <<- dataset[dates, i]}
                             },

                             set.time.labels = function(){
                               rownames(data)  <<- time2Char(time, make_unique = T)
                             },

                             updateForecast = function(figures, model_type = 'arima', vf = T){
                               "
                               Trains the forecast model using the latest (based on the current time) history data for the given figures
                               "
                               # Verifications:
                               if (vf){
                                 nms = numFigs()
                                 assert(length(nms) > 0, "The time series has no numeric figures to be forecasted!", match.call()[[1]])
                                 figures = verify(figures, 'character', domain = nms, default = nms[1], varname = 'figures')
                               }

                               for (fig in figures){
                                 if (!inherits(forecast[[fig]], 'TSP.MODEL')){forecast[[fig]] <<- TSP.MODEL(seasonalities = character())}
                                 tspm = forecast[[fig]]
                                 if ((!is.null(tspm$model[[model_type]])) & (length(tspm$train.time) > 0)) {
                                   curtime = now()
                                   maxtime = max(tspm$train.time)

                                   if (curtime == maxtime){return(NULL)}
                                   else if (curtime > maxtime){
                                     new_period = which(time[sequence(ctn)] > max(tspm$train.time))
                                     cat('Updating forecast model ', fig, ' from ', as.character(time[min(new_period)]), ' until ', as.character(time[max(new_period)]), ' ... ')
                                     tspm$update(new_time = time[new_period], new_data = data[new_period, fig])
                                     cat('Done! \n ')
                                   } else {
                                     cat('Training forecast model ', fig, ' from ', as.character(time[1]), ' until ', as.character(time[ctn]), ' ... ')
                                     tspm$train(time = time[sequence(ctn)], data = data[sequence(ctn), fig], model_type = model_type)
                                     cat('Done! \n ')
                                   }
                                 } else {
                                   cat('Training forecast model ', fig, ' from ', as.character(time[1]), ' until ', as.character(time[ctn]), ' ... ')
                                   tspm$train(time = time[sequence(ctn)], data = data[sequence(ctn), fig], model_type = model_type)
                                   cat('Done! \n ')
                                 }
                               }
                             },

                             predictNext = function(N = 1, figure = NULL, model_type = 'arima', vf = T){
                               "
                               Predicts 'N' future values of the 'figure' using a trained model
                               "
                               # Verifications:
                               if (vf){
                                 nmsfcst = names(forecast)
                                 assert(length(nmsfcst) > 0, "No forecast model has been trained. Use updateForecast() first!", match.call()[[1]])
                                 figure = verify(figure, 'character', domain = nmsfcst, lengths = 1, default = nmsfcst[1], varname = 'figure')
                                 verify(N, c('integer', 'numeric'), varname = 'N', null_allowed = F)
                               }

                               N = min(N.int - ctn, N)
                               res = forecast[[figure]]$predict(time = time[ctn + 1:N], model_type = model_type)
                               return(res)
                             },

                             aggregate.cat = function(period = stn:ctn, figures = NULL, cat_figs = NULL, func = mean, rownames = T){
                               # Verifications:
                               cat_figs = verify(cat_figs, c('character', 'factor', 'integer'), domain = catFigs(), default = catFigs[1], varname = 'cat_figs')
                               figures  = verify(figures, 'character', domain = numFigs(), default = numFigs[1], varname = 'figures')
                               cat_figs = unique(cat_figs)
                               figures  = unique(figures)

                               by = list()
                               for (cf in cat_figs){
                                 by[[cf]] <- data[period, cf]
                               }

                               D = aggregate(data[period, figures, drop = F], by = by, FUN = func)
                               if (rownames){
                                 rownames(D)  <- apply(D[, cat_figs] %>% mutate_if(is.factor, as.character), 1, paste, collapse = '-')
                                 D[,cat_figs] <- NULL
                               }
                               return(D)
                             },

                             aggregate.rule = function(period = stn:etn, agg_rule = agg.rule){
                               # todo: add verifications for argument agg_rule
                               agg_rule$functions = tolower(agg_rule$functions)
                               agg_rule$figures   = tolower(agg_rule$figures)
                               rownames(agg_rule) = agg_rule$figures

                               nc      = dim(agg_rule)[1]
                               df      = data.frame.na(nrow = 1, ncol = nc, col_names = rownames(agg_rule))
                               tt      = time[period]

                               if (length(tt) > 0){
                                 for (j in 1:nc){
                                   # agg_rule$functions can contain all descriptive stats: mean, median, sd, var, max, min, low.quar, high.quar, count, histogram, ...
                                   v = data[period, agg_rule$figures[j]]
                                   switch(agg_rule$functions[j],
                                          "mean"  = {u = try(mean(v))},
                                          "sum"   = {u = try(sum(v))},
                                          "max"   = {u = try(max(v))},
                                          "min"   = {u = try(min(v))},
                                          "most.frequent" = {u = try(most.common(v))},
                                          "count" = {u = try(length(v))})

                                   verify(u, err_msg = "The function in the agg_rule can not be applied on its associated figure")
                                   df[1, j] <- u
                                 }
                               }
                               return(df)
                             },

                             aggregate.rule.figures = function(period = sequence(ctn), agg_rule = agg.rule){
                               # todo: add verifications for argument agg_rule
                               agg_rule$functions = tolower(agg_rule$functions)
                               agg_rule$figures   = tolower(agg_rule$figures)
                               rownames(agg_rule) = agg_rule$figures

                               nc      = dim(agg_rule)[1]
                               df      = data.frame.na(nrow = 1, ncol = nc, col_names = rownames(agg_rule))
                               tt      = time[period]

                               if (length(tt) > 0){
                                 for (j in sequence(nc)){
                                   # agg_rule$functions can contain all descriptive stats: mean, median, sd, var, max, min, low.quar, high.quar, count, histogram, ...
                                   v = data[period, agg_rule$figures[j]]
                                   switch(agg_rule$functions[j],
                                          "mean"  = {u = try(mean(v))},
                                          "sum"   = {u = try(sum(v))},
                                          "max"   = {u = try(max(v))},
                                          "min"   = {u = try(min(v))},
                                          "most.frequent" = {u = try(most.common(v))},
                                          "count" = {u = try(length(v))})

                                   verify(u, err_msg = "The function in the agg_rule can not be applied on its associated figure")
                                   df[1, j] <- u
                                 }
                               }
                               return(df)
                             },

                             # argument y must be an object of class TIME.SERIES
                             aggregate.rule.in = function(obj){
                               y  = obj$copy()
                               tt = time[]
                               df = data.frame.na(nrow = 0, ncol = dim(agg.rule)[1], col_names = agg.rule$figures)
                               for (i in sequence(y$N.int)){
                                 if (i == y$N.int){indx.i = (tt >= y$time[i])}
                                 else {indx.i = (tt < y$time[i + 1]) & (tt >= y$time[i])}
                                 df = rbind(df, aggregate.rule(indx.i, agg.rule))
                               }

                               y$data  = cbind(y$data, df)

                               return(y)
                             },

                             extract = function(period = stn:etn, figures = colnames(data), ...){
                               TIME.SERIES(dataset = data[period, figures], timeset = time[period], center = center, ...)
                             },

                             to.data.frame = function(period = sequence(N.int), figures = sequence(ncol(data)), time_class = 'timeDate', timeFieldName = 'Time'){
                               verify(time_class, 'character', domain = valid.time.classes, varname = 'time_class')
                               df = data[period, figures, drop = F]
                               if (!is.null(time_class)){df[, timeFieldName] <- as.time(time[period], time_class)}
                               N  <- ncol(df)
                               df <- df[, c(N, sequence(N-1))]
                               return(df)
                             },

                             fix = function(){
                               N.int <<- length(time)
                               if (etn > N.int){etn <<- N.int}
                               if (ctn > etn){  ctn <<- etn}
                             },
                             # The first column of df must be time, will change later
                             from.data.frame = function(df){
                               time <<- as.timeDate(df[, 1], zone = zone, FinCenter = center)
                               data <<- df[-1]
                               fix()
                             },

                             remove.figures = function(figures = colnames(data)){
                               verify(figures, 'character', varname = 'figures')
                               NC = which(colnames(data) %in%  figures)
                               if (length(NC) > 0){data <<- data[, - NC, drop = F]}
                             },

                             catFigs  = function(){return(nominals(data))},

                             numFigs      = function(){return(numerics(data))},

                             from.time.series = function(tsr){
                               time <<- tsr$time
                               data <<- tsr$data
                               fix()
                             },

                             time.number = function(Time){
                               if (inherits(Time, c('integer', 'numeric'))){
                                 if (Time > N.int){Time = N.int}
                                 if (Time < 1){Time = 1}
                                 return(as.integer(Time))
                               } else {Time = as.time(Time, target_class = 'timeDate')}

                               if (Time > max(time)){Time = max(time)}
                               if (Time < min(time)){Time = min(time)}
                               return(which(time >= Time)[1])
                             },

                             goto = function(Time){
                               ctn <<- time.number(Time)
                               if (stn > ctn){stn <<- ctn}
                             },

                             jump = function(N = 1){
                               goto(ctn + N)
                             },

                             now = function(){
                               return(time[ctn])
                             },

                             # Returns the moving average of the figure
                             # todo: put high.pass.mean threshold in the settings default is NA which means all values are accepted
                             mov.avr = function(figure, ...){
                               return(high.pass.moving.mean(v = data[sequence(ctn), figure], ...))
                             },

                             timeBreak.doy = function(figure, years = NULL, labels = NULL, pretty = T, sort.rows = T, year.start = '01-01'){
                               # Verifications:
                               verify(year.start, 'character', lengths = 1, varname = 'character', null_allowed = F)
                               assert(inherits(try(as.Date(paste('2015', year.start, sep = '-'))), 'Date'), "Argument 'year.start' must be in format: 'mm-dd'", match.call()[[1]])
                               append.interval.start(interval = "custom" , field_name = 'Year.Start', custom_starts = paste((year(min(time))-1):year(max(time)), year.start, sep = "-"))
                               factorize('Year.Start')

                               ys      = ifelse(year.start == '01-01', '', 'FY') %++% substr(as.character(unique(data$Year.Start)),1,4)
                               labels  = verify(labels, 'character', lengths = length(ys), default = ys, varname = 'labels')
                               years   = verify(years,  'character', default = labels, varname = 'years')

                               levels(data$Year.Start) <<- labels

                               ys    = years %^% labels

                               Z = data.frame()

                               for (i in ys){
                                 Zi = data[data$Year.Start == i, figure, drop = F]
                                 rows <- substr(rownames(Zi),6,10)
                                 Z[rows,as.character(i)] = Zi[, 1]
                               }
                               tc = rownames(Z)
                               if (sort.rows){
                                 tc = sort(tc)
                                 tc = c(tc[tc >= year.start],tc[tc < year.start])
                                 Z  = Z[tc, , drop = F]
                               }
                               if (pretty){
                                 tc = substr(tc, 4,5) %++% ' ' %++% mntlabel[as.integer(substr(tc, 1,2))]
                                 rownames(Z) <- tc
                               }
                               data$Year.Start <<- NULL
                               return(Z)
                             },

                             timeBreak.woy = function(figure, years = NULL, labels = NULL, year.start = '01-01', func = mean){
                               # Verifications:
                               verify(year.start, 'character', lengths = 1, varname = 'character', null_allowed = F)
                               assert(inherits(try(as.Date(paste('2015', year.start, sep = '-'))), 'Date'), "Argument 'year.start' must be in format: 'mm-dd'", match.call()[[1]])
                               append.interval.start(interval = "custom" , field_name = 'Year.Start', custom_starts = paste((year(min(time))-1):year(max(time)), year.start, sep = "-"))
                               factorize('Year.Start')
                               ys      = ifelse(year.start == '01-01', '', 'FY') %++% substr(as.character(unique(data$Year.Start)),1,4)
                               labels  = verify(labels, 'character', lengths = length(ys), default = ys, varname = 'labels')
                               years   = verify(years,  'character', default = labels, varname = 'years')
                               levels(data$Year.Start) <<- labels
                               ys    = years %^% labels

                               append.interval.start(interval = "week" , field_name = 'Week.Start')

                               Z = data.frame()

                               for (i in ys){
                                 Zi = data[data$Year.Start == i, figure, drop = F]
                                 ws = data$Week.Start[data$Year.Start == i]
                                 Zi = aggregate(Zi, by = list(ws), FUN = func)
                                 rows <- strftime(Zi$Group.1, format="%W")
                                 Z['Week ' %++% rows, as.character(i)] = Zi[, 2]
                               }
                               data$Week.Start <<- NULL
                               data$Year.Start <<- NULL
                               return(Z)
                             },

                             timeBreak.moy = function(figure, years = NULL, labels = NULL, year.start = '01-01', func = mean){
                               # Verifications:
                               verify(year.start, 'character', lengths = 1, varname = 'character', null_allowed = F)
                               assert(inherits(try(as.Date(paste('2015', year.start, sep = '-'))), 'Date'), "Argument 'year.start' must be in format: 'mm-dd'", match.call()[[1]])
                               append.interval.start(interval = "custom" , field_name = 'Year.Start', custom_starts = paste((year(min(time))-1):year(max(time)), year.start, sep = "-"))
                               factorize('Year.Start')
                               ys      = ifelse(year.start == '01-01', '', 'FY') %++% substr(as.character(unique(data$Year.Start)),1,4)
                               labels  = verify(labels, 'character', lengths = length(ys), default = ys, varname = 'labels')
                               years   = verify(years,  'character', default = labels, varname = 'years')
                               levels(data$Year.Start) <<- labels
                               ys    = years %^% labels

                               append.interval.start(interval = "month" , field_name = 'Month.Start')

                               Z = data.frame()

                               for (i in ys){
                                 Zi = data[data$Year.Start == i, figure, drop = F]
                                 ms = data$Month.Start[data$Year.Start == i]
                                 Zi = aggregate(Zi, by = list(ms), FUN = func)
                                 rows <- months(as.Date(Zi$Group.1))
                                 Z[rows, as.character(i)] = Zi[, 2]
                               }
                               data$Week.Start <<- NULL
                               data$Year.Start <<- NULL
                               return(Z)
                             },

                             average = function(peri = stn:ctn, figures = numFigs()){
                               colMeans(data[peri, figures, drop = F], na.rm = T)
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



                             # Prediction methods:


                             forecast.arima = function(figure, jumper = 1, from = 2, until = N.int, weight = 360){
                               assert(require(forecast), "Package 'forecast' is not installed!", err_src = match.call()[[1]])
                               if (from < 2){from = 2}
                               if (until > N.int){until = N.int}
                               ar = NULL

                               i  = from
                               PU = rep(NA, N.int)
                               PE = rep(NA, N.int)
                               while (i < until + 1){
                                 cat(i,'- ')
                                 if (i > weight){st = i - weight} else {st = 1}
                                 x = data[st:(i-1) ,figure]

                                 if (i < 14){
                                   PU[i] = mean(x)
                                   PE[i] = sd(x)
                                   fw = 1
                                 } else {
                                   # tsm   = ts(x, frequency = 14, start = c(1, 1))
                                   # ar    = try(arima(tsm, order = c(7,1,7), optim.control = list(maxit = 1000), method="ML"), silent = TRUE)
                                   ar    = try(forecast::Arima(x, order = c(3,1,3), seasonal = list(order = c(1,1,1), period = 14), optim.control = list(maxit = 1000), method="ML", model = ar), silent = TRUE)

                                   if (inherits(ar, "try-error")){
                                     PU[i] = mean(x, na.rm = T)
                                     PE[i] = sd(x, na.rm = T)
                                     fw = 1
                                   } else {
                                     fw = min(jumper, N.int - i + 1)
                                     res = predict(ar, fw)
                                     PU[i:(i + fw - 1)] = res$pred
                                     PE[i:(i + fw - 1)] = res$se
                                   }
                                 }
                                 i = i + fw
                               }
                               as.char.time = time2Char(time[from:until], make_unique = T)
                               forecast[[figure]] <<- TSP.MODEL()
                               forecast[[figure]]$pred[as.char.time] <<- PU[from:until]
                               forecast[[figure]]$serr[as.char.time] <<- PE[from:until]
                               cat('\n')
                             },

                             factorize = function(figures){
                               for (fig in figures){data[,fig]  <<- as.factor(data[,fig])}
                             },

                             extend = function(N = NULL, until = NULL, period = 'DSTday', dataset = data.frame()){
                               until = as.timeDate(until)
                               ptset = timeSequence(from = time[N.int], to = until, by = period, FinCenter = center, zone = center, length.out = N)
                               ptset = ptset[-1]
                               if (is.empty(dataset)){
                                 dataset = data.frame.na(nrow = length(ptset), ncol = dim(data)[2])
                                 colnames(dataset) = names(data)
                                 rownames(dataset) = time2Char(ptset)
                                 # todo: support different number of rows and columns
                               } else {
                                 assert((dim(dataset)[1] == length(ptset)) & dim(dataset)[2] == dim(data)[2],
                                        "Given dataset does not match", match.call()[[1]])
                               }

                               time  <<- c(time, ptset)
                               data  <<- rbind(data, dataset)
                               N.int <<- length(time)
                             },

                             characterize = function(figures){
                               for (fig in figures){data[,fig]  <<- as.character(data[,fig])}
                             },

                             #     # Argument figure: Character contains the name of the column in data on which the extraction is applied.
                             #     #                  figure should refer to a categorical column (i.e: logical, character, factor)
                             #     # Argument values: vector of the same type of the specified column.
                             #     #                  values in the column specified by 'figure' to match for filter pass
                             #     # returns a filtered object of the TIME.SERIES
                             #     filter.on.figure = function(figure, values)

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
                               U = data[, figures] %>% mutate(dateTimeVar = as.Date(time)) %>%
                                 reshape2::melt(id = 'dateTimeVar', variable.name = idVarName)
                               gvisMotionChart(U, idvar = idVarName, timevar = 'dateTimeVar', sizevar = 'value', options = list(state = stateSettings))
                             }, # Regular Motioan Chart, remember googleVis motionchart only accepts date or numeric as time

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

                                                              gvisCalendar(to.data.frame(period, figure, time_class = 'Date'), datevar = "time", numvar  = figure, options = config, ...)
                                                            })})
                             },

                             plot.history = function(period = stn:ctn, figures = numFigs(), package = 'dygraphs', type = 'ts.line', click_input_id = NULL, config = NULL, ...){
                               type = tolower(type)
                               verify(type, 'character', domain = 'ts.line', varname = 'type')
                               verify(package, 'character', c('googleVis', 'dygraphs', 'plotly'), varname = 'package')
                               verify(figures, 'character', domain = numerics(data), varname = "figures", null_allowed = F)
                               assert(require(package, character.only = T), "Package " %++% package %++% " is not installed!", err_src = match.call()[[1]])
                               # assert(require('niravis'), "Package niravis is not installed!", err_src = match.call()[[1]])
                               switch(package,
                                      'googleVis' = {switch(type,
                                                            'ts.line' = {
                                                              # Visualization methods:
                                                              # GoogleVis AnnotatedTimeLine is ts.line for googleVis
                                                              # todo: support annotationVar, titleVar
                                                              # todo: support transfer other option fields into plot settings
                                                              DF   = data.frame(TIME = double(), VALUE = numeric(), GROUP = character())
                                                              for (var in figures){
                                                                v    = try(as.numeric(data[,var]))
                                                                verify(v, err_msg = "error_msg_1") # todo message
                                                                # todo: remove missing values
                                                                DF = rbind(DF, data.frame(TIME = as.POSIXlt(time), VALUE = v, GROUP = var))
                                                              }
                                                              if (is.null(config)){config = gglvis.tsline.settings}
                                                              if (!is.null(click_input_id)){config$gvis.listener.jscode = gglvis.click.jscript(click_input_id)}
                                                              g  = gvisAnnotatedTimeLine(DF, datevar  = "TIME", numvar   = "VALUE", idvar    = "GROUP", titlevar = "", annotationvar = "", date.format = format(DF$TIME[1]), options = config, ...)
                                                              return(g)
                                                            })},
                                      'plotly' = {switch(type,
                                                         'ts.line' = {
                                                           # todo: apply config
                                                           p  = to.data.frame(period, figures, time_class = 'POSIXct') %>%
                                                             plotly.multi(x = 'Time', y = figures, config = config, ...)
                                                           return(p)
                                                         })},
                                      'dygraphs' = {switch(type,
                                                           'ts.line' = {
                                                             D = to.data.frame(period, figures, time_class = 'character')
                                                             if (is.null(config)){config = dygraphs.tsline.settings}
                                                             d = dygraph(D, width = config$width, height = config$height, main = config$title, xlab = config$xLabel, ylab = config$yLabel, ...)
                                                             d = dygraphs.tsline.apply.settings(d, config)
                                                             if (!is.null(click_input_id)){d = d %>% dyCallbacks(clickCallback = dygraphs.click.jscript(click_input_id))}
                                                             return(d)
                                                           })})

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
setMethod("names", "TIME.SERIES", function(x) names(x$data))
setMethod("head", "TIME.SERIES", function(x, ...) head(x$data, ...))
setMethod("tail", "TIME.SERIES", function(x, ...) tail(x$data, ...))
setMethod("dim", "TIME.SERIES", function(x) dim(x$data))
setMethod("colSums", "TIME.SERIES", function(x) colSums(x$data))
setMethod("rowSums", "TIME.SERIES", function(x) rowSums(x$data))
setMethod("length", "TIME.SERIES", function(x) length(x$time))
setMethod("show", "TIME.SERIES", function(object) show(object$data))

setGeneric("duration", function(x) standardGeneric("duration"))
setMethod("duration", "TIME.SERIES", function(x) max(x$time) - min(x$time))

#' @export
row.bind = function(x, y){

  N1     = length(x$time)
  N2     = length(y$time)
  tst    = c(x$time, y$time)
  fig.1  = names(x)
  fig.2  = names(y)

  figs   = unique(c(fig.1, fig.2))
  if (!('name' %in% figs)){figs = c(figs, 'name')}
  dst    = x$data

  p1 = sequence(N1)
  p2 = (N1 + 1):(N1 + N2)

  for (fig in fig.2){dst[p2, fig]  = y$data[,fig]}

  if (!('name' %in% fig.2)){dst[p2, 'name'] = y$name}

  rownames(dst) = time2Char(tst, make_unique = T)

  # todo: support for other features, forec, zone, center and ...
  TIME.SERIES(timeset = tst, dataset = dst, ctn = length(tst), center = x$center)
}

# Functions working with TIME.SERIES objects:
#' @export
plot.TIME.SERIES = function(obj, figures = 1, period = obj$stn:obj$ctn, type = 'o', ...){
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
summary.TIME.SERIES = function(obj){
  summary(obj$data)
}


#' @export
'[.TIME.SERIES'   = function(obj, period = sequence(obj$N.int), figures = colnames(obj$data)){
  x = obj$copy()
  x$time  = x$time[period, drop = F]
  x$data  = x$data[period, figures, drop = F]
  # x$forec = x$forec[period, drop = F]
  # x$fserr = x$forec[period, drop = F]
  x$fix()

  return(x)
  # todo: pass the forec and fserr as well
  #   TIME.SERIES(dataset = obj$data[period, figures, drop = F], timeset = obj$time[period],
  #               name = obj$name, ID = obj$ID,
  #               format = obj$format, zone = obj$zone, center = obj$center,
  #               forec = obj$forec[period, figures, drop = F], fserr = obj$fserr[period, figures, drop = F],
  #               ...
  #               )
}

#' @export
'names<-.TIME.SERIES' = function(obj, value){
  colnames(obj$data)  <- value
  # colnames(obj$forec) <- value
  # colnames(obj$fserr) <- value
  return(obj)
}

#' @export
'[<-.TIME.SERIES' = function(obj, value, ...){
  obj$data[...] <- value
  return(obj)
}

#' @export
'+.TIME.SERIES' = function(obj1, obj2, ...){
  # First of all determine which columns should be added

  common.cols   = intersect(names(obj1), names(obj2))
  cols.tb.added = c()
  for (fig in common.cols){
    flag = (class(obj1$data[,fig]) %in% valid.addables.classes) &
      (class(obj2$data[,fig]) %in% valid.addables.classes)
    if (flag){cols.tb.added = c(cols.tb.added, fig)}
  }
  if (length(cols.tb.added) == 0){return(NA)}
  x      = obj1$copy()
  if (identical(obj1$time, obj2$time)){
    x$data = obj1$data[cols.tb.added] + obj2$data[cols.tb.added]
  } else{
    d1 = obj1$to.data.frame(sequence(obj1$N.int), cols.tb.added)
    d2 = obj2$to.data.frame(sequence(obj2$N.int), cols.tb.added)
    dm = merge(d1, d2, by = 'time', all = T)
    dm = dm[,paste0(cols.tb.added,'.x')] + dm[,paste0(cols.tb.added,'.y')]
    x$from.data.frame(dm)
  }
  return(x)
}

#' Column binds two time series objects. If the time stamps are not identical,
#' the right side object will be aggregated into the left side object according to property \code{agg.rule} of the right object
#' @param obj1 A TIME.SERIES object
#' @param obj2 A TIME.SERIES object
#' @return The combined TIME.SERIES object
#' @export
'&&.TIME.SERIES' = function(obj1, obj2){
  x       = obj1$copy()
  if (length(x) == 0){x$from.time.series(obj2)}
  else if (identical(x$time, obj2$time)){
    x$data  = cbind(x$data, obj2$data)
  } else {x = obj2$aggregate.rule.in(x)}
  return(x)
}

#' Column binds and merges two time series objects
#' @param obj1 A TIME.SERIES object
#' @param obj2 A TIME.SERIES object
#' @return The merged TIME.SERIES object
#' @export
'&.TIME.SERIES' = function(obj1, obj2){
  x       = obj1$copy()
  if (length(x) == 0){x$from.time.series(obj2)}
  else if (identical(x$time, obj2$time)){
    x$data  = cbind(x$data, obj2$data)
    # x$forec = cbind(x$forec, obj2$forec)
    # x$fserr = cbind(x$fserr, obj2$fserr)
  } else {
    # Merge Scenario: todo: copy is not required here
    d1 = x$to.data.frame(sequence(x$N.int), )
    d2 = obj2$to.data.frame(sequence(obj2$N.int), )
    dm = merge(d1, d2, by = 'time', all = T)
    x$from.data.frame(dm)
  }

  return(x)
}

#' @export
'|.TIME.SERIES' = function(obj1, obj2){
  row.bind(obj1, obj2)
}

#' @export
TIME.SERIES.PERIODIC <- function(from, until, period = "hours", ...){
  ptset = timeSequence(from = from, to = until, by = period)
  TIME.SERIES(timeset = ptset, ...)
}

#' @export
as.TIME.SERIES = function(x, start = Sys.Date(), freq = 'day', ...){
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


# TIME.SERIES.PERIODIC <- setRefClass("TIME.SERIES.PERIODIC", contains = "TIME.SERIES",
#                                     fields = list(
#                                       period = "numeric"
#                                     ),
#                                     methods = list(
#                                       initialize = function(from, until, period_str = "hours", ...){
#
#                                         ptset = timeSequence(from = from, to = until, by = period_str)
#                                         callSuper(timeset = ptset, ...)
#                                         period <<- switch(period_str,
#                                                           "hours" = {3600},
#                                                           "days"  = {24*3600})
#                                         date.strs <- as.character(ptset, zone = zone, FinCenter = center)
#                                         rownames(data) <<- date.strs
#                                         rownames(forec) <<- date.strs
#                                         rownames(fserr) <<- date.strs
#                                       }
#                                     )
#)



