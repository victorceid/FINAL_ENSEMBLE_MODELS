CLEAN_EPIWEEK<-function(states_data=NULL,ES27=TRUE,ES64=FALSE, US_STATE=3, auto=FALSE, my_n_ahead=1){

  ######## IMPLEMENT IN OTHER MODELS ########
  all_my_models<-c()
  ######## IMPLEMENT IN OTHER MODELS ########  
    
  ES27=!ES64
  # Empty list that will be utilized later
  my_preds_list<-listenv() 
  plan(multisession,workers=1) # Not necessary on our case
  # chose the ensemble 64 or keep the ensemble 27
  if(ES27){
    pdq=c(0,1,2) # Possible ARIMA p,d,q's.
    my_order_params<-permutations(3,3,pdq, repeats.allowed = TRUE) # 27 permutations
  }
  if(ES64){
    pdq=c(0,1,2,3) # Possible ARIMA p,d,q's.
    my_order_params<-permutations(4,3,pdq, repeats.allowed = TRUE) # 64 permutations
  }
  my_preds_list[[1]]%<-% Predict_ARIMAX(states_data,US_STATE=US_STATE, my_n_ahead=my_n_ahead, look_back_amount = 104, order_params=my_order_params,auto_seasonal = FALSE, auto=auto ) %packages% "forecast" # get the predictions and quantiles
  # resolve using the future package
  suppressWarnings(invisible(resolved(my_preds_list[[1]]))) 
  #Predictions and quantiles into seperate lists
  list_all_preds<-list() 
  list_all_quantiles<- list()
  list_all_models<- list()
  list_all_preds[[1]]<- my_preds_list[[1]][[1]][[1]]# Prediction
  list_all_quantiles[[1]]<-my_preds_list[[1]][[2]][[1]]# Quantiles
  list_all_models[[1]]<-my_preds_list[[1]][[3]][[1]]# Number of models
  # Format according to predictions, quantiles, week ahead and US State index
  my_tibble_quantiles<- FormatForScoring_correct(pred_intervals=list_all_quantiles, states_data, model_name = "TestModel", my_n_week_ahead = my_n_ahead, state_number=US_STATE)
  #Format the current dataset for calculating the WIS with the score_forecasts function
  single_state_formated<-NULL
  single_state_formated<-as.data.frame(states_data[[US_STATE]])
  single_state_formated["target_variable"]<-"cases" # rename cases as target_variable
  single_state_formated["model"]<-my_tibble_quantiles[1,"model"]# insert 1 as the index in Model
  single_state_formated<- single_state_formated %>% rename_at("cases", ~'value') # rename the column named cases to values
  #####################
  # CALCULATE THE WIS #
  #####################
  my_forecast_scores<-score_forecasts(my_tibble_quantiles, single_state_formated)
  
  ########################################
  # Get the Absolute Errors by each week #
  ########################################
  # create a dataframe with dates and all cases
  all_cases<-data.frame(states_data[[US_STATE]]$cases, states_data[[US_STATE]]$target_end_date)
  colnames(all_cases)<-c("cases","Dates")
  # create a dataset with predictions and dates
  predictions<-list_all_preds[[1]]
  colnames(predictions)<-c("Dates","predictions")
  # inner_join them to get the same data
  predictions_and_cases <- inner_join(all_cases,predictions, by="Dates")
  # calculate the error.
  #######################################
  my_errors<-data.frame((predictions_and_cases$cases-expm1(predictions_and_cases$predictions)), predictions_and_cases$Dates) 
  colnames(my_errors)<-c("error","Dates") 
  
  ########################
  # GET WIS BY EACH WEEK #
  ########################
  weekly_wis<-data.frame(as.Date(c(my_forecast_scores[,"forecast_date"]$forecast_date)),c(my_forecast_scores[,"wis"]$wis))
  colnames(weekly_wis)<-c("Dates","WIS")
  #############################################################
  # Final data frame with WIS and Absolute Error by each week #
  #############################################################
  WIS_errors<-inner_join(weekly_wis,my_errors, by="Dates")
  WIS_error_models<-cbind(WIS_errors,list_all_models[[1]])
  colnames(WIS_error_models)<-c("Dates","WIS","error","Number_of_models")
  #return(WIS_MAE)
  return(WIS_error_models)
}

##################
# EPIWEEK ARIMAX #
##################

Predict_ARIMAX <- function(states_data,US_STATE=US_STATE, my_n_ahead=1, look_back_amount = 104, order_params=NULL, auto=FALSE,auto_seasonal=FALSE, test_value="Test") {
  
  single_state=list(states_data[[US_STATE]])
  
  models<-list()#models[[state]][[model for data it was trained on]]
  prediction<-list()#[[state]][[df containing date and predictions]]
  prediction_quantile<-list()
  #model_gofs<-list(list())#[[state]][[df containing goodness of fit statistics]]
  for(i in 1:1 ){ #!!!!!!! this should be i = 1
    temp_<-list()
    prediction_df<- data.frame("Prediction_For_Date"= NULL, "Prediction" = NULL)
    prediction_df_quantile<- data.frame("pi_level"= NULL, "lower" = NULL, "uppper" = NULL, "quantile"= NULL, "mid point" = NULL)
    prediction_quantile_ls<- list()
    #model_gofs_df<- data.frame("Date"= NULL, "R2"= NULL, "AIC" = NULL, "BIC" = NULL)
    
    #for(it in 1:NROW(order_params)){
    #  model_gofs[[i]][[it]]<- data.frame("Date"= NULL, "R2"= NULL, "AIC" = NULL, "BIC" = NULL)
    #}
    for(iter in  1:(NROW(single_state[[i]])-(look_back_amount))){
      sample_data<- iter:(look_back_amount+iter-1)# we may not be looking back the full time period when I split up the data????
      fit<- NULL
      model_aic_scores<-c()
      model_id<-1
      
      ############################### build the epiweeks dataframe #####################################
      
      current_state_data= data.frame(single_state[[1]][sample_data,])
      mean_data<-(current_state_data$cases[1:52] + current_state_data$cases[53:104])/2
      epiweek_dataset<-data.frame(append(mean_data,mean_data))
      exog_var<-c(epiweek_dataset[104,1], epiweek_dataset[104,1], epiweek_dataset[104,1], epiweek_dataset[104,1])
      
      ############################### build the epiweeks dataframe #####################################
      
      checker<-FALSE
      if(n_unique(log(single_state[[i]]$cases[sample_data]+1)) >10){
        for(j in 1:nrow(order_params) ){#seq_along(order_params[,1]) ){
          fit1p<-NULL
          fit<- NULL
          doh<-FALSE
          tryCatch(
            expr = {
              
              if(!auto){
                fit<-Arima(log1p(single_state[[i]]$cases[sample_data]), xreg=log1p(epiweek_dataset[,1]), order = order_params[j,], method = "CSS-ML") #, method = c("CSS") )# method=ML cause problem at order=(3,0,3)
              }
              else{
                fit<-invisible(auto.arima(log1p(single_state[[i]]$cases[sample_data]), xreg=log1p(epiweek_dataset[,1]) ,stepwise=TRUE,approximation=FALSE,
                                          seasonal=FALSE, # This will extent to SARIMA
                                          allowdrift=FALSE,
                                          trace=TRUE))
              }
              temp_[[j]]<-fit#by doing this here there if arima throws and error we still have the last working model of that parameter set
              model_aic_scores[model_id]<- fit$aic
              if(is.na(fit$aic) ){
                print("fit$aic is na")
              }
            }
            ,error = function(e){ 
            }
            
          )#end try cathc
          if(is.null(fit) || is.null(temp_[[j]])){
            temp_[[j]]<-NA
            model_aic_scores[model_id]<- NA
            checker<-TRUE
            #print("what")
          }
          else
            temp_[[j]]<-fit
          model_id<-model_id+1
          if(any(is.na(sqrt(diag(fit$var.coef))))){
          }
          
          if(auto)
            break#here for auto.arima only
        }
        
        predicted_value<- numeric(my_n_ahead)# list()
        n_models<- 0
        my_quantiles_total<-0
        pi<-numeric(my_n_ahead)
        m<- numeric(my_n_ahead)
        s<- numeric(my_n_ahead)
        model_id<-1
        
        min_aic<- min(model_aic_scores, na.rm = TRUE)
        total_aic<-sum(exp(-.5*(model_aic_scores-min_aic)), na.rm =TRUE)
        model_weights<- c()
        flu_dates<- single_state[[i]]$target_end_date[sample_data]
        last_date <- max(flu_dates)
        prediction_date <- seq.Date(from = last_date + 7 , by = "week", length.out = my_n_ahead)
        
        ########################################################################################
        ######## IMPLEMENT IN OTHER MODELS ########
        
        my_n_models<-0
        for(my_model in temp_){
          if(length(my_model)>0 && !is.na(my_model[1]) && !(is.na(my_model$aic))){
            my_n_models<-my_n_models+1 
          }}
        
        all_my_models<-append(all_my_models,my_n_models)
        
        
        pi_lower<-numeric(23*my_n_ahead)
        pi_upper<-numeric(23*my_n_ahead)
        
        ######## IMPLEMENT IN OTHER MODELS ########

        sims<-c()
        for(my_model in temp_){
          if(length(my_model)>0 && !is.na(my_model[1]) && !(is.na(my_model$aic))){
            model_weights_<- exp(-.5*(my_model$aic - min_aic))/total_aic
            predicted_value<- model_weights_*predict(my_model, n.ahead = my_n_ahead , newxreg=log1p(exog_var[my_n_ahead]+numeric(my_n_ahead)))$pred[my_n_ahead] + predicted_value
            
            
            ######## IMPLEMENT IN OTHER MODELS ########
            pi<-forecast(my_model, h = my_n_ahead, xreg=log1p(exog_var[my_n_ahead]+numeric(my_n_ahead)), level =  c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))
            pi_lower<-model_weights_*pi[["lower"]]+pi_lower
            pi_upper<-model_weights_*pi[["upper"]]+pi_upper
            ######## IMPLEMENT IN OTHER MODELS ########
            
            
            
            ### correct !!! 
            if(is.na(predicted_value[my_n_ahead])){
              print("predicted_value is na")
            }
            new.sims<-c()
            fc <- forecast(my_model, h=my_n_ahead, xreg=log1p(exog_var[my_n_ahead]+numeric(my_n_ahead)), level=99) ### forecast for 99% confidence
            m <- fc$mean[my_n_ahead]  ## fc$mean[1] or fc$mean[my_n_ahead] 
            s <- ((fc$upper[my_n_ahead]-fc$lower[my_n_ahead])/2.58/2)  # fc$upper[1] fc$lower[my_n_ahead]
            n<-ceiling(model_weights_*1e6)
            new.sims <- rnorm(n, m=m, sd=s)
            sims <- c(sims, new.sims)
            n_models<- n_models +1
            
          }
          model_id<-model_id+1
        }
        if((NROW(sample_data)+1) <= nrow(single_state[[i]])){
          tmp_df<- data.frame("Pred_For_Date"= prediction_date[my_n_ahead], "Prediction" = predicted_value[my_n_ahead])
          
          prediction_df<-rbind(prediction_df, tmp_df)
          
          # Here I define the 23 probabilities for which I want to find the quantiles
          probabilities <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
          
          # Calculating the quantiles based on the gaussian distribuition.
          my_quantiles <- quantile(sims, probs=probabilities)
          
          #my_quantiles<- qnorm(c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), m[1], s[1])
          prediction_df_quantile<- data.frame("pi_level"= NULL, "lower" = NULL, "uppper" = NULL, "quantile"= NULL, "mid point" = NULL)
          
          for(j in 1:23){ 
            
            ######## IMPLEMENT IN OTHER MODELS ########
            tmp_df_quantile<- data.frame("pi_level"= pi[["level"]][j]*.01, "lower" = pi_lower[[j+(23*(my_n_ahead-1))]], "uppper" = pi_upper[[j+(23*(my_n_ahead-1))]],"quantile"= my_quantiles[j], "point_forecast" = predicted_value[my_n_ahead])
            ######## IMPLEMENT IN OTHER MODELS ########
            #tmp_df_quantile<- data.frame("pi_level"= pi[["level"]][j]*.01, "lower" = pi[["lower"]][[j+(23*(my_n_ahead-1))]], "uppper" = pi[["upper"]][[j+(23*(my_n_ahead-1))]],"quantile"= my_quantiles[j], "point_forecast" = predicted_value[my_n_ahead])
            prediction_df_quantile<-rbind(prediction_df_quantile, tmp_df_quantile)
            
          }
          
          prediction_quantile_ls[[toString(prediction_date[my_n_ahead])]]<-prediction_df_quantile
          
        }
        
      }
      else
        print(paste0("not enough unique values ", i, " sample_data ",sample_data ) )
    }
    print("...")
    prediction[[i]]<-prediction_df
    prediction_quantile[[i]]<- prediction_quantile_ls
    
    if(i>=1)
      break;
  }
  
  ######## IMPLEMENT IN OTHER MODELS ########
  df_all_my_models<-data.frame(all_my_models)
  ######## IMPLEMENT IN OTHER MODELS ########
  return(list("Point_ForeCast "=prediction, "Quantiles"=prediction_quantile, "Number_of_models"=df_all_my_models) )
}


#############################################################
# This function formats the dataset for calculating the WIS #
#############################################################

FormatForScoring_correct <- function(pred_intervals, state_number=NULL, grouped_data, model_name, my_n_week_ahead=1, my_temporal_resolution="wk", my_target_variable="cases") {
  my_tibble<- NULL
  my_tibble<-tibble(model=c(""),forecast_date=c(""), location=c(double() ), horizon=c(double() ),
                    temporal_resolution=c(""), target_variable=c(""), target_end_date=c(as.Date(c()) ), type= c(""), quantile=c(double() ),
                    value =c(double()))
  
  for(i in 1:NROW(pred_intervals) ){
    dates_to_get<- names(pred_intervals[[i]])
    #my_location_name<- state_codes_population[state_codes_population$location==state_number,]$location_name
    my_location<-grouped_data[[state_number]]$location[1]
    
    for(dates_ in dates_to_get){
      
      my_target_end_date<-as.Date(dates_)-7
      my_tibble<- my_tibble%>%add_row(model=model_name,forecast_date=dates_, location=my_location, horizon=my_n_week_ahead,
                                      temporal_resolution=my_temporal_resolution, target_variable=my_target_variable, target_end_date=my_target_end_date, type= "point", quantile=NA,
                                      value = expm1(pred_intervals[[1]][[dates_]]$point_forecast[1]) )
      for(quantile_level in pred_intervals[[i]][dates_]){
        
        my_quantile_value<-expm1(quantile_level$quantile)
        
        my_tibble<-my_tibble%>%add_row(model=model_name,forecast_date=dates_, location=my_location, horizon=my_n_week_ahead,
                                       temporal_resolution=my_temporal_resolution, target_variable=my_target_variable, target_end_date=my_target_end_date, type= "quantile",
                                       quantile=quantile_level$pi_level, value = my_quantile_value)
      }
    }
  }
  return(my_tibble)
}

#################################
# This function gets the Absolute Error for each State and by each predicted week.
#################################

GetErrors_Each_State<- function(my_data = NULL, prediction = NULL, US_STATE = 1){
  i=US_STATE
  error_list<-list()
  prediction_row_iter<- 1
  error<-0
  error_df<- data.frame("forecast_date"= NULL, "absolute_error" = NULL)
  if(nrow(prediction[[1]]) !=0 ){
    for(j in 1:length(my_data[[i]]$cases) ){
      if(!(prediction_row_iter <= nrow(prediction[[1]]) ) )
        break
      my_row<- my_data[[i]][j,]
      #print(paste0(my_row$target_end_date," ", prediction[[i]]$Pred_For_Date[prediction_row_iter], " prediction_row_iter ", prediction_row_iter, " i", i) )
      
      if(my_row$target_end_date != prediction[[1]]$Pred_For_Date[prediction_row_iter]){
        #print(paste0(my_row$target_end_date," pred date ", prediction[[i]]$Pred_For_Date[prediction_row_iter]," i ",i," start" ) )
        
        if(year(my_row$target_end_date) != year(prediction[[1]]$Pred_For_Date[prediction_row_iter]) && j>5){
          if(year(my_row$target_end_date) > year(prediction[[1]]$Pred_For_Date[prediction_row_iter]) && j>5){
            prediction_row_iter<- prediction_row_iter + 1
          }
        }
        else{
          while(my_row$target_end_date > prediction[[1]]$Pred_For_Date[prediction_row_iter] && j>5){
            prediction_row_iter<- prediction_row_iter + 1
          }
          if(my_row$target_end_date < prediction[[1]]$Pred_For_Date[prediction_row_iter]){
            j<- j-1
          }
        }
        
        #print(paste0(my_row$target_end_date," error date ", prediction[[i]]$Pred_For_Date[prediction_row_iter]," end" ) )
      }
      
      if(my_row$target_end_date == prediction[[1]]$Pred_For_Date[prediction_row_iter]){
        #error<-mase(my_row$cases, expm1(prediction[[1]]$Prediction[prediction_row_iter]), step_size = 1)
        error<- my_row$cases - expm1(prediction[[1]]$Prediction[prediction_row_iter])
        tmp_df<- data.frame("forecast_date"= prediction[[1]]$Pred_For_Date[prediction_row_iter], "absolute_error" = error)
        error_df<-rbind(error_df, tmp_df)
        prediction_row_iter<- prediction_row_iter + 1 
      }
    }
  }
  else{
    print(paste0("nrow(prediction[[i]]) !=0 ", i) )
  }
  
  error_list[[1]]<- error_df
  return(error_list)
  
}

###############################################################
# This function combines the states data and the states codes # 
###############################################################

combining_states_data<-function(my_data=NULL, state_codes=NULL){
  
  my_data = subset(my_data, select = c(STATE,YEAR,EPI_WEEK,ILITOTAL))
  state_codes = subset(state_codes, select = c(location,location_name))
  names(state_codes)<- c('STATE_NUMBER','STATE')
  
  my_data<-cbind(my_data, MMWRweek2Date(MMWRyear=my_data$YEAR, MMWRweek=my_data$EPI_WEEK))
  
  suppressWarnings(invisible(my_data[apply(my_data, 1, purrr::compose(is.finite, all)),]))
  library(dplyr)
  
  # Joining datasets
  
  final_data <- my_data %>%
    left_join(state_codes, by = "STATE")
  
  names(final_data)<- c('state_name','MMWRyear','MMWRweek','cases','target_end_date','location')
  final_data$location<-as.numeric(final_data$location)
  final_data$cases<-as.numeric(final_data$cases)
  
  final_data$target_end_date = as.Date(final_data$target_end_date,format = "%Y/%m/%d")
  # Create a list of dataframes, split by state
  # not sure why this is used
  #grouped_by_location <- split(final_data, f = list("location"))
  final_data<-drop_na(final_data)
  grouped_data <-final_data %>% group_split(location)
  return(grouped_data)
}