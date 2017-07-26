
## This function calculates the VIFs for all the independent variables, 
## and if they have values bigger than the choosed limit, the function 
## removes the biggest value and calculate again. It repeates this 
## operation until all the variables have the accepted VIF.
## https://www.kaggle.com/robertoruiz/dealing-with-multicollinearity
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
    }
    
    return(names(in_dat))
  }
}



## this function ranks variable importance with mRMR package
## http://home.penglab.com/proj/mRMR/
## http://amunategui.github.io/variable-importance-shuffler/index.html
create_mRMR_varimp_df <- function(df, 
                                  predictor_vars, prediction_var, 
                                  rnd_dgt=3, feature_cnt=10) {
  
  df <- df[ , c(predictor_vars, prediction_var)]
  df <- as.data.frame(sapply(df, as.numeric))
  dd <- mRMR.data(data=df)
  feats <- mRMR.classic(data=dd, target_indices=ncol(df), feature_count=feature_cnt)
  var_imp_df <- round(data.frame('importance'=feats@mi_matrix[nrow(feats@mi_matrix), ]), rnd_dgt)
  var_imp_df$feature <- rownames(var_imp_df)
  row.names(var_imp_df) <- NULL
  var_imp_df <- na.omit(var_imp_df)
  var_imp_df <- sortByCol(var_imp_df, col='importance')
  return(var_imp_df)
}

