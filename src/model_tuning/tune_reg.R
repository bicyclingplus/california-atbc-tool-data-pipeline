# ============================================================================
# tune_reg.R  --  Tune REGULARIZATION on class accuracy
# ----------------------------------------------------------------------------
# The earlier grids never varied regularization (lambda_l1/l2, bagging, max_depth);
# these sat at LightGBM "off" defaults. Hold the already-tuned params at their
# per-model winners and grid the regularization parameters, judged by volume-class
# accuracy under spatial 5-fold CV with early stopping.
#
# Usage: Rscript tune_reg.R <A_bike|B_bike|A_ped|B_ped>
# ============================================================================
source("C:/Users/Dillon/projects/california-atbc-tool-data-pipeline/src/model_tuning/tune_config.R")
suppressMessages({library(dplyr); library(sf); library(lightgbm); library(rsample); library(purrr); library(tidyr)})
# Single source of truth for the predictor sets: modeling.R (PREDICTORS_A/B now
# include the PRISM climate features temp_min/temp_max alongside precip_annual).
source(file.path(LOCAL,"src/functions/modeling.R"))

bike <- readRDS(file.path(LOCAL,"docs/bike_train_ambient.rds"))
ped  <- readRDS(file.path(LOCAL,"docs/ped_train_ambient.rds"))
AMB  <- c("amb_strava_250m","amb_strava_500m","amb_strava_1000m","amb_strava_2000m")
# NOTE: these *_ambient.rds snapshots store RAW ambient sums, so we log1p here to
# match extract_ambient(). When regenerating snapshots from the PRISM pipeline,
# confirm whether the AMB cols are already log1p'd (extract_ambient does it) --
# if so, DROP these two lines to avoid double-logging.
bike <- bike %>% mutate(across(all_of(AMB), ~log1p(.)))
ped  <- ped  %>% mutate(across(all_of(AMB), ~log1p(.)))
PRED_A <- PREDICTORS_A; PRED_B <- PREDICTORS_B

# Already-tuned, fixed per-model params (the non-regularization winners).
fixed_params <- function(target){
  if(identical(target,"aadb")) list(tweedie_variance_power=1.9, num_leaves=95, min_data_in_leaf=50, feature_fraction=0.7)
  else                         list(tweedie_variance_power=1.6, num_leaves=95, min_data_in_leaf=20, feature_fraction=1.0)
}

score <- function(o,p){ok<-is.finite(o)&is.finite(p);o<-o[ok];p<-p[ok]
  q<-quantile(o,c(0,1/3,2/3,1),na.rm=T)
  oc<-cut(o,q,include.lowest=T,labels=c("low","mid","high"))
  pc<-cut(p,q,include.lowest=T,labels=c("low","mid","high")); pc[p>q[4]]<-"high"; pc[is.na(pc)]<-"high"
  cm<-table(oc,pc); ord<-c(low=1,mid=2,high=3)
  c(class_acc=sum(diag(cm))/sum(cm),
    off2=mean(abs(ord[as.character(oc)]-ord[as.character(pc)])==2),
    rmse=sqrt(mean((o-p)^2)))}

prep <- function(df,preds,tg){d<-df%>%select(any_of(c(tg,"spatial_id",preds)))%>%
  mutate(across(where(is.character),as.factor))%>%mutate(across(where(is.numeric),~replace_na(.,0)))%>%filter(!is.na(.data[[tg]]))
  list(x=model.matrix(~.-1,d%>%select(all_of(preds))), y=pmax(d[[tg]],0), sid=d$spatial_id)}

eval_cfg <- function(dat,fl,p){
  oof<-rep(NA,length(dat$y))
  for(s in fl){val<-s; tr<-setdiff(seq_along(dat$y),val)
    dtr<-lgb.Dataset(dat$x[tr,,drop=F],label=dat$y[tr]); dv<-lgb.Dataset(dat$x[val,,drop=F],label=dat$y[val],reference=dtr)
    m<-lgb.train(c(p, list(objective="tweedie",learning_rate=0.05,num_threads=NUM_THREADS,verbosity=-1)),
      dtr,nrounds=2000,valids=list(v=dv),early_stopping_rounds=50,verbose=-1,eval_freq=0)
    oof[val]<-predict(m,dat$x[val,,drop=F])}
  score(dat$y, pmax(oof,0))}

run <- function(df,preds,tg,label){
  dat<-prep(df,preds,tg); set.seed(123)
  fl<-map(group_vfold_cv(tibble(spatial_id=dat$sid),group=spatial_id,v=5)$splits, ~as.integer(complement(.x)))
  fp <- fixed_params(tg)
  grid<-expand.grid(lambda_l2=c(0,0.5,2,5), lambda_l1=c(0,0.5,2),
                    bagging_fraction=c(1.0,0.8,0.6), max_depth=c(-1,8))
  res<-map_dfr(seq_len(nrow(grid)), function(i){
    g<-as.list(grid[i,])
    # bagging requires bagging_freq>0 to take effect
    if(g$bagging_fraction < 1) g$bagging_freq <- 1
    s<-eval_cfg(dat,fl,c(fp,g))
    tibble(!!!grid[i,], !!!as.list(round(s,4)))})
  saveRDS(res, file.path(LOCAL,paste0("docs/reg_",label,".rds")))
  cat("\n#####",label,"##### top 8 by class_acc (baseline = no reg, first row of plain grid)\n")
  print(as.data.frame(res%>%arrange(desc(class_acc))%>%head(8)), row.names=FALSE)
  base_acc <- (res%>%filter(lambda_l2==0,lambda_l1==0,bagging_fraction==1,max_depth==-1))$class_acc
  cat("NO-REG baseline class_acc:", round(base_acc,4), "\n")
}

args<-commandArgs(trailingOnly=TRUE); w<-if(length(args)>=1) args[1] else "A_bike"
spec<-switch(w, A_bike=list(bike,PRED_A,"aadb"), B_bike=list(bike,PRED_B,"aadb"),
                A_ped=list(ped,PRED_A,"aadp"),  B_ped=list(ped,PRED_B,"aadp"), stop("bad: ",w))
cat("=== reg tune:",w,"|",format(Sys.time(),"%H:%M:%S"),"===\n")
run(spec[[1]],spec[[2]],spec[[3]],w)
cat("=== DONE",format(Sys.time(),"%H:%M:%S"),"===\n")
