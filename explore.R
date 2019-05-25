#' ---
#' title: "Exploring NIH RePORT Data"
#' author: "Alex Bokov"
#' date: "24/05/2019"
#' ---
#' 
#+ message=F,echo=F
# init -----

# set to > 0 for verbose initialization
debug <- 0;

# vector of additional packages to install, if needed. If none needed, should be
# an empty string
.packages <- c( '' );

# name of this script
.currentscript <- "explore.R"; 

# vector of other scripts which need to run before this one. If none needed, 
# should be an empty string
.deps <- c( 'dictionary.R' ); 

# load stuff ----
# load project-wide settings, including your personalized config.R
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
# load any additional packages needed by just this script
if(length(.packages) > 1 || .packages != '') instrequire(.packages);
# start logging
tself(scriptname=.currentscript);
# track what objects here come from other scripts
.origfiles <- ls(); .loadedobjects <- c();
# run scripts on which this one depends, if any that have not been
# cached yet
if(length(.deps)>1 || .deps != ''){
  for(ii in .deps){
    .depdata <- paste0(ii,'.rdata');
    if(!file.exists(.depdata)) {
      system(sprintf('R -e "source(\'%s\')"',ii))};
    .loadedobjects <- union(.loadedobjects,tload(.depdata));
  }
}
#+ echo=F
#############################################################
# Your code goes below, content provided only as an example #
#############################################################

#' ### Data Dictionary
#' 
#' Quality control, descriptive statistics, etc.

#+ echo=F
# characterization ----
set.caption('Data Dictionary');
set.alignment(row.names='right')
.oldopt00 <- panderOptions('table.continues');
panderOptions('table.continues','Data Dictionary (continued)');
#  render the Data Dictionary table
pander(dct0[,-1],row.names=dct0$column,split.tables=Inf); 
#  reset this option to its previous value
panderOptions('table.continues',.oldopt00);

#' ### Filters
#' 
#' Unusable activity codes:
activit_xcl <- '^F|^U|^P|^R44|^T|^K99|^R00';
#' Search-replace patterns (updated FOA numbers)
foareplace <- rbind(
  cbind(c('PA-13-302','PA-16-160','PA-11-260','PA-18-484'),'PA-19-056')
  ,c('PAR-17-159','PAR-19-072')
  ,c('RFA-EB-15-006','RFA-EB-17-005')
  ,c('RFA-MH-17-257','RFA-MH-19-147')
);

foadefunct <- c('PA-14-156','PA-14-155','PAR-15-288');

#' Predictors
# Uncomment the below line after putting in the actual predictor column names
# from your dat0
#predictorvars <- c('FOO','BAR','BAZ','BAT');
#' Outcomes
# Uncomment the below line after putting in the actual outcome column names
# from your dat0
#outcomevars <- c('BAN','BAX');
#' All analysis-ready variables
# Uncomment the below line after predictorvars and outcomevars already exist
#mainvars <- c(outcomevars, predictorvars);
#' ### Ranking FOAs
#' 
#' #### Plots
dat1 <- subset(dat0,!is.na(foa) & 
                 !grepl(activit_xcl,activity) & 
                 !foa %in% foadefunct) %>% 
  mutate(foa=submulti(foa,foareplace));
dat1foa <- group_by(dat1,foa) %>% 
  summarise(score=median(as.numeric(match.score)),n=length(foa)) %>%
  subset(n>1);
ggplot(dat1foa,aes(x=reorder(foa,desc(score)),y=score
                   ,width=n/40,fill=n)) + 
  geom_bar(stat='identity',position='identity',width=0.5) +
  theme(axis.text.x= element_text(angle=90,hjust = 1)
        ,axis.title.x = element_blank()) + 
  scale_fill_gradient(low = "red", high = "green");
#geom_hline(yintercept=3,col='red');

#' #### Best fit and most common
.foacount <- top_n(dat1foa,5,n);
.foascore <- top_n(dat1foa,5,score);
set.caption('Most Promising FOAs');
set.alignment(row.names='right')
pander(foatop<-rbind(.foacount,.foascore)); 

#' 
#' 
#' To explore pairwise relationships between all variables of interest.
#+ ggpairs_plot
# Uncomment the below after mainvars already exists and you have chosen a 
# discrete variable to take the place of VAR1 (note that you don't quote that
# one)
#ggpairs(dat0[,mainvars],mapping=aes(color=VAR1));
#' ### Cohort Characterization (step 10)
#' 
#' To explore possible covariates
# Uncomment the below code after mainvars exists and you have chosen a discrete
# variable to take the place of VAR1 (this time you do quote it)
#
#pander(print(CreateTableOne(
#  vars = setdiff(mainvars,'VAR1'),strata='VAR1',data = dat0
#  , includeNA = T), printToggle=F), caption='Group Characterization');

#' ### Data Analysis
#' 
#' Fitting the actual statistical models.
#+ echo=F
# analysis ----

#+ echo=F
#############################################################
# End of your code, start of boilerplate code               #
#############################################################

# save out with audit trail ----
# Saving original file-list so we don't keep exporting functions and 
# environment variables to other scripts. Could also replace .origfiles
# with the expression c(.origfiles,.loadedobjects) and then, nothing
# get inherited from previous files, only the stuff created in this 
# script gets saved. If this doesn't make sense, don't worry about it.
tsave(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles)
      ,verbose=F);

#' ### Audit Trail
#+ echo=F
.wt <- walktrail();
pander(.wt[order(.wt$sequence),-5],split.tables=Inf,justify='left',missing=''
       ,row.names=F);
#+ echo=F,eval=F
c()
