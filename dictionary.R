#' ---
#' title: "Build Data Dictionary"
#' author: 
#' - "Alex F. Bokov^[UT Health, Department of Epidemiology and Biostatistics]"
#' date: "09/14/2018"
#' ---
#' 
#+ message=F,echo=F
# init ----
debug <- 0;
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
#' Saving original file-list so we don't keep exporting functions and 
#' environment variables to other scripts
.origfiles <- ls();
# read student pre-run script if it exists ----
if('pre_dictionary.R' %in% list.files()) source('pre_dictionary.R');

#+ echo=F
# read dat0 ----
#' generic read function which auto-guesses file formats:
#dat0 <- t_autoread(inputdata,file_args=file_args);
dat1 <- dat0 <- openxlsx::read.xlsx(inputdata);
dat1$id <- rep(grep('\\d+',dat1$SPIN.ID,val=T),each=8);
dat1 <- sapply(unique(dat1$id)
               ,function(xx) with(subset(dat1,id==xx)
                                  ,data.frame(id=xx
                                              ,contact=Program.Title[2]
                                              ,phone=Program.Title[3]
                                              ,email=Program.Title[4]
                                              ,sponsor_url=Program.Title[5]
                                              ,program_url=Program.Title[6]
                                              ,dates=Program.Title[7]
                                              # extract activity codes
                                              ,activity_cd=paste0(
                                                na.omit(unique(
                                                  ifelse(
                                                    grepl(
                                                      '[RUK][0-9]{2}'
                                                      ,Program.Title[c(1,8)])
                                                    ,gsub(
                                                      '^.*([RUK][0-9]{2}).*$'
                                                      ,'\\1'
                                                      ,Program.Title[c(1,8)])
                                                    ,NA))),collapse=';')
                                              ,name=Program.Title[1]
                                              ,synopsis=gsub('&#10;',''
                                                             ,Program.Title[8])
                                              ,sponsor=Sponsor.Name[1]
                                              ,sponsor_num=Sponsor.Number[1]
                                              ,next_deadline=Deadline.Date[1]
                                              ,funding=Funding.Amount[1]
                                              ,fit=if('Fit' %in% names(dat1)){
                                                Fit[1]} else NA
                                              ,stringsAsFactors = F
                                              )),simplify=F) %>% 
  do.call(rbind,.) %>% arrange(desc(fit));

id_blacklist <- if(file.exists('id_blacklist.txt')){
  read.csv('id_blacklist.txt',header=F)[,1]} else c();

#+ echo=F
# make data dictionary ----
#' ## Create the data dictionary
dct1 <- tblinfo(dat1);

#+ echo=F
# a few dat0 hacks ----
#' ## Raw Data Ops
#' 
#' Since you're messing around with the raw data anyway, if there is anything 
#' you will need later which does not depend on the processing steps in the
#' `data.R` script, you might as well get it out of the way in this section

#+ echo=F
# save out ----
#' ## Save all the processed data to an rdata file 
#' 
write.csv(dat1,'grant_opps.csv',row.names=F);
write.csv(subset(dat1,fit==3 & !as.numeric(id) %in% id_blacklist)
          ,'grant_shortlist.csv',row.names=F);
#' ...which includes the audit trail
suppressWarnings(tsave(file=paste0(.currentscript,'.rdata')
                       ,list=setdiff(ls(),.origfiles)));
#+ echo=F,eval=F
c()