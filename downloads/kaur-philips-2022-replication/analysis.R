# Article: A Moveable Benefit? Spillover Effects of Quotas on Women's Numerical Representation
# Authors: Komal Preet Kaur, Andrew Q. Philips


#* install libraries ----
library(pacman)
p_load(foreign, haven, tidyverse, stargazer, lmtest, plm, texreg, did, estimatr, modelsummary,staggered)

#* dependent variables:
# DV-1: %women candidates (pc.wom)
# DV-2: % races with at least one woman candidate (prob.con)
# DV-3: vote share % (vote.share.wom)
# DV-4: % women winners  (won.wom.overall)
# DV-5: % races with women winners (prob.con.won)

#* reading data ----
grp.both <- readRDS("grpboth.rds", refhook = NULL) #for DV-1 (pc.wom), 3 (vote.share.wom), 4 (won.wom.overall)
grp.both.con <- readRDS("grpbothcon.rds", refhook = NULL) #for DV-2 (prob.con), 5 (prob.con.won)

#* table-1 ----
c1 <- lm(pc.wom ~ treat50_post + pc01_state_name-1 + election_counter-1, data = grp.both)
c2 <- lm(pc.wom ~ treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both)
c3 <- lm(prob.con ~ treat50_post + pc01_state_name-1 + election_counter-1, data = grp.both.con2)
c4 <- lm(prob.con ~ treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both.con2)

stargazer(c1,c2,type="text", 
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("\\% Woman candidates"),
          covariate.labels = c('50\\% reservation', 'Treated state'),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y'), c('Election FE', 'Y', 'Y'), c('Treated state x Election trend', '', 'Y')),
          se = starprep(c1,c2, clusters = grp.both$pc01_state_name,se_type="stata"))

stargazer(c3,c4,type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("\\% Races with at least one woman candidate"),
          covariate.labels = c('50\\% reservation', 'Treated state'),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y'), c('Election FE', 'Y', 'Y'), c('Treated state x Election trend', '', 'Y')),
          se = starprep(c3,c4, clusters = grp.both.con2$pc01_state_name,se_type="stata"))


#* table- 2 ----
c5 <- lm(vote.share.wom ~ treat50_post + pc01_state_name-1 + election_counter-1, data = grp.both)
c6 <- lm(vote.share.wom ~ treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both)
c7 <- lm(won.wom.overall ~ treat50_post + pc01_state_name-1 + election_counter-1, data = grp.both)
c8 <- lm(won.wom.overall ~ treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter -1, data = grp.both)

stargazer(c5,c6,c7,c8, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("Vote share", "\\Winners"),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y','Y','Y'), c('Election FE', 'Y', 'Y','Y','Y'), c('Treated state x Election trend', '', 'Y', '','Y')),
          se = starprep(c5,c6,c7,c8, clusters = grp.both$pc01_state_name,se_type="stata"))


c9 <- lm(prob.con.won ~ treat50_post + pc01_state_name-1 + election_counter -1, data = grp.both.con2)
c10 <- lm(prob.con.won ~ treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both.con2)
stargazer(c9,c10, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y'), c('Election FE', 'Y', 'Y'), c('Treated state x Election trend', '', 'Y')),
          se = starprep(c9,c10, clusters = grp.both.con2$pc01_state_name,se_type="stata"))

#*table 3 ----
#Comparing gender-based indicators in treatment and control states for showing that 50% women reservation was randomly assigned. 

# Treated states: 17
# Control: 8 #telangana is in control group but it was not a separate state in 2005-06.

library(readxl)
ttest_nfhs <- read_excel("data/ttest_nfhs.xlsx")

# tfr=total fertility rate
# sr=sex ratio
# imr=infant mortality rate
# son_pref = son preference over daughters among women
# edu12 = educated women (more than equal to 12 years)
# edu10 = women with 10+ years of edu
# emp = employment among married women


treat <- ttest_nfhs %>% 
  filter(quota_status == "treated") 

control <- ttest_nfhs %>% 
  filter(quota_status == "control") 


t.test(treat$tfr, control$tfr, paired = FALSE, 
       alternative = "two.sided",
       mu = 0, var.equal = FALSE,
       conf.level = 0.95) 

t.test(treat$imr, control$imr,paired = FALSE, 
       alternative = "two.sided",
       mu = 0, var.equal = FALSE,
       conf.level = 0.95) 

t.test(treat$sr, control$sr,paired = FALSE, 
       alternative = "two.sided",
       mu = 0, var.equal = FALSE,
       conf.level = 0.95) 

t.test(treat$son_pref, control$son_pref,paired = FALSE, 
       alternative = "two.sided",
       mu = 0, var.equal = FALSE,
       conf.level = 0.95) 

t.test(treat$edu10, control$edu10,paired = FALSE, 
       alternative = "two.sided",
       mu = 0, var.equal = FALSE,
       conf.level = 0.95) 

t.test(treat$emp, control$emp,paired = FALSE, 
       alternative = "two.sided",
       mu = 0, var.equal = FALSE,
       conf.level = 0.95) 

#########################################################
#* table S4 ----
c11 <- lm(deposit.wom ~ treat50_post + pc01_state_name-1 + election_counter-1, data = grp.both)
c12 <- lm(deposit.wom ~ treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both)

stargazer(c11,c12, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("Deposit Loss"),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y'), c('Election FE', 'Y', 'Y'), c('Treated state x Election trend', '', 'Y')),
          se = starprep(c11,c12, clusters = grp.both$pc01_state_name,se_type="stata"))


#* table S5 ----
b1 <- lm(pc.wom ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter -1, data = grp.both)
b2 <- lm(pc.wom ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both)
stargazer(b1,b2, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("\\% Women candidates"),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y'), c('Election FE', 'Y', 'Y'), c('Treated state x Election trend', '', 'Y')),
          se = starprep(b1,b2,  clusters = grp.both$pc01_state_name,se_type="stata")) 


b3 <- lm(prob.con ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter-1, data = grp.both.con2)
b4 <- lm(prob.con ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both.con2)
stargazer(b3,b4, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("\\% Races with at least one woman candidate"),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y'), c('Election FE', 'Y', 'Y'), c('Treated state x Election trend', '', 'Y')),
          se = starprep(b3,b4,  clusters = grp.both.con2$pc01_state_name,se_type="stata"))

#* table S6 ----

b5 <- lm(vote.share.wom ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter -1, data = grp.both)
b6 <- lm(vote.share.wom ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both)
b7 <- lm(deposit.wom ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter -1, data = grp.both)
b8 <- lm(deposit.wom ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both)
stargazer(b5, b6, b7,b8,type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("Vote share", "Deposit loss"),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y', 'Y','Y'), c('Election FE', 'Y', 'Y', 'Y','Y'), c('Treated state x Election trend', '', 'Y', '', 'Y')),
          se = starprep(b5, b6, b7,b8,  clusters = grp.both$pc01_state_name,se_type="stata"))

#* table S7 ----
b9 <- lm(won.wom.overall ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter-1, data = grp.both)
b10 <- lm(won.wom.overall ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both)
stargazer(b9,b10, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("\\% Women winners"),
          omit = c("pc01_state_name", "election_counter", 'treatedstate'),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y'), c('Election FE', 'Y', 'Y'), c('Treated state x Election trend', '', 'Y')),
          se = starprep(b9,b10, clusters = grp.both$pc01_state_name,se_type="stata"))

b11 <- lm(prob.con.won ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter-1, data = grp.both.con2)
b12 <- lm(prob.con.won ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = grp.both.con2)
stargazer(b11,b12, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("\\% Races with women winners"),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y'), c('Election FE', 'Y', 'Y'), c('Treated state x Election trend', '', 'Y')),
          se = starprep(b11,b12,  clusters = grp.both.con2$pc01_state_name,se_type="stata"))


#* table S8 ----
grp.both.na <- grp.both %>% drop_na() 
grp.both.con2.na <- grp.both.con2 %>% drop_na() 

l1 <- lm(pc.wom ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter -1 + l.pc.wom, data = grp.both.na)
l2 <- lm(vote.share.wom ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter -1 + l.vote.share.wom, data = grp.both.na)
l3 <- lm(deposit.wom ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter-1 + l.deposit.wom, data = grp.both.na)
l4 <- lm(won.wom.overall ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter-1 + l.won.wom.overall, data = grp.both.na)

stargazer(l1,l2,l3,l4, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("Candidates","Vote share", "Deposit loss","Winners"),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y', 'Y','Y'), c('Election FE', 'Y', 'Y', 'Y','Y')),
          se = starprep(l1,l2,l3,l4,  clusters = grp.both.na$pc01_state_name,se_type="stata"))

l5 <- lm(prob.con ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter-1 + l.prob.con, data = grp.both.con2.na)
l6 <- lm(prob.con.won ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter-1 + l.prob.con.won, data = grp.both.con2.na)
stargazer(l5, l6, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("\\% Races with at least one woman candidate", "\\% Races with women winners"),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          add.lines=list(c('State FE', 'Y','Y'), c('Election FE', 'Y', 'Y')),
          se = starprep(l5, l6, clusters = grp.both.con2.na$pc01_state_name,se_type="stata"))

#* table S9 ----
rm.kar <- grp.both %>% filter(!pc01_state_name %in% c("karnataka", "andhra pradesh"))
b2 <- lm(pc.wom ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter -1, data = rm.kar)
b6 <- lm(pc.wom ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = rm.kar)
stargazer(b2,b6, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          dep.var.labels = c("Candidates"),
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          se = starprep(b2,b6,  clusters = rm.kar$pc01_state_name,se_type="stata"))
rm.kar.con <- grp.both.con2 %>% filter(!pc01_state_name %in% c("karnataka", "andhra pradesh"))

c2 <- lm(prob.con ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter-1, data = rm.kar.con)
c6 <- lm(prob.con ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = rm.kar.con)

stargazer(c2,c6, type = "text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          omit = c("pc01_state_name", "election_counter", 'treatedstate'),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          se = starprep(c2,c6,clusters = rm.kar.con$pc01_state_name,se_type="stata"))

#* table S10 ----
b8 <- lm(vote.share.wom ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter -1, data = rm.kar)
b12 <- lm(vote.share.wom ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = rm.kar)
b14 <- lm(deposit.wom ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter -1, data = rm.kar)
b18 <- lm(deposit.wom ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = rm.kar)
b20 <- lm(won.wom.overall ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter-1, data = rm.kar)
b24 <- lm(won.wom.overall ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = rm.kar)
stargazer(b8,b12,b14,b18,b20, b24, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          se = starprep(b8,b12,b14,b18,b20, b24, clusters = rm.kar$pc01_state_name,se_type="stata"))


c20 <- lm(prob.con.won ~ treat33_post + treat50_post + pc01_state_name-1 + election_counter -1, data = rm.kar.con)
c22 <- lm(prob.con.won ~ treat33_post + treat50_post + pc01_state_name-1 + treatedstate*election_counter + election_counter-1, data = rm.kar.con)

stargazer(c20,c22, type="text",
          align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,
          omit = c("pc01_state_name", "election_counter"),
          notes= "Standard errors clustered by state are given in parentheses", notes.align = "l", notes.append = T,
          se = starprep(c20,c22, clusters = rm.kar.con$pc01_state_name, se_type="stata"))


############################################## end of the script ###############################################



