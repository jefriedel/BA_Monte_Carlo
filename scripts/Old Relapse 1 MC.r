#Setup----

#Extract IVs
part_list = unique(Relapse[,"Subject"])
group_list = unique(unique(Relapse[,"Group"]))
session_list = unique(unique(Relapse[,"Session"]))
condition_list = unique(unique(Relapse[,"Condition"]))

#Log previous session----

#Prepare data frame for log proportion previous session
#Create new data frame that has temporary NaN values appended to original
#Includes corrected number of responses by adding 1 to all response counts
  #Correction is to handle 0 responses emitted which leads to problems with divide by zero
  #and problems with taking the log of 0
Relapse = data.frame(Relapse,
                     setNames(
                       data.frame(Relapse[,"Responses"]+1,
                         rep(NaN, NROW(Relapse))),
                       c("corrected_resp","log_previous")))


#Calculate and store log prop prev sess.
for(p in part_list){
  #Data location
  
  #Temporary data for ease of reading next line
  temp_resp = Relapse[Relapse$Subject==p,"corrected_resp"]
  
  
  Relapse[Relapse$Subject == p, "log_previous"] = 
                        #First session calculated as NA  
                        c(NA, log2(
                          #second session to last session  
                          temp_resp[2:max(session_list)] /
                          #divided by 1 session to penultimate session
                          temp_resp[1:max(session_list) - 1]))
}

rm(temp_resp, p)

#Number of simulations
runs = 10000
#Setup data storate
MC_means = data.frame(1:runs)
MC_sds = data.frame(1:runs)
MC_exp = NULL

#loop for simulations across groups
for(grp in group_list) {
  #Extract experimental data
  exp_data = Relapse[Relapse$Condition == "Reinstatement" &
                       Relapse$Group == grp,
                     "log_previous"]
  MC_exp[[toString(grp)]][["data"]] = exp_data
  MC_exp[[toString(grp)]][["mean"]] = mean(exp_data)
  MC_exp[[toString(grp)]][["sd"]] = sd(exp_data)
  
  #Extract log prop for group, remove first session where no log prop exists
  usable_sessions = Relapse[Relapse$Session > 1 &
                              Relapse$Group == grp, "log_previous"]
  
  #Sample from usable sessions, convert into matrix with same number of columns as exp data
  MC_matrix = matrix(sample(usable_sessions,
                            runs * NROW(exp_data),
                            replace = TRUE),
                     runs,
                     NROW(exp_data))
  
  #Calculate simulated means, store data
  MC_means = cbind(MC_means,
                   setNames(data.frame(apply(MC_matrix, 1, mean))
                            , c(toString(grp))))
  
  #Calculate simuatled SDs store data
  MC_sds = cbind(MC_sds,
                 setNames(data.frame(apply(MC_matrix, 1, sd)),
                          c(toString(grp))))
}

rm(exp_data,grp,usable_sessions,MC_matrix)

#Clear initialized values in data frames
MC_means[,1] = NULL
MC_sds[,1]= NULL

#write.csv(MC_means,"MC Means.csv")
#write.csv(MC_sds, "MC SDs.csv")

MC_exp[["MC"]]$mean = MC_means
MC_exp[["MC"]]$sd = MC_sds

for(grp in group_list){

hist(MC_means[, grp],
     breaks = 30,
     main = paste("Simulated mean log prop. previous session for",grp),
     xlab = "Bins (mean log prop.)")
    
    MC_exp[[grp]][["MC"]]$mean = MC_means[,grp]
    MC_exp[[grp]][["MC"]]$sd = MC_sds[,grp]
}

rm(list=setdiff(ls(),c("MC_exp")))

mean_sess = NULL


#Comps are Sal-Sal and Amp-Sal and Amp-Sal and Sal-Amp

comps = matrix(c(
    toString(group_list[1]),
    toString(group_list[2]),
    toString(group_list[2]),
    toString(group_list[3])
),
nrow = 2,
ncol = 2)

comp = 1

#Extract log prop for group, remove first session where no log prop exists
usable_sessions1 = Relapse[Relapse$Group == comps[comp,1], "Responses"]
usable_sessions2 = Relapse[Relapse$Group == comps[comp,2], "Responses"]

#Sample from usable sessions, convert into matrix with same number of columns as exp data
MC_matrix1 = matrix(sample(usable_sessions1,
                           runs * NROW(unique(Relapse[Relapse$Group == comps[comp, 1], "Subject"])),
                           replace = TRUE),
                    runs,
                    NROW(unique(Relapse[Relapse$Group == comps[comp, 1], "Subject"])))/30

MC_matrix2 = matrix(sample(usable_sessions2,
                           runs * NROW(unique(Relapse[Relapse$Group == comps[comp, 2], "Subject"])),
                           replace = TRUE),
                    runs,
                    NROW(unique(Relapse[Relapse$Group == comps[comp, 2], "Subject"])))/30

MC_diff = apply(MC_matrix1,1,mean)-apply(MC_matrix2,1,mean)
MC_sds = sqrt(((NROW(MC_matrix1[1,]) - 1) *
                   apply(MC_matrix1, 1, var) +
                   (NROW(MC_matrix2[1,]) - 1) *
                   apply(MC_matrix2, 1, var)) /
                  (NROW(MC_matrix1[1,]) +
                       NROW(MC_matrix2[1,]) - 2))


exp_diff =  mean(Relapse[Relapse$Group == comps[comp, 1] &
                             Relapse$Condition == "Reinstatement", "Responses"] / 30) -
    mean(Relapse[Relapse$Group == comps[comp, 2] &
                     Relapse$Condition == "Reinstatement", "Responses"] / 30)

hist(MC_diff,
     breaks = 30,
     main = paste("Mean differences", comps[comp, 1], "-", comps[comp, 2]))


#Check how to add to specific plots
points(exp_summary$`Resp`$BL$mean,750,
       pch=24,col ="red",bg="red")


