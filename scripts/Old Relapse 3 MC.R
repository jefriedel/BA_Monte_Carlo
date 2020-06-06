#NOTE: Code as written assumes some prior knowledge with R and 
#general programming skills. See basic and packaged function help files for
#more specifics

#Data are from Berry et al. (2018)
#Data in long format
Relapse = read.csv("Berry Relapse.csv")

#NOTE: I made an error when originally formatting the data into long format in Excel
#Subjects were given unique ID numbers across experiments, despite being the same subjects.
#Only matters for the cross experiment comparisons. For cross experiment comparisons, 
#subject numbers are correct (it seemed easier to do at the time)

#Setup----

#Extract lists for subject numbers
sub_list = unique(Relapse[,"Subject"])

#Titled reinf/list but it is rich vs. lean component
reinf_list = unique(unique(Relapse[,"Reinf"]))

#Phase of experiment, baseline, extinction, relapse test
condition_list = unique(unique(Relapse[,"Condition"]))

#Titled group, but really ABA vs ABC renewal
group_list = unique(unique(Relapse[,"Group"]))

#How manu simulations to perform
runs = 10000

#Random seed# so that results are replicable, value obtained from Random.org
set.seed(497259230)

#Calc log prop rate of response----

#For each subject
for (s in sub_list) {
    #and by each component
    for (r in reinf_list) {
        #Get rates of response for each subject
        rates = Relapse[Relapse$Subject == s &
                            Relapse$Reinf == r, "Rate"]
        
        #Correction (see paper for discussion on correction)
        rates = rates + 1
        
        #Log rate for current subject and condition
        log_rate = c(NaN, #log rate for first session can't be calculated
                     log2(rates[2:NROW(rates)] #rate of session n
                          / rates[1:NROW(rates) - 1]))  #rate of session n -1
        
        #Store log prop. rate in data frame
        Relapse[Relapse$Subject == s &
                    Relapse$Reinf == r, "log_rate"] = log_rate
        
    }
}

rm(r,s,log_rate,rates)

#Initialize data frame for results
MC_results = list()

#Differences in rich/lean in baseline and also renewal-----

#For each experiment (ABA and ABC)
for(group in group_list) {
    
    #Get the session number for last session of baseline
    last_base = max(Relapse[Relapse$Condition == "Baseline" & #Filter for baseline
                                Relapse$Group == group, "Session"])#Filter for group,
    
    #Long formula, rates of response for rich - lean
    base_diffs = Relapse[Relapse$Group == group & #For renewal type
                             Relapse$Session == last_base & #and last baseline session
                             Relapse$Reinf == "Rich", "Rate"] - #Rich component
        Relapse[Relapse$Group == group & #For renewal type
                    Relapse$Session == last_base & #and last baseline session
                    Relapse$Reinf == "Lean", "Rate"] #lean component
    
    #calculate experimental mean difference and SD of differences
    base_mean = mean(base_diffs)
    base_sds = sd(base_diffs)
    
    #Store data in MC_results lsit
    MC_results[[group]][["Diff"]][["Base"]]$"exp_mean" = base_mean
    MC_results[[group]][["Diff"]][["Base"]]$"exp_sd" = base_sds
    
    #Same analysis as above, but for relapse testing sessions
    relapse_diffs = Relapse[Relapse$Group == group &
                                Relapse$Condition == "Renewal" &
                                Relapse$Reinf == "Rich", "Rate"] -
        Relapse[Relapse$Group == group &
                    Relapse$Condition == "Renewal" &
                    Relapse$Reinf == "Lean", "Rate"]
    
    relapse_mean = mean(relapse_diffs)
    relapse_sd = sd(relapse_diffs)
    
    MC_results[[group]][["Diff"]][["Renew"]]$"exp_mean" = relapse_mean
    MC_results[[group]][["Diff"]][["Renew"]]$"exp_sd" = relapse_sd
    
    #Code to select with replacement rates from across baseline sessions within condition
    #Creates a list that is then broken down into dyads of sessions below
    #Differences in dyads are then broken down into a matrix that has "runs" number of rows
    #by number of subjects
    current_samp = sample(Relapse[
        #Sample from raw data
        
            Relapse$Group == group &
                #Sessions in whcih it is the current renewal type
            
            Relapse$Condition == "Baseline", "Rate"],
                #and baseline sessions, get rates of response
                          
        runs * 2 * NROW(relapse_diffs),
            #Number of runs desired * 2 for dyads of responses * number of subjects
        
        replace = TRUE)
            #Sample with replacement
    
    #Take response list, break into dyads
    current_samp = matrix(current_samp,
                          nrow = runs * NROW(relapse_diffs),
                          ncol = 2)
    
    #Take mean differences between dyads
    current_samp = current_samp[, 1] - current_samp[, 2]
    
    #Create matrix of differences
    current_samp = matrix(current_samp,
                          nrow = runs,
                          ncol = NROW(relapse_diffs))
    
    #calculate mean differences
    #Apply function runs a function along data matrix by rows or columns
    mc_means = apply(current_samp, 1, mean)
    
    #Store means, calculate and store SDs
    MC_results[[group]][["Diff"]][["Sims"]]$"mc_mean" = mc_means
    MC_results[[group]][["Diff"]][["Sims"]]$"mc_sd" = apply(current_samp, 1, sd)
    
    #Simple histogram for summary
    hist(mc_means,
         breaks = 20,
         main = "Simulated differences in rate of response",
         xlab = "Mean of differences")
    
    #Add experimental difference in baseline
    points(base_mean,
           1500,
           pch = 21,
           col = "red",
           bg = "red")
    
    #Add experimental difference in relapse testing
    points(relapse_mean,
           1500,
           pch = 25,
           col = "blue",
           bg = "blue")
    
    #store how many of the simulated samples are greater than the experimental
    MC_results[[group]][["Diff"]][["Base"]]$"p" = sum(mc_means >= base_mean) /
        runs
    MC_results[[group]][["Diff"]][["Renew"]]$"p" = sum(mc_means >= relapse_mean) /
        runs
}

#Clean up temporary results
rm(
    base_diffs,
    base_mean,
    base_sds,
    group,
    last_base,
    mc_means,
    relapse_diffs,
    relapse_mean,
    relapse_sd,
    current_samp
)

#Increases in response rate for renewal - log prop rate of response----

#Similar to the analysis above for rates of response with small differences
#for log proportion rate of response

#For each type of renewal
for(group in group_list) {
    
    #Pull log rates for current renewal type
    renewal_log = Relapse[Relapse$Group == group &
                              Relapse$Condition == "Renewal", "log_rate"]
    
    #Pull log rates for rich component for all sessions with log rates
    rich_log = Relapse[Relapse$Group == group &
                           Relapse$Session > 2 &
                           Relapse$Reinf == "Rich", "log_rate"]
    
    #Pull for lean components
    lean_log = Relapse[Relapse$Group == group &
                           Relapse$Session > 2 &
                           Relapse$Reinf == "Lean", "log_rate"]
    
    
    #Random selection with replacement for rich and lean components
    #sample from rich/lean log rates, number of runs times number of subjects, with replaceemtn
    rich_sample = sample(rich_log, runs * NROW(renewal_log) / 2, replace = TRUE)
    lean_sample = sample(lean_log, runs * NROW(renewal_log) / 2, replace = TRUE)
    
    #Transform vectors into matrix runs long by subjects wide
    rich_sample = matrix(rich_sample,
                         nrow = runs,
                         ncol = NROW(renewal_log) / 2)
    lean_sample = matrix(lean_sample,
                         nrow = runs,
                         ncol = NROW(renewal_log) / 2)
    
    #COmbine samples
    full_sample = cbind(rich_sample, lean_sample)
    
    #Calculate experimental mean log prop.
    exp_mean = mean(renewal_log)
    
    #Store mean and sd log rate
    MC_results[[group]][["Relapse"]]$"exp_mean" = exp_mean
    MC_results[[group]][["Relapse"]]$"exp_sd" = sd(renewal_log)
    
    #Calculate mean log rates for simulated samples, mean and sd by rows (rows for simulations)
    mc_means = apply(full_sample, 1, mean)
    mc_sds = apply(full_sample, 1, sd)
    
    #Store MC means and sds
    MC_results[[group]][["Relapse"]]$"mc_mean" = mc_means
    MC_results[[group]][["Relapse"]]$"mc_sd" = mc_sds
    
    #Histogram just FYI
    hist(
        mc_means,
        breaks = 20,
        main = "Simulated Means of log prop. response",
        xlab = "Means",
        xlim = c(-1, 5)
    )
    
    points(exp_mean,
           1200,
           pch = 21,
           col = "red",
           bg = "red")
}

#Clean up
rm(rich_sample, lean_sample, lean_log, rich_log, renewal_log,exp_mean,group,
   mc_means,mc_sds,full_sample)

#Renewal comparison between experiments----

#Individual sub values were added to the subject list before I realized
#that they were the same subjects. This fixes the subject numbers
Relapse[Relapse$Subject == 9, "Subject"] = 1
Relapse[Relapse$Subject == 10, "Subject"] = 2
Relapse[Relapse$Subject == 11, "Subject"] = 3
Relapse[Relapse$Subject == 12, "Subject"] = 5
Relapse[Relapse$Subject == 13, "Subject"] = 6
Relapse[Relapse$Subject == 14, "Subject"] = 7
Relapse[Relapse$Subject == 15, "Subject"] = 8

#List of usable subjects, to handle subject not in both experiments
usable_sub = c(1, 2, 3, 5, 6, 7, 8)

#Initialize data frame
full_sample = NULL

#Create sample list, rerun for each subject and add new column
for (s in usable_sub) {
    
    full_sample = cbind(
        full_sample,
        # bind new column for subject to all previous data
        
        #ABA renewal resample - ABC renewal resample
        sample(Relapse[Relapse$Group == "ABA" &
                           Relapse$Session >= 2 &
                           Relapse$Subject == s, "Rate"],
               runs, #Runs number long
               replace = TRUE) - #sample with replacement NOTE SUBTRACTION
            
            #Same as above, but for ABC
            sample(Relapse[Relapse$Group == "ABC" &
                               Relapse$Session >= 1 &
                               Relapse$Subject == s, "Rate"],
                   runs,
                   replace = TRUE)
    )
}

#Calculate means and SDS
mc_mean = apply(full_sample, 1, mean)
mc_sd = apply(full_sample, 1, sd)

#Pull renewal data, omit missing subject
renewal = Relapse[Relapse$Subject != 4 &
                      Relapse$Condition == "Renewal", ]

#Calculate experimental differences in rate of response for rich component
rich_exp = renewal[renewal$Reinf == "Rich" &
                   renewal$Group == "ABA", "Rate"] -
           renewal[renewal$Reinf == "Rich" &
                   renewal$Group == "ABC", "Rate"]

#Same as above, 
lean_exp = renewal[renewal$Reinf == "Lean" &
                   renewal$Group == "ABA", "Rate"] -
           renewal[renewal$Reinf == "Lean" &
                   renewal$Group == "ABC", "Rate"]

#Calculate experimental means
rich_exp_mean = mean(rich_exp)
lean_exp_mean = mean(lean_exp)

#Histograms just FYI
hist(mc_mean,
     breaks = 20,
     main = "Simulated mean differences across exps",
     xlab = "Means")

#Add experimental data
points(rich_exp_mean,
       1200,
       pch = 21,
       col = "red",
       bg = "red")

points(lean_exp_mean,
       1200,
       pch = 25,
       col = "blue",
       bg = "blue")

#Store data to main output list
MC_results[["Cross"]]$"mc_mean" = mc_mean
MC_results[["Cross"]]$"mc_sd" = mc_sd

#Store experimental mean, sd, and MC based p values
MC_results[["Cross"]][["rich"]]$"mean" = rich_exp_mean
MC_results[["Cross"]][["rich"]]$"sd" = sd(rich_exp)
MC_results[["Cross"]][["rich"]]$"p" = sum(mc_mean >= rich_exp_mean) / runs

MC_results[["Cross"]][["lean"]]$"mean" = lean_exp_mean
MC_results[["Cross"]][["lean"]]$"sd" = sd(lean_exp)
MC_results[["Cross"]][["lean"]]$"p" = sum(mc_mean >= lean_exp_mean) / runs

#clean up
rm(
    renewal,
    full_sample,
    lean_exp,
    lean_exp_mean,
    mc_mean,
    mc_sd,
    rich_exp,
    rich_exp_mean,
    s,
    usable_sub
)