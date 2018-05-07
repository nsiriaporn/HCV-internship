# HCV-internship
Codes for my internship project

#### Trajectory analysis ####

# trajectories of departments of patient 1
print(Patient.Invasive.Procedure$DEPARTMENT[which(Patient.Invasive.Procedure$ID_PATIENT==1)])


patients_proc <- Patient.Invasive.Procedure[,c("ID_PATIENT", "DEPARTMENT")]

library(dplyr)
#departments patients had their procedures
collapsed_proc <- patients_proc %>% group_by(ID_PATIENT) %>%
  summarise(departments = paste(DEPARTMENT, collapse=", "), counts = length(DEPARTMENT))

proc_analysis <- Patient.Invasive.Procedure[, c("ID_PATIENTINVASIVEPROC", "ID_PATIENT", "DEPARTMENT", "INTRAVENOUS",
                                                "SUTURES", "BLOOD_TRANSFUSION", "BLOOD_SAMPLE", "INJECTION",
                                                "ENDOSCOPY", "GASTRIC_LAVAGE", "IUD_INSERTION", "CARDIAC_CATHETER",
                                                "DIALYSIS", "WOUND_DRESSING", "BLOODGLUCOSE", "ENDOTRACHEALINTU",
                                                "DRAINAGECATHETER", "OTHER_INVPROC")]
View(proc_analysis[which(proc_analysis$ID_PATIENT==4),])

# data frame with departments and HCV RNA prevalence
prevalence_dept <- data.frame(names(table(Trajectory_HCV_S$DEPARTMENT)))
names(prevalence_dept) <- c("departments")
prevalence_dept$prevalence <- round(dept.traj.RNA$positive/dept.traj$total, digits = 4)
#view the probability for the given department
prevalence_dept$prevalence[which(prevalence_dept$departments==20)]


#### per-procedure risk df ####
pp_risk <- c("endoscopy", "endotracheal intubation", "hemodialysis", "injection", "blood glucose",
             "stitches", "blood sample", "IV catheter", "blood transfusion", "drainage catheter",
             "cardiac catheter", "gastric lavage", "wound dressing")
pp_transmission <- c(1.727,1.727,1.970,2.200,2.200,2.302,2.500,2.864,3.414,3.579,
                     4.063,6.937,10.009)
pp_transmission <- pp_transmission/100

pp_lowsensitivity <- c(0.785,0.785,0.868,1.000,1.000,1.046,1.136,1.302,1.552,1.627,
                       1.847,3.153,4.550)
pp_lowsensitivity <- pp_lowsensitivity/100

pp_highsensitivity <- c(7.223,7.223,7.986,9.200,9.200,9.627,10.455,11.977,14.277,
                        14.969,16.991,29.009,41.856)
pp_highsensitivity <- pp_highsensitivity/100

pp_risk_df <- data.frame(pp_risk, pp_transmission, pp_lowsensitivity, pp_highsensitivity)

# 1 - (dept prev * per-procedure)
1-(prevalence_dept$prevalence[which(prevalence_dept$departments==-1)])*(pp_risk_df$pp_transmission[which(pp_risk_df$pp_risk=="IV catheter")])
#### risk for patient 4 ####
View(proc_analysis[which(proc_analysis$ID_PATIENT==4),])
1-(1-(prevalence_dept$prevalence[which(prevalence_dept$departments==-1)])*(pp_risk_df$pp_transmission[which(pp_risk_df$pp_risk=="IV catheter")]))*
  (1-(prevalence_dept$prevalence[which(prevalence_dept$departments==19)])*(pp_risk_df$pp_transmission[which(pp_risk_df$pp_risk=="blood sample")]))*
  (1-(prevalence_dept$prevalence[which(prevalence_dept$departments==18)])*(pp_risk_df$pp_transmission[which(pp_risk_df$pp_risk=="IV catheter")]))*
  (1-(prevalence_dept$prevalence[which(prevalence_dept$departments==1)])*(pp_risk_df$pp_transmission[which(pp_risk_df$pp_risk=="endoscopy")]))
#0.011657 = 1.166%

#sensitivity analysis 1%
1-(1-(prevalence_dept$prevalence[which(prevalence_dept$departments==-1)])*(pp_risk_df$pp_lowsensitivity[which(pp_risk_df$pp_risk=="IV catheter")]))*
  (1-(prevalence_dept$prevalence[which(prevalence_dept$departments==19)])*(pp_risk_df$pp_lowsensitivity[which(pp_risk_df$pp_risk=="blood sample")]))*
  (1-(prevalence_dept$prevalence[which(prevalence_dept$departments==18)])*(pp_risk_df$pp_lowsensitivity[which(pp_risk_df$pp_risk=="IV catheter")]))*
  (1-(prevalence_dept$prevalence[which(prevalence_dept$departments==1)])*(pp_risk_df$pp_lowsensitivity[which(pp_risk_df$pp_risk=="endoscopy")]))
#0.00527 = 0.527%

#sensitivity analysis 9.2%
1-(1-(prevalence_dept$prevalence[which(prevalence_dept$departments==-1)])*(pp_risk_df$pp_highsensitivity[which(pp_risk_df$pp_risk=="IV catheter")]))*
  (1-(prevalence_dept$prevalence[which(prevalence_dept$departments==19)])*(pp_risk_df$pp_highsensitivity[which(pp_risk_df$pp_risk=="blood sample")]))*
  (1-(prevalence_dept$prevalence[which(prevalence_dept$departments==18)])*(pp_risk_df$pp_highsensitivity[which(pp_risk_df$pp_risk=="IV catheter")]))*
  (1-(prevalence_dept$prevalence[which(prevalence_dept$departments==1)])*(pp_risk_df$pp_highsensitivity[which(pp_risk_df$pp_risk=="endoscopy")]))
#0.04781 = 4.781%


#### create data frame where it shows patients and procedures done ####

# number of IV cath done for patient 4
sum(proc_analysis$INTRAVENOUS[which(proc_analysis$ID_PATIENT==4)], na.rm = TRUE)

# procedures for patient 4
colSums(proc_analysis[which(proc_analysis$ID_PATIENT==4), c(4:17)], na.rm = TRUE)

# departments for patient 4 
proc_analysis$DEPARTMENT[which(proc_analysis$ID_PATIENT==4)]

# departments for patient 4 and IV cath
print(proc_analysis$DEPARTMENT[which(proc_analysis$ID_PATIENT==4 & proc_analysis$INTRAVENOUS==1)])

# procedure ID for IV cath in patient 4
print(proc_analysis$ID_PATIENTINVASIVEPROC[which(proc_analysis$ID_PATIENT==4 & proc_analysis$INTRAVENOUS==1)])

# department for procedure ID = 39
proc_analysis$DEPARTMENT[which(proc_analysis$ID_PATIENTINVASIVEPROC==39)]

# merge patient_proc with prevalence
patient_dept_prev <- merge(patients_proc, prevalence_dept, by="DEPARTMENT")

# department prevalence for patient 4
patient_dept_prev$prevalence[which(patient_dept_prev$ID_PATIENT==4)]

## melting patient with procedures
patient_procedures <- Patient.Invasive.Procedure[, c("ID_PATIENT", "INTRAVENOUS",
                                                "SUTURES", "BLOOD_TRANSFUSION", "BLOOD_SAMPLE", "INJECTION",
                                                "ENDOSCOPY", "GASTRIC_LAVAGE", "IUD_INSERTION", "CARDIAC_CATHETER",
                                                "DIALYSIS", "WOUND_DRESSING", "BLOODGLUCOSE", "ENDOTRACHEALINTU",
                                                "DRAINAGECATHETER")]
library(reshape2)
mpatient_procedures <- melt(patient_procedures, id=c("ID_PATIENT"), na.rm = T)
