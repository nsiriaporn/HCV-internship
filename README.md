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
probability_dept <- data.frame(names(table(Trajectory_HCV_S$DEPARTMENT)))
names(probability_dept) <- c("departments")
probability_dept$probability <- round(dept.traj.RNA$positive/dept.traj$total, digits = 4)
#view the probability for the given department
probability_dept$probability[which(probability_dept$departments==20)]

#### create data frame where it shows patients and procedures done ####

# number of IV cath done for patient 1
sum(proc_analysis$INTRAVENOUS[which(proc_analysis$ID_PATIENT==1)], na.rm = TRUE)

# procedures for patient 1
colSums(proc_analysis[which(proc_analysis$ID_PATIENT==1), c(4:17)], na.rm = TRUE)

# departments for patient 1 and IV cath
print(proc_analysis$DEPARTMENT[which(proc_analysis$ID_PATIENT==1 & proc_analysis$INTRAVENOUS==1)])

# procedure ID for IV cath in patient 1
print(proc_analysis$ID_PATIENTINVASIVEPROC[which(proc_analysis$ID_PATIENT==1 & proc_analysis$INTRAVENOUS==1)])

# department for procedure ID = 1
proc_analysis$DEPARTMENT[which(proc_analysis$ID_PATIENTINVASIVEPROC==1)]
