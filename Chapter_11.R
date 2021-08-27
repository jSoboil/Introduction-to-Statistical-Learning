# Survival Analysis and Censored Data -------------------------------------
# I.e., time-to-event analysis. Although this might sound like a regression problem
# there is an important complication: many or some of the patients have survived
# until the end of the study. Such a patient's survival time is said to be 
# censored: we know that it is at least five years, but we do not know its true 
# value. However, we do not want to discard this subset of surviving patients, as 
# the fact that they have survived ≥ 5 years amounts to valueable information.

## A Closer Look at Censoring ----------------------------------------------
# In order to analyse survival data, we inevitably have to make a set of 
# assumptions . E.g., suppose that a number of patients drop out of a cancer 
# study early because they are very sick or because of toxicity etc.An analysis
# that does not take into account the reason why patients dropped out of the study
# will likely overestimate the true expected survival time. Similarly, suppose 
# that males who are very sick are more likely to drop out than females who are
# very sick. Then a comparison of male and female survival times may wrongly 
# suggest that makes survive longer than females. 

# In general, we need to assume that the censoring mechanism is *independent*: 
# conditional on the features, event time T is independent of the censoring time
# C. The examples above *violate* the assumption of independent censoring. 
# Typically it is not possible to determine from the data itself whether the 
# censoring mechanism is independent. The usual data sleuthing and casual 
# reasoning applies.


# The Kaplan-Meier Survival Curve -----------------------------------------























