# multiobjective-survey-exclusion
Repository containing code used for "Exploring the impact of area exclusions on stock assessment indices using multi-objective optimisation" (ICES JMS). 
If using this code, you should cite the paper above.

R code written by Eleanor MacLeod, Python code written by Tiwonge Banda (GitHub.com/tiwobanda).
Note that the Python code runs on HTCondor. It may not directly run on a non-condor environment without modification.

Python code for each method runs through the "Haddock_SurveyIndex_Gridded_allqs.R" script to calculate the % difference on the survey indices.
Code for the Leave-one-out and Greedy methods is stored in the "LOO_Greedy implementation" script - the method is defined at the top of the script.

Files for the NSGA-II algorithm are:
  nsgaii_custom.py
  condor_evaluator.py
  solution_evaluator
  solve.py
  survey_index_mop.py

Any questions please feel free to contact the authors of the paper.
