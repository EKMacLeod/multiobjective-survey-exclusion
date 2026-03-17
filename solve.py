from os import listdir
import pandas as pd
# import numpy as np
# from jmetal.algorithm.multiobjective.nsgaii import NSGAII
from nsgaii_custom import NSGAII_custom
from jmetal.operator import PolynomialMutation, SBXCrossover
from jmetal.util.termination_criterion import StoppingByEvaluations
from jmetal.util.observer import ProgressBarObserver
from jmetal.util.solution import get_non_dominated_solutions

from condor_evaluator import CondorEvaluator
from survey_index_mop import SurveyIndex3, OptResultsManager

def run_optimisation(rseed):
    pop_size = 200
    n_gens = 200
    n_evaluations = pop_size * n_gens

    q1_true_index = pd.read_csv("Haddock_Index_Q1.csv")
    q3_true_index = pd.read_csv("Haddock_Index_Q3.csv")
    grid_ids = pd.read_csv("combined_allqs_res05_ordered.csv")
    executable_path = "wrapper.sh"
    variable_threshold = 0.5

    problem = SurveyIndex3(
        rseed=rseed,
        grid_ids=grid_ids,
        q1_true_index=q1_true_index,
        q3_true_index=q3_true_index,

        solution_evaluator_path="solution_evaluator.py", # same as defined in wrapper.sh
        r_evaluator_path="Haddock_SurveyIndex_AllQs_21.R", # same as defined in solution_evaluator
        other_input_files=listdir("input_files"),
        executable_path=executable_path,
        variable_threshold=variable_threshold)

    solver = NSGAII_custom(
        problem=problem,
        population_size=pop_size,
        offspring_population_size=pop_size,
        mutation=PolynomialMutation(probability=1.0 / problem.number_of_variables(), distribution_index=20),
        crossover=SBXCrossover(probability=0.8, distribution_index=20),
        termination_criterion=StoppingByEvaluations(max_evaluations=n_evaluations),
        population_evaluator=CondorEvaluator())

    rm = OptResultsManager(target_problem=problem, solver_name=solver.get_name(), rseed=rseed) 

    solver.observable.register(ProgressBarObserver(max=n_evaluations))
    solver.run()
    front = get_non_dominated_solutions(solver.get_result())
    rm.save_results(front)


if __name__ == '__main__':

    rseed = 100
    run_optimisation(rseed)
