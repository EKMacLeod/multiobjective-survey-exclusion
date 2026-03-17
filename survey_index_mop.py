from os import makedirs, path
import pandas as pd
import numpy as np
from random import seed
import csv
from jmetal.core.problem import FloatProblem
from jmetal.core.solution import FloatSolution
import random


# import shutil
# import os

class SurveyIndex3(FloatProblem):
    def __init__(
            self,
            rseed: int,
            grid_ids: pd.DataFrame,
            q1_true_index: pd.DataFrame,
            q3_true_index: pd.DataFrame,

            solution_evaluator_path: str,
            r_evaluator_path: str,
            other_input_files: list[str],
            executable_path: str,
            rscript_path: str = "/usr/bin/Rscript",
            variable_threshold: float = 0.5):

        super().__init__()
        self.rseed = rseed
        seed(rseed)
        # self.grid_ids = grid_ids.excluded
        self.grid_ids = grid_ids.iloc[:, 1]

        self.q1_true_index = q1_true_index
        self.q3_true_index = q3_true_index

        self.quarters = ["q1", "q3"]
        self.solution_evaluator_path = solution_evaluator_path
        self.r_evaluator_path = r_evaluator_path
        self.other_input_files = other_input_files
        self.executable_path = executable_path
        self.rscript_path = rscript_path
        self.variable_threshold = variable_threshold

        self.obj_labels = ["n_grids_removed", "impact_on_q1", "impact_on_q3"]
        self.obj_directions = [self.MINIMIZE, self.MINIMIZE, self.MINIMIZE]
        self.lower_bound = [0.0 for _ in range(self.number_of_variables())]
        self.upper_bound = [1.0 for _ in range(self.number_of_variables())]

        self.all_solutions = pd.DataFrame(
            columns=[str(i) for i in range(self.number_of_variables())] + [j for j in self.obj_labels])

    def number_of_objectives(self) -> int:
        return len(self.obj_directions)

    def number_of_variables(self) -> int:
        return len(self.grid_ids)

    def number_of_constraints(self) -> int:
        return 0

    def custom_solution_variables(self, n_remove):
        solution_variables = [random.uniform(0, self.variable_threshold) for _ in
                              range(self.number_of_variables() - n_remove)]

        for _ in range(n_remove):
            obove_threshold = random.uniform(self.variable_threshold, 1.0)
            solution_variables.append(obove_threshold)

        random.shuffle(solution_variables)
        return solution_variables

    def priority_solution_variables(self, proc_id):

        to_exclude = self.ordered_grids_to_remove(proc_id)
        indices_to_exclude = self.get_indices_for_excluded_grids(to_exclude)

        solution_variables = np.random.uniform(
            self.lower_bound,
            self.variable_threshold,
            size=self.number_of_variables())

        for index in indices_to_exclude:
            solution_variables[index] = np.random.uniform(self.variable_threshold, self.upper_bound[0])

        return solution_variables.tolist()

    def ordered_grids_to_remove(self, proc_id):
        jobs = []
        excluded = []

        for i, grid_id in enumerate(self.grid_ids):
            if i == 0:
                excluded.append(grid_id)
                continue
            job = excluded.copy()
            excluded.append(grid_id)
            jobs.append(job)

        return jobs[proc_id]

    def get_indices_for_excluded_grids(self, to_exclude):
        indexes = []
        for value in to_exclude:
            indexes.extend([index for index, element in enumerate(self.grid_ids) if element == value])
        return indexes

    def create_solution(self) -> FloatSolution:
        new_solution = FloatSolution(
            self.lower_bound,
            self.upper_bound,
            self.number_of_objectives(),
            self.number_of_constraints()
        )
        new_solution.variables = [
            random.uniform(self.lower_bound[i] * 1.0, self.upper_bound[i] * 1.0)
            for i in range(self.number_of_variables())
        ]

        return new_solution

    def evaluate(self, solution: FloatSolution) -> FloatSolution:
        pass

    def record_solution(self, solution: FloatSolution):
        evaluated_solution = [i for i in solution.variables] + [j for j in solution.objectives]
        self.all_solutions.loc[len(self.all_solutions.index)] = evaluated_solution

    def write_solution(self, solution: FloatSolution):
        sol = [i for i in solution.variables] + [i for i in solution.objectives]

        with open(f'{self.name()}_{self.rseed}_results_real_time.csv', 'a') as file:
            writer = csv.writer(file, lineterminator="\n")
            writer.writerow(sol)

    def penalise_failed_solution(self, solution: FloatSolution) -> FloatSolution:
        solution.objectives[0] = len([x for x in solution.variables if x >= self.variable_threshold])
        solution.objectives[1] = 11
        solution.objectives[1] = 11
        return solution

    def name(self):
        return "SurveyIndex3"


class OptResultsManager:
    def __init__(self, target_problem, solver_name, rseed):
        self.target_problem = target_problem
        self.problem_name = target_problem.name()
        self.solver_name = solver_name
        self.rseed = rseed
        self.all_solutions = target_problem.all_solutions

    def save_pareto_solutions(self, front) -> None:
        solutions_df = pd.DataFrame(columns=self.target_problem.all_solutions.columns)

        for solution in front:
            row = solution.variables + solution.objectives
            solutions_df.loc[len(solutions_df)] = row

        solutions_df.to_csv(
            path_or_buf=f"{self.problem_name}_{self.solver_name}_{self.rseed}_pareto_solutions.csv",
            index=False)

    def save_all_solutions(self):
        self.all_solutions = self.all_solutions.to_csv(
            path_or_buf=f"{self.problem_name}_{self.solver_name}_{self.rseed}_all_solutions.csv",
            index=False)

    def save_results(self, front):
        self.save_pareto_solutions(front)
        self.save_all_solutions()
