#!/usr/bin/env python3
import os
import subprocess
import sys
from os import listdir, getcwd
import json
import numpy as np
import pandas as pd
import pickle
from jmetal.core.solution import FloatSolution
from jmetal.core.problem import FloatProblem


def evaluate(problem: FloatProblem, solution: FloatSolution, solution_id: int, gen_folder=None) -> FloatSolution:
    # remove grid_ids corresponding to solution variable >=0.5
    grids_to_remove = [idx for idx, val in enumerate(solution.variables) if val >= problem.variable_threshold]
    grids_to_remove = problem.grid_ids[grids_to_remove]
    grids_to_remove = [i for i in grids_to_remove]

    if len(grids_to_remove) == len(solution.variables):
        solution.objectives[0] = 10
        solution.objectives[1] = len(grids_to_remove)

        # create dummy index file
        data = {"years": np.arange(2010, 2024),
                **{str(i): np.zeros(14) for i in range(1, 9)}}
        df = pd.DataFrame(data)

        df.to_csv(path_or_buf=f"Index_for_solution_{solution_id}.csv", index=False)

    else:
        # call R evaluation script and evaluate
        q_results = []
        for q in problem.quarters:
            new_index = compute_survey_index(problem, solution_id, grids_to_remove, q)
            result = compare_indices_allages(problem, q, new_index)
            q_results.append(result)

        # assign objective values
        solution.objectives[0] = len(grids_to_remove)
        solution.objectives[1] = q_results[0]
        solution.objectives[2] = q_results[1]

    # pickle solution
    if gen_folder is None:
        pickle_solution(solution_id, solution)
        print(f"Solution {solution_id} successfully pickled after evaluation")

    return solution


def compare_indices_allages(problem, q, new_index):
    """
    Compare the true and the new index for a given age_group or all age_groups.
    """

    def calculate_impact(real, new):
        return np.sum(np.abs(new / real - 1))

    if q not in problem.quarters:
        raise ValueError("Invalid value for q. Expected 'q1' or 'q3'.")

    age_groups_q1 = range(1, 9)
    age_groups_q3 = range(9)

    age_groups = age_groups_q1 if q == "q1" else age_groups_q3
    true_index = problem.q1_true_index if q == "q1" else problem.q3_true_index

    impacts = []
    for age in age_groups:
        real = true_index.iloc[:, age].to_numpy()
        new = new_index.iloc[:, age].to_numpy()
        impact = calculate_impact(real, new)
        impacts.append(impact)

    return -np.mean(impacts)


def compute_survey_index(problem: FloatProblem, solution_id: int, grids_to_remove: list, q, retries=3, gen_folder=None) -> pd.DataFrame:
    """ Compute the index of abundance in R and drop a csv file.
    """
    # Convert grids_to_remove to JSON
    grids_to_remove_json = json.dumps(grids_to_remove)

    # Call R script
    input_string = f"{grids_to_remove_json}\n{solution_id}\n{q}\n"

    csv_file_path = f"Index_for_solution_{solution_id}_{q}.csv"

    for attempt in range(1, retries + 1):
        try:
            proc = subprocess.Popen([problem.rscript_path, problem.r_evaluator_path], stdin=subprocess.PIPE)
            proc.communicate(input=input_string.encode())
            new_index = pd.read_csv(csv_file_path)
            return new_index

        except FileNotFoundError as e:
            print(f"Attempt {attempt} failed: {e}")
            if attempt == retries:
                print("All attempts failed. Raising exception.")
                raise
            else:
                print("Retrying...")


def unpickle_problem():
    with open("problem_inst.pkl", "rb") as f:
        return pickle.load(f)


def unpickle_solution(id):
    with open(f"solution_{id}.pkl", "rb") as file:
        return pickle.load(file)


def pickle_solution(id, solution):
    with open(f"result_{id}.pkl", "wb") as file:
        pickle.dump(solution, file)


if __name__ == '__main__':
    # solution_id = int(sys.argv[1])
    # problem = unpickle_problem()
    #
    # solution = unpickle_solution(solution_id)
    # print(
    #     f"Successfully unpickled solution {solution_id} with: {len([i for i in solution.variables if i >= problem.variable_threshold])} variables >= variable_threshold")
    #
    # evaluate(problem, solution, solution_id)
    # print(f"listdir at the end of evaluation: {listdir(getcwd())}")
