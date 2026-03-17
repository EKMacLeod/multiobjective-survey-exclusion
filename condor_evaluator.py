from os import makedirs, path
from jmetal.util.evaluator import Evaluator
# from jmetal.core.problem import Problem
from jmetal.core.solution import FloatSolution
from jmetal.core.problem import FloatProblem
from typing import Generic, List, TypeVar


import htcondor
import classad
import time
import pickle

S = TypeVar("S")


class CondorEvaluator(Evaluator[S]):

    def __init__(self) -> None:
        super().__init__()
        self.cluster_id = None
        self.schedd = htcondor.Schedd()

    @staticmethod
    def create_gen_folder():
        folder_name = "1"
        folder_exists = True

        while folder_exists:
            if not path.exists(folder_name):
                makedirs(folder_name)
                folder_exists = False
            else:
                folder_number = int(folder_name) + 1
                folder_name = str(folder_number)

        sub_folders = ['log', 'errout', 'output', 'pickle_in', 'pickle_out', 'indices']

        for folder in sub_folders:
            makedirs(f"{folder_name}/{folder}")

        return folder_name

    def evaluate_solution(self, problem: FloatProblem, pop_size: int) -> None:

        input_files_string = ', '.join([
            *problem.other_input_files,
            problem.solution_evaluator_path,
            problem.r_evaluator_path,
            f"{self.gen_folder}/pickle_out/problem_inst.pkl",
            f"{self.gen_folder}/pickle_out/solution_$(ProcId).pkl",
            "survey_index_mop.py"
        ])

        submit_info = htcondor.Submit({
            'executable': f'{problem.executable_path}',
            'requirements': '(Machine != "rg-nsc-3xs-02")',
            'arguments': '$(ProcId)',
            'should_transfer_files': 'YES',
            'when_to_transfer_output': 'ON_EXIT',

            'transfer_input_files': input_files_string,
            'transfer_output_remaps': f'"result_$(ProcId).pkl={self.gen_folder}/pickle_in/result_$(ProcId).pkl; \
                    Index_for_solution_$(ProcId)_q1.csv={self.gen_folder}/indices/Index_for_solution_$(ProcId)_q1.csv; \
                    Index_for_solution_$(ProcId)_q3.csv={self.gen_folder}/indices/Index_for_solution_$(ProcId)_q3.csv"',

            'output': f'{self.gen_folder}/output/output_$(ProcId).out',
            'error': f'{self.gen_folder}/errout/error_$(ProcId).err',
            'log': f'{self.gen_folder}/log/log_$(ProcId).log',

            'max_retries': '2',
            'request_cpus': '1',
            'request_memory': '2G',
            'request_disk': '1G',
        })

        # print(submit_info)

        submit_result = self.schedd.submit(submit_info, count=pop_size)
        self.cluster_id = submit_result.cluster()

    def evaluate(self, solution_list: List[S], problem: FloatProblem) -> List[S]:

        evaluated_solutions = []
        self.gen_folder = CondorEvaluator.create_gen_folder()

        self.pickle_problem(problem)

        for id, solution in enumerate(solution_list):
            self.pickle_solution(id, solution)

        self.evaluate_solution(problem=problem, pop_size=len(solution_list))
        print(f"Running jobs for gen {self.gen_folder} (cluster {self.cluster_id})...")

        is_completed = False
        # count = 0
        while not is_completed:
            jobs = self.schedd.query(constraint=f"ClusterId == {self.cluster_id}")

            if all(job['JobStatus'] == 4 for job in jobs):

                returned = []
                for solution_id in range(len(solution_list)):
                    try:
                        e_solution = self.unpickle_solution(solution_id, successful=True)
                        evaluated_solutions.append(e_solution)
                        problem.record_solution(e_solution)
                        problem.write_solution(e_solution)
                        returned.append(solution_id)

                    except:
                        print(f"-Pickle file for solution {solution_id} in gen {self.gen_folder} not found. Penalising solution")
                        p_solution = self.unpickle_solution(solution_id, successful=False)
                        p_solution = problem.penalise_failed_solution(p_solution)
                        evaluated_solutions.append(p_solution)
                        problem.record_solution(p_solution)

                print(f"-Evaluation of jobs for gen {self.gen_folder} (cluster {self.cluster_id}) evaluation completed. {len(returned)} solutions returned")
                is_completed = True

            else:
                # print(f"Running jobs for cluster {self.cluster_id}.")
                time.sleep(30)

        return evaluated_solutions

    def pickle_problem(self, problem: FloatProblem) -> None:
        with open(f"{self.gen_folder}/pickle_out/problem_inst.pkl", "wb") as file:
            pickle.dump(problem, file)

    def pickle_solution(self, id: int, solution: FloatSolution) -> None:
        with open(f"{self.gen_folder}/pickle_out/solution_{id}.pkl", "wb") as file:
            pickle.dump(solution, file)

    # def unpickle_solution(self, solution_id: int) -> None:
    #     with open(f"{self.gen_folder}/pickle_in/result_{self.cluster_id}_{solution_id}.pkl", "rb") as file:
    #         return pickle.load(file)
    #
    # def unpickle_failed_solution(self, solution_id: int) -> None:
    #     with open(f"{self.gen_folder}/pickle_out/solution_{solution_id}.pkl", "rb") as file:
    #         return pickle.load(file)

    def unpickle_solution(self, solution_id: int, successful: bool = True):
        file_name = f"pickle_in/result_{solution_id}.pkl" if successful else f"pickle_out/solution_{solution_id}.pkl"
        file_path = f"{self.gen_folder}/{file_name}"

        with open(file_path, 'rb') as file:
            solution = pickle.load(file)

        return solution
