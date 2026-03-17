#!/usr/bin/env python3

# import importlib
import subprocess
import sys
import numpy as np
import pandas as pd
# import time
import json
import platform
import time


def get_r_script_path():
    """
    Get the path to Rscript depending on the current OS. """

    pform = platform.system()
    paths = {
        "Windows": "C:/Program Files/R/R-4.4.2/bin/Rscript.exe",
        "Linux": "/usr/bin/Rscript",
    }
    return paths.get(pform, None)


class LeaveOneGridOut:
    """
        This class removes one grid_id at a time and measure its impact on both quarters.

        Attributes
            proc_id (int): The id of the process, also used to identify a solution.
            q1_true_index (pd.DataFrame): A dataframe of the true index for q1.
            q3_true_index (pd.DataFrame): A dataframe of the true index for q3.
            grid_ids (pd.DataFrame): A dataframe of unique grid cells got the given resolution, not in any order.
        """

    def __init__(self,
                 proc_id: int,
                 q1_true_index: pd.DataFrame,
                 q3_true_index: pd.DataFrame,
                 grid_ids: pd.DataFrame,
                 ):

        self.proc_id = proc_id
        self.q1_true_index = q1_true_index
        self.q3_true_index = q3_true_index
        self.grid_ids = grid_ids

        self.quarters = ["q1", "q3"]
        self.r_script_path = get_r_script_path()
        self.r_evaluator_path = "Haddock_SurveyIndex_Gridded_allqs.R"
        self.output_file = pd.DataFrame(
            columns=[
                "id",
                "grid_id_removed",
                "impact_on_q1",
                "impact_on_q3"
            ])
        self.grids_to_remove = self.grid_ids.iloc[proc_id, 1]

    def compute_survey_index(self, q):
        """ 
            Call an R script to carry out the modeling and generate a SurveyIndex, 
            which is then saved in a CSV file.
            
            Parameters:
                q (str): The quarter to evaluate. Expected values are 'q1' or 'q3'.

            Returns:
                pandas.DataFrame: The resulting data frame from the CSV file.
            
            """
        grids_to_remove_json = json.dumps(self.grids_to_remove)
        input_string = f"{grids_to_remove_json}\n{self.proc_id}\n{q}\n"

        proc = subprocess.Popen([self.r_script_path, self.r_evaluator_path], stdin=subprocess.PIPE)
        proc.communicate(input=input_string.encode())

        # ...wait for evaluation to complete... #
        new_index = pd.read_csv(f"Index_for_solution_{self.proc_id}_{q}.csv")
        return new_index

    def compare_indices_allages(self, q, new_index):
        """
            Compare the true and the new index for a given age_group or all age_groups.

            Args:
                q (str): The quarter to evaluate. Expected values are 'q1' or 'q3'.
                new_index (pd.DataFrame): The new index to be compared, containing age group data.
            """

        def calculate_impact(real, new):
            return np.sum(np.abs(new / real - 1))

        if q not in self.quarters:
            raise ValueError("Invalid value for q. Expected 'q1' or 'q3'.")

        age_groups_q1 = range(1, 9)
        age_groups_q3 = range(9)

        age_groups = age_groups_q1 if q == "q1" else age_groups_q3
        true_index = self.q1_true_index if q == "q1" else self.q3_true_index

        impacts = []
        for age in age_groups:
            real = true_index.iloc[:, age].to_numpy()
            new = new_index.iloc[:, age].to_numpy()
            impact = calculate_impact(real, new)
            impacts.append(impact)

        return -np.mean(impacts)

    def execute(self):
        """
            Evaluate the modeling process for a given ID by running the R script, comparing indices,
            and logging the results.
            """
        start_time = time.time()
        q_results = []
        for q in self.quarters:
            new_index = self.compute_survey_index(q)
            result = self.compare_indices_allages(q, new_index)
            q_results.append(result)

        self.output_file.loc[len(self.output_file.index)] = [self.proc_id, self.grids_to_remove, q_results[0], q_results[1]]
        self.output_file.to_csv(f"output_{self.proc_id}.csv", index=False)

        end_time = time.time()
        total_time = end_time - start_time

        self.record_time(total_time)

        print("complete")
    
    def record_time(self, total_time):
        filename = f"time_log_{self.proc_id}.csv"
        with open(filename, "a") as f:
            f.write({self.proc_id},{total_time})   


class Greedy(LeaveOneGridOut):

    '''Greedy. Removes grid_ids commulatively'''

    def __init__(self,
                 proc_id: int,
                 q1_true_index: pd.DataFrame,
                 q3_true_index: pd.DataFrame,
                 grid_ids: pd.DataFrame,
                 ):
        super().__init__(
            proc_id=proc_id,
            q1_true_index=q1_true_index,
            q3_true_index=q3_true_index,
            grid_ids=grid_ids,
        )

        self.grids_to_remove = self.ordered_grids_to_remove()

    """
    This class removes grid_cells cumulatively. 
    :param grid_ids: a dataframe of unique grid_ids in descending order of impact resuting from LeaveOneGridOut. 
    """

    def ordered_grids_to_remove(self):
        jobs = []
        excluded = []

        ordered_grid_ids = self.grid_ids.iloc[:, 1]
        for i, grid_id in enumerate(ordered_grid_ids):
            if i == 0:
                excluded.append(grid_id)
                continue
            job = excluded.copy()
            excluded.append(grid_id)
            jobs.append(job)
        return jobs[self.proc_id]


if __name__ == '__main__':

    ### This runs Leave one onet and greedy. Uncomment the correct one below.    

    experiment = "loo" # Leave one out
    # experiment = "greedy" # Greedy

    # proc_id = int(sys.argv[1])
    proc_id = 12

    q1_true_index = pd.read_csv("Haddock_Index_Q1.csv")
    q3_true_index = pd.read_csv("Haddock_Index_Q3.csv")

    if experiment == "loo":
        grid_ids = pd.read_csv("unique_grid_cells_0.5.csv")
        loo = LeaveOneGridOut(proc_id, q1_true_index, q3_true_index, grid_ids)
        loo.execute()

    elif experiment == "greedy":
        ordered_grid_ids = pd.read_csv("combined_allqs_res05_ordered.csv")  # correct this
        lgc = Greedy(proc_id, q1_true_index, q3_true_index, ordered_grid_ids)
        lgc.execute()

    else:
        print("Invalid experiment name")
