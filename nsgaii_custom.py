from jmetal.algorithm.multiobjective.nsgaii import NSGAII

from jmetal.config import store
from jmetal.core.operator import Crossover, Mutation, Selection
from jmetal.core.problem import Problem
# from jmetal.core.solution import FloatSolution
from jmetal.operator import BinaryTournamentSelection
from jmetal.util.comparator import Comparator, MultiComparator
from jmetal.util.density_estimator import CrowdingDistance
from jmetal.util.evaluator import Evaluator
from jmetal.util.ranking import FastNonDominatedRanking
from jmetal.util.termination_criterion import TerminationCriterion

from typing import Generator, List, TypeVar
import numpy as np

import random
from typing import List, TypeVar

S = TypeVar("S")
R = TypeVar("R")


class NSGAII_custom(NSGAII):
    def __init__(
            self,
            problem: Problem,
            population_size: int,
            offspring_population_size: int,
            mutation: Mutation,
            crossover: Crossover,
            selection: Selection = BinaryTournamentSelection(
                MultiComparator([FastNonDominatedRanking.get_comparator(), CrowdingDistance.get_comparator()])),
            termination_criterion: TerminationCriterion = store.default_termination_criteria,
            population_generator: Generator = store.default_generator,
            population_evaluator: Evaluator = store.default_evaluator,
            dominance_comparator: Comparator = store.default_comparator,
    ):

        super(NSGAII_custom, self).__init__(
            problem=problem,
            population_size=population_size,
            offspring_population_size=offspring_population_size,
            mutation=mutation,
            crossover=crossover,
            selection=selection,
            termination_criterion=termination_criterion,
            population_evaluator=population_evaluator,
            population_generator=population_generator)
        self.dominance_comparator = dominance_comparator

    def create_initial_solutions(self) -> List[S]:

        starts = [1, 2, 5, 10, 15, 25, 50, 75, 100, 150]
        ends = [1, 5, 10, 15, 25, 50, 75, 100, 150, 200]

        percentage = 0.05
        num_in_category = int(self.population_size * percentage)

        # create priority_solutions
        priority_solutions = []
        for proc_id in reversed(range(self.population_size // 2)):
            solution = self.problem.create_solution()
            solution.variables = self.problem.priority_solution_variables(proc_id)
            priority_solutions.append(solution)

        # create custom_solutions
        custom_solutions = []
        print(f"--creating initial population. num_in_category={num_in_category}")

        for i in range(len(starts)):

            for _ in range(num_in_category):
                n_remove = random.randint(starts[i], ends[i])
                solution = self.problem.create_solution()
                solution.variables = self.problem.custom_solution_variables(n_remove=n_remove)
                custom_solutions.append(solution)

        return priority_solutions + custom_solutions

    def get_name(self) -> str:
        return "NSGAIIcust"
