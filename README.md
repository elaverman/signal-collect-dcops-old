signal-collect-dcops
======================

Iterative approximate best-response algorithms for solving Distributed Constraint Optimization Problems in Signal/Collect.

About
-------------------------------------
The Signal/Collect DCOPs package offers several iterative approximate best-response algorithms for solving DCOPs. [1]

- DSAN (Distributed Simulated Annealing) [2]
- DSA with A-E variants (Distributed Stochastic Algorithm) [3] with A-E variants according to [2]
- JSFP-I (Joint Strategy Fictitious Play with Inertia) [4]


Usage
-------------------------------------
Ensure Java 7 is used for the compilation and install SBT, as described in the README file of the "signal-collect" project.

The project has the following dependencies:
- signal-collect
- signal-collect-graphs
- signal-collect-evaluations

Follow the compilation instructions in the "signal-collect" README.

After cloning the repository, go to the project folder and start SBT on the command line. Use the "assembly" command in SBT to generate a .jar file with dependencies.

To generate an Eclipse project, use the "eclipse" command on the SBT prompt and then follow the description in the "How to Develop in Eclipse" section of the "signal-collect" README. 

Note
-----
Before submitting a job to a Torque host it is imperative to re-run "assembly" in SBT in order to have the last version run. 


Bibliography:
-----------------------
- [1]  Archie C. Chapman, Alex Rogers, Nicholas R. Jennings and David S. Leslie (2011). A unifying framework for iterative approximate best-response algorithms for distributed constraint optimization problems. The Knowledge Engineering Review, 26, pp 411-444. doi:10.1017/S0269888911000178. 
- [2] Arshad, Silaghi, 2003. "Distributed Simulated Annealing and comparison to DSA", In Proceedings of the 4th International Workshop on Distributed Constraint Reasoning, Acapulco, Mexico
- [3] Zhang, Wang, Wittenburg, 2002. "Distributed stochastic search for constraint satisfaction and optimization: Parallelism, phase transitions and performance", In Proceedings  of AAAI-02 Workshop on Probabilistic Approaches in Search, 2002, pp. 53–59
- [4] Marden, Jason R., Gürdal Arslan, and Jeff S. Shamma. "Joint strategy fictitious play with inertia for potential games." Automatic Control, IEEE Transactions on 54.2 (2009): 208-220.