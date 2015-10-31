extern "C" void printMinisatStats (Minisat::Solver*);

extern "C" Minisat::Solver* newMinisatSolver (void);

extern "C" void deleteMinisatSolver (Minisat::Solver* solver);

extern "C" Minisat::Var newMinisatVar (Minisat::Solver* solver, int upol, int dvar);

extern "C" void releaseMinisatVar (Minisat::Solver* solver, Minisat::Var var, int sign);

extern "C" int addMinisatClause (Minisat::Solver* solver, Minisat::vec<Minisat::Lit>* ps);

extern "C" int simplifyMinisat (Minisat::Solver* solver);

extern "C" int solveMinisatWithAssumptions (Minisat::Solver* solver, Minisat::vec<Minisat::Lit>* assumps);

extern "C" int solveMinisat (Minisat::Solver* solver);

extern "C" int valueMinisatVar (Minisat::Solver* solver, Minisat::Var var);

extern "C" Minisat::vec<Minisat::Lit>* newMinisatVecLit (void);

extern "C" void deleteMinisatVecLit (Minisat::vec<Minisat::Lit>* v);

extern "C" void pushMinisatVar (Minisat::vec<Minisat::Lit>* v, Minisat::Var var, int sign);

