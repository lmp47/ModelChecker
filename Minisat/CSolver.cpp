#include "minisat/minisat/core/Solver.h"
#include "minisat/minisat/core/SolverTypes.h"
#include "minisat/minisat/mtl/Vec.h"
#include "CSolver.h"
#include <iostream>

extern "C" void printMinisatStats (Minisat::Solver* solver){
  solver -> printStats();
}

extern "C" Minisat::Solver* newMinisatSolver (void) {
  return new Minisat::Solver;
}

extern "C" void deleteMinisatSolver (Minisat::Solver* solver) {
  delete solver;
}

extern "C" Minisat::Var newMinisatVar (Minisat::Solver* solver, int upol, int dvar) {
  Minisat::lbool* user_pol = new Minisat::lbool((bool)upol);
  return solver -> newVar (*user_pol, dvar);
}

extern "C" void releaseMinisatVar (Minisat::Solver* solver, Minisat::Var var, int sign) {
  solver -> releaseVar (Minisat::mkLit(var, sign));
}

extern "C" int addMinisatClause (Minisat::Solver* solver, Minisat::vec<Minisat::Lit>* ps) {
  return solver -> addClause (*ps); //returns bool
}

extern "C" int simplifyMinisat (Minisat::Solver* solver) {
  return solver -> simplify(); //returns bool
}

extern "C" int solveMinisatWithAssumps (Minisat::Solver* solver, Minisat::vec<Minisat::Lit>* assumps) {
  return solver -> solve(*assumps); //returns bool
}

extern "C" int solveMinisat (Minisat::Solver* solver) {
  bool res = solver -> solve();
  return solver -> solve(); //returns bool
}

extern "C" int valueMinisatVar (Minisat::Solver* solver, Minisat::Var var) {
  Minisat::lbool value = solver -> value(var);
  return Minisat::toInt(value);
}

extern "C" Minisat::vec<Minisat::Lit>* newMinisatVecLit (void) {
  return new Minisat::vec<Minisat::Lit>();
}

extern "C" void deleteMinisatVecLit (Minisat::vec<Minisat::Lit>* v) {
  delete v;
}
extern "C" void pushMinisatVar (Minisat::vec<Minisat::Lit>* v, Minisat::Var var, int sign) {
  v -> push(Minisat::mkLit(var, sign));
}

