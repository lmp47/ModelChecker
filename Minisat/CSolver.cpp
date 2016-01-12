#include "minisat/minisat/simp/SimpSolver.h"
#include "minisat/minisat/mtl/Vec.h"
#include "CSolver.h"
#include <iostream>

extern "C" void printMinisatStats (minisatSolver* solver){
  solver -> printStats();
}

extern "C" Minisat::SimpSolver* newMinisatSolver (void) {
  return new Minisat::SimpSolver;
}

extern "C" void deleteMinisatSolver (Minisat::SimpSolver* solver) {
  delete solver;
}

extern "C" Minisat::Var newMinisatVar (Minisat::SimpSolver* solver, int upol, int dvar) {
  Minisat::lbool* user_pol = new Minisat::lbool((bool)upol);
  return solver -> newVar (*user_pol, dvar);
}

extern "C" void releaseMinisatVar (Minisat::SimpSolver* solver, Minisat::Var var, int sign) {
  solver -> releaseVar (Minisat::mkLit(var, sign));
}

extern "C" int addMinisatClause (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* ps) {
  return solver -> addClause (*ps); //returns bool
}

extern "C" int simplifyMinisat (Minisat::SimpSolver* solver) {
  return solver -> simplify(); //returns bool
}
extern "C" result *solveMinisatWithAssumps (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* assumps) {
  res.solved = solver -> solve(*assumps); //returns bool
  if (res.solved) {
    res.conflictSize = 0;
  }
  else {
    res.conflictSize = (solver -> conflict).size();
  }
  res.modelSize = (solver -> model).size();
  res.model = &((solver -> model)[0]);
  res.conflict = (Minisat::Lit*) &((solver -> conflict).toVec()[0]);
  return &res;
}

extern "C" int solveMinisat (Minisat::SimpSolver* solver) {
  return solver -> solve(); //returns bool
}

extern "C" int* getMinisatConflictVec (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* assumps) {
  solver -> solve(*assumps);
  return (int*) &((solver -> conflict).toVec()[0]);
}

extern "C" int getMinisatConflictSize (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* assumps) {
  solver -> solve(*assumps);
  return (solver -> conflict).size();
}

extern "C" int valueMinisatLit (Minisat::Lit lit) {
  return Minisat::sign(lit);
}

extern "C" int varMinisatLit (Minisat::Lit lit) {
  return Minisat::var(lit);
}

extern "C" int valueMinisatVar (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* assumps, Minisat::Var var) {
  res.solved = solver -> solve(*assumps);
  Minisat::lbool value = (solver -> model)[var];
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

