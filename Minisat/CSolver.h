#ifdef __cplusplus
#define minisatSolver Minisat::Solver
#define minisatVar Minisat::Var
#define minisatLit Minisat::Lit
#define minisatVecLit Minisat::vec<Minisat::Lit>*
extern "C" {
#else
typedef int minisatSolver;
typedef int minisatVar;
typedef int minisatLit;
typedef void minisatVecLit;
#endif

typedef struct result result;

struct result {
  unsigned solved;
  unsigned modelSize;
  unsigned conflictSize;
  Minisat::lbool *model;
  Minisat::Lit *conflict;
} res = {0, 0, 0, 0, 0};

void printMinisatStats (Minisat::Solver*);

Minisat::Solver* newMinisatSolver (void);

void deleteMinisatSolver (Minisat::Solver* solver);

Minisat::Var newMinisatVar (Minisat::Solver* solver, int upol, int dvar);

void releaseMinisatVar (Minisat::Solver* solver, Minisat::Var var, int sign);

int addMinisatClause (Minisat::Solver* solver, Minisat::vec<Minisat::Lit>* ps);

int simplifyMinisat (Minisat::Solver* solver);

result *solveMinisatWithAssumps (Minisat::Solver* solver, Minisat::vec<Minisat::Lit>* assumps);

int solveMinisat (Minisat::Solver* solver);

int *getMinisatConflictVec (Minisat::Solver* solver, Minisat::vec<Minisat::Lit>* assumps);

int getMinisatConflictSize (Minisat::Solver* solver, Minisat::vec<Minisat::Lit>* assumps);

int valueMinisatLit (Minisat::Lit lit);

int varMinisatLit (Minisat::Lit lit);

int valueMinisatVar (Minisat::Solver* solver, Minisat::vec<Minisat::Lit>* assumps, Minisat::Var var);

Minisat::vec<Minisat::Lit>* newMinisatVecLit (void);

void deleteMinisatVecLit (Minisat::vec<Minisat::Lit>* v);

void pushMinisatVar (Minisat::vec<Minisat::Lit>* v, Minisat::Var var, int sign);

#ifdef __cplusplus
}
#endif
