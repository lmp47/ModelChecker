#ifdef __cplusplus
#define minisatSolver Minisat::SimpSolver
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

void printMinisatStats (Minisat::SimpSolver*);

Minisat::SimpSolver* newMinisatSolver (void);

void deleteMinisatSolver (Minisat::SimpSolver* solver);

Minisat::Var newMinisatVar (Minisat::SimpSolver* solver, int upol, int dvar);

void releaseMinisatVar (Minisat::SimpSolver* solver, Minisat::Var var, int sign);

int addMinisatClause (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* ps);

int simplifyMinisat (Minisat::SimpSolver* solver);

result *solveMinisatWithAssumps (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* assumps);

int solveMinisat (Minisat::SimpSolver* solver);

int *getMinisatConflictVec (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* assumps);

int getMinisatConflictSize (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* assumps);

int valueMinisatLit (Minisat::Lit lit);

int varMinisatLit (Minisat::Lit lit);

int valueMinisatVar (Minisat::SimpSolver* solver, Minisat::vec<Minisat::Lit>* assumps, Minisat::Var var);

Minisat::vec<Minisat::Lit>* newMinisatVecLit (void);

void deleteMinisatVecLit (Minisat::vec<Minisat::Lit>* v);

void pushMinisatVar (Minisat::vec<Minisat::Lit>* v, Minisat::Var var, int sign);

#ifdef __cplusplus
}
#endif
