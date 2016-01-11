#ifdef __cplusplus
#define minisatSolver Minisat::SimpSolver
#define minisatVar Minisat::Var
#define minisatLit Minisat::Lit
#define lbool Minisat::lbool
#define litptr Minisat::Lit
#define minisatVecLit Minisat::vec<Minisat::Lit>*
extern "C" {
#else
typedef int minisatSolver;
typedef int minisatVar;
typedef int minisatLit;
typedef void lbool;
typedef void litptr;
typedef void minisatVecLit;
#endif

typedef struct result result;

struct result {
  unsigned solved;
  unsigned modelSize;
  unsigned conflictSize;
  lbool* model;
  litptr* conflict;
} res = {0, 0, 0, 0, 0};

void printMinisatStats (minisatSolver*);

minisatSolver* newMinisatSolver (void);

void deleteMinisatSolver (minisatSolver* solver);

minisatVar newMinisatVar (minisatSolver* solver, int upol, int dvar);

void releaseMinisatVar (minisatSolver* solver, minisatVar var, int sign);

int addMinisatClause (minisatSolver* solver, minisatVecLit* ps);

int simplifyMinisat (minisatSolver* solver);

result *solveMinisatWithAssumps (minisatSolver* solver, minisatVecLit* assumps);

int solveMinisat (minisatSolver* solver);

int *getMinisatConflictVec (minisatSolver* solver, minisatVecLit* assumps);

int getMinisatConflictSize (minisatSolver* solver, minisatVecLit* assumps);

int valueMinisatLit (minisatLit lit);

int varMinisatLit (minisatLit lit);

int valueMinisatVar (minisatSolver* solver, minisatVecLit* assumps, minisatVar var);

minisatVecLit* newMinisatVecLit (void);

void deleteMinisatVecLit (minisatVecLit* v);

void pushMinisatVar (minisatVecLit* v, minisatVar var, int sign);

#ifdef __cplusplus
}
#endif
