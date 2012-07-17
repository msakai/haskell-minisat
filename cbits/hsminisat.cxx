/*
Copyright (c) 2008, Masahiro Sakai
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/

#include "core/Solver.h"
using namespace Minisat;

extern "C" vec<Lit>* hsminisat_newVecLit(void)
{
    return new vec<Lit>();
}

extern "C" void hsminisat_deleteVecLit(vec<Lit>* p)
{
    delete p;
}

extern "C" void hsminisat_vecLit_pushVar(vec<Lit>* p, int var, int sign)
{
    p->push(mkLit(var, (bool)sign));
}

extern "C" Solver* hsminisat_newSolver()
{
    return new Solver();
}

extern "C" void hsminisat_deleteSolver(Solver* solver)
{
    delete solver;
}

extern "C" Var hsminisat_newVar(Solver* solver, int polarity, int dvar)
{
    return solver->newVar(polarity, dvar);
}

extern "C" int hsminisat_addClause(Solver* solver, vec<Lit>* clause)
{
    return solver->addClause(*clause);
}

extern "C" int hsminisat_simplify(Solver* solver)
{
    return solver->simplify();
}

extern "C" int hsminisat_solve(Solver* solver, vec<Lit>* assumps)
{
    return assumps ? solver->solve(*assumps) : solver->solve();
}

extern "C" int hsminisat_okay(Solver* solver)
{
    return solver->okay();
}

extern "C" void hsminisat_setPolarity(Solver* solver, Var v, int b)
{
    solver->setPolarity(v, b);
}

extern "C" void hsminisat_setDecisionVar(Solver* solver, Var v, int b)
{
    solver->setDecisionVar(v, b);
}

static inline int lbool2int(const lbool& b)
{
    return (b==l_True) ? 1 : (b==l_False) ? -1 : 0;
}

extern "C" int hsminisat_value(Solver* solver, Var x)
{
    return lbool2int(solver->value(x));
}

extern "C" int hsminisat_modelValue(Solver* solver, Var v)
{
    Lit l = mkLit(v, false);
    return lbool2int(solver->modelValue(l));
}

extern "C" int hsminisat_nAssigns(Solver* solver) { return solver->nAssigns(); }
extern "C" int hsminisat_nClauses(Solver* solver) { return solver->nClauses(); }
extern "C" int hsminisat_nLearnts(Solver* solver) { return solver->nLearnts(); }
extern "C" int hsminisat_nVars   (Solver* solver) { return solver->nVars();    }
