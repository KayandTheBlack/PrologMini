#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model; // for each var --> undef, true, false
vector<int> modelStack; // model represented as a stack (per fer backtracking.)
// si hi ha un 0, marca que ultima popeada es decision.
uint indexOfNextLitToPropagate; //punter que marca index del literal a propagar
uint decisionLevel; // # decisions a la pila, per sortir si insatisfactible (si 0)


//MINE:
vector<pair<int, int> > litRecAppearance; //literal recent appearances (punctuation - lit); (0,n-1)
vector<vector<int> > litclauses; // clauses in which lit appears or opp lit appears (for numVars

//TODO: Implement heuristic! do insertion sort after increments, remake the readClauses part. (natural sort function would work(?))
// 		cada 1000 conflictes, dividir per 2 ? p. ex
uint counter; //COUNTS #conflicts mod heuristic (1000)
bool cmpInv (pair<int,int> a, pair<int,int> b) {
	return (a.first>b.first);
}
void readClauses( ){
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses); 
  //MINE:
  litclauses.resize(2*numVars+1);

  litRecAppearance.resize(numVars);
  for(uint i = 0; i<numVars; i++) litRecAppearance[i].second = i+1; //second is literals
  //-
  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
		clauses[i].push_back(lit);
		//MINE:
		litRecAppearance[abs(lit)-1].first++; //first is #appearances of litteral
		if(lit<0) litclauses[-lit+numVars].push_back(i);
		else litclauses[lit].push_back(i);
		//-
    }
  }
  sort(litRecAppearance.begin(), litRecAppearance.end(), cmpInv);
}



int currentValueInModel(int lit){  //Retorna true false o undef mirant model.
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){ //posa a l'stack y model
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;		
}


bool propagateGivesConflict ( ) {
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    //-
	int oppLit = -modelStack[indexOfNextLitToPropagate];
	int indexOflitClause;
	if(oppLit < 0) indexOflitClause = -oppLit+numVars;
	else indexOflitClause = oppLit;
	//-
    ++indexOfNextLitToPropagate;
    for (uint j = 0; j < litclauses[indexOflitClause].size(); ++j) { //CONJUNT D'OPOSAT! (Teoricament funciona)
	  uint i = litclauses[indexOflitClause][j];
	  //no cal tocar, ja ho fa bé (idea: mira si clausula es ja certa,  ...
	  bool someLitTrue = false;
	  int numUndefs = 0;
	  int lastLitUndef = 0;
	  for (uint k = 0; not someLitTrue and k < clauses[i].size(); ++k){ //comproba clausula, surt si hi ha algun cert
		int val = currentValueInModel(clauses[i][k]);
		if (val == TRUE) someLitTrue = true;
		else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[i][k]; }
	  }
	  if (not someLitTrue and numUndefs == 0) {// conflict! all lits false increment activityCounter of all lits/vars in clause
		for(uint l = 0; l < numVars; l++) {
			for (uint k=0; k<clauses[i].size(); k++) // for all elements in the clause, search in vector
			    if (abs(clauses[i][k]) == litRecAppearance[l].second) {
				litRecAppearance[l].first++;
				uint m = l; 
				while (m>0 and litRecAppearance[m].first > litRecAppearance[m-1].first) {
					pair<int,int> aux = litRecAppearance[m-1];
					litRecAppearance[m-1] = litRecAppearance[m];
					litRecAppearance[m] = aux;
					m--;
				}
			    }
		}
		//counter
		counter++;
		if (counter == 100000) { 
			counter = 0;
			for (uint l=0; l<numVars; l++) litRecAppearance[l].first /=2;
		}
		return true;
	  }
	  else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef); // clause undefined, set something to true!
    }    
  }
  return false;
}


void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}


// Heuristic for finding the next decision literal: (NEED TO CHANGE)
int getNextDecisionLiteral(){
  for (uint i = 0; i <= numVars; ++i) {
	if (model[abs(litRecAppearance[i].second)] == UNDEF) return abs(litRecAppearance[i].second);
  }
  return 0; // reurns 0 when all literals are defined MINE max instead of 0
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:"; //NO ESTA BEN PROPAGAT: visitem menys de les que toca. (mirar propagategives...)
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  counter = 0;
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    while ( propagateGivesConflict() ) {
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
