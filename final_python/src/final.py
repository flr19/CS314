from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False


	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
		if isinstance(t,Variable):
			return set([t])
		elif isinstance(t,Function):
			thisset = set()
			for i in range(len(t.terms)):
				thisset=thisset.union(self.variables_of_term(t.terms[i]))
			return thisset
		else:
			return set()
     
		

	def variables_of_clause (self, c : Rule) -> set :
		thisset=set()
		thisset=self.variables_of_term(c.head)
		for i in range(len(c.body.terms)):
			thisset=thisset.union(self.variables_of_term(c.body.terms[i]))
		return thisset

		#return set(c.Variable)


	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
	def substitute_in_term (self, s : dict, t : Term) -> Term:
		if isinstance(t, Variable):
			if t in s.keys():
				return s[t]
			return t
		if isinstance(t, Function):
			new_terms = []
			for term in t.terms:     # Iterate  the function t's parameters
				new_terms.append(self.substitute_in_term(s,term)) # Add substituted term to new_terms 
			return Function(t.relation, new_terms)
		else:
			return t

	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		newhead=self.substitute_in_term(s,c.head)
		newbody=[]
		for i in range(len(c.body.terms)):
			newbody.append(self.substitute_in_term(s,c.body.terms[i]))

		return Rule(newhead,RuleBody(newbody))
		


	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
	def unify (self, t1: Term, t2: Term) -> dict:
		d={}
		d=self.unifyhelper(t1,t2,d)
		return d
	
	def unifyhelper(self, t1: Term, t2: Term, s) -> dict:
		t1=self.substitute_in_term(s,t1)
		t2=self.substitute_in_term(s,t2)
			#s[str(t2)]=str(self.substitute_in_term(s,t2))
		varsX=self.variables_of_term(t1)
		varsY=self.variables_of_term(t2)
		if(isinstance(t1,Variable) and t1 not in varsY):
			#s={str(t1):str(t2)}
			for key in s.keys():
				s[key]=self.substitute_in_term({t1:t2}, s[key])
			s[t1]=t2
			return s
		if(isinstance(t2,Variable) and t2 not in varsX):
			for key in s.keys():
				s[key]=self.substitute_in_term({t2:t1}, s[key])
			s[t2]=t1
			return s
		if isinstance(t1,Function) and isinstance(t2,Function):
			if(len(t1.terms)==len(t2.terms) and t1.relation==t2.relation):
				for (x,y) in zip(t1.terms,t2.terms):
					s=self.unifyhelper(x,y,s)
				return s
			else:
				raise Not_unifiable()
		if t1==t2:
			return s
		else:
			raise Not_unifiable()

	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))
	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)


	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''
	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
		goal=pgoal
		resolvent=[]
		for i in goal:
			resolvent.append(i)
		while(resolvent):
			index=random.randint(0,len(resolvent)-1)
			a=resolvent[index]
			#aprime=self.freshen(program[random.randint(0,len(program)-1)])
			unified=[]
			for i in range(len(program)):
				aprime=program[i]
				try:
					theta=self.unify(a, aprime.head)
					unified.append(aprime)
				except Not_unifiable:
					continue
			if(len(unified)==0):
				return pgoal
			aprime=self.freshen(unified[random.randint(0,len(unified)-1)])
			theta=self.unify(a,aprime.head)
			resolvent.pop(index)
			for i in aprime.body.terms:
				resolvent.append(i)
			for i in range(len(pgoal)):
				pgoal[i]=self.substitute_in_term(theta,pgoal[i])
			for i in range(len(resolvent)):
				resolvent[i]=self.substitute_in_term(theta,resolvent[i])
		return pgoal


	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		solutionslist=[]
		resolvent=[]
		for i in pgoal:
			resolvent.append(i)
		self.dfs(resolvent,pgoal,solutionslist,program)
		return solutionslist



	def dfs (self,resolvent,goal,solutions:List[List[Term]] ,program):
		if len(resolvent)==0:
			solutions.append(goal)
			return True
		while(resolvent):
			chosen_goal=resolvent.pop(0)
			print(chosen_goal)
			searched=False
			for rule in program:
				
				try:
					rule=self.freshen(rule)
					
					theta=self.unify(chosen_goal,rule.head)
					print("rule",rule.head)
					new_resolvent=[]
					new_goal=[]
					for i in range(len(resolvent)):
						new_resolvent.append(resolvent[i])
					for i in range(len(goal)):
						new_goal.append(goal[i])
					for i in range(len(rule.body.terms)):
						new_resolvent.append(rule.body.terms[i])
					for i in range(len(new_resolvent)):
						new_resolvent[i]=self.substitute_in_term(theta,new_resolvent[i])
					for i in range(len(new_goal)):
						new_goal[i]=self.substitute_in_term(theta,new_goal[i])
					for i in theta.keys():
						print("sss", i,theta[i])
					for i in new_resolvent:
						print("res",i)
					for i in new_goal:
						print("goal", i)
					result = self.dfs(new_resolvent, new_goal, solutions,program)
					searched=searched or result
				except Not_unifiable:
					continue
			return searched