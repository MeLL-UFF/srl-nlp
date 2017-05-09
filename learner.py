from sys import argv
from json import load
import probfoil

class Learner:

	def __init__(self, kb = None, prolog_out = 'learn_task.pl', done = False):
        self.path_pl_file_out = prolog_out
        self.done = done
        if not done:
            self.pl_file_out = open(prolog_out, 'w')
            settings = '''
% Modes
mode(male(+)).
mode(parent(+,+)).
mode(parent(+,-)).
mode(parent(-,+)).

% Type definitions
base(parent(person,person)).
base(male(person)).
base(female(person)).
base(mother(person,person)).
base(grandmother(person,person)).
base(father(person,person)).
base(male_ancestor(person,person)).
base(female_ancestor(person,person)).
'''
            self.pl_file_out.write(settings)

	def new_example(self, clauses, target):
		assert not done and not self.pl_file_out.closed(), 'This learner is alread configured'
        #TODO

	def learn(self):
        if not done and not self.pl_file_out.closed():
            self.pl_file_out.close()
        probfoil.probfoil.main([self.path_pl_file_out])
        #TOD

	def __repr(self):
		return 'ProbLog'

def _doc2example(document):
	clauses = []
	target = []
	return clauses, target

if __name__ == '__main__':
	file_list = ['out_0.txt']
	doc_list = map(load, file_list)
	all_docs = reduce(lambda x,y: x+y, doc_list)

	learner= Learner()
	for doc in docs:
		learner.new_example(*_doc2example(doc))
	learner.learn()
	print learner