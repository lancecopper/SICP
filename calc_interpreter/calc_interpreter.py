# The core of the interpreter for the Calculator language is a recursive 
# function called calc_eval that evaluates a tree-structured expression object.

# takes a string as input and either returns the result of evaluating that string 
# if it is a well-formed Calculator expression or raises an appropriate exception if it is not.

from operator import mul
from functools import reduce

known_operators = ['add', 'sub', 'mul', 'div', '+', '-', '*', '/']

class Exp(object):
	"""
	A call expression in Calculator.
	"""
	def __init__(self, operator, operands):
		self.operator = operator
		self.operands = operands

	def __repr__(self):
		return 'Exp({0}, {1})'.format(repr(self.operator), repr(self.operands))

	def __str__(self):
		operand_strs = ', '.join(map(str, self.operands))
		return '{0}({1})'.format(self.operator, operand_strs)

def calc_eval(exp):
	"""Evaluate a caculator expressions."""
	if type(exp) in (int, float):
		return exp
	elif type(exp) == Exp:
		arguments = list(map(calc_eval, exp.operands))
		return calc_apply(exp.operator, arguments)

def calc_apply(operator, args):
	"""Apply the name operator to a list of args."""
	if operator in ('add', '+'):
		return sum(args)
	if operator in ('sub', '-'):
		if len(args) == 0:
			return TypeError(operator + ' requires at least 1 argument')
		if len(args) == 1:
			return -args[0]
		return sum(args[:1] + [-arg for arg in args[1:]])
	if operator in ('mul', '*'):
		return reduce(mul, args, 1)
	if operator in ('div', '/'):
		if len(args) != 2:
			raise TypeError(operator + ' requires exactly 2 arguments')
		numer,denom = args
		return numer/denom

def read_eval_print_loop():
	"""Run a read-eval-print loop for calculator."""
	while True:
		try:
			expression_tree = calc_parse(input('calc> '))
			print(calc_eval(expression_tree))
		except (SyntaxError, TypeError, ZeroDivisionError) as err:
			print(type(err).__name__ + ':', err)
		except (KeyboardInterrupt, EOFError): 				# <Control>-D, etc.
			print('Calculation completed.')
			return

def calc_parse(line):
	"""Parse a line of calculator input and return an expression tree."""
	tokens = tokenize(line)
	expression_tree = analyze(tokens)
	if len(tokens) > 0:
		raise SyntaxError('Extra token(s): ' + ' '.join(tokens))
	return expression_tree

def tokenize(line):
	"""Convert a string into a list of tokens."""
	spaced = line.replace('(', ' ( ').replace(')', ' ) ').replace(',', ' , ')
	return spaced.split()


def analyze(tokens):
	"""Create a tree of nested lists from a sequence of tokens."""
	assert_non_empty(tokens)
	token = analyze_token(tokens.pop(0))
	if type(token) in (int, float):
		return token
	if token in known_operators:
		if len(tokens) == 0 or tokens.pop(0) != '(':
			raise SyntaxError('expected ( after ' + token)
		return Exp(token, analyze_operands(tokens))
	else:
		raise SyntaxError('unexpected ' + token)

def analyze_operands(tokens):
	"""Analyze a sequence of comma-separated operands."""
	assert_non_empty(tokens)
	operands = []
	while tokens[0] != ')':
		if operands and tokens.pop(0) != ',':
			raise SyntaxError('expected ,')				# Remove ,
		operands.append(analyze(tokens))
		assert_non_empty(tokens)
	tokens.pop(0)										# Remove )
	return operands

def assert_non_empty(tokens):
	"""Raise an exceptionif token is empty."""
	if len(tokens) == 0:
		raise SyntaxError('unexpected end of line')

def analyze_token(token):
	"""Return the value of token if it can be analyzed as a number, or token."""
	try:
		return int(token)
	except (TypeError, ValueError):
		try:
			return float(token)
		except (TypeError, ValueError):
			return token


if __name__ == "__main__":
	read_eval_print_loop()



