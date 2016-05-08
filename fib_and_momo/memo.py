def fib(n):
	if n == 1:
		return 0
	if n == 2:
		return 1
	return fib(n-2) + fib(n-1)


def memo(f):
	"""Return a memoized version of single-argument function f."""
	cache = {}
	def memoized(n):
		if n not in cache:
			cache[n] = f(n)
		return cache[n]
	return memoized

fib1=memo(fib)

if __name__=='__main__':
	a = fib(40)
	print(a)
