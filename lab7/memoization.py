def memoizate(func):
	memo = {}
	def wrapper(*args):
		if args not in memo:
			memo[args] = func(*args)
		return memo[args]
	return wrapper

if __name__ == "__main__":
	@memoizate
	def find_divisors_memo(n):
	    divisors = []
	    for i in range(1, n+1):
	        if n % i == 0:
	            divisors.append(i)
	    return divisors

	print(find_divisors_memo(1236))