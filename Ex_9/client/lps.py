def lps(x):
    n = len(x)
    old = [0]*(n+1)
    curr = [1]*(n)
    for i in range(n-1):
        old, curr = curr, [
            c + 2 if x[j] == x[j+i+1] else max(a, b)
            for j, (a,b,c) in enumerate(zip(curr, curr[1:], old[1:]))
        ]
    return (n - curr[0])
