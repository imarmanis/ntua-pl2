#!/usr/bin/env python3

from bs4 import BeautifulSoup as BS
import requests
import sys

if len(sys.argv) < 2:
    print("Usage: {} <server-url>".format(sys.argv[0]))
    exit()
url = sys.argv[1]

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

with requests.session() as s:
    r = s.get(url)
    i = 1
    while True:
        bs = BS(r.content, features='html.parser')
        question = bs.find(id="question")
        if not question:
            print("Can't find question...")
        A = question.text
        print("Round {}, length : {}, {}".format(i, len(A), A))
        ans = lps(A)
        print("Answer: {}".format(ans))
        r = s.post(url, data={'answer' : ans} )
        bs = BS(r.content, features='html.parser')
        right = bs.find(class_="right")
        if not right:
            wrong = bs.find(class_="wrong")
            if not wrong:
                print(wrong.text)
            else:
                print()
                print("Bad response from server...")
            break
        print(right.text)
        if bs.find(class_="congratulations"):
            print("Quiz solved !")
            break
        r = s.post(url, data={'again' : 'Continue!'})
        i += 1
