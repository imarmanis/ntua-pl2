#!/usr/bin/env python3

from bs4 import BeautifulSoup as BS
from lps import lps
import requests
import sys


if len(sys.argv) < 2:
    print("Usage: {} <server-url>".format(sys.argv[0]))
    exit()
url = sys.argv[1]


with requests.session() as s:
    r = s.get(url)
    i = 1
    while True:
        bs = BS(r.content, features='html.parser')
        question = bs.find(id="question")
        if not question:
            print("Can't find question...")
        A = question.text.strip()
        print("Round {}, length : {}, {}".format(i, len(A), A))
        ans = lps(A)
        print("Answer: {}".format(ans))
        r = s.post(url, data={'answer' : ans} )
        bs = BS(r.content, features='html.parser')
        right = bs.find(class_="right")
        if not right:
            wrong = bs.find(class_="wrong")
            if wrong:
                print(wrong.text)
            else:
                print("Bad response from server...")
            break
        print(right.text)
        if bs.find(class_="congratulations"):
            print("Quiz solved !")
            break
        r = s.post(url, data={'again' : 'Continue!'})
        i += 1
