#!/usr/bin/env python

import numpy
data = [
[-0.107358,-0.049577,+0.126623,-0.106948,-0.755809],
[+0.051781,-0.265425,+0.548136,+0.051735,-0.026964],
[+0.941475,-0.975603,+0.411370,+0.755263,-0.055232],
[-0.770979,-0.360797,-0.476552,-0.656793,-0.346903],
[+0.738044,-0.380252,-0.769260,+0.635805,-0.476552],
]

def median3x3(input):
    out = []
    for i in range(1, len(input) - 1):
        jout = []
        for j in range(1, len(input[0]) - 1):
            tmp = numpy.median(input[i-1][j-1:j+2]+input[i][j-1:j+2]+input[i+1][j-1:j+2])
            jout.append(tmp)
        out.append(jout)
    return out

res=median3x3(data)
for r in data:
    print r
