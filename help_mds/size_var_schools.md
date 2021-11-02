---
title: "Size Variable"
author: "Joshua Eagan"
date: "9/16/2021"
output: html_document
---

## What is this input?
The size variable input in this app allows you to display an additional variable
(other than your color variable) on the map. Schools are represented by 
points in the 'school mapper' section of the app, so you can use the size of
these points to convey information about one of your numeric variables. The 
variable you choose for this input must be continuous (for our purposes, it 
must not contain any non-numeric characters.) Larger values will correspond with
larger points and smaller values will correspond with smaller points. 

## Log Scale
Since many variables increase exponentially with population size, your
size variable will be transformed using a 'log transformation' to compensate 
for this. Specifically, 2ln(x)=ln(10x), which means that the log transformation
of your variable will double every time your variable increases by a factor of
10. In this app, numbers below 1 are translated to a very small but visible size
for convenience. See
<https://en.wikipedia.org/wiki/Data_transformation_(statistics)> for more 
information.