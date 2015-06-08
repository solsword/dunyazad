#!/bin/sh
pdfcrop --margins="0 -233 0 -177" obvious-report.pdf obvious-q5.pdf
pdfcrop --margins="0 -293 0 -117" obvious-report.pdf obvious-q7.pdf
pdfcrop --margins="0 -233 0 -117" obvious-report.pdf obvious-q5-q7.pdf
pdfcrop --margins="0 -351 0 -59" report-by-stakes.pdf stakes-q8.pdf
