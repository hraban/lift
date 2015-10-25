# LIFT - the LIsp Framework for Testing

## Introduction

The LIsp Framework for Testing (LIFT) is a unit and system test tool for LISP. 
Though inspired by [SUnit][] and [JUnit][], it's built with Lisp in mind. 
In LIFT, [testcases][] are organized into hierarchical [testsuites][] each of 
which can have its own [fixture][]. When run, a testcase can succeed, fail, 
or error. LIFT supports randomized testing, benchmarking, profiling, and reporting.
