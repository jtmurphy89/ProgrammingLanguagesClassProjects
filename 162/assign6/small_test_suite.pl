#!/usr/bin/perl

# Small test suite for testing the interpreter

use strict;

# gets back the output in a string
sub runQuery($) {
    my $query = shift();
    my $output = `scala -cp scala-parser-combinators.jar:. miniprolog.interpreter.Interpreter prolog_test_suite.pl '$query'`;
    chomp($output);
    return $output;
}

# A test is a reference to a hash:
# name: Name of the test
# query: Query to run
# output: Expected output

sub makeTest($$$) {
    my ($name, $query, $expected) = @_;
    return { name => $name, query => $query, expected => $expected };
}

sub runTest($) {
    my $test = shift();
    print $test->{name} . ": ";
    my $output = runQuery($test->{query});
    if ($output eq $test->{expected}) {
        print "passed!\n";
    } else {
        print ("FAIL.\n\tQuery: " . $test->{query} . 
               "\n\tSaw: $output\n\tExpected: " . $test->{expected} . "\n");
    }
}

my @tests = (
    makeTest("Basic write", "write(X).", "_0"),
    makeTest("Truth", "true.", ""),
    makeTest("Failure", "fail, write(X).", ""),
    makeTest("Conjunction", "write(X), write(X).", "_0\n_0"),
    makeTest("Disjunction 1", "fail; write(X).", "_0"),
    makeTest("Disjunction 2", "(write(X), fail; write(X)).", "_0\n_0"),
    makeTest("Unification 1", "X = 3, write(X).", "3"),
    makeTest("Unification 2", "X = foo, write(X).", "foo"),
    makeTest("Unification 3", "X = bar(1), write(X).", "bar(1)"),
    makeTest("Unification 4", "X = bar(1, 2), write(X).", "bar(1, 2)"),
    makeTest("Unification 5", "Y = foo(1), X = bar(Y), write(X).", "bar(foo(1))"),
    makeTest("Unification 6", "X = bar(Y), Y = foo(1), write(X).", "bar(foo(1))"),
    makeTest("Unification 7", "X = 1, write(X), X = 1, write(X).", "1\n1"),
    makeTest("Unification 8", "X = 1, write(X), X = 2, write(X).", "1"),
    makeTest("Unification 9", "X = foo, write(X), X = foo, write(X).", "foo\nfoo"),
    makeTest("Unification 10", "X = foo, write(X), X = bar, write(X).", "foo"),
    makeTest("Unification 11", "X = foo(1), write(X), X = foo(1), write(X).", "foo(1)\nfoo(1)"),
    makeTest("Unification 12", "X = foo(1), write(X), X = foo(2), write(X).", "foo(1)"),
    makeTest("Unification 13", "X = Y, X = 1, write(Y).", "1"),
    makeTest("Unification 14", "X = Y, X = 1, write(X).", "1"),
    makeTest("Unification 15", "X = Y, Y = 1, write(Y).", "1"),
    makeTest("Unification 16", "foo(X, 1) = foo(2, Y), write(X), write(Y).", "2\n1"),
    makeTest("Unification 17", "foo(X, 1, X) = foo(Y, Y, 2), write(X).", ""),
    makeTest("Compare 1", "1 < 1, write(X).", ""),
    makeTest("Compare 2", "1 < 2, write(X).", "_2"),
    makeTest("Compare 3", "X = 1, X < 1, write(X).", ""),
    makeTest("Compare 4", "X = 1, X < 2, write(X).", "1"),
    makeTest("Compare 5", "X = 1, Y = 2, X < Y, write(X), write(Y).", "1\n2"),
    makeTest("Eval 1", "X is 2 + 1, write(X).", "3"),
    makeTest("Eval 2", "X = 6, Y is X + 1, write(Y).", "7"),
    makeTest("Eval 3", "X = 3, X is 2 + 1, write(X).", "3"),
    makeTest("Check 1", "simpleDigit(X), write(X).", "1"),
    makeTest("Check 2", "simpleDigit(1), X = 2, write(X).", "2"),
    makeTest("Check 3", "digit(X), write(X), fail.", "1\n2\n3"),
    makeTest("Check 4", "append([], [], X), write(X).", "[]"),
    makeTest("Check 5", "append([], [], []), X = 1, write(X).", "1"),
    makeTest("Check 6", "append([], [1, 2], X), write(X).", "[1, 2]"),
    makeTest("Check 7", "append([], [1, 2], [1, 2]), X = 1, write(X).", "1"),
    makeTest("Check 8", "append([1], [2], X), write(X).", "[1, 2]"),
    makeTest("Check 9", "append([1], [2], [1, 2]), X = 1, write(X).", "1"),
    makeTest("Check 10", "append(X, [2, 3], [0, 1, 2, 3]), write(X).", "[0, 1]"),
    makeTest("Check 11", "append([0, 1], X, [0, 1, 2, 3]), write(X).", "[2, 3]"),
    makeTest("Check 12", "append(X, Y, [0, 1, 2, 3]), write(X), write(Y), fail.",
             "[]\n[0, 1, 2, 3]\n[0]\n[1, 2, 3]\n[0, 1]\n[2, 3]\n[0, 1, 2]\n[3]\n[0, 1, 2, 3]\n[]")
    );

foreach my $test (@tests) {
    runTest($test);
}
