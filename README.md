# aoc2019

To run a Day's solution go to the root dir of the project, open the file in the cabal repl and run its part1 or part2 method.
To run the tests: cabal test, or start the repl with cabal repl all_tests. 
Note: cabal test will write all test output in the log file.
To see test results immediately, run cabal test --test-show-details=streaming 

Build and run a fresh executable:
```
cabal clean
cabal configure
cabal build
cabal run aoc2019
```

Build and run the test suite, in case you wouldn' want to use cabal test:
```
cabal clean
cabal configure --enable-tests
cabal build all_tests
cabal run all_tests
```


