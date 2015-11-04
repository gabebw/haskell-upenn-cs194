# Spring 2013 HW2

To run it:

```
$ ghci LogAnalysis.hs
> testParse (map parseMessage . lines) 10 "error.log"
> testWhatWentWrong (map parseMessage . lines) whatWentWrong "error.log"
```

More about mustard watches: http://wadler.blogspot.com/2013/03/mustard-watch.html
