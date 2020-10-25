# deltafinder

A quick repl'd together thing to take a csv dump from my oscilloscope, graph the signals and the delay between the outset of signal A and onset of signal B.

It's not generic, it won't do anything cool except take some csv and graph it with some underlying assumptions.


## Usage


Run the project directly:

    $ clojure -M -m abb.deltafinder "settings.edn"

Where your edn looks something like this:

``` clojure
{:cols       [0 1] ;; columns to extract
 :names      [:midi :audio] ;; names of the column
 :thresholds [2 2] ;; on/outset thresholds
 :tails      [500 500] ;; time after last threshold violation to define 'outset'
 :sr         500 ;; samplerate in khz
 :daw        "Studio One" ;; DAW name
 :buffer     128 ;; buffer size
 :setting    "Minimum" ;; Extra settings
 :version    "5.0.2" ;; DAW version
 :index      1} ;; index of file
```

It will look for csv files in the form of `[daw][buffer][setting][index].csv` (without the [])

## License

Copyright © 2020 Robertrandolph

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
