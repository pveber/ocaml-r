#camlp4o;;
#require "R.interpreter";;
#require "R.syntax";;
#require "R.graphics";;
#require "R.stats";;

let o = <:rscript<

a <- $i:1$
print(a)
print(summary(rnorm(100)))

>>
;;
