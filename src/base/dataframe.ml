let subset2 = R.symbol ~generic: true "[[.data.frame"

class virtual dataframe = object (self)
  inherit Listing.listing
  method row_names = R.strings_of_t (self#attribute "row.names")
  method column : 'a. int -> 'a R.t = fun x -> R.eval subset2 [
    R.arg (fun x -> x) (R.cast __underlying)  ;
    R.arg R.int        x           ]
  method element : 'a. int -> int -> 'a R.t = fun x y -> R.eval subset2 [
    R.arg (fun x -> x) (R.cast __underlying)     ;
    R.arg R.int        x              ;
    R.arg R.int        y              ]
end

let dataframe r = object inherit dataframe inherit R.instance r end









