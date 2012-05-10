class virtual listing = object (self)
  inherit R.s3
  method names = R.strings_of_t (self#attribute "names")
end

