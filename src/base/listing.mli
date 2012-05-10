(**  Virtual class for R list S3 objects. *)
class virtual listing : object
  inherit R.s3
  method names : string list
end
