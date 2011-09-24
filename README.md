# Closchema

A clojure library that implements http://json-schema.org/.

(The original author's idea was that it becomes a superset of json
schema, to also allow more powerful function-based validation.  We are
probably not following that path, and limit ourselves to pure JSON
schema checking.)

## Examples

        (require 'com.bigml.closchema)


        (validate {:type "array" :items {:type "string"}} ["1" "2"])
        => true


        (validate {:type "array" :items {:type "string"}} ["1" 2])
        => false

        (report-errors
          (validate {:type "array" :items {:type "string"}} ["1" 2 {}]))
          => '(.. errors )

## TODO

The entire spec http://tools.ietf.org/html/draft-zyp-json-schema-03 is
not implemented yet. We'll gradually support more features when needed.
