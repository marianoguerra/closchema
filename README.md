# Closchema

A clojure library that implements
[JSON schema](http://json-schema.org/) validation.

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

- The entire spec http://tools.ietf.org/html/draft-zyp-json-schema-03 is
not implemented yet. We'll gradually support more features when needed.

- The original author's idea was that the checker would accept a
superset of json schema, allowing more powerful function-based
validation.  We are probably not following that path, and limit
ourselves to pure JSON schema checking.
