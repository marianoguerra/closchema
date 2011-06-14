Closchema
=========

A clojure library that implements http://json-schema.org/.

The idea is that it becomes a superset of json schema, to also allow
more powerful function-based validation.


Examples
==========

        (require 'com.bigml.closchema)


        (validate {:type "array" :items {:type "string"}} ["1" "2"])
        => true


        (validate {:type "array" :items {:type "string"}} ["1" 2])
        => false

        (report
          (validate {:type "array" :items {:type "string"}} ["1" 2 {}]))
          => '(.. errors )



Development
===========
you should know the drill:

	lein install
	lein swank
	./test.sh


TODO
====

The entire spec http://tools.ietf.org/html/draft-zyp-json-schema-03 is
not implemented yet. We'll gradually support more features when needed.
