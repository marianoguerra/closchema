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

## JSON Object Referencing (Schema extension)

This library implements the **file-based** part of referencing described here (at the end of the post):

http://www.sitepen.com/blog/2008/10/31/json-schema/

Right now, you can only specify filenames (not full URLs), and they must be located in your Java resources directory, as specified on the classpath.  **There is no ID-based referencing as of this readme.**

With this extension, one can specify a reference to a file containing a schema using the key `$ref` when describing a property:

```
{
    "type" : object
    "properties" : {
        "id" : {"type" : "integer"}
        "address" : {"$ref" : "path/to/file"}
    }
}
```
Reading is lazy, so the schema is not read until we hit it at validation time.  Files are read using `slurp` and `clojure.data.json`.  Schema references are cached using `memoize`.

## TODO

- The entire spec http://tools.ietf.org/html/draft-zyp-json-schema-03 is not implemented yet. We'll gradually support more features when needed.