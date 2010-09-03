(ns test-closchema
 (:use [closchema] :reload-all) 
 (:use lazytest.describe))  

(comment
(defmacro
  "Convience to allow to bind simple values with the given options in lazytest."
  [v] `(context [] ~v))
)

(def base-schema {:type "object"
                  :properties {:id    {:type "number"}
                               :name  {:type "string"}
                               :description {:optional true :type "string"}}})
 
(describe validate "object"
    
  (testing "with properties schema"
    (given [s base-schema] 
 
    (it "should accept object with only required properties"
      (validate s {:id 1 :name "shoe"})) 

    (it "should accept object with optional properties"
      (validate s {:id 1 :name "shoe" :description "style"}))
    
    (it "should not accept when required property is not present" 
      (not (validate s {:id 1})))
    
    (it "should not accept when property type is incorrect"
      (not (validate s {:id "1" :name "bag"})))))
  
   
 (testing "with additional properties schema"

    (testing "set to false"
      (given [s (merge base-schema {:additionalProperties false})] 

      (it "should not allow any properties that are not defined in schema"
        (not (validate s {:id 1 :name "wine" :age "13 years"})))

      (it "should allow any properties that are defined in schema as optional"
        (validate s {:id 1 :name "wine" :description "good"}))))

    (testing "defining a schema"
      (given [s (merge base-schema {:additionalProperties {:type "string"}})]

      (it "should enforce that all extra properties conform to the schema"
        (and 
         (validate s {:id 1 :name "wine" :age "13 years" :country "france"})
         (not (validate s {:id 1 :name "wine" :age 13})))))))

 (testing "with required fields"
  (given [s (assoc base-schema :properties (merge (base-schema :properties) 
                                  {:address  {:type "string" :optional true}
                                   :address2 {:type "string" :requires "address" :optional true}}))]
    (it "should validate with non-required"
     (validate s {:id 1 :name "john" :address "street"}) )
    (it "should validate when both are present"
     (validate s {:id 1 :name "john" :address "street" :address2 "country"}) )
    (it "should not validate nem required is not present"
     (not (validate s {:id 1 :name "john" :address2 "country"})))) )
 )

 

(describe validate "items"

  (testing "with no items definition"
    (it "should allow any"
      (and 
       (validate {:type "array"} [ 1 2 3])
       (validate {:type "array" :items []} [1 "2" 3]))))
  
  (testing "with object schema" 
   (given [s1 {:type "array" :items {:type "string"}}
           s2 {:type "array" :items {:type "object" :properties {:name {:type "string"}}}}]
    (it "should accept empty array"
      (validate s1 []))
     
    (it "should accept homogenous array"
      (and  
       (validate s1 ["a" "b" "c"])
       (validate s2 [{:name "half"} {:name "life"}])))
 
    (it "should not accept heterogenous array"
      (not (validate s1 ["a" "b" 3])))

    (it "should not accept if inner item does not follow item schema" 
      (not (validate s2 [{:name "quake"} {:id 1}])))))


    
  (testing validate "with array of schemas"      
      
    (testing "with single object definitions"
      (given [s {:type "array" :items [{:type "string"} {:type "string"}]}]
      
      (it "should enforce tuple typing"
        (and  
         (validate s ["a" "b"])
         (not (validate s ["a"]))
         (not (validate s ["a" 1]))))

      (it "should allow additional properties to be defined by default"
        (and 
         (validate s ["a" "b" "c"])
         (validate s ["a" "b" 1 2 3 {}])))

      )

    (testing "with additional properties bounded"
      (given [s {:type "array" :items [{:type "number"}] :additionalProperties {:type "string"}}] 

      (it "should ensure type schema for additional properties"
        (and 
         (validate s [1 "a" "b" "c"])
         (not (validate s [1 2 "a"]))))

      (it "should still enforce tuple typing"
        (not (validate s ["a" 1])))))

      
    (testing "with aditional properties disabled"
      (given [s {:type "array" :items [{:type "number"}] :additionalProperties false}] 
      
      (it "should be strict about tuple typing"
        (validate s [1])) 

      (it "should be strict about tuple typing"  
        (not (validate s [1 "a"])))))

    (testing "with unique items equal true"      
      (given [s {:type "array" :uniqueItems true}]
      
      (testing "should allow array with same items"
        (it "of type numbers" (validate s [1 1])) 
        (it "of type strings" (validate s ["a" "a" "a"]))
        (it "of type objects" (validate s [{:a 1} {:a 1} {:a 1}])))
 
      (testing "should not allow different items"
        (it "of type numbers" (not (validate s [1 2 3])))
        (it "of type strings" (not (validate s ["a" "b"])))
        (it "of type objects" (not (validate s [{:a 1} {:b 1} {:a 1}])))))))))


(describe validate "string"
 (testing "with common string"
  (given [s {:type "string"}] 
      (it "should validate with string"
       (validate s "foobar")))) 
 (testing "with mininum only"
  (given [s {:type "string" :minLength 3}] 
      (it "should validate if within the lengths"
        (validate s "foobar")) 
      (it "should not validate if not within the lengths"
        (not (validate s "fo"))))) 
 (testing "with maxinum only"
  (given [s {:type "string" :maxLength 5}] 
      (it "should validate if within the lengths"
       (validate s "fooba")) 
      (it "should not validate if not within the lengths"
       (not (validate s "foobar"))))) 
 (testing "with mininum and maxinum"
  (given [s {:type "string" :maxLength 5 :minLength 3}] 
      (it "should validate if within the lengths"
       (and 
        (validate s "foo")
        (validate s "fooba")) )
      (it "should not validate if not within the lengths"
       (and 
           (not (validate s "fo"))
           (not (validate s "foobar"))))) )
 (testing "pattern"
  (given [s {:type "string" :pattern "^[a-zA-Z0-9]+$"}]
   (it "should validate if pattern matches"
       (and 
        (validate s "fooBar")
        (validate s "fooBar123")
        (validate s "123fooBar")))
   (it "should not validate if pattern dont match"
       (and 
        (not  (validate s "foo-Bar"))
        (not (validate s "foo_bar")))))) 
 (testing "enum"
   (given [s {:type "string" :enum ["foo" "bar"] }]
    (it "should validate if string is in enum"
     (and 
         (validate s "foo")
         (validate s "bar")))
    (it "should not validate if string is not in enum"
     (and  
         (not (validate s "foobar"))
         (not (validate s "ar"))))) ) 
 )
