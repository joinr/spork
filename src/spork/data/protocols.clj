;;shared protocols for data structures.
(ns spork.data.protocols)

(defprotocol IFrontBack
  (front [q])
  (back [q]))

(defprotocol IInsertable
  (insert [q v]))
