(ns spork.util.excel.docjure
  (:require [spork.util.metaprogramming :as util]))

;;Monkey patches to control docjure stuff. May migrate perf improvements and
;;date ignorance back in here.

;;For now, we disable logging.
(System/setProperty  "log4j2.loggerContextFactory" "org.apache.logging.log4j.simple.SimpleLoggerContextFactory")

(require 'dk.ative.docjure.spreadsheet)

;;retain backwards compatibility with some downstream stuff.  we just provide a facade
;;for the docjure implementation and direct there.

(util/import-vars
 [dk.ative.docjure.spreadsheet
  add-name!
  add-row!
  add-rows!
  add-sheet!
  apply-date-format!
  as-font
  assert-type
  auto-size-all-columns!
  auto-size-column!
  border
  cell-fn
  cell-reference
  cell-seq
  color-index
  column-index-seq
  create-cell-style!
  create-font!
  create-sparse-workbook
  create-workbook
  create-xls-workbook
  escape-cell
  get-font
  get-row-styles
  horiz-align
  into-seq
  load-workbook
  load-workbook-from-file
  load-workbook-from-resource
  load-workbook-from-stream
  read-cell
  read-cell-value
  remove-all-rows!
  remove-row!
  row-seq
  row-vec
  row?
  save-workbook!
  save-workbook-into-file!
  save-workbook-into-stream!
  select-cell
  select-columns
  select-name
  select-sheet
  set-cell!
  set-cell-comment!
  set-cell-style!
  set-font
  set-row-style!
  set-row-styles!
  sheet-name
  sheet-name?
  sheet-seq
  sheet?
  string-cell?
  vert-align
  whens
  workbook?])
