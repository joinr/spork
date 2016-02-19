;TOM SPOON 9 July 2012
;A set of extensions for much more flexible file IO operations, folder functions
;and convenience functions for high-level IO.  Many things that are msising in
;clojure.java.io are here.

;Maps are used as a basis for defining folder structure, and can serve as a 
;serialization platform as well.  Provides quick access to common system paths,
;and functions for creating relative paths from a given directory.

;Also provides a simple API for zipping files.  Maps can be persisted to 
;zipfiles.
(ns spork.util.io
    (:require [clojure [string :as strlib] 
               [set :as setlib]]
              [clojure.core.reducers :as r]
              [clojure.java [io :as io]]
              [spork.util.general :as general]
              [clojure [pprint :as pp]]))


;;replacement for line-seq, allows a more useful idiom
;;for reading files, and is slightly more efficient (no intermediate
;;calls to seq, less garbage).
(defn line-reducer
  "Given a string literal that encodes a path, or a newline-delimited 
   sequence of lines, returns a reducible obj that iterates over each line (string) 
   delimited by \newline."
  [path-or-string]
  (let [reader-fn (if (general/path? path-or-string)
                    clojure.java.io/reader
                    general/string-reader)]
    (reify clojure.core.protocols/CollReduce
      (coll-reduce [o f init]
        (with-open [^java.io.BufferedReader rdr (reader-fn path-or-string)]
          (loop [acc init]
            (if (reduced? acc) @acc 
                (if-let [ln (.readLine rdr)]
                  (recur (f acc ln))
                  acc)))))
      (coll-reduce [o f]
        (with-open [^java.io.BufferedReader rdr (reader-fn path-or-string)]
          (if-let [l1 (.readLine rdr)]
            (loop [acc l1]
              (if (reduced? acc) @acc 
                  (if-let [ln (.readLine rdr)]
                    (recur (f acc ln))
                    acc)))
            nil)))
      )))

;;a map of the environment vars, really handy.
(def  env-map 
  (->> (System/getenv) (map (fn [[k v]] [(keyword k) v])) (into {})))

(defn get-env
  "Fetches an environment variable with the same name from the environment map.
   Note, variable names are expected to be keywordized versions of their env 
   variable names, so PATH would be :PATH, TheVar woul be :TheVar ."
  [k] (get env-map k))

(def emptyq clojure.lang.PersistentQueue/EMPTY)
(defn as-directory [s] 
  (if (= (subs s (dec (count s))) "\\")
    s
    (str s "\\")))

(def home-path (System/getProperty "user.home"))

(defn deep-copy 
	"Copies all files in sourcedir to targetdir.  Creates folders as needed"
	[sourcedir targetdir] 
	(let [source-paths (map #(.getPath %) (file-seq (io/file sourcedir)))
		  dest-paths   (map #(str targetdir (subs % (count sourcedir))) source-paths)
		  pathmap (zipmap source-paths dest-paths)]
	  (doseq [kv pathmap]
		(let [[kf vf] (map io/file kv)]		
			(when (not (.exists vf)) (io/make-parents vf))
			(when (.isFile kf) (io/copy kf vf))))))


(declare relative-path)

(defmacro with-path
  "Given a root directory, and a collection of bindings in the form 
   [path [subdir1 subdir2...file]], evals body inside an expression 
   with *root* bound to the root path, and each binding available as 
   a fully-realized file path (relative-path *root* %) is called on 
   each pathlist)."
  [root bindings body]
  (let [binds (mapcat 
                (fn [[nm pathlist]] 
                  (list nm (list 'relative-path '*root* pathlist)))
                    (partition 2 bindings))]               
    `(let [~'*root* ~root
           ~@binds]
       ~body)))

(def common-paths {:home home-path 
                   :docs (str home-path  "\\Documents")
                   :javapath (System/getProperty "sun.boot.library.path")
                   :startdir (System/getProperty "user.dir")
                   :tempdir (System/getProperty "java.io.tmpdir")
;                   :path   (strlib/split (System/getProperty "java.library.path")
;                             #";")
;                   :classpath (strlib/split (System/getProperty "java.class.path")
;                               #";")
                   :javahome  (System/getProperty "java.home")})

(defn common-path
  "Fetch a common path from the JVM session.  valid keys are
   :home      - The user's home directory. 
   :docs      - The user's Documents folder (typically a windows thing, may drop).
   :javapath  - The path to java executables.
   :startdir  - The directory the JVM started in.
   :tempdir   - The directory the JVM will use for temporary files.
   :path      - A list of paths on the system Path.
   :classpath - A list of paths on the Class Path.
   :javahome  - The path to the Java Runtime Environment."
  [key] (get common-paths key))                           

(defn relative-path
  "Given a root path (a string), and a list of strings, generates a relative
   path.  Auxillary function for making paths easier to deal with."
  [root pathlist]
  (let [compath (common-path root)]    
    (apply str (as-directory (if compath compath root)) 
                             (butlast (interleave pathlist (repeat "\\"))))))

(def ^:dynamic *current-dir* (common-path :startdir))

(defmacro with-currentdir
  "Supplies the most recent binding of *current-dir* as the root to with-path, 
   operating identically to with-path."
  [binds & body]
  `(with-path *current-dir* ~binds ~@body))

(defn load-script 
  "Identical to load-file, except it assumes the filename is relative to a 
   scripts directory relative to the current dir."
  [fname]
  (load-file (relative-path *current-dir* ["scripts" fname])))

;loaded from the 1.2 version of clojure.java.io, dunno why it was dropped.
(defn delete-file-recursively
  "Delete file f. If it's a directory, recursively delete all its contents.
   Raise an exception if any deletion fails unless silently is true."
  [f & [silently]]
  (let [f (io/file f)]
    (if (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-file-recursively child silently)))
    (io/delete-file f silently)))

;Generic file utilities that will be moved into a standalone file.
(defn make-folders!
  "make-parents requires a file.  This supplies a dumb file, and creates the 
   folder structure necessary."
  ([root pathlist]
    (io/make-parents (relative-path root pathlist)))
  ([root]  (make-folders! root ["blah.txt"])))

(defn clear-folders!
  "Ensures that every file in the folder path is wiped out.  A predicate 
   function pred may be supplied, which should be of type 
   pred::java.io.File -> boolean."
  ([pred path]
	  (doseq [f (filter pred (file-seq (io/file path)))]
	    (io/delete-file f)))
  ([path] (clear-folders! 
            (fn [f] (not (.isDirectory f))) path)))


(defmacro with-temp-dir
  "Evaluats body inside a context in which *tmpdir* is bound to a path that 
   references a newly-created directory in the system's temporary directories.
   Expressions in body can reference the path defined by *tmpdir*.  After body
   evaluates, the directory at *tmpdir* is recursively deleted."
  [& body]
  `(let [dir# (relative-path :tempdir  [(str "tmp_" (gensym))])
         ~'*tmpdir* dir#
         ~'_     (make-folders! dir#)]
     (try ~@body
          (finally (delete-file-recursively dir#)))))   

;;replace with mapv...this is old and not needed.  also not a huge deal though.
(defn vector-map
  "Helper function to keep vector->vector transforms."
  [f v]
  (reduce #(conj %1 (f %2)) [] v))

(defn hock
  "A variation of spit.  hock takes the same args as spit, but ensures that 
   the parents in the path exist."
  [path contents & options]
  (let [f (io/file path)]
    (do 
	    (if (.exists f)
	      (io/delete-file path)
	      (make-folders! path []))
     (if (seq options)
       (spit f contents options)
       (spit f contents)))))

(defn list-path
  "Split the file path into a vector of strings."
  [fl] 
  (strlib/split  
    (if (string? fl) 
      fl
     (.getPath fl)) #"\\"))
  
(defn butlast-vec
  "Similar to butlast, but uses vector operations to avoid sequence op 
   overhead."
  [v]
  (when (seq v)
    (subvec v 0 (dec (count v)))))

;utilities for persisting clojure structures to the file system.
(def prefixmap {:keyword "KEY_"
                :vector "VEC_"
                :list "LIST_"})

(def suffixmap {:clj ".clj"
                :json ".json"
                :csv ".csv"})
(defn- recognizer
  "Given a map of :keyword to regex string, converts strings into regex patterns.
   Used for pattern matching factories."
  [m] 
  (for [[k v] m]
     [k (re-pattern v)]))

(def regmap (recognizer prefixmap))
(def sufregmap (recognizer suffixmap))

(defn- drop-prefix
  "Converts a prefixed string into a regular string."
  [s pre] 
  (subs s (count (get prefixmap pre))))

(defn- matcher
  "Given a string and a recognizer map, applies each recognizer in the map until
   it finds a result.  Returns the relative key of the recognizer that matched.
   Used for string-based dispatching."
  [s rmap] 
  (loop [remaining (seq rmap)]
    (if-let [[t re] (first remaining)]      
	    (if (re-find re s)
	      t
	      (recur (rest remaining)))
     :string)))

(defn- filename->type
  "Matches a filename to a prefix type, either a clojure keyword, or a :string."
  [fname]  (matcher fname regmap))
(defn- filename->ext
  "Matches a filename extensions to a known suffix type (clj, json, csv)"  
  [fname] (matcher fname sufregmap))

(defn drop-ext
  "Chops the extension off a filename.  If the filename has no extension, fname
   is returned.  Extensions are assumed to be of the form [filename.extension]
   If . characters are used in the file name, infers that the last is adjacent
   to the extension.  Thus, names like 'this.is.a.really.bad.name.txt'  will 
   return 'this.is.a.really.bad.name' "
  [fname]
  (let [res 
           (let [pieces (strlib/split fname #"\.")]
             (apply str  (butlast (interleave (butlast pieces) (repeat \.)))))]
    (if (empty? res) fname res)))

(defmulti key->filename 
  "coerces a key in a map into a compatible filesystem key" class)
(defmethod key->filename clojure.lang.Keyword [s]
  (str (get prefixmap :keyword) (subs (str s) 1)))
(defmethod key->filename java.lang.String [s] 
  s)

(defmulti filename->key
  "returns a compatible associative key from a path."
  filename->type)

(defmethod filename->key :keyword [s] 
  (keyword (drop-prefix s :keyword)))

(defmethod filename->key :string [s] 
  (drop-ext s))

(defn drop-keys
  "Return a m with keys removed."
  [m keys] 
  (reduce #(dissoc %1 %2) m keys))

(defn get-mapkeys
  "Returns a map of seqs.
   :mapkeys are all keys whose values are maps.
   :nonmapkeys are all keys with non-map values."
  [m]
    (group-by 
      #(if (map? (get m %)) 
           :mapkeys 
           :nonmapkeys) (keys m)))

(defn map->folders!
  "Unwraps a hashmap to a directory.  If rootpath does not exist, creates the 
   structure.  Keys in the map correspond to file/foldernames.  Where values are 
   hashmaps, map->folders! is called recursively with a relative path.  Values 
   that are not associatve are serialized based on filetype (normally .clj files
   with clojure strings)."
  [m rootpath & {:keys [filetype pretty? logged? clear? condensed? literal?
                        printed?]   
                 :or   {filetype :clj pretty? false logged? false
                        clear? true condensed? true literal? false 
                        printed? false}}]
  
  (let [ext (if literal? "" (str \. (subs (str filetype) 1)))
        _   (if clear? (if (.exists (io/file rootpath))
                             (delete-file-recursively rootpath)))
        printf (cond printed? print
                     pretty? pp/pprint
                     :else prn)  
        fatwriter (fn [root m] 
                    (doseq [k (keys m)] ;every key gets a file.
                        (hock (relative-path root [(str (key->filename k) ext)])
                              (with-out-str (printf (get m k))))))
        thinwriter (fn [root m] ;writes map as a single file
                     (let [target (relative-path root [(str "mapentries" ".clj")])
                           data (with-out-str (printf m))                                                
                           _    (if logged? (println ["hocking " target data]))]
                       (hock target data)))
        writef     (if condensed? thinwriter fatwriter)]
    (loop [mapq {rootpath m}]
      (when (seq mapq)
	      (let [currentpath (first (keys mapq))
	            currentmap  (get mapq currentpath)
	            {:keys [mapkeys nonmapkeys]} (get-mapkeys currentmap)]                  
         (do  ;(println ["Files :" (drop-keys currentmap mapkeys)
              ;         "Folders: " (drop-keys currentmap nonmapkeys)])
              (writef currentpath (drop-keys currentmap mapkeys))
	            (recur (reduce (fn [acc fldrkey] 
	                            (assoc acc (relative-path currentpath 
	                              [(key->filename fldrkey)]) 
	                                   (get currentmap fldrkey)))
	                        (dissoc mapq currentpath) mapkeys))))))))


;define a file-backed-map....
;each file in the directory structure is the name of a key in the map...
;directories are maps.
(defn folders->map
  "Reads a folder located at path, and derives a hashmap from the folder 
   structure.  Can be used with or without map->folders! , as the derivation
   is general enough.  Deriving a hashmap follows 2 simple rules: 
   all files in the current directory are read.  Files are assumed to be 
   string-serialized clojure expressions [for now], which are read and 
   consumed into a dynamically-built hashmap.  Each directory forms a level of 
   the hashmap, with the files serving as containers for map entries, either 
   [k v] pairs, or {} maps.  {} maps are decompsed into a sequence of [k v] 
   pairs.  All [k v] pairs are reduced using assoc-in, where the key path 
   for assoc-in is relative to the directory structure and the key of the [k v]
   pair.  Directories are seen as special [k v] pairs, where the directory name
   is itself a key.  Thus, directories correspond to nested maps. "
  [path & {:keys [loadf extensions-regex logged?] 
                           :or {loadf (fn [p] 
                                        (let [s (slurp p)]
                                          (if (= s "")
                                            nil 
                                            (read-string s))))
                                extensions-regex 
                                      #"\.clj|\.CLJ|\.json|\.JSON"
                                logged? false}}]                                      
  (let [drop-path #(subs % (inc (count path)))
        get-ext #(re-find extensions-regex %)
        drop-ext #(subs % 0 (- (count %) (count (get-ext %)) ))
        fls (filter #(.isFile %) (file-seq (io/file path)))]
    (reduce (fn [accmap fl]
              (let [fpath     (.getPath fl)
                    fname     (.getName fl)
                    rawpath   (list-path (drop-path fpath))
                    cleanpath (vector-map filename->key (butlast-vec rawpath))]
                (do (if logged? (println (str "processing " fpath)))
	                  (let [data (loadf fpath)]
	                    (if (map? data)
	                      (reduce (fn [acc [k v]]
                                     (assoc-in acc (conj cleanpath k) v)) 
	                              accmap data)
                      (assoc-in  accmap (conj cleanpath (first data)) 
                                 (rest data))))))) {} fls)))

(defn build-folders!
  "Builds the structure for a set of folders defined by folderspec, in root 
   directory defined by the path rootdir.  A folderspec is simply a map where 
   nested maps represent subdirectories ala 
   {:output {} :input {}}, which expands to rootdir/output, rootdir/input"
  [rootdir folderspec]
  (map->folders! folderspec (as-directory rootdir) :condensed? false))  

;an empty readme-file, to be used with folder spec in map->folders! 
(defn readme [& [txt]]
  "Creates a readme file, with optional text included."
  {"readme.txt" (or txt "Insert comments here.")})

;an empty, to be used with folder spec in map->folders! 
(def blankfile {"blank.txt" "blank!"})

;From ripper.io 
;explicit helpers to wrap java.io.file class.
(defn fexists?
  "Checks for the existence of file.  File can be a path or a File object."
  [file] (.exists (io/file file)))
(defn fname
  "Returns the name of file.  File can be a path or a File object."
  [file] (.getName (io/file file)))
(defn fdir
  "Returns the parent or containing directory of file.  File can be a path or 
   a File object."
  [file] (.getPath (.getParentFile (io/file file))))
(defn fpath
  "Returns the path for file.  Filre can be a path or a File object."
  [file] (.getPath (io/file file)))
(defn fext
  "Returns the logical extension for file, if it has one.  Extensions are 
   assumed to be found after the rightmost . character in a filename."
  [file]
  (apply str 
     (reverse 
       (take-while #(not= \. %)
           (reverse (fname file))))))

(defn folder?
  "Predicate to test the referenced path as a folder.  If the file object 
   defined by path does not exist, it will also return false."
  [file] (.isDirectory (io/as-file file)))

(defn file?
  "Predicate to test the referenced path as a file.  If the file object 
   defined by path does not exist, it will also return false."
  [file] (.isFile (io/as-file file)))

(defn list-files
  "Return a sequence of all files and folders that are children of path."
  [path] (seq (.listFiles (io/file path))))

(defn find-files
  "Return a lazy seq of all files under root where filterf returns true.
   filterf :: File -> Boolean"
  [root filterf]
  (let [fs (file-seq (clojure.java.io/file root))]
    (filter filterf fs)))

(defn make-file!
  "Creates a new file at filepath.  New file is empty."
  [filepath]
  (let [f (io/file filepath)]
    (do (make-folders! (.getParent f))
        (.createNewFile (io/file filepath))
        f))) 

(def fcopy clojure.java.io/copy) 

;Since URIs are really useful for cross-platform stuff, and for dealing with 
;ZIP files, here are some helper functions...
(defn file->uri
  "Maps a file to a uniform resource indicator."
  [f] (.toURI (io/file f)))
(defn uri-path
  "Returns a string path from a uniform resource indicator."
  [^java.net.URI u] (.getPath u))
(defn uri->file
  "Parses the resource at u into a file."
  [^java.net.URI u] (io/file (uri-path u)))
(defn path->uripath
  "Parses the path into a uniform resource indicator path."
  [p] (str "file:/" (strlib/replace p "\\" "/")))

(defn path->uri
  "Returns a uniform resource indicator relative to path p."
  [p] (java.net.URI. (path->uripath p)))

(comment 
;If we weren't passed a path or paths at the command line,
;we'll initiate a file dialog.  This uses some java interop...
;My skill with Swing is limited but growing. 
(defn select-file
  ([initpath]
  (let [fc (JFileChooser. initpath)
        res (. fc showOpenDialog nil)]
    (if (= res (. JFileChooser APPROVE_OPTION))
      (str (.getSelectedFile fc))
      nil)))
  ([] (select-file @*path*)))

(defn- folder-chooser [initpath]
  (let [f (JFileChooser. initpath)]
    (do 
      (.setFileSelectionMode f JFileChooser/DIRECTORIES_ONLY)
      f)))

(defn select-folder
  ([initpath]
  (let [f (folder-chooser initpath)
        res (. f showOpenDialog nil)]
    (if (= res (. JFileChooser APPROVE_OPTION))
      (str (.getSelectedFile f))
      nil)))
  ([] (select-folder @*path*)))
)
