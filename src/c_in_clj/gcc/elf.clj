(ns c-in-clj.gcc.elf
  (:import
   [System.IO Stream SeekOrigin
    EndOfStreamException FileStream
    FileMode FileAccess FileShare
    MemoryStream]))

;; Byte Buffer Functions

(def ^:dynamic ^Stream *buf* "The current buffer")

(defn open-buffer [filename]
  "Opens a buffer for the specified filename"
  (cond
   (instance? Stream filename filename) filename
   (instance? |System.Byte[]| filename) (MemoryStream. filename)
   (instance? String filename) (FileStream. filename FileMode/OpenOrCreate)))

(defmacro with-buffer
  "Opens a buffer on the specified filename and sets the *buf* var to that buffer within the
specified body."
  [filename & body]
  `(with-open [buf# (open-buffer ~filename)]
     (binding [*buf* buf#]
       ~@body)))

(defn bpos
  "Returns the position in the current buffer"
  [] (.Position *buf*))

(defn blen
  "Returns the length of the current buffer"
  [] (.Length *buf*))

(defn bleft
  "Returns the number of bytes left in the current buffer
starting from the current position"
  [] (- (blen) (bpos)))

(defn bgoto
  "Moves the position of the current buffer to the specified position."
  [position]
  (.Seek *buf* position SeekOrigin/Begin))

(defmacro ^:private save-position [& body]
  `(let [cur-pos# (bpos)
         res# (do ~@body)]
     (bgoto cur-pos#)
     res#))

(defmacro ^:private jump-position [position & body]
  `(save-position
    (bgoto ~position)
    ~@body))

(defn brbyte
  "Reads a byte from the current buffer at the current position or at the specified
position if one is provided."
  ([] (.ReadByte *buf*))
  ([position]
     (jump-position
      position
      (brbyte))))

(defn brbytes
  "Reads the specified number of bytes from the buffer
at the current position or at the specified
position if one is provided. Throws a System.IO.EndOfStreamException
if the specified number of bytes cannot be read from the buffer."
  ([count]
     (let [bytes (make-array Byte count)
           nread (.Read *buf* bytes 0 count)]
       (if (not= nread count)
         (throw (EndOfStreamException.))
         bytes)))
  ([position count]
     (jump-position
      position
      (brbytes count))))

(defn brntstr
  "Reads a null terminated string from the buffer at the current or
specified position."
  ([]
     (loop [cur (brbyte)
            chars []]
       (if (= 0 cur)
         (apply str chars)
         (recur (brbyte) (conj chars (char cur))))))
  ([position]
     (jump-position
      position
      (brntstr))))

(defmacro defreader [name nbytes args & body]
  `(defn ~name []
     (let [~args [( brbytes ~nbytes) 0]]
       ~@body)))

(defreader read-u16 2 [bytes index] (BitConverter/ToUInt16 bytes index))
(defreader read-i16 2 [bytes index] (BitConverter/ToInt16 bytes index))
(defreader read-u32 4 [bytes index] (BitConverter/ToUInt32 bytes index))
(defreader read-i32 4 [bytes index] (BitConverter/ToInt32 bytes index))
(defreader read-u64 8 [bytes index] (BitConverter/ToUInt64 bytes index))
(defreader read-i64 8 [bytes index] (BitConverter/ToInt64 bytes index))

(defn- elf-type->reader32 [type & {:keys [len]}]
  (case type
    :uchar (fn [] (brbytes len))
    :uint16_t read-u16
    :uint32_t read-u32
    :ElfN_Addr read-u32
    :ElfN_Off read-u32))

(def ^:private elf32-structs (atom {}))
(def ^:private elf64-structs (atom {}))

(defmacro elf-enum [name val-map]
  `(def ~name ~val-map))

(defn- compile-elf-struct [name forms arch]
  (let [fields
        (doall
         (for [[type name len] forms]
           (let [name (keyword name)
                 type (keyword type)
                 reader (case arch
                          32 (elf-type->reader32 type :len len))]
             {:type type :name name :len len
              :reader reader})))]
    (case arch
      32 (swap! elf32-structs assoc (keyword name) {:fields fields}))))

(def ^:private ^:dynamic *elf-arch* 32)

(defn read-elf-struct [type]
  (let [{:keys [fields record]}
        (case *elf-arch*
                 32 (get @elf32-structs type))
        data
        (for [{:keys [reader name]} fields]
          [name (reader)])
        data (into {} (doall data))]
    data))

(defmacro elf-struct32
  [name & forms]
  (compile-elf-struct name forms 32)
  (let [kname (keyword name)
        name32 (symbol (str "Elf32_" name))]
    `(do
      (defrecord ~name32 [])
      (swap! elf32-structs assoc-in [~kname :record] ~name32))))

(defmacro elf-struct64
  [name & forms])

(defmacro elf-structN
  [name & forms]
  `(do
     (elf-struct32 ~name ~@forms)))

(elf-structN
 Ehdr
 (uchar e_ident 16) ;;  Magic number and other info 
 (uint16_t    e_type) ;;  Object file type 
 (uint16_t    e_machine) ;;  Architecture 
 (uint32_t    e_version) ;;  Object file version 
 (ElfN_Addr    e_entry) ;;  Entry point virtual address 
 (ElfN_Off     e_phoff) ;;  Program header table file offset 
 (ElfN_Off     e_shoff) ;;  Section header table file offset 
 (uint32_t    e_flags) ;;  Processor-specific flags 
 (uint16_t    e_ehsize) ;;  ELF header size in bytes 
 (uint16_t    e_phentsize) ;;  Program header table entry size 
 (uint16_t    e_phnum) ;;  Program header table entry count 
 (uint16_t    e_shentsize) ;;  Section header table entry size 
 (uint16_t    e_shnum) ;;  Section header table entry count 
 (uint16_t    e_shstrndx)) ;;  Section header string table index 

(elf-struct32
 Phdr
 (uint32_t    p_type) ;;  Segment type 
 (ElfN_Off     p_offset) ;;  Segment file offset 
 (ElfN_Addr    p_vaddr) ;;  Segment virtual address 
 (ElfN_Addr    p_paddr) ;;  Segment physical address 
 (uint32_t    p_filesz) ;;  Segment size in file 
 (uint32_t    p_memsz) ;;  Segment size in memory 
 (uint32_t    p_flags) ;;  Segment flags 
 (uint32_t    p_align)) ;;  Segment alignment 

(elf-struct32
 Shdr
 (uint32_t    sh_name) ;;  Section name (string tbl index) 
 (uint32_t    sh_type) ;;  Section type 
 (uint32_t    sh_flags) ;;  Section flags 
 (ElfN_Addr    sh_addr) ;;  Section virtual addr at execution 
 (ElfN_Off     sh_offset) ;;  Section file offset 
 (uint32_t    sh_size) ;;  Section size in bytes 
 (uint32_t    sh_link) ;;  Link to another section 
 (uint32_t    sh_info) ;;  Additional section information 
 (uint32_t    sh_addralign) ;;  Section alignment 
 (uint32_t    sh_entsize)) ;;  Entry size if section holds table 

(defn- read-elf-Shdr [ehdr]
  (let [{:keys [e_shentsize e_shnum e_shoff e_shstrndx]} ehdr]
    (when (> e_shnum 0)
      (bgoto e_shoff)
      (let [shdrs
            (doall
             (for [i (range e_shnum)]
               (let [bytes (brbytes e_shentsize)]
                 (with-buffer bytes
                   (read-elf-struct :Shdr)))))
            str-hdr (when (< e_shstrndx e_shnum)
                      (nth shdrs e_shstrndx))
            str-offset (:sh_offset str-hdr)]
        (doall
         (for [{:keys [sh_name] :as shdr} shdrs]
           (let [str-pos (+ str-offset sh_name)]
             (assoc shdr :name (brntstr str-pos)))))))))
 
(defn read-elf32 [filename]
  (with-buffer filename
    (binding [*elf-arch* 32]
      (let [ehdr (read-elf-struct :Ehdr)
            shdrs (read-elf-Shdr ehdr)]
        {:elf-header ehdr
         :section-headers shdrs
         :program-headers nil}))))

(read-elf32 "c:/tmp/test1.o")
