(ns c-in-clj.gcc.elf
  (:import
   [System.IO Stream SeekOrigin
    EndOfStreamException FileStream
    FileMode FileAccess FileShare
    MemoryStream Path]))

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

(defn- make-enum [& kvps]
  (let [res (apply hash-map kvps)
        val-map (zipmap (vals res) (keys res))]
    (with-meta res {:val-map val-map})))

(defmacro elfenum [name & kvps]
  `(def ~name (make-enum ~@kvps)))

(defn- to-enum-name [enum value]
  (let [{:keys [val-map]} (meta enum)]
    (or (get val-map value) value)))

(defn- elf-type->reader32 [type {:keys [len enum convert]}]
  (let [enum (eval enum)
        convert (eval convert)
        reader
        (case type
          :uchar (if len (fn [] (brbytes len)) brbyte)
          :uint16_t read-u16
          :uint32_t read-u32
          :int32_t read-i32
          :ElfN_Addr read-u32
          :ElfN_Off read-u32)
        reader (if enum
                 (fn read-enum [] (to-enum-name enum (reader)))
                 reader)
        reader (if convert
                 (fn read-convert [] (convert (reader)))
                 reader)]
    reader))

(def ^:private elf32-structs (atom {}))
(def ^:private elf64-structs (atom {}))

(defn- compile-elf-struct [name forms arch]
  (let [fields
        (doall
         (for [[type name & {:keys [len] :as opts}] forms]
           (let [name (keyword name)
                 type (keyword type)
                 reader (case arch
                          32 (elf-type->reader32 type opts))]
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
 (uchar e_ident :len 16) ;;  Magic number and other info 
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

(elfenum
 ELF32_P_TYPE
 :PT_NULL 0
 :PT_LOAD 1
 :PT_DYNAMIC 2
 :PT_INTERP 3
 :PT_NOTE 4
 :PT_SHLIB 5
 :PT_PHDR 6)

(elf-struct32
 Phdr
 (uint32_t    p_type :enum ELF32_P_TYPE) ;;  Segment type 
 (ElfN_Off     p_offset) ;;  Segment file offset 
 (ElfN_Addr    p_vaddr) ;;  Segment virtual address 
 (ElfN_Addr    p_paddr) ;;  Segment physical address 
 (uint32_t    p_filesz) ;;  Segment size in file 
 (uint32_t    p_memsz) ;;  Segment size in memory 
 (uint32_t    p_flags) ;;  Segment flags 
 (uint32_t    p_align)) ;;  Segment alignment 

(elfenum sh_type_enum
         :SHT_NULL 0
         :SHT_PROGBITS 1
         :SHT_SYMTAB 2
         :SHT_STRTAB 3
         :SHT_RELA 4
         :SHT_HASH 5
         :SHT_DYNAMIC 6
         :SHT_NOTE 7
         :SHT_NOBITS 8
         :SHT_REL 9
         :SHT_SHLIB 10
         :SHT_DYNSYM 11
         ;; :SHT_LOPROC 0x70000000
         ;; :SHT_HIPROC 0x7fffffff
         ;; :SHT_LOUSER 0x80000000
         ;; :SHT_HIUSER 0xffffffff
         )

(def SHN_LORESERVE 0xff00)

(elfenum shnum_special_enum
         :SHN_UNDEF 0
         ;; :SHN_LORESERVE 0xff00
         ;; :SHN_LOPROC 0xff00
         ;; :SHN_HIPROC 0xff1f
         :SHN_ABS 0xfff1
         :SHN_COMMON 0xfff2
         ;;:SHN_HIRESERVE 0xffff
         )

(elf-struct32
 Shdr
 (uint32_t    sh_name) ;;  Section name (string tbl index) 
 (uint32_t    sh_type :enum sh_type_enum) ;;  Section type 
 (uint32_t    sh_flags) ;;  Section flags 
 (ElfN_Addr    sh_addr) ;;  Section virtual addr at execution 
 (ElfN_Off     sh_offset) ;;  Section file offset 
 (uint32_t    sh_size) ;;  Section size in bytes 
 (uint32_t    sh_link) ;;  Link to another section 
 (uint32_t    sh_info) ;;  Additional section information 
 (uint32_t    sh_addralign) ;;  Section alignment 
 (uint32_t    sh_entsize)) ;;  Entry size if section holds table 

(elfenum
 ELF32_ST_BIND
 :STB_LOCAL 0
 :STB_GLOBAL 1
 :STB_WEAK 2
 ;;:STB_LOPROC 13
 ;;:STB_HIPROC 15
 )

(elfenum
 ELF32_ST_TYPE
:STT_NOTYPE 0
:STT_OBJECT 1
:STT_FUNC 2
:STT_SECTION 3
:STT_FILE 4
;;:STT_LOPROC 13
;;:STT_HIPROC 15
)

(defn parse-st_info [st_info]
  (let [st_bind (bit-shift-right st_info 4)
        st_type (bit-and st_info 0xf)
        st_bind (to-enum-name ELF32_ST_BIND st_bind)
        st_type (to-enum-name ELF32_ST_TYPE st_type)]
    {:st_bind st_bind :st_type st_type}))

(elf-struct32
 Sym
 (uint32_t st_name)
 (ElfN_Addr st_value)
 (uint32_t st_size)
 (uchar st_info :convert parse-st_info)
 (uchar st_other)
 (uint16_t st_shndx :enum shnum_special_enum))

(defn parse-r_info [r_info]
  {:r_sym (bit-shift-right r_info 8)
   :r_type (byte (bit-and r_info 0xff))})

(elf-struct32
 Rel
 (ElfN_Addr r_offset)
 (uint32_t r_info :convert parse-r_info))

(elf-struct32
 Rela
 (ElfN_Addr r_offset)
 (uint32_t r_info :convert parse-r_info)
 (int32_t r_addend))

(defn- load-elf-table*
  ([type shdr] (load-elf-table* type shdr nil))
  ([type {:keys [sh_entsize sh_offset sh_size] :as shdr} num-entries]
     (let [num-entries
           (or num-entries
               (int (/ sh_size sh_entsize)))]
       (jump-position
        sh_offset
        (doall
         (for [i (range num-entries)]
           (with-buffer (brbytes sh_entsize) (read-elf-struct type))))))))

(defn- find-shdr [shdrs name]
  (first (filter #(= (:name %) name) shdrs)))

(defn- load-elf-sym-table [all-shdrs {:keys [sh_entsize sh_offset sh_size sh_link] :as shdr}]
  (let [syms (load-elf-table* :Sym shdr)
        strtab (nth all-shdrs sh_link)
        str-offset (:sh_offset strtab)]
    (doall
     (for [sym syms] 
       (let [st_name (:st_name sym)
             sym-name
             (when (and str-offset (not= st_name 0))
               (brntstr (+ str-offset st_name)))]
         (assoc sym :name sym-name))))))

(defn- load-elf-rel-table [{:keys [] :as shdr}]
  (load-elf-table* :Rel shdr))

(defn- load-elf-rela-table [{:keys [] :as shdr}]
  (load-elf-table* :Rela shdr))

(defn- load-prog-bits [{:keys [sh_offset sh_size]}]
  (brbytes sh_offset sh_size))

(defn- load-elf-Shdr [all-shdrs {:keys [sh_type] :as shdr}]
  (let [data
        (case sh_type
          :SHT_SYMTAB (load-elf-sym-table all-shdrs shdr)
          :SHT_DYNSYM (load-elf-sym-table all-shdrs shdr)
          :SHT_RELA (load-elf-rela-table shdr)
          :SHT_REL (load-elf-rel-table shdr)
          :SHT_PROGBITS (load-prog-bits shdr)
          nil)]
   (assoc shdr :data data)))

(defn- read-elf-hdrs* [entsize num off type]
  (when (> num 0)
    (bgoto off)
    (doall
     (for [i (range num)]
       (let [bytes (brbytes entsize)]
         (with-buffer bytes
           (read-elf-struct type)))))))

(defn- read-elf-Shdrs [ehdr]
  (let [{:keys [e_shentsize e_shnum e_shoff e_shstrndx]} ehdr]
    (when-let [shdrs (read-elf-hdrs* e_shentsize e_shnum e_shoff :Shdr)]
      (let [str-hdr (when (< e_shstrndx e_shnum)
                      (nth shdrs e_shstrndx))
            str-offset (:sh_offset str-hdr)
            shdrs
            (doall
             (for [{:keys [sh_name] :as shdr} shdrs]
               (let [str-pos (+ str-offset sh_name)]
                 (assoc shdr :name (brntstr str-pos)))))]
        (doall (map (partial load-elf-Shdr shdrs) shdrs))))))

(defn- load-elf-Phdr [{:keys [p_offset p_filesz] :as phdr}]
  (let [data (brbytes p_offset p_filesz)]
    (assoc phdr :data data)))

(defn- read-elf-Phdrs [ehdr]
  (let [{:keys [e_phentsize e_phnum e_phoff]} ehdr]
    (when-let [phdrs (read-elf-hdrs* e_phentsize e_phnum e_phoff :Phdr)]
      (doall (map load-elf-Phdr phdrs)))))
 
(defn read-elf32 [filename]
  (with-buffer filename
    (binding [*elf-arch* 32]
      (let [ehdr (read-elf-struct :Ehdr)
            shdrs (read-elf-Shdrs ehdr)
            phdrs (read-elf-Phdrs ehdr)]
        {:filename (Path/GetFullPath filename)
         :elf-header ehdr
         :sections shdrs
         :program-headers phdrs}))))

(defn find-elf-section [elf name]
  (find-shdr (:sections elf) name))

(defn find-sym-table [elf]
  (:data (find-elf-section elf ".symtab")))

(defn find-global-symbols [elf]
  (filter
   (fn [sym] (= (get-in sym [:st_info :st_bind]) :STB_GLOBAL))
   (find-sym-table elf)))

(defn filter-data-symbols [global-symbols]
  (filter
   (fn [sym] (< (get-in sym [:st_shndx]) SHN_LORESERVE))
   global-symbols))

(defn filter-abs-symbols [global-symbols]
  (filter
   (fn [sym] (= (get-in sym [:st_shndx]) :SHN_ABS))
   global-symbols))

(defn filter-undefined-symbols [global-symbols]
  (filter
   (fn [sym] (= (get-in sym [:st_shndx]) :SHN_UNDEF))
   global-symbols))
