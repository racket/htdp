#lang racket 

(provide
 ;; LTracks is one of:
 ;; -- '()
 ;; -- (cons Track LTracks)

 ;; Track is (make-track String String String N N Date N Date)
 ;; INTERPRETATION
 ;;  (make-track title artist album time track-number added-when played-n-times played-last)

 ;; Date is (make-date N N N N N N)
 ;; INTERPRETATION
 ;;  (make-date year month hour minute second)
 
 ;; LLists is one of:
 ;; -- '()
 ;; -- (cons Association LLists)
 ;; Association is (cons String (cons BSL-Value '()))

 ;; BSL-Value is one of: String or Integer or Date or Boolean

 ;; String -> LLists
 ;; create list of lists representation for all tracks in file, an XML export from an iTunes library
 ;; EFFECT read XML document from file 
 read-itunes-as-lists
 
 ;; String -> LTracks
 ;; create list of tracks representation for all tracks in file, an XML export from an iTunes library
 ;; EFFECT read XML document from file 
 read-itunes-as-tracks
 
 ;; Any Any Any Any Any Any Any Any -> Track or #false
 create-track 
 track?
 track-name
 track-artist
 track-album
 track-time
 track-track#
 track-added
 track-play#
 track-played
 
 ;; Any Any Any Any Any Any -> Date or #false
 create-date
 date?
 date-year
 date-month
 date-day
 date-hour
 date-minute
 date-second)

;; ---------------------------------------------------------------------------------------------------
(require xml/xml htdp/error)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; exports 
(define-struct track (name artist album time track# added play# played) #:transparent)

(define (create-track name artist album time track# added play# played)
  (if (and (string? name)
           (string? artist)
           (string? album)
           (natural? time)
           (natural? track#)
           (date? added)
           (natural? play#)
           (date? played))
      (make-track name artist album time track# added play# played)
      #false))

(define-struct date (year month day hour minute second) #:transparent)

(define (create-date year month day hour minute second)
  (if (and (natural? year)
           (natural? month)
           (natural? day)
           (natural? hour)
           (natural? minute)
           (natural? second))
      (make-date year month day hour minute second)
      #false))

(define (read-itunes-as-lists file)
  (check-arg 'read-itunes-as-list (file-exists? file) "name of existing file (string)" "first" file)
  (define extract (read-itunes 'read-itunes-as-list file dict->list))
  (with-input-from-file file extract))

(define (read-itunes-as-tracks file)
  (check-arg 'read-itunes-as-list (file-exists? file) "name of existing file (string)" "first" file)
  (define extract (read-itunes 'read-itunes-as-list file dict->track))
  (with-input-from-file file extract))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (define sample-itunes:xml-string
    #<<eos
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Major Version</key>
  <integer>1</integer>
  <key>Minor Version</key>
  <integer>1</integer>
  <key>Date</key>
  <date>2015-08-06T19:52:58Z</date>
  <key>Application Version</key>
  <string>12.2.1.16</string>
  <key>Features</key>
  <integer>5</integer>
  <key>Show Content Ratings</key>
  <true/>
  <key>Music Folder</key>
  <string>file</string>
  <key>Library Persistent ID</key>
  <string>9F4A0C652637256D</string>
  <key>Tracks</key>
  <dict>
  <key>442</key>
  <dict>
      <key>Track ID</key>
      <integer>442</integer>
      <key>Name</key>
      <string>Wild Child</string>
      <key>Artist</key>
      <string>Enya</string>
      <key>Album</key>
      <string>A Day Without Rain</string>
      <key>Genre</key>
      <string>New Age</string>
      <key>Kind</key>
      <string>MPEG audio file</string>
      <key>Size</key>
      <integer>4562044</integer>
      <key>Total Time</key>
      <integer>227996</integer>
      <key>Track Number</key>
      <integer>2</integer>
      <key>Track Count</key>
      <integer>11</integer>
      <key>Year</key>
      <integer>2000</integer>
      <key>Date Modified</key>
      <date>2002-07-17T00:00:11Z</date>
      <key>Date Added</key>
      <date>2002-07-17T03:55:14Z</date>
      <key>Bit Rate</key>
      <integer>160</integer>
      <key>Sample Rate</key>
      <integer>44100</integer>
      <key>Play Count</key>
      <integer>20</integer>
      <key>Play Date</key>
      <integer>3388484113</integer>
      <key>Play Date UTC</key>
      <date>2011-05-17T17:35:13Z</date>
      <key>Sort Album</key>
      <string>Day Without Rain</string>
      <key>Persistent ID</key>
      <string>EBBE9171392FA348</string>
      <key>Track Type</key>
      <string>File</string>
      <key>Location</key>
      <string>file</string>
      <key>File Folder Count</key>
      <integer>4</integer>
      <key>Library Folder Count</key>
      <integer>1</integer>
  </dict>
  <key>444</key>
  <dict>
      <key>Track ID</key>
      <integer>444</integer>
      <key>Name</key>
      <string>Only Time</string>
      <key>Artist</key>
      <string>Enya</string>
      <key>Album</key>
      <string>A Day Without Rain</string>
      <key>Genre</key>
      <string>New Age</string>
      <key>Kind</key>
      <string>MPEG audio file</string>
      <key>Size</key>
      <integer>4364035</integer>
      <key>Total Time</key>
      <integer>218096</integer>
      <key>Track Number</key>
      <integer>3</integer>
      <key>Track Count</key>
      <integer>11</integer>
      <key>Year</key>
      <integer>2000</integer>
      <key>Date Modified</key>
      <date>2002-07-17T00:00:21Z</date>
      <key>Date Added</key>
      <date>2002-07-17T03:55:42Z</date>
      <key>Bit Rate</key>
      <integer>160</integer>
      <key>Sample Rate</key>
      <integer>44100</integer>
      <key>Play Count</key>
      <integer>18</integer>
      <key>Play Date</key>
      <integer>3388484327</integer>
      <key>Play Date UTC</key>
      <date>2011-05-17T17:38:47Z</date>
      <key>Sort Album</key>
      <string>Day Without Rain</string>
      <key>Persistent ID</key>
      <string>EBBE9171392FA34A</string>
      <key>Track Type</key>
      <string>File</string>
      <key>Location</key>
      <string>file</string>
      <key>File Folder Count</key>
      <integer>4</integer>
      <key>Library Folder Count</key>
      <integer>1</integer>
  </dict>
  <key>446</key>
  </dict>
</dict>
</plist>
eos
    )
  (define sample-itunes:xexpr
    '((dict
       ()
       (key () "Track ID")
       (integer () "442")
       (key () "Name")
       (string () "Wild Child")
       (key () "Artist")
       (string () "Enya")
       (key () "Album")
       (string () "A Day Without Rain")
       (key () "Genre")
       (string () "New Age")
       (key () "Kind")
       (string () "MPEG audio file")
       (key () "Size")
       (integer () "4562044")
       (key () "Total Time")
       (integer () "227996")
       (key () "Track Number")
       (integer () "2")
       (key () "Track Count")
       (integer () "11")
       (key () "Year")
       (integer () "2000")
       (key () "Date Modified")
       (date () "2002-07-17T00:00:11Z")
       (key () "Date Added")
       (date () "2002-07-17T03:55:14Z")
       (key () "Bit Rate")
       (integer () "160")
       (key () "Sample Rate")
       (integer () "44100")
       (key () "Play Count")
       (integer () "20")
       (key () "Play Date")
       (integer () "3388484113")
       (key () "Play Date UTC")
       (date () "2011-05-17T17:35:13Z")
       (key () "Sort Album")
       (string () "Day Without Rain")
       (key () "Persistent ID")
       (string () "EBBE9171392FA348")
       (key () "Track Type")
       (string () "File")
       (key () "Location")
       (string () "file")
       (key () "File Folder Count")
       (integer () "4")
       (key () "Library Folder Count")
       (integer () "1"))
      (dict
       ()
       (key () "Track ID")
       (integer () "444")
       (key () "Name")
       (string () "Only Time")
       (key () "Artist")
       (string () "Enya")
       (key () "Album")
       (string () "A Day Without Rain")
       (key () "Genre")
       (string () "New Age")
       (key () "Kind")
       (string () "MPEG audio file")
       (key () "Size")
       (integer () "4364035")
       (key () "Total Time")
       (integer () "218096")
       (key () "Track Number")
       (integer () "3")
       (key () "Track Count")
       (integer () "11")
       (key () "Year")
       (integer () "2000")
       (key () "Date Modified")
       (date () "2002-07-17T00:00:21Z")
       (key () "Date Added")
       (date () "2002-07-17T03:55:42Z")
       (key () "Bit Rate")
       (integer () "160")
       (key () "Sample Rate")
       (integer () "44100")
       (key () "Play Count")
       (integer () "18")
       (key () "Play Date")
       (integer () "3388484327")
       (key () "Play Date UTC")
       (date () "2011-05-17T17:38:47Z")
       (key () "Sort Album")
       (string () "Day Without Rain")
       (key () "Persistent ID")
       (string () "EBBE9171392FA34A")
       (key () "Track Type")
       (string () "File")
       (key () "Location")
       (string () "file")
       (key () "File Folder Count")
       (integer () "4")
       (key () "Library Folder Count")
       (integer () "1"))))

  (check-equal?
   (with-input-from-string sample-itunes:xml-string (read-itunes 'x "x" values))
   sample-itunes:xexpr)

  (check-equal? 
   (with-input-from-string sample-itunes:xml-string (read-itunes 'x "x" dict->list))
   (map dict->list sample-itunes:xexpr))

  (check-equal? 
   (with-input-from-string sample-itunes:xml-string (read-itunes 'x "x" dict->track))
   (map dict->track sample-itunes:xexpr)))
  

;; Symbol String [Xexpr -> [Maybe X]] -> [-> [Listof X]]
;; create list of tracks
;; effetc: read XML document exported frim iTunes from stdin port
(define (read-itunes tag file process)
  (lambda ()
    (define itunes:xml
      (with-handlers ([exn:fail?
                       (lambda (x)
                         (check-arg tag #false "file with XML document" "first" file))])
        (read-xml/document)))
    (define plist:xexpr
      (match (xml->xexpr (cleanup (document-element itunes:xml)))
        [`(plist ((version ,n)) (dict ,_attributes ,body ...)) body]
        [else (check-arg tag #false "XML file exported from iTunes (1)" "first" file)]))
    (define tracks:xexpr-dict
      (match (dict-attribute plist:xexpr "Tracks")
        [`(dict ,_attributes ,d ...) d]
        [else (check-arg tag #false "XML file exported from iTunes (2)" "first" file)]))
    (define tracks:dict
      (filter (lambda (x) (and (cons? x) (eq? (first x) 'dict))) tracks:xexpr-dict))
    (define tracks:processed (map process tracks:dict))
    (filter values tracks:processed)))

(define cleanup (eliminate-whitespace '(plist dict) values))

;; ---------------------------------------------------------------------------------------------------
;; Xexpr [dict] -> Track
;; create a track from a dict
(define (dict->track d)
  (create-track
   (dict-attribute d "Name")
   (dict-attribute d "Artist")
   (dict-attribute d "Album")
   (dict-attribute d "Total Time")
   (dict-attribute d "Track Number")
   (dict-attribute d "Date Added")
   (dict-attribute d "Play Count")
   (dict-attribute d "Play Date UTC")))

;; ---------------------------------------------------------------------------------------------------
;; Xexpr [dict] String -> Xexpr or #false
;; find the first attribute that goes with key k
(define (dict-attribute d k)
  (let loop ([d (xexpr-body d)])
    (cond
      [(empty? d) #false]
      [else (define key (first d))
            (and (cons? (rest d))
                 (let ([attribute (second d)])
                   (match key
                     [`(key ,attributes ,key-string)
                      (if (string=? key-string k)
                          (attribute->value attribute)
                          (loop (rest (rest d))))]
                     [_ (error 'dict-attribute "wrong key format: ~e" key)])))])))

;; ---------------------------------------------------------------------------------------------------
;; Xexpr [dict] -> [Listof String (U String Integer Date)]
;; map a dict to a list of pairs, the name of the attribute and its value 
(define (dict->list d)
  (let loop ([d (xexpr-body d)])
    (cond
      [(empty? d) '()]
      [else (define key (first d))
            (if (empty? (rest d))
                (error 'dict->list "wrong format: ~e" d)
                (cons `(,(third key) ,(attribute->value (second d)))
                      (loop (rest (rest d)))))])))

;; ---------------------------------------------------------------------------------------------------
;; Xexpr -> SL-Value
;; turn an Xexpr representation of an attribute into a *SL value 
(define (attribute->value a)
  (match a
    [`(string ,_attributes ,s ...) (foldr ~a "" s)]
    [`(integer ,_attributes ,i) (string->number i)]
    [`(real ,_attributes ,i) (string->number i)]
    [`(date ,_attributes ,d)
     ;; Contents should conform to a subset of ISO 8601 
     ;; (in particular, YYYY '-' MM '-' DD 'T' HH ':' MM ':' SS 'Z'.  
     ;; Smaller units may be omitted with a loss of precision)
     (define d:struct
       (regexp-match #px"(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d).(\\d\\d):(\\d\\d):(\\d\\d)" d))

     (define the-pieces (map string->number (rest d:struct)))

     (if d:struct
         (apply make-date the-pieces)
         (error 'attribute->value "not a proper date: ~e" d))]
    [`(true ,_attributes) #true]
    [`(false ,_attributes) #false]
    [`(dict ,_attributes ,d ...)
     a

     ;; it's worth considering whether dicts should be turned into association lists 
     #;
     (let loop ([d d])
       (cond
         [(empty? d) '()]
         [else (cons (list (key->value (first d)) (attribute->value (second d)))
                     (loop (rest (rest d))))]))]
    [else (error 'attribute->value "unknown kind of value: ~e" a)]))

;; Xexpr -> String 
(define (key->value x)
  (match x
    [`(key ,_attributes ,(? string? s)) s]
    [else (error 'key->value "unknown kind of dict-key: ~e" x)]))

(define xexpr-body cddr)
