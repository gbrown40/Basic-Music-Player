;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 16 r|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ASSIGNMENT 16

(require 2htdp/universe)
(require 2htdp/image)
(require neu-fall18)
(require 2htdp/batch-io)

(define S1 (file-as-bytes "/Users/gracebrown/Downloads/Toto - Africa (Video).mp3"))
(define S2 (file-as-bytes
            "/Users/gracebrown/Downloads/Alabama Shakes - Sound and Color (Video).mp3"))

; play-sound : String -> FeedbackString 
; Opens a simple music player and plays the MP3 that is represented by the passed-in String.
; The String should be the raw bytes of the MP3. The player does not return until the
; user clicks the Like button (returning "like"), the user clicks the Dislike button 
; (returning "dislike"), or the song ends with no user clicks (returning "none").

; A Package is a (make-package MusicPlayer ClientMsg)
; - and dictacts the next state of the world as well as
; - the message sent from the client to the server

; A ClientMsg is a Nat
; representing a request for a SongMsg with a specific ID#

; A ServerMsg is one of:
; - ErrorMsg
; - SongMsg
; - MetadataMsg
 
; An ErrorMsg is a (list "ERROR" String)
; where the second string is the message from the server about what went wrong
 
; A SongMsg is a (list "SONG" Nat Metadata String)
; - where the Nat is the song's unique ID#
; - the Metadata is information about the song
; - and the String is the actual byte-string of music
 
; A Metadata is a (list String String Number String)
; - where the first String is the song's title
; - the second String is the song's artist
; - the Number is the length of the song (in seconds)
; - and the third String is the song's album
(define MD1 (list "Africa" "Toto" 60 "Toto IV"))
(define MD2 (list "Sound & Color" "Alabama Shakes" 70 "Sound & Color"))
(define MD3 (list "Happy Birthday" "Cher" 100 "Happy Bday"))
#;(define (md-temp md)
    (cond
      [(empty? md) ...]
      [(cons? md) ... (first md)
                  ... (second md)
                  ... (third md)
                  ... (fourth md)]))

; A IDMetaPair is a (list Nat Metadata)
; - where the nat is the id of the song
; - and the metadata contains all of the metadata of the song
(define IP1 (list 1 MD1))
(define IP2 (list 2 MD2))
(define IP3 (list 3 MD3))
#;(define (mp-temp mp)
    (cond
      [(empty? mp) ...]
      [(cons? mp) ... (first mp)
                  ... (md-temp (second mp))]))

; A MetadataMsg is a (list "METADATA" [List-of IDMetaPair])
; where the list contains all of the metadata and ids of the songs available on the server
(define MM (list "METADATA" (list IP1 IP2)))
#;(define (mm-temp mm)
    (cond
      [(empty? mm) ...]
      [(cons? mm) ... (first mm)
                  ... (mp-temp (second mm))
                  ... (mm-temp (rest mm))]))

; A SongOption is a (make-songoption [List-of IDMetaPair] Nat)
(define-struct songoption [metadatalist index])
; - where metadatalist is the list of IDMetaPairs
; - where index is the current song selected (index starting at 0)
(define SO0 (make-songoption '() 0))
(define SO1 (make-songoption (list IP1 IP2) 1))
(define SO2 (make-songoption (list IP1) 0))
(define SO3 (make-songoption (list IP1 IP2 IP3) 1))
#;(define (so-temp so)
    (songoption-metadalist so) ...
    (songoption-nat so))

; A PlayerResult is one of:
; - MusicPlayer
; - Package

(define-struct songhistory [name artist album id count])
; A SongHistory is a (make-songhistory String String String Nat Nat)
; - where the first String is the name of the song
; - where the second String is the artist of the song
; - where the third String is the album of the song
; - where the first Nat is the song ID#
; - where the second Nat is the number of times the song has been played
(define SH1 (make-songhistory "Africa" "Toto" "Toto IV" 1 3))
(define SH2 (make-songhistory "Sound & Color" "Alabama Shakes" "Sound & Color" 2 8))
; songhistory-temp : SongHistory -> ??
#;
(define (songhistory-temp sh)
  (songhistory-name sh)
  (songhistory-artist sh)
  (songhistory-album sh)
  (songhistory-id sh)
  (songhistory-count sh))

; A History is a [List-of SongHistory]
; and represents the history of which songs have been played how many times
(define H0 '())
(define H1 (list (make-songhistory "Africa" "Toto" "Toto IV" 1 3)))
(define H2 (list (make-songhistory "Africa" "Toto" "Toto IV" 1 3)
                 (make-songhistory "Sound & Color" "Alabama Shakes" "Sound & Color" 2 8)))

; history-temp : History -> ???
#;
(define (history-temp h)
  (cond
    [(empty? h) ...]
    [(cons? h) ... (songhistory-temp (first h)) ...
               (history-temp (rest h))]))

; A FeedbackString is one of:
; - ""
; - "dislike"
; - "like"
; - "none"
; Interpretation: The feedback that the user gave to the last song played.  The string
; "none" represents that the user gave no feedback, and the string "" represents
; that no feedback has been received yet (i.e., we are playing the first song).
(define FEEDBACKSTRING-EMPTY "")
(define FEEDBACKSTRING-DISLIKE "dislike")
(define FEEDBACKSTRING-LIKE "like")
(define FEEDBACKSTRING-NONE "none")
#;(define (feedbackstring-temp fs)
    (cond
      [(string=? fs FEEDBACKSTRING-EMPTY) ...]
      [(string=? fs FEEDBACKSTRING-DISLIKE) ...]
      [(string=? fs FEEDBACKSTRING-LIKE) ...]
      [(string=? fs FEEDBACKSTRING-NONE) ...]))

(define-struct song [name id artist length album byte-string])
; A Song is a (make-song String Nat String Number String String)
; - where the first String is the song's title
; - where the Nat is the song's unique ID#
; - the second String is the song's artist
; - the Number is the length of the song (in seconds)
; - the third String is the song's album
; - the fourth String is the actual byte-string of music
(define SONG-1 (make-song "Africa" 1 "Toto" 295 "Toto IV" S1))
(define SONG-2 (make-song "Sound & Color" 2 "Alabama Shakes" 181 "Sound & Color" S2))
; song-temp : Song -> ???
#; (define (song-temp sng)
     ... (song-name sng)
     ... (song-id sng)
     ... (song-artist sng)
     ... (song-length sng)
     ... (song-album sng)
     ... (song-byte-string sng) ...)

; A Status is one of:
; - "waiting"
; - "none"
; - "pending"
; - Song
; and represents the request status for songs, where "none" means no request made yet,
; "pending" means a request has been sent, and Song is the song recieved from a request.
(define STATUS-W "waiting")
(define STATUS-N "none")
(define STATUS-P "pending")
(define STATUS-S-1 SONG-1)
(define STATUS-S-2 SONG-2)
; status-temp : Status -> ???
#; (define (status-temp st)
     (cond
       [(string=? "waiting" st) ...]
       [(string=? "none" st) ...]
       [(string=? "pending" st) ...]
       [(song-temp (song? st)) ...]))

(define-struct player [status feedback history songoption]) 
; A MusicPlayer is a (make-player Status FeedbackString History SongOption)
; Interpretation: The state of the music player
; - status is whether the player has sent or recieved a request or song, respectively
; - feedback is the feedback received from the user for the last song played
; - history is the count of all the songs available on the server
; - songoption is all of the songs available on the server with an index of songs chosen
(define MUSICPLAYER-0 (make-player "waiting" "" H0 SO0))
(define MUSICPLAYER-1 (make-player "none" "" H0 SO1))
(define MUSICPLAYER-2 (make-player "pending" "" H1 SO2))
(define MUSICPLAYER-3 (make-player SONG-1 "like" H2 SO1))
(define MUSICPLAYER-4 (make-player SONG-2 "like" H1 SO1))
(define MUSICPLAYER-5 (make-player SONG-2 "like" H1 SO3))
#; (define (musicplayer-temp playr)
     ... (status-temp playr) ...
     ... (feedbackstring-temp (player-feedback playr)) ...
     ... (history-temp (player-history playr)) ...
     ... (so-temp (player-songoption playr)))

(define BACKGROUND (rectangle 200 200 "solid" "white"))
(define INSTRUCTIONS (text "Press the up or down arrow key to move through the songs.
 Press enter to a select a song. Then press space to play that song." 10 "black"))

; music-player : MusicPlayer -> MusicPlayer
; given a MusicPlayer, creates a program that plays and cycles through songs
(define (music-player playr)
  (player-feedback
   (history->file
    (big-bang (build-music-player "player-history.csv" playr)
      [register "dictionary.ccs.neu.edu"]
      [port 10001]
      [to-draw draw-world]
      [on-key key-handler]
      [on-receive handle-network]))))

; build-music-player : File -> MusicPlayer
; Given a file containing the History, returns a new MusicPlayer updated with that History
; If the file does not exist, returns the given MusicPlayer
(check-expect (build-music-player "fundies" MUSICPLAYER-1) MUSICPLAYER-1)
(define (build-music-player f playr)
  (cond
    [(file-exists? f)
     (make-player (player-status playr)
                  (player-feedback playr)
                  (struct-convert (read-csv-file f))
                  (player-songoption playr))]
    [else playr]))

; struct-convert : [List-of [List-of String]] -> [List-of SongHistory]
; given a list of a list of strings, converts it to a list of SongHistories
(check-expect (struct-convert (list (list "Africa" "Toto" "Toto IV" "1" "3")
                                    (list "Sound & Color" "Alabama Shakes" "Sound & Color" "2" "8")))
              H2)
(check-expect (struct-convert (list (list "Africa" "Toto" "Toto IV" "1" "3")))
              H1)
(define (struct-convert lolos)
  (local [; to-struct : [List-of String] -> SongHistory
          ; given a list of strings, converts it to a SongHistory
          (define (to-struct los)
            (make-songhistory (first los) (second los) (third los)
                              (string->number (fourth los)) (string->number (fifth los))))]
    (map to-struct lolos)))

; history-file : Playr -> String
; writes the History of a Playr to a text file
(define (history->file playr)
  (write-file "player-history.csv" (history-content playr)))
  
; history-content : MusicPlayer -> String
; converts the History of a given MusicPlayer to a string
(check-expect (history-content MUSICPLAYER-1) "")
(check-expect (history-content MUSICPLAYER-2) "Africa, Toto, Toto IV, 1, 3 \n")
(check-expect (history-content MUSICPLAYER-3)
              "Africa, Toto, Toto IV, 1, 3 \nSound & Color, Alabama Shakes, Sound & Color, 2, 8 \n")
(define (history-content playr)
  (local [; write-history : SongHistory String -> String
          ; Returns a string of a SongHistory
          (define (write-history sh combined)
            (string-append (songhistory-name sh) ", "
                           (songhistory-artist sh) ", "
                           (songhistory-album sh) ", "
                           (number->string
                            (songhistory-id sh)) ", "
                                                 (number->string
                                                  (songhistory-count sh)) " " "\n" combined))]
    (foldr write-history "" (player-history playr))))

; update-song-count : History Song -> History
; Given a History and a Song, increases the count of that Song in History
(check-expect (update-song-count H0 SONG-1) (list (make-songhistory
                                                   "Africa" "Toto" "Toto IV" 1 1)))
(check-expect (update-song-count H1 SONG-1) (list (make-songhistory "Africa" "Toto" "Toto IV" 1 4)))
(check-expect (update-song-count H2 SONG-2) (list (make-songhistory "Africa" "Toto" "Toto IV" 1 3)
                                                  (make-songhistory "Sound & Color" "Alabama Shakes"
                                                                    "Sound & Color" 2 9)))
(define (update-song-count h s)
  (cond
    [(empty? h) (list (make-songhistory (song-name s) (song-artist s) (song-album s)
                                        (song-id s) 1))]
    [(cons? h) (if (equal? (song-id s) (songhistory-id (first h)))
                   (cons (make-songhistory (song-name s) (song-artist s) (song-album s)
                                           (song-id s)
                                           (add1 (songhistory-count (first h))))
                         (rest h))
                   (cons (first h)
                         (update-song-count (rest h) s)))]))

; draw-world: MusicPlayer -> Image
; Draws the user interface for the music player, showing the most recently provided feedback,
; the history of the songs played,
; and instructions for how to play the next song.
(check-expect (draw-world MUSICPLAYER-0) (overlay (above
                                                   (text "Please wait for the server to connect" 20
                                                         "black")
                                                   (text "" 10 "black")) BACKGROUND))
(check-expect (draw-world MUSICPLAYER-1) (overlay (above
                                                   (above (text "Africa 0" 10 "black")
                                                          (text "Sound & Color 0" 10 "blue"))
                                                   (text "" 10 "black")
                                                   INSTRUCTIONS) BACKGROUND))
(check-expect (draw-world MUSICPLAYER-3) (overlay (above
                                                   (text "Africa by Toto" 10 "black")
                                                   (text "Africa 3" 10 "black")
                                                   (text "Sound & Color 8" 10 "blue")
                                                   (text "like" 10 "black")
                                                   INSTRUCTIONS) BACKGROUND))
(check-expect (draw-world MUSICPLAYER-4) (overlay (above
                                                   (text "Sound & Color by Alabama Shakes" 10 "black")
                                                   (text "Africa 3" 10 "black")
                                                   (text "Sound & Color 0" 10 "blue")
                                                   (text "like" 10 "black")
                                                   INSTRUCTIONS) BACKGROUND))
(define (draw-world playr)
  (overlay
   (cond
     [(or (equal? (player-status playr) "pending") (equal? (player-status playr) "none"))
      (above (songs-image playr)
             (text (player-feedback playr) 10 "black") INSTRUCTIONS)]
     [(equal? (player-status playr) "waiting")
      (above (songs-image playr)
             (text (player-feedback playr) 10 "black"))]
     [(song? (player-status playr))
      (above
       (text (string-append
              (song-name (player-status playr)) " by "
              (song-artist (player-status playr))) 10 "black")
       (above (songs-image playr)
              (text (player-feedback playr) 10 "black") INSTRUCTIONS))]) BACKGROUND))

; songlist : MusicPlayer -> [List-of String]
; given a MusicPlayer, returns a list of all the songs available on the server with a count of how
; times they have been played (count of 0 for a song that has not been played)
(check-expect (songlist MUSICPLAYER-1) (list "Africa 0" "Sound & Color 0"))
(check-expect (songlist MUSICPLAYER-3) (list "Africa 3" "Sound & Color 8"))
(check-expect (songlist MUSICPLAYER-4) (list "Africa 3" "Sound & Color 0"))
(define (songlist playr)
  (local [; draw-songs : IDMetaPair [List-of String] -> [List-of String]
          ; Given an IDMetaPair and a list, returns a new list with that IDMetaPair with a count of 0
          ; if it is not found in the history or a count of how many times it has been played
          (define (draw-songs mp folded)
            (append
             (local [; in-history : SongHistory String -> String
                     ; given a SongHistory and an String, returns a new String
                     ; with that IDMetaPair appended to the old String
                     ; if it matches with the SongHistory
                     (define (in-history sh combined)
                       (if (equal? (first mp) (songhistory-id sh))
                           (list (string-append
                                  (first (second mp)) " "
                                  (number->string (songhistory-count sh))))
                           combined))]
               (if (empty? (foldr in-history '() (player-history playr)))
                   (list (string-append (first (second mp)) " 0"))
                   (foldr in-history '() (player-history playr)))) folded))]
    (foldr draw-songs '() (songoption-metadatalist (player-songoption playr)))))

; songs-image : MusicPlayer -> Image
; given a MusicPlayer, returns an Image of all the songs available on the server with a count of how
; times they have been played (count of 0 for a song that has not been played).
; The song that user currently selected is drawn in blue and the rest in black.
(check-expect (songs-image MUSICPLAYER-0) (text "Please wait for the server to connect" 20 "black"))
(check-expect (songs-image MUSICPLAYER-1) (above (text "Africa 0" 10 "black")
                                                 (text "Sound & Color 0" 10 "blue")))
(check-expect (songs-image MUSICPLAYER-3) (above (text "Africa 3" 10 "black")
                                                 (text "Sound & Color 8" 10 "blue")))
(check-expect (songs-image MUSICPLAYER-4) (above (text "Africa 3" 10 "black")
                                                 (text "Sound & Color 0" 10 "blue")))
(define (songs-image playr)
  (if (equal? (player-status playr) "waiting")
      (text "Please wait for the server to connect" 20 "black")
      (local [; color-songs : [List-of String] Number -> Image
              ; given a list of strings and an index, returns an image of all of the strings,
              ; with the string that matches up with the index currently selected by the user
              ; colored blue and the rest black
              (define (color-songs los i)
                (cond
                  [(empty? los) empty-image]
                  [(cons? los) (if (equal? (songoption-index (player-songoption playr)) i)
                                   (above (text (first los) 10 "blue")
                                          (color-songs (rest los) (+ 1 i)))
                                   (above (text (first los) 10 "black")
                                          (color-songs (rest los) (+ 1 i))))]))]
        (color-songs (songlist playr) 0))))

; key-handler: MusicPlayer KeyEvent -> PlayerResult
; Given a MusicPlayer and Key Event, plays the next song if the key pressed is a space.
(check-expect (key-handler MUSICPLAYER-0 "up") MUSICPLAYER-0)
(check-expect (key-handler MUSICPLAYER-1 "up") (make-player "none" "" H0 (make-songoption
                                                                          (list IP1 IP2) 0)))
(check-expect (key-handler MUSICPLAYER-2 "up") (make-player "pending" "" H1 (make-songoption
                                                                             (list IP1) 0)))
(check-expect (key-handler MUSICPLAYER-5 "up") (make-player SONG-2 "like" H1 (make-songoption
                                                                              (list IP1 IP2 IP3) 0)))
(check-expect (key-handler MUSICPLAYER-1 "down") (make-player "none" "" H0 (make-songoption
                                                                            (list IP1 IP2) 0)))
(check-expect (key-handler MUSICPLAYER-2 "down") (make-player "pending" "" H1 (make-songoption
                                                                               (list IP1) 0)))
(check-expect (key-handler MUSICPLAYER-5 "down") (make-player SONG-2 "like" H1 (make-songoption
                                                                                (list IP1 IP2 IP3)
                                                                                2)))
(check-expect (key-handler MUSICPLAYER-1 "\r") (make-package (make-player "pending" "" H0 SO1) 2))
(check-expect (key-handler MUSICPLAYER-2 "\r") (make-package (make-player "pending" "" H1 SO2) 1))
(check-expect (key-handler MUSICPLAYER-3 "\r") (make-package (make-player "pending" "like" H2 SO1) 2))
(check-expect (key-handler MUSICPLAYER-1 " ") MUSICPLAYER-1)
(define (key-handler playr key)
  (if (equal? (player-status playr) "waiting")
      playr
      (cond
        [(key=? " " key)
         (if (song? (player-status playr))   
             (make-player "none" (play-sound (song-byte-string (player-status playr)))
                          (update-song-count (player-history playr) (player-status playr))
                          (player-songoption playr))
             playr)]
        [(key=? "up" key) (make-player (player-status playr) (player-feedback playr)
                                       (player-history playr)
                                       (up-key-handler (player-songoption playr)))]
        [(key=? "down" key) (make-player (player-status playr) (player-feedback playr)
                                         (player-history playr) (down-key-handler
                                                                 (player-songoption playr)))]
        [(key=? "\r" key)  (make-package (make-player "pending" (player-feedback playr)
                                                      (player-history playr)
                                                      (player-songoption playr))
                                         (first (list-ref
                                                 (songoption-metadatalist
                                                  (player-songoption playr))
                                                 (songoption-index
                                                  (player-songoption playr)))))]
        [else playr])))

; up-key-handler : SongOption -> SongOption
; Given a SongOption, subtracts one from the index
(check-expect (up-key-handler SO1) (make-songoption (list IP1 IP2) 0))
(check-expect (up-key-handler SO2) (make-songoption (list IP1) 0))
(check-expect (up-key-handler SO3) (make-songoption (list IP1 IP2 IP3) 0))
(define (up-key-handler so)
  (if (equal? 0 (songoption-index so))
      (make-songoption (songoption-metadatalist so) (- (length (songoption-metadatalist so)) 1))
      (make-songoption (songoption-metadatalist so) (- (songoption-index so) 1))))

; down-key-handler : SongOption -> SongOption
; Given a SongOption, adds one to the index
(check-expect (down-key-handler SO1) (make-songoption (list IP1 IP2) 0))
(check-expect (down-key-handler SO2) (make-songoption (list IP1) 0))
(check-expect (down-key-handler SO3) (make-songoption (list IP1 IP2 IP3) 2))
(define (down-key-handler so)
  (if (equal? (- (length (songoption-metadatalist so)) 1) (songoption-index so))
      (make-songoption (songoption-metadatalist so) 0)
      (make-songoption (songoption-metadatalist so) (+ (songoption-index so) 1))))

; handle-network : MusicPlayer ServerMsg -> PlayerResult
; produces a new music player based on the message recieved from the server
(check-expect (handle-network MUSICPLAYER-0 (list "METADATA" (list IP1 IP2 IP3)))
              (make-player "none" "" H0 (make-songoption (list IP1 IP2 IP3) 0)))
(check-expect (handle-network MUSICPLAYER-1 (list "ERROR" "error")) MUSICPLAYER-1)
(check-expect (handle-network MUSICPLAYER-2
                              (list "SONG" 1 (list "Africa" "Toto" 295 "Toto IV") S1))
              (make-player SONG-1 "" H1 SO2))
(define (handle-network playr msg)
  (cond [(string=? "ERROR" (first msg)) playr]
        [(and (string=? "SONG" (first msg)) (equal? (player-status playr) "pending"))
         (make-player (make-song (first (third msg)) (second msg) (second (third msg))
                                 (third (third msg)) (fourth (third msg)) (fourth msg))
                      (player-feedback playr) (player-history playr)
                      (player-songoption playr))]
        [(string=? "METADATA" (first msg))
         (make-player "none" (player-feedback playr)
                      (player-history playr) (make-songoption (second msg) 0))]))
