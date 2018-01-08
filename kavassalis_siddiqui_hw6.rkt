;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname kavassalis-siddiqui-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; Fivos Kavassalis/ fikavassalis
;; Hajera Siddiqui/ hhsiddiqui


;; a TimeOfDay is one of                an Itemization 
;; "daytime" 
;; "primetime"
;; "off-hour"


(define-struct ad(name-prod duration cost nationally? time-of-day num-times))
;; an Ad is a (make-ad String Natural Number Boolean TimeOfDay Natural)
;; interp. represents an ad where
;; name-prod is the name of the product the ad is for
;; duration is the duration of the ad (in seconds)
;; cost is the cost to produce the ad (in thousands of dollars)
;; nationally? shows whether or not the ad is to be aired nationally (as opposed to locally)
;; time-of-day is the time of day that the ad is to be aired (either daytime, primetime, or off-hour)
;; and num-times is the number of times the ad is to be aired

(define NIKE-AD (make-ad "Courage" 62 506 true "primetime" 1000))
(define APPLE-COMMERCIAL (make-ad "iPhone X" 69 356 false "daytime" 600))


;; a ListOfAd is one of
;; empty
;; (cons Ad ListOfAd)

(define ALOA-1 (cons NIKE-AD (cons (make-ad "Greatness" 46 307 true "off-hour" 400) empty))) 
(define ALOA-2 (cons (make-ad "Dior Perfume" 31 609 true "primetime" 900)(cons (make-ad "Sony Smart TV" 28 609 false "daytime" 888)(cons APPLE-COMMERCIAL empty))))  

;;
;; 1.
;; 
 

;; count-long-primetime: ListOfAd Natural -> Number
;; consumes a list of ads and an amount of time (in seconds)
;; and produces the number of primetime ads in the list that run longer than the given number of seconds. 

(check-expect (count-long-primetime empty 60)0)
(check-expect (count-long-primetime ALOA-2 28)1) 
(check-expect (count-long-primetime (cons (make-ad "The Best Ad" 30 710 true "primetime" 990) empty) 47)0)

   
(define (count-long-primetime aloa a-time)
  (local [(define (long-and-primetime? an-ad) (and (> (ad-duration an-ad) a-time) (string=? (ad-time-of-day an-ad) "primetime")))]
    (length (filter long-and-primetime? aloa))))

;;
;; 2.
;;

;; national-ads: ListOfAd -> ListOfAd
;; consumes a list of ads and produces a list of all the ads airing nationally

(check-expect (national-ads empty) empty)
(check-expect (national-ads ALOA-1) ALOA-1)
(check-expect (national-ads (cons (make-ad "Ad number 1" 31 209 true "off-hour" 400)(cons (make-ad "Ad number 2" 29 196 false "daytime" 450)(cons (make-ad "Ad number 3" 28 203 true "primetime" 360) empty))))(cons (make-ad "Ad number 1" 31 209 true "off-hour" 400)(cons (make-ad "Ad number 3" 28 203 true "primetime" 360) empty))) 
(check-expect (national-ads (cons (make-ad "Ad 1" 31 209 false "off-hour" 400)(cons (make-ad "Ad 2" 29 196 false "daytime" 450)(cons (make-ad "Ad 3" 28 203 true "primetime" 360) empty))))(cons (make-ad "Ad 3" 28 203 true "primetime" 360) empty)) 

(define (national-ads aloa)
  (filter ad-nationally? aloa))

;;
;; 3.
;;

;; a ListOfString is one of
;; empty
;; (cons String ListOfString)

  
;; late-night-products: ListOfAd -> ListOfString
;; consumes a list of ads and produces a list of strings
;; The list that is produced contains the names of the products that are advertised during the off-hours

(check-expect (late-night-products empty) empty)
(check-expect (late-night-products (cons (make-ad "Ad number 1" 31 209 true "off-hour" 400)(cons (make-ad "Ad number 2" 29 196 false "daytime" 450)(cons (make-ad "Ad number 3" 28 203 true "primetime" 360) empty))))(cons "Ad number 1" empty))
(check-expect (late-night-products (cons (make-ad "Ad number 1" 31 209 true "primetime" 400)(cons (make-ad "Ad number 2" 29 196 false "off-hour" 450)(cons (make-ad "Ad number 3" 28 203 true "off-hour" 360) empty))))(cons "Ad number 2"(cons "Ad number 3" empty)))


(define (late-night-products aloa)
  (local [(define (is-off-hour? an-ad) (string=? (ad-time-of-day an-ad) "off-hour"))]
    (map ad-name-prod (filter is-off-hour? aloa))))
           


;;
;; 4.
;;

(define ONE-PERCENT 1/100)

;; adjust-for-inflation: ListOfAds -> ListOfAds
;; consumes a list of ads and produces a list where the production cost
;; for each ad in the list has increased by 1%

(check-expect (adjust-for-inflation empty) empty)
(check-expect (adjust-for-inflation (list (make-ad "Ad 1" 32 100 true "primetime" 50))) (list (make-ad "Ad 1" 32 101 true "primetime" 50)))
(check-expect (adjust-for-inflation (list (make-ad "Ad 2" 23 78 false "off-hour" 30) (make-ad "Ad 3" 29 128 true "daytime" 55))) (list (make-ad "Ad 2" 23 78.78 false "off-hour" 30) (make-ad "Ad 3" 29 129.28 true "daytime" 55)))

(define (adjust-for-inflation aloa)
  (local [(define (increase an-ad) (make-ad (ad-name-prod an-ad)
                                            (ad-duration an-ad)
                                            (+ (ad-cost an-ad) (* ONE-PERCENT (ad-cost an-ad)))
                                            (ad-nationally? an-ad)
                                            (ad-time-of-day an-ad)
                                            (ad-num-times an-ad)))]
    (map increase aloa)))


;;
;; 5.
;;

(define-struct message (sender text flag?))
;; interp: a Message is a (make-message User String Boolean)
;; where sender is the user that sent the message,
;; text is the text of the message and
;; flag? indicates whether or not the user has read the message (true if not read)

(define-struct user (username messages))
;; interp: a User is a (make-user String ListOfMessages)
;; where username is the username of the user and
;; messages is a list of messages in his or her mailbox

;; a MailSystem is a ListOfUser which is one of:
;; empty
;; (cons User ListOfUser)

;; mailsys: MailSystem
;; remembers info about each user in the mail system
(define mailsys (list ))


;;
;; 6.
;;

;; add-user: String -> void
;; consumes a username and produces void
;; EFFECT: Adds a new user with the given username amd an empty mailbox to the mail system
;; Assume: the username is not already in the mail system

(define (add-user a-username)
  (set! mailsys (cons (make-user a-username empty) mailsys)))


;;
;; 7.
;;

;; send-email: String String String -> void
;; consumes the name of the sender of an email, the name of the recipient of the email,
;; and the text of an email message, and produces void.
;; EFFECT: Stores a new unread message in the recipient's mailbox
;; ASSUME: The named recipient is a user in the mail system

(define (send-emails name-sender name-recipient a-text)
  (send-emails-list name-sender name-recipient a-text mailsys))


;; send-emails-list: String String String MailSystem -> void
;; consumes the name of the sender of an email, the name of the recipient of the email,
;; the text of an email message, a mail system and produces void
;; EFFECT: Stores a new unread message in the recipient's mailbox
;; ASSUME: The named recipient is a user in the mail system

(define (send-emails-list name-sender name-recipient a-text alou)
  (set-user-messages! (helper name-recipient alou) (cons (make-message (helper name-sender alou) a-text true) (user-messages (helper name-recipient alou)))))
  

;; helper: String MailSystem -> User
;; consumes the name of a user and a list of users and
;; produces the user with the given name
;; ASSUME: The ListOfUser given is not an empty list and the username given belongs to a User in the list

(define (helper a-username alou)
  (if (string=? a-username (user-username (first alou)))
      (first alou)
      (helper a-username (rest alou)))) 


;;
;; 8.
;;

;; get-unread-messages-and-mark-read: String -> ListOfMessages
;; consumes a username and produces a list of messages.
;; The produced list contains the (previously) unread messages
;; in the mailbox of the user with the given name
;; ASSUME: The username is a valid user in the mail system
;; EFFECT: All such unread messages in the named user's mailbox
;; have been set to read

(define (get-unread-messages-and-mark-read username-of-user)
  (get-unread-messages-and-mark-read-list username-of-user mailsys)) 





;; get-unread-messages-and-mark-read-list: String MailSystem -> ListOfMessages
;; consumes a username and a mail system and produces a list of messages.
;; The produced list contains the (previously) unread messages
;; in the mailbox of the user with the given name
;; ASSUME: The username is a valid user in the mail system
;; EFFECT: All such unread messages in the named user's mailbox
;; have been set to read

(define (get-unread-messages-and-mark-read-list username-of-user alou)
  (cond [(empty? (user-messages (helper1 username-of-user alou))) empty]
        [(cons? (user-messages (helper1 username-of-user alou))) (local [(define (make-false a-message) (make-message (message-sender a-message) (message-text a-message) false))] 
                                                                   (map make-false (local [(define (true? a-message) (not (false? (message-flag? a-message))))]
                                                                                     (filter true? (user-messages (helper1 username-of-user alou))))))]))






;; helper1: String MailSystem -> User
;; consumes the name of a user and a list of users and
;; produces the user with the given name
;; ASSUME: The ListOfUser given is not an empty list and the username given belongs to a User in the list

(define (helper1 a-username alou)
  (if (string=? a-username (user-username (first alou)))
      (first alou)
      (helper1 a-username (rest alou))))



;;
;; 9.
;;


;; most-messages: -> User
;; The function doesn't consume anything and
;; produces the user in the mailsystem with the largest number of messages in his/her mailbox.
;; If there are no users in the system, the function produces an appropriate error.
;; If two or more users have the most messages, the function just needs to return one of them (it doesn't matter which one)


(define (most-messages)
  (local [(define (mostmessages alou acc)
            (cond [(empty? alou) acc]
                  [(cons? alou) (if (> (length (user-messages (first alou)))  (length (user-messages acc)))
                                    (mostmessages (rest alou) (first alou))
                                    (mostmessages (rest alou) acc))]))]
    (cond [(empty? mailsys)  "there are no users in the system"]
          [(cons? mailsys)  (mostmessages mailsys (first mailsys))]))) 
   

;;
;; 10.
;;

"show mailsys"
mailsys

"Add a new user with the username given and an empty mailbox to the mail system"
(add-user "Fivos")
(add-user "Hajera")
(add-user "Nick")
(add-user "Clint")
(add-user "Maria")
(add-user "Jamie")
(add-user "Sherane")
(add-user "Keysha")
mailsys

"Stores a new unread message in the recipient's mailbox with Fivos being the sender, Hajera being the recipient and the text: Hello"
(send-emails "Fivos" "Hajera" "Hello")

"Stores a new unread message in the recipient's mailbox with Nick being the sender, Fivos being the recipient and the text: What's up?"
(send-emails "Nick" "Fivos" "What's up?")

"Stores a new unread message in the recipient's mailbox with Fivos being the sender, Nick being the recipient and the text: What's up?"
(send-emails "Fivos" "Nick" "What's up?")

"Stores a new unread message in the recipient's mailbox with Nick being the sender, Fivos being the recipient and the text: All good"
(send-emails "Nick" "Fivos" "All good")

"Stores a new unread message in the recipient's mailbox with Fivos being the sender, Nick being the recipient and the text: Have any plans today?"
(send-emails "Fivos" "Nick" "Have any plans today?")

"Stores a new unread message in the recipient's mailbox with Hajera being the sender, Nick being the recipient and the text: How are you?"
(send-emails "Hajera" "Nick" "How are you?")

"Stores a new unread message in the recipient's mailbox with Sherane being the sender, Jamie being the recipient and the text: Hi!"
(send-emails "Sherane" "Jamie" "Hi!")

"Stores a new unread message in the recipient's mailbox with Jamie being the sender, Sherane being the recipient and the text: How do you do?"
(send-emails "Jamie" "Sherane" "How do you do?")
mailsys

(get-unread-messages-and-mark-read "Maria")
(get-unread-messages-and-mark-read "Nick")
(get-unread-messages-and-mark-read "Fivos")

(most-messages)