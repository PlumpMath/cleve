;;;; api.lisp

(in-package :cleve-api)


;;; Variables

(defparameter *api-server* "http://api.eve-online.com/")
(defparameter *proxy* nil)  ; '("127.0.0.1" 8080))

(defparameter +numbers+ '(:account-id :account-key :amount :arg-id1 :balance
                          :character-id :client-id :corporation-id :error-code
                          :journal-transaction-id :owner-id1 :owner-id2 :price
                          :quantity :ref-id :ref-type-id :ship-jumps
                          :solar-system-id :station-id :tax-amount
                          :tax-receiver-id :transaction-id :type-id))


;;; Common Functions

(defun lispify (string)
  (loop with prev-char = nil
        with result = nil
        for char across string
        do (cond ;; "X" => "-x"
                 ((and (upper-case-p char)
                       (> (length result) 1)  ; don't put a #\- at the front
                       ;; TODO use lower-case-p?
                       (not (upper-case-p prev-char))) ; don't seperate abbrevs
                  (push #\- result)
                  (push char result))
                 ;; " " => "-"
                 ((equal char #\Space)
                  (push #\- result))
                 ;; "_" => "-"
                 ((equal char #\_)
                  (when (> (length result) 1)  ; don't put a #\- at the front
                    (push #\- result)))
                 ;; Otherwise just collect the char.
                 (t (push char result)))
           (setf prev-char char)
        finally (return (string-upcase (coerce (reverse result) 'string)))))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))


(defun errmsg (&rest args)
  (format *error-output* (apply #'mkstr args)))


(defun str2bool (string)
  (if (equal (string-downcase string) "false")
      nil
      t))


;;; Conditions

(define-condition eve-api-error (error)
  ((code :initarg :code :reader code)
   (message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "EVE API Error ~D: ~A"
                     (code condition) (message condition)))))


;;; Classes & Methods (& Functions)

(defmethod attr= ((attribute sax::standard-attribute) string)
  (string= (sax:attribute-local-name attribute) string))

(defun attr-member (item attributes)
  (member item attributes :key #'sax:attribute-local-name :test #'string=))

(defmethod value ((attribute sax::standard-attribute))
  (sax:attribute-value attribute))

(defmethod value2number ((attribute sax::standard-attribute))
  (when (> (length (sax:attribute-value attribute)) 0)
    (let ((*read-default-float-format* 'double-float))
      (parse-number (sax:attribute-value attribute)))))

;;; eve-api-handler

(defclass eve-api-handler (sax:default-handler)
  ((current-element :accessor current-element :initform nil)
   (parent-elements :accessor parent-elements :initform nil)
   (cached-until :accessor cached-until :initform nil)
   (current-time :accessor current-time :initform nil)
   (error-code :accessor error-code :initform nil)
   (error-message :accessor error-message :initform nil)))

(defmethod elt= ((handler eve-api-handler) string)
  (when (current-element handler)
    (string= (current-element handler) string)))

(defmethod sax:characters :before ((handler eve-api-handler) data)
  (when (elt= handler "currentTime")
    (setf (current-time handler) data))
  (when (elt= handler "cachedUntil")
    (setf (cached-until handler) data))
  (when (elt= handler "error")
    (setf (error-message handler) data)
    (error 'eve-api-error :code (error-code handler)
                          :message (error-message handler))))

(defmethod sax:end-element :after ((handler eve-api-handler) namespace-uri
                                   local-name qname)
  (if (parent-elements handler)
      (setf (current-element handler) (pop (parent-elements handler)))
      (setf (current-element handler) nil)))

(defmethod sax:start-element :before ((handler eve-api-handler) namespace-uri
                                      local-name qname attributes)
  (when (current-element handler)
    (push (current-element handler) (parent-elements handler)))
  (setf (current-element handler) local-name)
  (when (elt= handler "error")
    (loop for attr in attributes
          when (attr= attr "code")
            do (setf (error-code handler) (value2number attr)))))

;;; rowset-handler

(defclass rowset-handler (eve-api-handler)
  ((columns :accessor columns :initform nil)
   (key :accessor key :initform nil)
   (name :accessor name :initform nil)
   (rows :accessor rows :initform nil)))

(defmethod sax:start-element ((handler rowset-handler)
                              namespace-uri local-name qname attributes)
  (when (elt= handler "rowset")
    (loop for attr in attributes
          when (attr= attr "columns")
            do (setf (columns handler)
                     (loop for column in (split-sequence #\, (value attr))
                           collect (intern (lispify column) :keyword)))
          when (attr= attr "key")
            do (setf (key handler) (intern (lispify (value attr)) :keyword))
          when (attr= attr "name")
            do (setf (name handler) (value attr))))
  (when (elt= handler "row")
    (push (loop for attr in (reverse attributes)  ; CXML has them reversed
                for i from 0
                for column = (elt (columns handler) i)
                append (list column (if (member column +numbers+)
                                        (value2number attr)
                                        (value attr))))
          (rows handler))))

;;; account-account-status-handler

(defclass account-account-status-handler (eve-api-handler)
  ((create-date :accessor create-date :initform nil)
   (logon-count :accessor logon-count :initform nil)
   (logon-minutes :accessor logon-minutes :initform nil)
   (paid-until :accessor paid-until :initform nil)
   (user-id :accessor user-id :initform nil)))

(defmethod sax:characters ((handler account-account-status-handler) data)
  (cond ((elt= handler "createDate")
         (setf (create-date handler) data))
        ((elt= handler "logonCount")
         (setf (logon-count handler) (parse-number data)))
        ((elt= handler "logonMinutes")
         (setf (logon-minutes handler) (parse-number data)))
        ((elt= handler "paidUntil")
         (setf (paid-until handler) data))
        ((elt= handler "userID")
         (setf (user-id handler) (parse-number data)))))

;;; char-character-sheet-handler
;;;
;;; TODO low hanging fruit has been picked, doesn't support implants and ...
;;;      corp* stuff yet.

(defclass char-character-sheet-handler (eve-api-handler)
  ((alliance-id :accessor alliance-id :initform nil)
   (alliance-name :accessor alliance-name :initform nil)
   (ancestry :accessor ancestry :initform nil)
   (balance :accessor balance :initform nil)
   (blood-line :accessor blood-line :initform nil)
   (certificates :accessor certificates :initform nil)
   (character-id :accessor character-id :initform nil)
   (clone-name :accessor clone-name :initform nil)
   (clone-skill-points :accessor clone-skill-points :initform nil)
   (corporation-id :accessor corporation-id :initform nil)
   (corporation-name :accessor corporation-name :initform nil)
   (date-of-birth :accessor date-of-birth :initform nil)
   (gender :accessor gender :initform nil)
   (name :accessor name :initform nil)
   (race :accessor race :initform nil)
   (skills :accessor skills :initform nil)))

(defmethod sax:characters ((handler char-character-sheet-handler) data)
  (cond ((elt= handler "allianceID")
         (setf (alliance-id handler) (parse-number data)))
        ((elt= handler "allianceName")
         (setf (alliance-name handler) data))
        ((elt= handler "ancestry")
         (setf (ancestry handler) data))
        ((elt= handler "balance")
         (setf (balance handler) (parse-number data)))
        ((elt= handler "bloodLine")
         (setf (blood-line handler) data))
        ((elt= handler "characterID")
         (setf (character-id handler) (parse-number data)))
        ((elt= handler "cloneName")
         (setf (clone-name handler) data))
        ((elt= handler "cloneSkillPoints")
         (setf (clone-skill-points handler) (parse-number data)))
        ((elt= handler "corporationID")
         (setf (corporation-id handler) (parse-number data)))
        ((elt= handler "corporationName")
         (setf (corporation-name handler) data))
        ((elt= handler "DoB")
         (setf (date-of-birth handler) data))
        ((elt= handler "gender")
         (setf (gender handler) data))
        ((elt= handler "name")
         (setf (name handler) data))
        ((elt= handler "race")
         (setf (race handler) data))))

(defmethod sax:start-element ((handler char-character-sheet-handler)
                              namespace-uri local-name qname attributes)
  (cond ((and (elt= handler "row") (attr-member "skillpoints" attributes))
         (push (nreverse (loop for attr in attributes
                               when (attr= attr "level")
                                 append (list (value2number attr) :level)
                               when (attr= attr "published")
                                 append (list (value2number attr) :published)
                               when (attr= attr "skillpoints")
                                 append (list (value2number attr) :skillpoints)
                               when (attr= attr "typeID")
                                 append (list (value2number attr) :type-id)))
               (skills handler)))
        ((and (elt= handler "row") (attr-member "certificateID" attributes))
         ;; TODO LOOP should become something like GET-ATTR.
         (push (loop with result = nil
                     for attr in attributes
                     when (attr= attr "certificateID")
                       do (setf result (value2number attr))
                     finally (return result))
               (certificates handler)))))

;;; char-wallet-journal-handler

(defclass char-wallet-journal-handler (eve-api-handler)
  ((journal :accessor journal :initform nil)
   (lowest-ref-id :accessor lowest-ref-id :initform nil)))

(defmethod sax:start-element ((handler char-wallet-journal-handler)
                              namespace-uri local-name qname attributes)
  (when (elt= handler "row")
    (push (loop for attr in attributes
                when (attr= attr "amount")
                  append (list :amount (value2number attr))
                when (attr= attr "argID1")
                  append (list :arg-id-1 (value2number attr))
                when (attr= attr "argName1")
                  append (list :arg-name-1 (value attr))
                when (attr= attr "balance")
                  append (list :balance (value2number attr))
                when (attr= attr "date")
                  append (list :date (value attr))
                when (attr= attr "ownerID1")
                  append (list :owner-id-1 (value2number attr))
                when (attr= attr "ownerName1")
                  append (list :owner-name-1 (value attr))
                when (attr= attr "ownerID2")
                  append (list :owner-id-2 (value2number attr))
                when (attr= attr "ownerName2")
                  append (list :owner-name-2 (value attr))
                when (attr= attr "reason")
                  append (list :reason (value attr))
                when (attr= attr "refID")
                  append (list :ref-id (value2number attr))
                ;; TODO readup on LOOP and combine the forms above and below
                when (and (attr= attr "refID")
                          (or (null (lowest-ref-id handler))
                              (< (value2number attr) (lowest-ref-id handler))))
                  do (setf (lowest-ref-id handler) (value2number attr))
                when (attr= attr "refTypeID")
                  append (list :ref-type-id (value2number attr))
                when (attr= attr "taxReceiverID")
                  append (list :tax-receiver-id (value2number attr))
                when (attr= attr "taxAmount")
                  append (list :tax-amount (value2number attr)))
          (journal handler))))

;;; char-wallet-transactions-handler

(defclass char-wallet-transactions-handler (eve-api-handler)
  ((lowest-trans-id :accessor lowest-trans-id :initform nil)
   (transactions :accessor transactions :initform nil)))

(defmethod sax:start-element ((handler char-wallet-transactions-handler)
                              namespace-uri local-name qname attributes)
  (when (elt= handler "row")
    (push (loop for attr in attributes
                when (attr= attr "clientID")
                  append (list :client-id (value2number attr))
                when (attr= attr "clientName")
                  append (list :client-name (value attr))
                when (attr= attr "journalTransactionID")
                  append (list :journal-transaction-id (value2number attr))
                when (attr= attr "price")
                  append (list :price (value2number attr))
                when (attr= attr "quantity")
                  append (list :quantity (value2number attr))
                when (attr= attr "stationID")
                  append (list :station-id (value2number attr))
                when (attr= attr "stationName")
                  append (list :station-name (value attr))
                when (attr= attr "transactionDateTime")
                  append (list :transaction-date-time (value attr))
                when (attr= attr "transactionFor")
                  append (list :transaction-for (value attr))
                when (attr= attr "transactionID")
                  append (list :transaction-id (value2number attr))
                ;; TODO readup on LOOP and combine the forms above and below
                when (and (attr= attr "transactionID")
                        (or (null (lowest-trans-id handler))
                            (< (value2number attr) (lowest-trans-id handler))))
                  do (setf (lowest-trans-id handler) (value2number attr))
                when (attr= attr "transactionType")
                  append (list :transaction-type (value attr))
                when (attr= attr "typeID")
                  append (list :type-id (value2number attr))
                when (attr= attr "typeName")
                  append (list :type-name (value attr)))
          (transactions handler))))

;;; eve-character-info-handler

(defclass eve-character-info-handler (eve-api-handler)
  ((account-balance :accessor account-balance :initform nil)
   (bloodline :accessor bloodline :initform nil)
   (character-id :accessor character-id :initform nil)
   (character-name :accessor character-name :initform nil)
   (corporation :accessor corporation :initform nil)
   (corporation-date :accessor corporation-date :initform nil)
   (corporation-id :accessor corporation-id :initform nil)
   (last-known-location :accessor last-known-location :initform nil)
   (next-training-ends :accessor next-training-ends :initform nil)
   (security-status :accessor security-status :initform nil)
   (ship-name :accessor ship-name :initform nil)
   (ship-type-id :accessor ship-type-id :initform nil)
   (ship-type-name :accessor ship-type-name :initform nil)
   (skill-points :accessor skill-points :initform nil)
   (race :accessor race :initform nil)))

(defmethod sax:characters ((handler eve-character-info-handler) data)
  (cond ((elt= handler "accountBalance")
         (setf (account-balance handler) (parse-number data)))
        ((elt= handler "bloodline")
         (setf (bloodline handler) data))
        ((elt= handler "characterID")
         (setf (character-id handler) (parse-number data)))
        ((elt= handler "characterName")
         (setf (character-name handler) data))
        ((elt= handler "corporation")
         (setf (corporation handler) data))
        ((elt= handler "corporationDate")
         (setf (corporation-date handler) data))
        ((elt= handler "corporationID")
         (setf (corporation-id handler) (parse-number data)))
        ((elt= handler "lastKnownLocation")
         (setf (last-known-location handler) data))
        ((elt= handler "nextTrainingEnds")
         (setf (next-training-ends handler) data))
        ((elt= handler "securityStatus")
         (setf (security-status handler) (parse-number data)))
        ((elt= handler "shipName")
         (setf (ship-name handler) data))
        ((elt= handler "shipTypeID")
         (setf (ship-type-id handler) (parse-number data)))
        ((elt= handler "shipTypeName")
         (setf (ship-type-name handler) data))
        ((elt= handler "skillPoints")
         (setf (skill-points handler) (parse-number data)))
        ((elt= handler "race")
         (setf (race handler) data))))

;;; server-status-handler

(defclass server-status-handler (eve-api-handler)
  ((online-players :accessor online-players :initform nil)
   (server-open :accessor server-open :initform nil)))

(defmethod sax:characters ((handler server-status-handler) data)
  (cond ((elt= handler "onlinePlayers")
         (setf (online-players handler) (parse-number data)))
        ((elt= handler "serverOpen")
         (setf (server-open handler) (str2bool data)))))


;;; EVE API Functions

(defun api-url (relative-path)
  "Returns a concatenation of *API-SERVER* and RELATIVE-PATH."
  (mkstr *api-server* relative-path))


(defun api-request (url &key (parameters nil) (proxy *proxy*))
  (let ((res (handler-case (drakma:http-request url :method :post :proxy proxy
                                                    :parameters parameters)
               (usocket:timeout-error ()
                 (errmsg "Request for " url " timed out.~%")
                 nil))))
    res))


(defun parse-rowset-response (response)
  (when response
    (let ((handler (make-instance 'rowset-handler)))
      (cxml:parse response handler)
      (rows handler))))


;;; API Functions

;; Not completely tested.
;; FIXME user-id is nil
(defun account-account-status (key-id v-code char-id)
  (let ((res (api-request (api-url "account/AccountStatus.xml.aspx")
                          :parameters `(("keyid"       . ,(mkstr key-id))
                                        ("vcode"       . ,v-code)
                                        ("characterid" . ,(mkstr char-id)))))
        (hnd nil))
    (when res
      (setf hnd (make-instance 'account-account-status-handler))
      (cxml:parse res hnd)
      (list :create-date (create-date hnd)
            :logon-count (logon-count hnd)
            :logon-minutes (logon-minutes hnd)
            :paid-until (paid-until hnd)
            :user-id (user-id hnd)))))


(defun account-characters (key-id v-code)
  (let* ((res (api-request (api-url "account/Characters.xml.aspx")
                           :parameters `(("keyid" . ,(mkstr key-id))
                                         ("vcode" . ,v-code))))
         (rows (parse-rowset-response res)))
    (when rows
      (nreverse rows))))


;; FIXME <key accessMask...> isn't supported yet
(defun account-api-key-info (key-id v-code)
  (let* ((res (api-request (api-url "account/APIKeyInfo.xml.aspx")
                           :parameters `(("keyid" . ,(mkstr key-id))
                                         ("vcode" . ,v-code))))
         (rows (parse-rowset-response res)))
    (when rows
      (nreverse rows))))


;; untested for Odyssey
(defun char-account-balance (user-id api-key character-id)
  (let* ((res (api-request (api-url "char/AccountBalance.xml.aspx")
                       :parameters `(("userid" . ,(mkstr user-id))
                                     ("apikey" . ,api-key)
                                     ("characterid" . ,(mkstr character-id)))))
         (rows (parse-rowset-response res)))
    (when rows
      (first rows))))


;; untested for Odyssey
(defun char-character-sheet (user-id api-key character-id &optional (xml nil))
  (let ((res (if xml
                 xml
                 (api-request (api-url "char/CharacterSheet.xml.aspx")
                      :parameters `(("userid" . ,(mkstr user-id))
                                    ("apikey" . ,api-key)
                                    ("characterid" . ,(mkstr character-id))))))
        (hnd nil))
    (when res
      (setf hnd (make-instance 'char-character-sheet-handler))
      (cxml:parse res hnd)
      (list :alliance-id (alliance-id hnd)
            :alliance-name (alliance-name hnd)
            :ancestry (ancestry hnd)
            :balance (balance hnd)
            :blood-line (blood-line hnd)
            :character-id (character-id hnd)
            :clone-name (clone-name hnd)
            :clone-skill-points (clone-skill-points hnd)
            :corporation-id (corporation-id hnd)
            :corporation-name (corporation-name hnd)
            :date-of-birth (date-of-birth hnd)
            :gender (gender hnd)
            :name (name hnd)
            :race (race hnd)
            :certificates (nreverse (certificates hnd))
            :skills (nreverse (skills hnd))))))


;; untested for Odyssey
(defun char-wallet-journal (user-id api-key character-id
                            &key (all nil) (before-ref-id nil))
  (let* ((pars (append `(("userid" . ,(mkstr user-id))
                         ("apikey" . ,api-key)
                         ("characterid" . ,(mkstr character-id)))
                       (when before-ref-id
                         `(("beforerefid" . ,(mkstr before-ref-id))))))
         (res (api-request (api-url "char/WalletJournal.xml.aspx")
                           :parameters pars))
         (hnd nil))
    (when res
      (setf hnd (make-instance 'char-wallet-journal-handler))
      (cxml:parse res hnd)
      (when all
        (loop with n-entries = (length (journal hnd))
              while t  ; TODO hmm...
              for more = (api-request (api-url "char/WalletJournal.xml.aspx")
                          :parameters `(("userid" . ,(mkstr user-id))
                                        ("apikey" . ,api-key)
                                        ("characterid" . ,(mkstr character-id))
                                        ("beforerefid" . ,(mkstr (lowest-ref-id hnd)))))
              do (cxml:parse more hnd)
                 (when (= n-entries (length (journal hnd)))
                   (loop-finish))
                 (setf n-entries (length (journal hnd)))))
      (reverse (journal hnd)))))


;; untested for Odyssey
(defun char-wallet-transactions (user-id api-key character-id
                                 &key (all nil) (before-trans-id nil))
  (let* ((pars (append `(("userid" . ,(mkstr user-id))
                         ("apikey" . ,api-key)
                         ("characterid" . ,(mkstr character-id)))
                       (when before-trans-id
                         `(("beforetransid" . ,(mkstr before-trans-id))))))
         (res (api-request (api-url "char/WalletTransactions.xml.aspx")
                           :parameters pars))
         (hnd nil))
    (when res
      (setf hnd (make-instance 'char-wallet-transactions-handler))
      (cxml:parse res hnd)
      (when all
        (loop with n-trans = (length (transactions hnd))
              while t  ; TODO hmm...
              for more = (api-request (api-url "char/WalletTransactions.xml.aspx")
                          :parameters `(("userid" . ,(mkstr user-id))
                                        ("apikey" . ,api-key)
                                        ("characterid" . ,(mkstr character-id))
                                        ("beforetransid" . ,(mkstr (lowest-trans-id hnd)))))
              do (format t "Getting more before: ~A~%" (lowest-trans-id hnd))
                 (cxml:parse more hnd)
                 (when (= n-trans (length (transactions hnd)))
                   (loop-finish))
                 (setf n-trans (length (transactions hnd)))))
      (reverse (transactions hnd)))))


;; untested for Odyssey
(defun eve-character-info (user-id api-key character-id)
  (let ((res (api-request (api-url "eve/CharacterInfo.xml.aspx")
                       :parameters `(("userid" . ,(mkstr user-id))
                                     ("apikey" . ,api-key)
                                     ("characterid" . ,(mkstr character-id)))))
        (hnd nil))
    (when res
      (setf hnd (make-instance 'eve-character-info-handler))
      (cxml:parse res hnd)
      (list :account-balance (account-balance hnd)
            :bloodline (bloodline hnd)
            :character-id (character-id hnd)
            :character-name (character-name hnd)
            :corporation (corporation hnd)
            :corporation-date (corporation-date hnd)
            :corporation-id (corporation-id hnd)
            :last-known-location (last-known-location hnd)
            :next-training-ends (next-training-ends hnd)
            :security-status (security-status hnd)
            :ship-name (ship-name hnd)
            :ship-type-id (ship-type-id hnd)
            :ship-type-name (ship-type-name hnd)
            :skill-points (skill-points hnd)
            :race (race hnd)))))


(defun eve-conquerable-station-list ()
  (let* ((res (api-request (api-url "eve/ConquerableStationList.xml.aspx")))
         (rows (parse-rowset-response res)))
    (when rows
      (nreverse rows))))


(defun eve-error-list ()
  (let* ((res (api-request (api-url "eve/ErrorList.xml.aspx")))
         (rows (parse-rowset-response res)))
    (when rows
      (nreverse rows))))


(defun eve-ref-types ()
  (let* ((res (api-request (api-url "eve/RefTypes.xml.aspx")))
         (rows (parse-rowset-response res)))
    (when rows
      (nreverse rows))))


(defun map-jumps ()
  (let* ((res (api-request (api-url "map/Jumps.xml.aspx")))
         (rows (parse-rowset-response res)))
    (when rows
      (nreverse rows))))


(defun server-server-status ()
  (let ((res (api-request (api-url "server/ServerStatus.xml.aspx")))
        (hnd nil))
    (when res
      (setf hnd (make-instance 'server-status-handler))
      (cxml:parse res hnd)
      (list :online-players (online-players hnd)
            :server-open (server-open hnd)))))
