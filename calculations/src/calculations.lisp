;;;; calculations.lisp
;;;;
;;;; Some well known abbreviations have been used in function and variable
;;;; names.  Here's a list:
;;;;
;;;; - cap : capacitor
;;;; - hp  : hit points
;;;; - max : maximum
;;;;
;;;; Notes:
;;;; - All attributes affecting Sensor Booster II: (clsql:query "SELECT * FROM dgmAttributeTypes WHERE attributeID IN (SELECT attributeID FROM dgmTypeAttributes WHERE typeID = 1952)")

(in-package :cleve-calculations)


;;; Globals

(defparameter tmp nil)

;; categoryID(16) = "Skill"
(defvar +n-skills+ (caar (clsql:query "SELECT COUNT(*) FROM invTypes WHERE groupID in (SELECT groupID FROM invGroups WHERE categoryID = 16)")))


;;; Common Functions

(defun dbgmsg (&rest args)
  (dolist (a args)
    (princ a *debug-io*)))


(defun errmsg (&rest args)
  (dolist (a args)
    (princ a *error-output*)))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))


(defun print-hash-table (hash-table)
  (maphash (lambda (k v) (format t "~S: ~S~%" k v))
           hash-table))


;;; Pilot Functions

(defun all-level-x-hash-table (&optional (level 5))
  (let ((skills (make-hash-table :size +n-skills+)))
    (loop for skill in (clsql:query "SELECT typeID FROM invTypes WHERE groupID in (SELECT groupID FROM invGroups WHERE categoryID = 16)")
          do (setf (gethash (car skill) skills) level))
    skills))


;;; Calculation Functions
;;;
;;; Add info to docstrings about expected unit for f.e. agility and mass.

(defun agility (ship-agility skill-level-evasive-maneuvering
                skill-level-spaceship-command)
  (* (- 1 (* 0.05 skill-level-evasive-maneuvering))
     (- 1 (* 0.02 skill-level-spaceship-command))
     ship-agility))


(defun align-speed (agility mass)
  (/ (* (log 2) agility mass)
     500000))


(defun armor-hp (ship-armor-hp skill-level-hull-upgrades)
  (* (+ 1 (* 0.05 skill-level-hull-upgrades))
     ship-armor-hp))


;; See http://www.eveonline.com/ingameboard.asp?a=topic&threadID=116993&page=3
;; Recharge rate vs. capacitor capacity
;; dc(C) = tau*(1-C/C_0)*sqrt(2*C/C_0 - (C/C_0)^2)
;; C_0 is the maximum capacity of the capacitor
;; tau is 4.8/T => T = average recharge rate
(defun cap-recharge (ship-cap-recharge ship-cap-total
                     skill-level-energy-systems-operation)
  "Returns the peak recharge rate in GJ/s."
  ;(* 4.8 (/ ship-cap-total
  ;(* 5000 (/ ship-cap-total
  ;           (* ship-cap-recharge
  ;              (- 1 (* 0.05 skill-level-energy-systems-operation))))
  ;   (sqrt (- (expt 0.70 2) (expt 0.70 4)))))
  (* 2499.5 (/ ship-cap-total
             (* ship-cap-recharge
                (- 1 (* 0.05 skill-level-energy-systems-operation))))))


(defun cap-total (ship-cap-capacity skill-level-energy-management)
  (* (+ 1 (* 0.05 skill-level-energy-management))
     ship-cap-capacity))


(defun cpu-total (ship-cpu &key (skill-level-cpu-management 0)
                                cpu-multipliers)
  (format t ">>> ~S~%" cpu-multipliers)
  (let ((multiplier (loop with multiplier = 1
                          for cp in cpu-multipliers
                          do (setf multiplier (* multiplier cp))
                          finally (return multiplier))))
    (* (+ 1 (* 0.05 skill-level-cpu-management))
       ship-cpu
       multiplier)))


(defun drone-range (skill-level-electronic-warfare-drone-interfacing
                    skill-level-scout-drone)
  (+ 20 (* 3 skill-level-electronic-warfare-drone-interfacing)
        (* 5 skill-level-scout-drone)))


(defun hull-hp (ship-hull-hp skill-level-mechanic)
  (* (+ 1 (* 0.05 skill-level-mechanic))
     ship-hull-hp))


(defun max-locked-targets (ship-max-locked-targets skill-level-targeting
                           skill-level-multitasking)
  (min (+ 2 skill-level-targeting skill-level-multitasking)
       ship-max-locked-targets))


(defun max-targeting-range (ship-max-targeting-range skill-level-target-range)
  (* (+ 1 (* 0.05 skill-level-target-range))
     ship-max-targeting-range))


(defun power-total (ship-power skill-level-engineering)
  (* (+ 1 (* 0.05 skill-level-engineering))
     ship-power))


(defun scan-resolution (ship-scan-resolution skill-level-signature-analysis)
  (* (+ 1 (* 0.05 skill-level-signature-analysis))
     ship-scan-resolution))


(defun shield-hp (ship-shield-hp skill-level-shield-management)
  (* (+ 1 (* 0.05 skill-level-shield-management))
     ship-shield-hp))


(defun speed (ship-max-velocity skill-level-navigation-level)
  (* (+ 1 (* 0.05 skill-level-navigation-level))
     ship-max-velocity))


;;;; Functions

(defun attribute-types (type-id)
  (clsql:query (mkstr "SELECT * FROM dgmAttributeTypes WHERE attributeID IN (SELECT attributeID FROM dgmTypeAttributes WHERE typeID = " type-id ")")))


(defun type-attributes (type-id attribute-id)
  (car (clsql:query (mkstr "SELECT * FROM dgmTypeAttributes WHERE typeID = " type-id " and attributeID = " attribute-id))))


;;;; Test Functions

(defun test-sb2 ()
  (clsql:query "SELECT * FROM dgmAttributeTypes WHERE attributeID IN (SELECT attributeID FROM dgmTypeAttributes WHERE typeID = 1952)"))


(defun test-stiletto ()
  "Saved as \"[CLEVE] Stiletto 01\" in EFT."
  (let ((ship-type-id 11198)
        (modules '(:high 2889 2889 2404
                   :medium 5973 3244 1952 6160
                   :low 2048 1236 3888))
        (rigs '(31328 26929))
        (skills (all-level-x-hash-table 5)))
    (let ((ship-cpu (fourth (type-attributes ship-type-id 48)))
          (skill-level-cpu-management (gethash 3426 skills)))
      (format t "CPU: x / ~,2F~%"
              (cpu-total ship-cpu
                         :skill-level-cpu-management skill-level-cpu-management
                         :cpu-multipliers (loop with result = nil
                                                for item in (append modules
                                                                    rigs)
                                                for cpu-multiplier = (when (numberp item)(fourth (type-attributes item 202)))
                                                for cpu-output-bonus = (when (numberp item)(fourth (type-attributes item 424)))
                                                do (when cpu-multiplier
                                                     (push cpu-multiplier result))
                                                   (when cpu-output-bonus
                                                     (push (1+ (/ cpu-output-bonus 100)) result))
                                                finally (return result)))))
    ))


(defun reload ()
  (load "calculations/src/calculations.lisp"))


(defun quit ()
  (cl-user::exit))


;;;; Main Program

(defun main ()
  )
