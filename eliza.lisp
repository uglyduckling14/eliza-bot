;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-


;;; ==============================

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defun read-line-no-punct ()
  "Read an input line ignoring punctuation."
  (read-from-string
    (concatenate 'string "(" (substitute-if #\space #'punctuation-p
                                            (read-line))
                 ")")))

(defun punctuation-p (char) (find char ".;:`!?#-()\\\""))

;;; ==============================

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa and so on."
  (sublis '((i . you) (you . i) (me . you) (am . are) (my . your) (your . my))
          words))

(defparameter *good-byes* '((bye)
			    (see you)
			    (see you later)
			    (so long)))

(defun eliza ()
  "Respond to user input using pattern matching rules. Asks for age and sex initially."
  (let ((age nil)
        (sex nil))
    ;; Ask for age
    (format t "Welcome to ELIZA your new symptom-checker! If this is an emergency STOP and call 911~%")
    (loop
     (format t "Please enter your age ")
     (let ((input (read-line)))
       (handler-case
           (progn
             (setf age (parse-integer input :junk-allowed nil))
             (when (and (integerp age) (> age 0))
               (format t "Thank you! You entered age ~A~%" age)
               (return)))
         (parse-error (e)
           (format t "Invalid input. Please enter a valid positive number~%")))))

    ;; Ask for sex
    (loop
      (format t "Please enter your sex (male female or other) ~%")
      (setf sex (string-trim '(#\Space #\Tab) (read-line)))
      (if (member sex '("male" "female" "other") :test #'string-equal)
          (progn
            (format t "Thank you! You entered sex ~A~%" sex)
            (return)) ;; Exit loop if the input is valid
        (format t "Invalid input. Please enter male female or other~%")))


    ;; Enter main ELIZA loop
    (catch :end-eliza-loop
      (loop
       (print 'eliza>)
       (let* ((input (read-line-no-punct))
              (response (flatten (use-eliza-rules input))))
         (if (or (member input *good-byes* :test #'equalp)
                 (member response *good-byes* :test #'equalp))
             (progn
               (print-with-spaces response)
               (throw :end-eliza-loop (values)))
           (print-with-spaces response)))))))

(defun print-with-spaces (list)
  (mapc #'(lambda (x) (prin1 x) (princ " ")) list))

(defun print-with-spaces (list)
  (format t "~{~a ~}" list))

;;; ==============================
(defparameter *eliza-rules*
  '(
    ;;; rule 1 - Fever
    (((?* ?x) fever (?* ?y))  
     (How high is your fever?)
     (Have you had this fever for long?)
     (Do you feel any chills or sweating?))

    ;;; rule 2 - Cough
    (((?* ?x) cough (?* ?y))  
     (How long have you been coughing?)
     (Is the cough dry or productive?)
     (Do you have any pain or discomfort when coughing?))

    ;;; rule 3 - Pain
    (((?* ?x) pain (?* ?y))  
     (Where is the pain located?)
     (On a scale from 1 to 10 how intense is the pain?)
     (Have you experienced this pain before?))

    ;;; rule 4 - Shortness of breath
    (((?* ?x) shortness of breath (?* ?y))  
     (When did you first notice shortness of breath?)
     (Are you experiencing difficulty breathing even at rest?)
     (Do you have any chest pain or tightness?))

    ;;; rule 5 - Headache
    (((?* ?x) headache (?* ?y))  
     (Where is the headache located?)
     (Is it a sharp or dull headache?)
     (Do you feel sensitive to light or sound?))

    ;;; rule 6 - Nausea
    (((?* ?x) nausea (?* ?y))  
     (Do you feel like vomiting?)
     (Have you had nausea for more than a day?)
     (Is there anything specific that triggers the nausea?))

    ;;; rule 7 - Vomiting
    (((?* ?x) vomiting (?* ?y))  
     (How many times have you vomited?)
     (Do you have any stomach pain or discomfort?)
     (What color is the vomit?))

    ;;; rule 8 - Fatigue
    (((?* ?x) fatigue (?* ?y))  
     (How long have you been feeling fatigued?)
     (Do you feel exhausted even after resting?)
     (Have you experienced any other symptoms along with fatigue?))

    ;;; rule 9 - Loss of appetite
    (((?* ?x) loss of appetite (?* ?y))  
     (Have you noticed a decrease in appetite for a long period of time?)
     (Are you experiencing any nausea or vomiting along with this?)
     (Do you have any changes in taste or smell?))

    ;;; rule 10 - Sore throat
    (((?* ?x) sore throat (?* ?y))  
     (Is your sore throat accompanied by any difficulty swallowing?)
     (Have you noticed any swelling in your neck?)
     (Do you have any other symptoms like fever or cough?))

    ;;; rule 11 - Diarrhea
    (((?* ?x) diarrhea (?* ?y))  
     (How many times have you had diarrhea today?)
     (Is the diarrhea watery or does it contain blood?)
     (Are you experiencing any abdominal cramping or bloating?))

    ;;; rule 12 - Dizziness
    (((?* ?x) dizziness (?* ?y))  
     (Do you feel lightheaded or like you might faint?)
     (Are you experiencing dizziness when standing up or sitting down?)
     (Have you had any recent changes in your vision or hearing?))

    ;;; rule 13 - Swelling
    (((?* ?x) swelling (?* ?y))  
     (Where is the swelling located?)
     (Has the swelling worsened or improved over time?)
     (Do you have any pain or tenderness at the site of the swelling?))

    ;;; rule 14 - Rashes
    (((?* ?x) rash (?* ?y))  
     (Where on your body is the rash located?)
     (Do you have any itching or burning sensations?)
     (Has the rash spread or changed in appearance?))

    ;;; rule 15 - Chills
    (((?* ?x) chills (?* ?y))  
     (Are you feeling cold and shaking uncontrollably?)
     (Do you have a fever along with the chills?)
     (Is there anything that helps you warm up?))

    ;;; rule 16 - Sweating
    (((?* ?x) sweating (?* ?y))  
     (Are you sweating more than usual?)
     (Is it happening at specific times like during the night?)
     (Do you feel hot or feverish?))

    ;;; rule 17 - Back pain
    (((?* ?x) back pain (?* ?y))  
     (Where exactly is the back pain located?)
     (Does the pain radiate to other parts of your body?)
     (Do you feel any numbness or tingling in your legs?))

    ;;; rule 18 - Skin changes
    (((?* ?x) skin (?* ?y))  
     (Have you noticed any new spots moles or changes in your skin?)
     (Have the spots changed in size color or shape?)
     (Have you had any history of skin cancer?))

    ;;; rule 19 - Muscle aches
    (((?* ?x) muscle aches (?* ?y))  
     (Where are you feeling muscle pain?)
     (Have you been physically active lately?)
     (Is the pain constant or does it come and go?))

    ;;; rule 20 - Urination changes
    (((?* ?x) urination (?* ?y))  
     (Have you noticed any changes in how often you urinate?)
     (Is there any pain or burning during urination?)
     (Have you noticed any blood or unusual color in your urine?))

    ;;; new rule - asking for more information
    (((?* ?x) symptom (?* ?y))  
     (Can you describe your symptom more specifically?)
     (When did you first notice this symptom?)
     (How severe is the symptom on a scale from 1 to 10?))

    ;;; rule for ending conversation
    (((?* ?x) bye (?* ?y)) 
     (Take care. Stay healthy! If you have time please fill out this survey "https://usu.co1.qualtrics.com/jfe/form/SV_5mu22ytm9CQYpSu")
     (Goodbye. Feel better soon!))
    
    (((?* ?x) help (?* ?y))
     (To end the conversation say bye. Otherwise try using common keywords like pain or dizziness. In emergencies call 911!))

    (((?* ?x))               
     (I am not sure I understand you fully try using keywords like fever or pain)
     (Can you rephrase what you are describing in another way?) (Give me more details))
  )
)


