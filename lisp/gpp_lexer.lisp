(defconstant keywords '(
  "and" 
  "or" 
  "not" 
  "equal" 
  "less" 
  "nil" 
  "list" 
  "append"
  "concat"
  "set" 
  "def" 
  "for" 
  "if" 
  "exit" 
  "load" 
  "display"
  "true" 
  "false"))

(defconstant operators '(
  "+" 
  "-" 
  "/" 
  "*" 
  "(" 
  ")" 
  ","))

(defconstant whitespaces '(
  #\Space 
  #\Tab 
  #\Return 
  #\Newline))

(defun is-whitespace (word)
  (if (member word whitespaces :test #'string=)
      t
      nil))

(defun is-keyword (word)
  (if (member word keywords :test #'string=)
      t
      nil))
(defun is-semicolon (word)
  (if (string= word ";")
      t
      nil))

(defun is-newline (word)
  (if (string= word #\Newline)
      t
      nil))

(defun is-operator (word)
  (if (member word operators :test #'string=)
      t
      nil))

(defun is-digit (char)
  (and (>= (char-code char) (char-code #\0))
       (<= (char-code char) (char-code #\9))))

(defun is-letter (char)
  (and (>= (char-code char) (char-code #\a))
       (<= (char-code char) (char-code #\z))))

(defun is-valuef (word)
  (let 
    ((length (length word))  ; Get the length of the input word
      (is-valuef t)           ; Initialize a flag to true
      (numb 0))               ; Initialize a counter for 'b' occurrences
    (if (< length 3)            ; If the length is less than 3, it is not a valuef
      (setq is-valuef nil)
      (progn
          ; Check if 'b' is at the beginning, if so, set the flag to false
        (if (char= (char word 0) #\b)
          (setq is-valuef nil)
          (progn
            ; Check if 'b' is at the end, if so, set the flag to false
            (if (char= (char word (- length 1)) #\b)
              (setq is-valuef nil)
              (loop for char across word
                do (if (char= char #\b)
                    (incf numb)
                    (if (and (not (is-digit char))
                             (not (char= char #\b)))
                                (setq is-valuef nil)))))
            (if (not (= numb 1))
                (setq is-valuef nil))))))
    is-valuef))

; if a word starts with a letter and includes only char and ints
; then it is a identifier
(defun is-identifier (word)
  (and (is-letter (char word 0))
       (loop for char across word
             always (or (is-letter char)
                        (is-digit char)))))

(defun is-comment (word)
  ; if the length of the word greater than 1 and the first two chars are semi-colons
  (and (> (length word) 1)
       (string= (string (char word 0)) ";")
       (string= (string (char word 1)) ";")))

(defun print-keyword (word)
  (format t "~a~%" (concatenate 'string "KW_" (string-upcase word))))

(defun print-operator (word)
  (cond
    ((string= word "+") (format t "~a~%" "OP_PLUS"))
    ((string= word "-") (format t "~a~%" "OP_MINUS"))
    ((string= word "/") (format t "~a~%" "OP_DIV"))
    ((string= word "*") (format t "~a~%" "OP_MULT"))
    ((string= word "(") (format t "~a~%" "OP_OP"))
    ((string= word ")") (format t "~a~%" "OP_CP"))
    ((string= word ",") (format t "~a~%" "OP_COMMA"))))

(defun what-is-this (word)
  (cond
    ((is-keyword word) (print-keyword word))
    ((is-operator word) (print-operator word))
    ((is-valuef word) (format t "VALUEF: ~a~%" word))
    ((is-identifier word) (format t "IDENTIFIER: ~a~%" word))
    ((is-comment word) (format t "COMMENT: ~a~%" word))
    (t (format t "SYNTAX_ERROR ~a cannot be tokenized~%" word)))
  )

(defun evaluate-line (line)
  (let ; Define some variables in local scope (length, current-token, pos)
    ((length (length line)) 
      (current-token "")
      (pos 0))
    (loop ; Loop through the line
      (if (>= pos length)
          (progn ; End of line
            (when (not (string= current-token ""))
              (what-is-this current-token))
            (return))
          (let ((char (char line pos))) ; Get the current character
            (cond
              ; Check for whitespace
              ((is-whitespace (string char))
               (when (not (string= current-token ""))
                 (what-is-this current-token))
               (setq current-token "")
               (incf pos)) ; Increment position

              ; Check for operator
              ((is-operator (string char))
                (if (string= current-token "")
                  (progn
                    (what-is-this (string char))
                    (incf pos))
                  (progn
                    (what-is-this current-token)
                    (what-is-this (string char))
                    (setq current-token "")
                    (incf pos))))

              ; If remaining line length is greater than 1 and current char is a semicolon 
              ; and the next char also is a semicolon, then,
              ; iterate through the line from position, concatenate the current-token
              ; until a new line is detected
              ((and (> (- length pos) 1)
                    (string= (string char) ";")
                    (string= (string (char line (+ pos 1))) ";"))
              ; if current-token is filled, print it and reset it
              (when (not (string= current-token ""))
                (what-is-this current-token)
                (setq current-token ""))
              (loop for char across (subseq line pos) do
                (if (is-newline char)
                    (progn
                      (what-is-this current-token)
                      (setq current-token "")
                      (incf pos))
                    (progn
                      (setq current-token (concatenate 'string current-token (string char)))
                      (incf pos)))))
              ; All other characters are part of a token
              (t
               (setq current-token (concatenate 'string current-token (string char)))
               (incf pos))))))))


(defun gppinterpreter (&optional filename)
  ; if not filename, then read from stdin
  (if (equal filename nil)
      (progn
        ; this is not a rule, however to terminate program it we need something
        (format t "~%(Enter 'quit' to exit.)~%")
        (loop
          (format t ">_ ")
          (finish-output)
          (let ((input (read-line)))
            (if (string= input "quit")
                (return)
                (evaluate-line input))))
          (format t "Good bye!~%"))
      (progn
        ; file will be read line by line
        (with-open-file (stream filename)
          (loop for line = (read-line stream nil)
                while line do
                (evaluate-line line))
          (gppinterpreter)))))


(defun main ()
  (if (> (length EXT:*ARGS*) 1)
    (progn
      (format t "Usage: gppinterpreter [filename]~%")
      (quit))
    (progn
      (if (equal EXT:*ARGS* nil)
          (gppinterpreter)
          (gppinterpreter (first EXT:*ARGS*))))))

(main)

