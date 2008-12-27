;;; Delimited readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These procedures ran their inner I/O loop in a C primitive in
;;; earlier versions of scsh. In a multi-threaded environment this
;;; causes lots of trouble in case the operation would
;;; block. Therefore the current implementation runs in Scheme but
;;; operates directly on the buffer of the port for speed. This also
;;; allows us to implement the push-back behaviour without a peek/read
;;; pair.
;;;

;;; (read-delimited delims [port delim-action])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns a string or the EOF object. DELIM-ACTION determines what to do
;;; with the terminating delimiter:
;;; - PEEK
;;;   Leave it in the input stream for later reading.
;;; - TRIM (the default)
;;;   Drop it on the floor.
;;; - CONCAT
;;;   Append it to the returned string.
;;; - SPLIT
;;;   Return it as a second return value.
;;;

(define (read-delimited delims . args)
  (let-optionals* args ((port         (current-input-port))
                        (delim-action 'trim))
    (let* ((delims (x->char-set delims))
           (eof? #f)
           (split #f)
           (result
            (string-unfold
             (lambda (c)
               (or (eof-object? c) (char-set-contains? delims c)))
             values
             (lambda (seed)
               (read-char port)
               (peek-char port))
             (peek-char port)
             ""
             (lambda (c)
               (if (eof-object? c)
                   (set! eof? #t))
               (case delim-action
                 ((peek)   "")
                 ((concat) (read-char port) (if eof? "" (string c)))
                 ((split)  (read-char port) (set! split c) "")
                 (else     (read-char port) ""))))))
      (if (and eof? (zero? (string-length result)))
          (set! result (eof-object)))
      (if split (values result split) result))))

;;; (read-line [port delim-action])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read in a line of data. Input is terminated by either a newline or EOF.
;;; The newline is trimmed from the string by default.

(define charset:newline (char-set #\newline))

(define (read-line . rest) (apply read-delimited charset:newline rest))


;;; (read-paragraph [port handle-delim])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define blank-line-regexp (make-regexp "^[[:white:]]*\\n$"))

(define (read-paragraph . args)
  (let-optionals* args ((port         (current-input-port))
                        (handle-delim 'trim))
    ;; First, skip all blank lines.
    (let lp ()
      (let ((line (read-line port 'concat)))
	(cond ((eof-object? line)
	       (if (eq? handle-delim 'split) (values line line) line))

	      ((regexp-match blank-line-regexp line #f #t #t) (lp))
              
	      ;; Then, read in non-blank lines.
	      (else
	       (let lp ((lines (list line)))
		 (let ((line (read-line port 'concat)))
		   (if (and (string? line)
			    (not (regexp-match blank-line-regexp line #f #t #t)))

		       (lp (cons line lines))

		       ;; Return the paragraph
		       (let ((lines->str
                              (lambda (lns) (apply string-append (reverse lns)))))
			 (case handle-delim
			   ((trim) (lines->str lines))

			   ((concat)
			    (lines->str
                             (if (eof-object? line) lines (cons line lines))))

			   ((split)
			    (values (lines->str lines) line))
                           
			   (else (error "Illegal HANDLE-DELIM parameter to READ-PARAGRAPH")))))))))))))

;; arch-tag: 41794555-4aa5-444d-a16a-86e7187ed7a6
