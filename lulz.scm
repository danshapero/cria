
(use-modules (rnrs))

(display (command-line))
(display "\n")

(define ofilename (cadr (command-line)))
(define ifilename (caddr (command-line)))

(if (file-exists? ofilename)
    (delete-file ofilename)
    #t)

(define o (open-output-file ofilename))

(define (string->code string)
  (read (open-input-string string)))

(define (file->code filename)
  (read (open-input-file filename)))


(display "define i32 @main(i32 %argc, i8** %argv) #0 {\n" o)
(display "  ret i32 0\n" o)
(display "}\n" o)

(close-output-port o)
