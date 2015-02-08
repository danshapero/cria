
(use-modules (rnrs))

(define (string->code string)
  (read (open-input-string string)))

(define (file->code filename)
  (read (open-input-file filename)))

(define (main args)
  (let ([input-filename (cadr args)]
        [output-filename (caddr args)])
    (begin
      (if (file-exists? output-filename)
          (delete-file output-filename)
          #t)
      (let ([code (file->code input-filename)]
            [oport (open-output-file output-filename)])
        (display "define i32 @main(i32 %argc, i8** %argv) {\n" oport)
        (display "  ret i32 0\n" oport)
        (display "}\n" oport)
        (close-output-port oport)))))

