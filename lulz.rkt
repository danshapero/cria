
#lang racket

(require racket/cmdline)

(define (command-line-arg k)
  (vector-ref (current-command-line-arguments) k))

(define source
  (file->list (command-line-arg 0)))

(define output
  (open-output-file (command-line-arg 1)))

(define (emit . strings)
  (for ([s strings])
    (display s output)
    (display "\n" output)))

(emit "define i32 @main(i32 %argc, i8** %argv) #0 {")
(emit "  ret i32 0")
(emit "}")
(emit "declare i32 @printf(i8*, ...) #1")
