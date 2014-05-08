(load "compiler.ss")
(driver
  '(begin
     (set! rcx 1)
     (set! rbx 2)
     (set! rbx (+ rbx rcx))
     (set! rcx (- rcx 5))
     (set! rax rcx)
     (set! rax (* rax rbx))))
(exit)
