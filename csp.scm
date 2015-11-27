(define-module (csp csp)
  #:use-module (srfi srfi-9)
  #:export (make-channel
            ->
            <-
            go))

;; jeden mutex i 4 condition variables хД
(define-record-type <channel>
  (%make-channel buffer recv-ready? send-ready? mutex recv-ready-cv send-ready-cv recv-complete-cv send-complete-cv)
  channel?
  (buffer channel-buffer set-channel-buffer!)
  (recv-ready? channel-recv-ready? set-channel-recv-ready!)
  (send-ready? channel-send-ready? set-channel-send-ready!)
  (mutex channel-mutex)
  (recv-ready-cv channel-recv-ready-cv)
  (send-ready-cv channel-send-ready-cv)
  (recv-complete-cv channel-recv-complete-cv)
  (send-complete-cv channel-send-complete-cv))

(define (make-channel)
    (%make-channel 'starting-state #f #f (make-mutex) (make-condition-variable) (make-condition-variable) (make-condition-variable) (make-condition-variable)))

;; skopiowane z ice-9/threads. Najsłabsze ogniwo- korutyny są wątkami systemowymi
(define-syntax-rule (go e0 e1 ...)
  (call-with-new-thread
   (lambda () e0 e1 ...)
   %thread-handler))
;; też skopiowane z ice-9/threads
(define-syntax-rule (with-mutex m e0 e1 ...)
  (let ((x m))
    (dynamic-wind
      (lambda () (lock-mutex x))
      (lambda () (begin e0 e1 ...))
      (lambda () (unlock-mutex x)))))

(define (-> chan val)
  (with-mutex (channel-mutex chan)
    (if (channel-send-ready? chan)
        (wait-condition-variable (channel-recv-complete-cv chan) (channel-mutex chan)))
    (set-channel-buffer! chan val)
    (if (channel-recv-ready? chan)
        (signal-condition-variable (channel-send-ready-cv chan))
        (begin
          (set-channel-send-ready! chan #t)
          (wait-condition-variable (channel-recv-ready-cv chan) (channel-mutex chan))
          (set-channel-send-ready! chan #f)))
    (signal-condition-variable (channel-send-complete-cv chan))
    val))

(define (<- chan)
  (with-mutex (channel-mutex chan)
    (if (channel-recv-ready? chan)
        (wait-condition-variable (channel-send-complete-cv chan) (channel-mutex chan)))
    (if (channel-send-ready? chan)
        (signal-condition-variable (channel-recv-ready-cv chan))
        (begin
          (set-channel-recv-ready! chan #t)
          (wait-condition-variable (channel-send-ready-cv chan) (channel-mutex chan))
          (set-channel-recv-ready! chan #f)))
    (signal-condition-variable (channel-recv-complete-cv chan))
    (channel-buffer chan)))


;; DO ZROBIENIA: porządny select obsługujący przyjmowanie i wysyłanie wiadomości
(define-syntax select
  (lambda (x)
    (syntax-case x (default <- ->)
      ((_ (name (<- chan) e0 e1 ...)) #'(let ((name (<- chan))) e0 e1 ...))
      ((_ (default ex0 ex1 ...)) #'(begin ex0 ex1 ...)))))

