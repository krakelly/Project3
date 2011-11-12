(load "load")
(define wise (instantiate place 'wise))
(define kate (instantiate person 'kate wise))
(can-go Soda 'east wise)
(can-go wise 'west Soda)
(define kirin (instantiate place 'kirin))
(can-go Soda 'north kirin)
(can-go kirin 'south Soda)
(define potstickers (instantiate thing 'potstickers))
(ask Kirin 'appear potstickers)
(define ashley (instantiate person 'ashley telegraph-ave))

(define (whereis name)
  (ask (ask name 'place) 'name))
(define (owner thing)
  (let ((object (ask thing 'possessor)))
  (if (equal? object 'no-one)
      'no-one
      (ask object 'name))))





  
