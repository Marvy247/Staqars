;; Reputation System - On-chain reputation scoring
;; Built for Stacks Builder Challenge by Marcus David

(define-constant err-unauthorized (err u102))
(define-constant err-self-endorse (err u103))

(define-map reputations principal { score: uint, endorsements: uint, last-update: uint })
(define-map endorsements { endorser: principal, endorsed: principal } bool)

(define-read-only (get-reputation (user principal))
  (default-to { score: u0, endorsements: u0, last-update: u0 }
    (map-get? reputations user))
)

(define-public (endorse (user principal))
  (begin
    (asserts! (not (is-eq tx-sender user)) err-self-endorse)
    (asserts! (is-none (map-get? endorsements { endorser: tx-sender, endorsed: user })) err-unauthorized)
    (map-set endorsements { endorser: tx-sender, endorsed: user } true)
    (let ((rep (get-reputation user)))
      (map-set reputations user {
        score: (+ (get score rep) u10),
        endorsements: (+ (get endorsements rep) u1),
        last-update: stacks-block-height
      })
    )
    (ok true)
  )
)

(define-public (report (user principal))
  (let ((rep (get-reputation user)))
    (map-set reputations user {
      score: (if (> (get score rep) u5) (- (get score rep) u5) u0),
      endorsements: (get endorsements rep),
      last-update: stacks-block-height
    })
    (ok true)
  )
)
