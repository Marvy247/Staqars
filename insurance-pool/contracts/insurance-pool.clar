;; Insurance Pool - Decentralized insurance
;; Built for Stacks Builder Challenge by Marcus David

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))

(define-data-var pool-balance uint u0)
(define-data-var next-policy-id uint u1)

(define-map policies
  uint
  { holder: principal, coverage: uint, premium: uint, start-block: uint, duration: uint, claimed: bool }
)

(define-map deposits principal uint)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies policy-id)
)

(define-public (deposit-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set deposits tx-sender (+ (default-to u0 (map-get? deposits tx-sender)) amount))
    (var-set pool-balance (+ (var-get pool-balance) amount))
    (ok true)
  )
)

(define-public (create-policy (coverage uint) (duration uint))
  (let
    ((policy-id (var-get next-policy-id))
     (premium (/ coverage u100)))
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (map-set policies policy-id {
      holder: tx-sender,
      coverage: coverage,
      premium: premium,
      start-block: stacks-block-height,
      duration: duration,
      claimed: false
    })
    (var-set next-policy-id (+ policy-id u1))
    (ok policy-id)
  )
)

(define-public (claim (policy-id uint))
  (match (map-get? policies policy-id)
    policy
      (begin
        (asserts! (is-eq tx-sender (get holder policy)) err-unauthorized)
        (asserts! (not (get claimed policy)) err-unauthorized)
        (try! (as-contract (stx-transfer? (get coverage policy) tx-sender (get holder policy))))
        (map-set policies policy-id (merge policy { claimed: true }))
        (ok true)
      )
    err-not-found
  )
)
