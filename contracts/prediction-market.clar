;; Prediction Market - Binary outcome predictions
;; Built for Stacks Builder Challenge by Marcus David

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-market-closed (err u102))

(define-data-var next-market-id uint u1)

(define-map markets
  uint
  {
    question: (string-ascii 200),
    end-block: uint,
    yes-pool: uint,
    no-pool: uint,
    resolved: bool,
    outcome: (optional bool)
  }
)

(define-map positions { market-id: uint, user: principal } { yes-amount: uint, no-amount: uint })

(define-read-only (get-market (market-id uint))
  (map-get? markets market-id)
)

(define-public (create-market (question (string-ascii 200)) (duration uint))
  (let ((market-id (var-get next-market-id)))
    (map-set markets market-id {
      question: question,
      end-block: (+ stacks-block-height duration),
      yes-pool: u0,
      no-pool: u0,
      resolved: false,
      outcome: none
    })
    (var-set next-market-id (+ market-id u1))
    (ok market-id)
  )
)

(define-public (buy-yes (market-id uint) (amount uint))
  (match (map-get? markets market-id)
    market
      (begin
        (asserts! (<= stacks-block-height (get end-block market)) err-market-closed)
        (try! (stx-transfer-memo? amount tx-sender (as-contract tx-sender) 0x70726564696374696f6e20627579))
        (let ((pos (default-to { yes-amount: u0, no-amount: u0 }
                     (map-get? positions { market-id: market-id, user: tx-sender }))))
          (map-set positions { market-id: market-id, user: tx-sender }
            { yes-amount: (+ (get yes-amount pos) amount), no-amount: (get no-amount pos) })
        )
        (map-set markets market-id (merge market { yes-pool: (+ (get yes-pool market) amount) }))
        (ok true)
      )
    err-not-found
  )
)

(define-public (resolve-market (market-id uint) (outcome bool))
  (match (map-get? markets market-id)
    market
      (begin
        (asserts! (is-eq tx-sender contract-owner) (err u102))
        (asserts! (> stacks-block-height (get end-block market)) err-market-closed)
        (map-set markets market-id (merge market { resolved: true, outcome: (some outcome) }))
        (ok true)
      )
    err-not-found
  )
)
