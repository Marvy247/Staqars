;; Multisig Wallet - Multi-signature transaction approval
;; Built for Stacks Builder Challenge by Marcus David

(define-constant err-not-signer (err u101))
(define-constant err-already-confirmed (err u102))
(define-constant err-not-executed (err u103))

(define-data-var threshold uint u2)
(define-data-var next-tx-id uint u1)

(define-map signers principal bool)
(define-map transactions
  uint
  { to: principal, amount: uint, confirmations: uint, executed: bool }
)
(define-map confirmations { tx-id: uint, signer: principal } bool)

(define-read-only (is-signer (user principal))
  (default-to false (map-get? signers user))
)

(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions tx-id)
)

(define-public (add-signer (signer principal))
  (begin
    (asserts! (is-signer tx-sender) err-not-signer)
    (map-set signers signer true)
    (ok true)
  )
)

(define-public (submit-transaction (to principal) (amount uint))
  (let ((tx-id (var-get next-tx-id)))
    (asserts! (is-signer tx-sender) err-not-signer)
    (map-set transactions tx-id {
      to: to,
      amount: amount,
      confirmations: u0,
      executed: false
    })
    (var-set next-tx-id (+ tx-id u1))
    (ok tx-id)
  )
)

(define-public (confirm-transaction (tx-id uint))
  (match (map-get? transactions tx-id)
    tx
      (begin
        (asserts! (is-signer tx-sender) err-not-signer)
        (asserts! (is-none (map-get? confirmations { tx-id: tx-id, signer: tx-sender })) err-already-confirmed)
        (map-set confirmations { tx-id: tx-id, signer: tx-sender } true)
        (map-set transactions tx-id (merge tx { confirmations: (+ (get confirmations tx) u1) }))
        (ok true)
      )
    (err u101)
  )
)

(define-public (execute-transaction (tx-id uint))
  (match (map-get? transactions tx-id)
    tx
      (begin
        (asserts! (>= (get confirmations tx) (var-get threshold)) err-not-executed)
        (asserts! (not (get executed tx)) err-already-confirmed)
        (try! (as-contract (stx-transfer? (get amount tx) tx-sender (get to tx))))
        (map-set transactions tx-id (merge tx { executed: true }))
        (ok true)
      )
    (err u101)
  )
)

(define-public (deposit (amount uint))
  (stx-transfer? amount tx-sender (as-contract tx-sender))
)
