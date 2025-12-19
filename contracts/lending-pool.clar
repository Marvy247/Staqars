;; Lending Pool - P2P lending with collateral
;; Built for Stacks Builder Challenge by Marcus David

(define-constant err-insufficient-collateral (err u101))
(define-constant err-not-found (err u102))

(define-data-var total-deposits uint u0)
(define-data-var total-borrowed uint u0)
(define-data-var collateral-ratio uint u150)

(define-map deposits principal uint)
(define-map loans principal { borrowed: uint, collateral: uint, start-block: uint })

(define-read-only (get-deposit (user principal))
  (default-to u0 (map-get? deposits user))
)

(define-read-only (get-loan (user principal))
  (map-get? loans user)
)

(define-public (deposit (amount uint))
  (begin
    (try! (stx-transfer-memo? amount tx-sender (as-contract tx-sender) 0x6c656e64696e67206465706f736974))
    (map-set deposits tx-sender (+ (get-deposit tx-sender) amount))
    (var-set total-deposits (+ (var-get total-deposits) amount))
    (ok true)
  )
)

(define-public (borrow (amount uint) (collateral uint))
  (let ((required-collateral (/ (* amount (var-get collateral-ratio)) u100)))
    (asserts! (>= collateral required-collateral) err-insufficient-collateral)
    (try! (stx-transfer-memo? collateral tx-sender (as-contract tx-sender) 0x636f6c6c61746572616c206465706f736974))
    (try! (as-contract (stx-transfer-memo? amount tx-sender tx-sender 0x6c6f616e20646973627572736d656e74)))
    (map-set loans tx-sender {
      borrowed: amount,
      collateral: collateral,
      start-block: stacks-block-height
    })
    (var-set total-borrowed (+ (var-get total-borrowed) amount))
    (ok true)
  )
)

(define-public (repay)
  (match (map-get? loans tx-sender)
    loan
      (let ((repay-amount (+ (get borrowed loan) (/ (get borrowed loan) u100))))
        (try! (stx-transfer-memo? repay-amount tx-sender (as-contract tx-sender) 0x6c6f616e207265706179))
        (try! (as-contract (stx-transfer-memo? (get collateral loan) tx-sender tx-sender 0x636f6c6c61746572616c2072657475726e)))
        (map-delete loans tx-sender)
        (var-set total-borrowed (- (var-get total-borrowed) (get borrowed loan)))
        (ok true)
      )
    err-not-found
  )
)

(define-public (withdraw (amount uint))
  (let ((balance (get-deposit tx-sender)))
    (asserts! (>= balance amount) err-not-found)
    (try! (as-contract (stx-transfer-memo? amount tx-sender tx-sender 0x6465706f73697420776974686472617761)))
    (map-set deposits tx-sender (- balance amount))
    (var-set total-deposits (- (var-get total-deposits) amount))
    (ok true)
  )
)
