;; Name Registry - Domain name system for Stacks
;; Built for Stacks Builder Challenge by Marcus David

(define-constant err-name-taken (err u101))
(define-constant err-not-owner (err u102))
(define-constant err-expired (err u103))

(define-data-var registration-fee uint u1000000)
(define-data-var renewal-fee uint u500000)

(define-map names
  (string-ascii 48)
  { owner: principal, registered-at: uint, expires-at: uint }
)

(define-read-only (get-name (name (string-ascii 48)))
  (map-get? names name)
)

(define-read-only (is-available (name (string-ascii 48)))
  (match (map-get? names name)
    registration
      (ok (> stacks-block-height (get expires-at registration)))
    (ok true)
  )
)

(define-public (register (name (string-ascii 48)) (duration uint))
  (let ((available (unwrap! (is-available name) err-name-taken)))
    (asserts! available err-name-taken)
    (try! (stx-transfer-memo? (var-get registration-fee) tx-sender (as-contract tx-sender) 0x6e616d6520726567697374726174696f6e))
    (map-set names name {
      owner: tx-sender,
      registered-at: stacks-block-height,
      expires-at: (+ stacks-block-height duration)
    })
    (ok true)
  )
)

(define-public (renew (name (string-ascii 48)) (duration uint))
  (match (map-get? names name)
    registration
      (begin
        (asserts! (is-eq tx-sender (get owner registration)) err-not-owner)
        (try! (stx-transfer-memo? (var-get renewal-fee) tx-sender (as-contract tx-sender) 0x6e616d652072656e6577616c))
        (map-set names name (merge registration {
          expires-at: (+ (get expires-at registration) duration)
        }))
        (ok true)
      )
    (err u101)
  )
)

(define-public (transfer (name (string-ascii 48)) (new-owner principal))
  (match (map-get? names name)
    registration
      (begin
        (asserts! (is-eq tx-sender (get owner registration)) err-not-owner)
        (asserts! (<= stacks-block-height (get expires-at registration)) err-expired)
        (map-set names name (merge registration { owner: new-owner }))
        (ok true)
      )
    (err u101)
  )
)
