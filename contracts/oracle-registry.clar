;; Oracle Registry - Decentralized data feeds
;; Built for Stacks Builder Challenge by Marcus David

(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u102))

(define-data-var next-oracle-id uint u1)

(define-map oracles uint { provider: principal, name: (string-ascii 50), active: bool })
(define-map data-feeds { oracle-id: uint, key: (string-ascii 50) } { value: uint, timestamp: uint })

(define-read-only (get-oracle (oracle-id uint))
  (map-get? oracles oracle-id)
)

(define-read-only (get-data (oracle-id uint) (key (string-ascii 50)))
  (map-get? data-feeds { oracle-id: oracle-id, key: key })
)

(define-public (register-oracle (name (string-ascii 50)))
  (let ((oracle-id (var-get next-oracle-id)))
    (map-set oracles oracle-id { provider: tx-sender, name: name, active: true })
    (var-set next-oracle-id (+ oracle-id u1))
    (ok oracle-id)
  )
)

(define-public (submit-data (oracle-id uint) (key (string-ascii 50)) (value uint))
  (match (map-get? oracles oracle-id)
    oracle
      (begin
        (asserts! (is-eq tx-sender (get provider oracle)) err-unauthorized)
        (map-set data-feeds { oracle-id: oracle-id, key: key }
          { value: value, timestamp: stacks-block-height })
        (ok true)
      )
    (err u101)
  )
)
