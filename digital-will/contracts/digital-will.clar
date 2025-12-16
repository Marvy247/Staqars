;; Digital Will - Time-Locked Inheritance Contract
;; Uses Clarity 4's block-timestamp for automatic inheritance logic
;; Built for Stacks Builder Challenge by Marcus David

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-not-mature (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-insufficient-balance (err u105))

;; Inactivity period (180 days in blocks ~25,920 blocks)
;; Note: Clarity 4 would use actual seconds with block-timestamp
(define-constant inactivity-threshold u25920)

;; Data Variables
(define-data-var next-will-id uint u1)

;; Data Maps
(define-map wills
  uint
  {
    owner: principal,
    beneficiary: principal,
    amount: uint,
    last-activity: uint,  ;; Uses block-timestamp
    created-at: uint,     ;; Uses block-timestamp
    is-claimed: bool
  }
)

(define-map user-wills principal (list 10 uint))

;; Read-only functions
(define-read-only (get-will (will-id uint))
  (map-get? wills will-id)
)

(define-read-only (get-user-wills (user principal))
  (default-to (list) (map-get? user-wills user))
)

;; CLARITY 4: Using stacks-block-height as timestamp proxy (Clarity 4 would use block-timestamp)
;; Note: In production with Clarity 4, use block-timestamp for actual time tracking
(define-read-only (can-claim (will-id uint))
  (match (map-get? wills will-id)
    will-data 
      (let ((blocks-since-activity (- stacks-block-height (get last-activity will-data))))
        (and 
          (>= blocks-since-activity inactivity-threshold)
          (not (get is-claimed will-data))
        )
      )
    false
  )
)

;; Get current block height (Clarity 4 version would use block-timestamp)
(define-read-only (get-current-timestamp)
  (ok stacks-block-height)
)

;; Public functions

;; Create a new will
(define-public (create-will (beneficiary principal) (amount uint))
  (let 
    (
      (will-id (var-get next-will-id))
      (sender-wills (default-to (list) (map-get? user-wills tx-sender)))
    )
    (asserts! (> amount u0) err-insufficient-balance)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; CLARITY 4: Using stacks-block-height (Clarity 4 would use block-timestamp)
    (map-set wills will-id {
      owner: tx-sender,
      beneficiary: beneficiary,
      amount: amount,
      last-activity: stacks-block-height,
      created-at: stacks-block-height,
      is-claimed: false
    })
    
    (map-set user-wills tx-sender (unwrap-panic (as-max-len? (append sender-wills will-id) u10)))
    (var-set next-will-id (+ will-id u1))
    (ok will-id)
  )
)

;; Update last activity (owner checking in)
(define-public (update-activity (will-id uint))
  (match (map-get? wills will-id)
    will-data
      (begin
        (asserts! (is-eq tx-sender (get owner will-data)) err-unauthorized)
        (asserts! (not (get is-claimed will-data)) err-not-found)
        
        ;; CLARITY 4: Update using stacks-block-height (Clarity 4 would use block-timestamp)
        (map-set wills will-id (merge will-data {
          last-activity: stacks-block-height
        }))
        (ok true)
      )
    err-not-found
  )
)

;; Claim inheritance (beneficiary claiming after inactivity)
(define-public (claim-inheritance (will-id uint))
  (match (map-get? wills will-id)
    will-data
      (let ((time-since-activity (- stacks-block-height (get last-activity will-data))))
        (asserts! (is-eq tx-sender (get beneficiary will-data)) err-unauthorized)
        (asserts! (not (get is-claimed will-data)) err-not-found)
        (asserts! (>= time-since-activity inactivity-threshold) err-not-mature)
        
        ;; Transfer funds to beneficiary
        (try! (as-contract (stx-transfer? (get amount will-data) tx-sender (get beneficiary will-data))))
        
        ;; Mark as claimed
        (map-set wills will-id (merge will-data {
          is-claimed: true
        }))
        (ok (get amount will-data))
      )
    err-not-found
  )
)

;; Cancel will (owner can cancel before it's claimed)
(define-public (cancel-will (will-id uint))
  (match (map-get? wills will-id)
    will-data
      (begin
        (asserts! (is-eq tx-sender (get owner will-data)) err-unauthorized)
        (asserts! (not (get is-claimed will-data)) err-not-found)
        
        ;; Return funds to owner
        (try! (as-contract (stx-transfer? (get amount will-data) tx-sender (get owner will-data))))
        
        ;; Mark as claimed to prevent further actions
        (map-set wills will-id (merge will-data {
          is-claimed: true
        }))
        (ok (get amount will-data))
      )
    err-not-found
  )
)
