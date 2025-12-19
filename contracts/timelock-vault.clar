;; Timelock Vault - Time-Locked Savings Vault
;; Lock tokens for a specific period to enforce savings discipline

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-still-locked (err u103))
(define-constant err-already-withdrawn (err u104))
(define-constant err-unauthorized (err u105))

;; Data Variables
(define-data-var vault-nonce uint u0)

;; Lock Types
(define-constant lock-type-fixed u1)
(define-constant lock-type-recurring u2)

;; Data Maps
(define-map vaults
  { vault-id: uint }
  {
    owner: principal,
    amount: uint,
    lock-type: uint,
    unlock-height: uint,
    created-at: uint,
    withdrawn: bool,
    auto-extend: bool
  }
)

(define-map user-vaults
  { owner: principal, index: uint }
  { vault-id: uint }
)

(define-map user-vault-count
  { owner: principal }
  { count: uint }
)

(define-map savings-goals
  { owner: principal }
  {
    target-amount: uint,
    current-amount: uint,
    deadline: uint,
    achieved: bool
  }
)

;; Read-Only Functions
(define-read-only (get-vault (vault-id uint))
  (map-get? vaults { vault-id: vault-id })
)

(define-read-only (is-unlocked (vault-id uint))
  (match (get-vault vault-id)
    vault (>= block-height (get unlock-height vault))
    false
  )
)

(define-read-only (get-user-vault-count (owner principal))
  (default-to
    { count: u0 }
    (map-get? user-vault-count { owner: owner })
  )
)

(define-read-only (get-user-vault-id (owner principal) (index uint))
  (map-get? user-vaults { owner: owner, index: index })
)

(define-read-only (get-savings-goal (owner principal))
  (map-get? savings-goals { owner: owner })
)

(define-read-only (time-until-unlock (vault-id uint))
  (match (get-vault vault-id)
    vault 
      (if (>= block-height (get unlock-height vault))
        (ok u0)
        (ok (- (get unlock-height vault) block-height))
      )
    err-not-found
  )
)

;; Public Functions
(define-public (create-vault (amount uint) (lock-duration uint) (lock-type uint) (auto-extend bool))
  (let (
    (vault-id (+ (var-get vault-nonce) u1))
    (unlock-height (+ block-height lock-duration))
    (count-info (get-user-vault-count tx-sender))
    (user-index (get count count-info))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> lock-duration u0) err-invalid-amount)
    
    (map-set vaults
      { vault-id: vault-id }
      {
        owner: tx-sender,
        amount: amount,
        lock-type: lock-type,
        unlock-height: unlock-height,
        created-at: block-height,
        withdrawn: false,
        auto-extend: auto-extend
      }
    )
    
    (map-set user-vaults
      { owner: tx-sender, index: user-index }
      { vault-id: vault-id }
    )
    
    (map-set user-vault-count
      { owner: tx-sender }
      { count: (+ user-index u1) }
    )
    
    (var-set vault-nonce vault-id)
    (ok vault-id)
  )
)

(define-public (withdraw (vault-id uint))
  (let (
    (vault (unwrap! (get-vault vault-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get owner vault)) err-unauthorized)
    (asserts! (>= block-height (get unlock-height vault)) err-still-locked)
    (asserts! (not (get withdrawn vault)) err-already-withdrawn)
    
    (map-set vaults
      { vault-id: vault-id }
      (merge vault { withdrawn: true })
    )
    
    (ok (get amount vault))
  )
)

(define-public (extend-lock (vault-id uint) (additional-duration uint))
  (let (
    (vault (unwrap! (get-vault vault-id) err-not-found))
    (new-unlock-height (+ (get unlock-height vault) additional-duration))
  )
    (asserts! (is-eq tx-sender (get owner vault)) err-unauthorized)
    (asserts! (> additional-duration u0) err-invalid-amount)
    (asserts! (not (get withdrawn vault)) err-already-withdrawn)
    
    (map-set vaults
      { vault-id: vault-id }
      (merge vault { unlock-height: new-unlock-height })
    )
    
    (ok new-unlock-height)
  )
)

(define-public (add-to-vault (vault-id uint) (amount uint))
  (let (
    (vault (unwrap! (get-vault vault-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get owner vault)) err-unauthorized)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (get withdrawn vault)) err-already-withdrawn)
    
    (map-set vaults
      { vault-id: vault-id }
      (merge vault {
        amount: (+ (get amount vault) amount)
      })
    )
    
    (ok true)
  )
)

(define-public (set-savings-goal (target-amount uint) (deadline uint))
  (begin
    (asserts! (> target-amount u0) err-invalid-amount)
    (asserts! (> deadline block-height) err-invalid-amount)
    
    (map-set savings-goals
      { owner: tx-sender }
      {
        target-amount: target-amount,
        current-amount: u0,
        deadline: deadline,
        achieved: false
      }
    )
    
    (ok true)
  )
)

(define-public (update-goal-progress (amount uint))
  (let (
    (goal (unwrap! (get-savings-goal tx-sender) err-not-found))
    (new-amount (+ (get current-amount goal) amount))
    (achieved (>= new-amount (get target-amount goal)))
  )
    (map-set savings-goals
      { owner: tx-sender }
      (merge goal {
        current-amount: new-amount,
        achieved: achieved
      })
    )
    
    (ok achieved)
  )
)

(define-public (emergency-withdraw (vault-id uint))
  (let (
    (vault (unwrap! (get-vault vault-id) err-not-found))
    (penalty (/ (get amount vault) u10))
    (withdraw-amount (- (get amount vault) penalty))
  )
    (asserts! (is-eq tx-sender (get owner vault)) err-unauthorized)
    (asserts! (not (get withdrawn vault)) err-already-withdrawn)
    
    (map-set vaults
      { vault-id: vault-id }
      (merge vault { withdrawn: true })
    )
    
    (ok withdraw-amount)
  )
)
