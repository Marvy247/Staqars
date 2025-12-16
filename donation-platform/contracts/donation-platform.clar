;; Donation Platform - Transparent Donation Tracking
;; Accept donations with full transparency and impact tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-campaign-ended (err u104))
(define-constant err-goal-not-met (err u105))

;; Data Variables
(define-data-var campaign-nonce uint u0)
(define-data-var donation-nonce uint u0)
(define-data-var platform-fee-percent uint u200) ;; 2%

;; Campaign Status
(define-constant status-active u1)
(define-constant status-completed u2)
(define-constant status-cancelled u3)

;; Data Maps
(define-map campaigns
  { campaign-id: uint }
  {
    organizer: principal,
    title: (string-ascii 128),
    description: (string-ascii 512),
    goal-amount: uint,
    raised-amount: uint,
    donor-count: uint,
    start-height: uint,
    end-height: uint,
    status: uint,
    withdrawn: bool
  }
)

(define-map donations
  { donation-id: uint }
  {
    campaign-id: uint,
    donor: principal,
    amount: uint,
    message: (optional (string-ascii 256)),
    timestamp: uint,
    matched: bool
  }
)

(define-map campaign-milestones
  { campaign-id: uint, milestone-id: uint }
  {
    description: (string-ascii 256),
    amount-needed: uint,
    completed: bool,
    completed-at: (optional uint)
  }
)

(define-map donor-stats
  { donor: principal }
  {
    total-donated: uint,
    campaigns-supported: uint,
    first-donation: uint
  }
)

(define-map organizer-stats
  { organizer: principal }
  {
    campaigns-created: uint,
    total-raised: uint,
    successful-campaigns: uint
  }
)

(define-map matching-pool
  { campaign-id: uint }
  { available: uint, matched: uint }
)

;; Read-Only Functions
(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

(define-read-only (get-donation (donation-id uint))
  (map-get? donations { donation-id: donation-id })
)

(define-read-only (get-milestone (campaign-id uint) (milestone-id uint))
  (map-get? campaign-milestones { campaign-id: campaign-id, milestone-id: milestone-id })
)

(define-read-only (get-donor-stats (donor principal))
  (default-to
    { total-donated: u0, campaigns-supported: u0, first-donation: u0 }
    (map-get? donor-stats { donor: donor })
  )
)

(define-read-only (get-organizer-stats (organizer principal))
  (default-to
    { campaigns-created: u0, total-raised: u0, successful-campaigns: u0 }
    (map-get? organizer-stats { organizer: organizer })
  )
)

(define-read-only (is-campaign-active (campaign-id uint))
  (match (get-campaign campaign-id)
    campaign
      (and
        (is-eq (get status campaign) status-active)
        (< block-height (get end-height campaign))
      )
    false
  )
)

(define-read-only (get-matching-pool (campaign-id uint))
  (default-to
    { available: u0, matched: u0 }
    (map-get? matching-pool { campaign-id: campaign-id })
  )
)

;; Public Functions
(define-public (create-campaign
    (title (string-ascii 128))
    (description (string-ascii 512))
    (goal-amount uint)
    (duration uint)
  )
  (let (
    (campaign-id (+ (var-get campaign-nonce) u1))
    (organizer-info (get-organizer-stats tx-sender))
  )
    (asserts! (> goal-amount u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-amount)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        organizer: tx-sender,
        title: title,
        description: description,
        goal-amount: goal-amount,
        raised-amount: u0,
        donor-count: u0,
        start-height: block-height,
        end-height: (+ block-height duration),
        status: status-active,
        withdrawn: false
      }
    )
    
    (map-set organizer-stats
      { organizer: tx-sender }
      (merge organizer-info {
        campaigns-created: (+ (get campaigns-created organizer-info) u1)
      })
    )
    
    (var-set campaign-nonce campaign-id)
    (ok campaign-id)
  )
)

(define-public (donate (campaign-id uint) (amount uint) (message (optional (string-ascii 256))))
  (let (
    (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    (donation-id (+ (var-get donation-nonce) u1))
    (donor-info (get-donor-stats tx-sender))
    (platform-fee (/ (* amount (var-get platform-fee-percent)) u10000))
    (net-amount (- amount platform-fee))
    (is-first-donation (is-eq (get total-donated donor-info) u0))
  )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (is-campaign-active campaign-id) err-campaign-ended)
    
    (map-set donations
      { donation-id: donation-id }
      {
        campaign-id: campaign-id,
        donor: tx-sender,
        amount: amount,
        message: message,
        timestamp: block-height,
        matched: false
      }
    )
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign {
        raised-amount: (+ (get raised-amount campaign) net-amount),
        donor-count: (if is-first-donation
          (+ (get donor-count campaign) u1)
          (get donor-count campaign)
        )
      })
    )
    
    (map-set donor-stats
      { donor: tx-sender }
      {
        total-donated: (+ (get total-donated donor-info) amount),
        campaigns-supported: (if is-first-donation
          (+ (get campaigns-supported donor-info) u1)
          (get campaigns-supported donor-info)
        ),
        first-donation: (if is-first-donation
          block-height
          (get first-donation donor-info)
        )
      }
    )
    
    (var-set donation-nonce donation-id)
    (ok donation-id)
  )
)

(define-public (withdraw-funds (campaign-id uint))
  (let (
    (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    (organizer-info (get-organizer-stats (get organizer campaign)))
    (goal-met (>= (get raised-amount campaign) (get goal-amount campaign)))
  )
    (asserts! (is-eq tx-sender (get organizer campaign)) err-unauthorized)
    (asserts! (not (get withdrawn campaign)) err-unauthorized)
    (asserts! (or goal-met (>= block-height (get end-height campaign))) err-campaign-ended)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign {
        status: status-completed,
        withdrawn: true
      })
    )
    
    (map-set organizer-stats
      { organizer: (get organizer campaign) }
      (merge organizer-info {
        total-raised: (+ (get total-raised organizer-info) (get raised-amount campaign)),
        successful-campaigns: (if goal-met
          (+ (get successful-campaigns organizer-info) u1)
          (get successful-campaigns organizer-info)
        )
      })
    )
    
    (ok (get raised-amount campaign))
  )
)

(define-public (add-milestone (campaign-id uint) (milestone-id uint) (description (string-ascii 256)) (amount-needed uint))
  (let (
    (campaign (unwrap! (get-campaign campaign-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get organizer campaign)) err-unauthorized)
    
    (map-set campaign-milestones
      { campaign-id: campaign-id, milestone-id: milestone-id }
      {
        description: description,
        amount-needed: amount-needed,
        completed: false,
        completed-at: none
      }
    )
    
    (ok true)
  )
)

(define-public (complete-milestone (campaign-id uint) (milestone-id uint))
  (let (
    (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    (milestone (unwrap! (get-milestone campaign-id milestone-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get organizer campaign)) err-unauthorized)
    (asserts! (>= (get raised-amount campaign) (get amount-needed milestone)) err-goal-not-met)
    
    (map-set campaign-milestones
      { campaign-id: campaign-id, milestone-id: milestone-id }
      (merge milestone {
        completed: true,
        completed-at: (some block-height)
      })
    )
    
    (ok true)
  )
)

(define-public (fund-matching-pool (campaign-id uint) (amount uint))
  (let (
    (pool (get-matching-pool campaign-id))
  )
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set matching-pool
      { campaign-id: campaign-id }
      {
        available: (+ (get available pool) amount),
        matched: (get matched pool)
      }
    )
    
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set platform-fee-percent new-fee)
    (ok true)
  )
)
