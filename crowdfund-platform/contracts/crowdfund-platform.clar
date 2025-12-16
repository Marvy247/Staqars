;; Crowdfund Platform - Milestone-based crowdfunding
;; Built for Stacks Builder Challenge by Marcus David

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-campaign-ended (err u103))
(define-constant err-goal-not-met (err u104))

(define-data-var next-campaign-id uint u1)

(define-map campaigns
  uint
  {
    creator: principal,
    title: (string-ascii 100),
    goal: uint,
    raised: uint,
    end-block: uint,
    withdrawn: bool,
    active: bool
  }
)

(define-map contributions { campaign-id: uint, contributor: principal } uint)

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns campaign-id)
)

(define-public (create-campaign (title (string-ascii 100)) (goal uint) (duration uint))
  (let ((campaign-id (var-get next-campaign-id)))
    (map-set campaigns campaign-id {
      creator: tx-sender,
      title: title,
      goal: goal,
      raised: u0,
      end-block: (+ stacks-block-height duration),
      withdrawn: false,
      active: true
    })
    (var-set next-campaign-id (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (contribute (campaign-id uint) (amount uint))
  (match (map-get? campaigns campaign-id)
    campaign
      (begin
        (asserts! (get active campaign) err-campaign-ended)
        (asserts! (<= stacks-block-height (get end-block campaign)) err-campaign-ended)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set contributions { campaign-id: campaign-id, contributor: tx-sender }
          (+ (default-to u0 (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender })) amount))
        (map-set campaigns campaign-id (merge campaign { raised: (+ (get raised campaign) amount) }))
        (ok true)
      )
    err-not-found
  )
)

(define-public (withdraw-funds (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign
      (begin
        (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
        (asserts! (> stacks-block-height (get end-block campaign)) err-campaign-ended)
        (asserts! (>= (get raised campaign) (get goal campaign)) err-goal-not-met)
        (asserts! (not (get withdrawn campaign)) err-unauthorized)
        (try! (as-contract (stx-transfer? (get raised campaign) tx-sender (get creator campaign))))
        (map-set campaigns campaign-id (merge campaign { withdrawn: true, active: false }))
        (ok true)
      )
    err-not-found
  )
)

(define-public (refund (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign
      (let ((contribution (default-to u0 (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender }))))
        (asserts! (> stacks-block-height (get end-block campaign)) err-campaign-ended)
        (asserts! (< (get raised campaign) (get goal campaign)) err-unauthorized)
        (asserts! (> contribution u0) err-not-found)
        (try! (as-contract (stx-transfer? contribution tx-sender tx-sender)))
        (map-delete contributions { campaign-id: campaign-id, contributor: tx-sender })
        (ok true)
      )
    err-not-found
  )
)
