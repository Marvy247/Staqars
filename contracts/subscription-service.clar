;; Subscription Service - Recurring payment platform
;; Built for Stacks Builder Challenge by Marcus David

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-subscribed (err u103))
(define-constant err-payment-too-early (err u104))

(define-data-var grace-period uint u144) ;; ~1 day in blocks
(define-data-var next-plan-id uint u1)

(define-map plans
  uint
  {
    provider: principal,
    name: (string-ascii 50),
    price: uint,
    billing-period: uint,
    active: bool
  }
)

(define-map subscriptions
  { plan-id: uint, subscriber: principal }
  {
    start-block: uint,
    last-payment-block: uint,
    next-payment-due: uint,
    active: bool
  }
)

(define-read-only (get-plan (plan-id uint))
  (map-get? plans plan-id)
)

(define-read-only (get-subscription (plan-id uint) (subscriber principal))
  (map-get? subscriptions { plan-id: plan-id, subscriber: subscriber })
)

(define-public (create-plan (name (string-ascii 50)) (price uint) (billing-period uint))
  (let ((plan-id (var-get next-plan-id)))
    (map-set plans plan-id {
      provider: tx-sender,
      name: name,
      price: price,
      billing-period: billing-period,
      active: true
    })
    (var-set next-plan-id (+ plan-id u1))
    (ok plan-id)
  )
)

(define-public (subscribe (plan-id uint))
  (match (map-get? plans plan-id)
    plan
      (begin
        (asserts! (get active plan) err-not-found)
        (asserts! (is-none (map-get? subscriptions { plan-id: plan-id, subscriber: tx-sender })) 
                  err-already-subscribed)
        
        (try! (stx-transfer-memo? (get price plan) tx-sender (get provider plan) 0x737562736372697074696f6e207061796d656e74))
        (map-set subscriptions { plan-id: plan-id, subscriber: tx-sender } {
          start-block: stacks-block-height,
          last-payment-block: stacks-block-height,
          next-payment-due: (+ stacks-block-height (get billing-period plan)),
          active: true
        })
        (ok true)
      )
    err-not-found
  )
)

(define-public (renew-subscription (plan-id uint))
  (match (map-get? plans plan-id)
    plan
      (match (map-get? subscriptions { plan-id: plan-id, subscriber: tx-sender })
        subscription
          (begin
            (asserts! (get active subscription) err-not-found)
            (asserts! (>= stacks-block-height (get next-payment-due subscription)) err-payment-too-early)
            
            (try! (stx-transfer-memo? (get price plan) tx-sender (get provider plan) 0x737562736372697074696f6e2072656e6577616c))
            (map-set subscriptions { plan-id: plan-id, subscriber: tx-sender }
              (merge subscription {
                last-payment-block: stacks-block-height,
                next-payment-due: (+ stacks-block-height (get billing-period plan))
              })
            )
            (ok true)
          )
        err-not-found
      )
    err-not-found
  )
)

(define-public (cancel-subscription (plan-id uint))
  (match (map-get? subscriptions { plan-id: plan-id, subscriber: tx-sender })
    subscription
      (begin
        (map-set subscriptions { plan-id: plan-id, subscriber: tx-sender }
          (merge subscription { active: false })
        )
        (ok true)
      )
    err-not-found
  )
)

(define-read-only (is-subscription-active (plan-id uint) (subscriber principal))
  (match (map-get? subscriptions { plan-id: plan-id, subscriber: subscriber })
    subscription
      (ok (and
        (get active subscription)
        (<= stacks-block-height (+ (get next-payment-due subscription) (var-get grace-period)))
      ))
    (ok false)
  )
)
