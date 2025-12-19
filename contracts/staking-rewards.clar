;; Staking Rewards - Stake STX to earn time-based rewards
;; Built for Stacks Builder Challenge by Marcus David

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-already-staking (err u102))
(define-constant err-insufficient-balance (err u103))

(define-data-var reward-rate uint u10) ;; 10% APY basis
(define-data-var total-staked uint u0)
(define-data-var min-stake-amount uint u1000000) ;; 1 STX minimum

(define-map stakes
  principal
  {
    amount: uint,
    start-block: uint,
    last-claim-block: uint
  }
)

(define-read-only (get-stake (staker principal))
  (map-get? stakes staker)
)

(define-read-only (calculate-rewards (staker principal))
  (match (map-get? stakes staker)
    stake-info
      (let
        ((blocks-staked (- stacks-block-height (get last-claim-block stake-info))))
        (ok (/ (* (* (get amount stake-info) (var-get reward-rate)) blocks-staked) u10000000))
      )
    (ok u0)
  )
)

(define-public (stake (amount uint))
  (begin
    (asserts! (>= amount (var-get min-stake-amount)) err-insufficient-balance)
    (asserts! (is-none (map-get? stakes tx-sender)) err-already-staking)
    (try! (stx-transfer-memo? amount tx-sender (as-contract tx-sender) 0x7374616b696e67206465706f736974))
    (map-set stakes tx-sender {
      amount: amount,
      start-block: stacks-block-height,
      last-claim-block: stacks-block-height
    })
    (var-set total-staked (+ (var-get total-staked) amount))
    (ok true)
  )
)

(define-public (unstake)
  (match (map-get? stakes tx-sender)
    stake-info
      (let ((rewards (unwrap! (calculate-rewards tx-sender) err-not-found)))
        (try! (as-contract (stx-transfer-memo? (get amount stake-info) tx-sender tx-sender 0x756e7374616b65207072696e636970616c)))
        (try! (as-contract (stx-transfer-memo? rewards tx-sender tx-sender 0x756e7374616b6520726577617264)))
        (map-delete stakes tx-sender)
        (var-set total-staked (- (var-get total-staked) (get amount stake-info)))
        (ok true)
      )
    err-not-found
  )
)

(define-public (claim-rewards)
  (match (map-get? stakes tx-sender)
    stake-info
      (let ((rewards (unwrap! (calculate-rewards tx-sender) err-not-found)))
        (try! (as-contract (stx-transfer-memo? rewards tx-sender tx-sender 0x726577617264732063616c696d)))
        (map-set stakes tx-sender (merge stake-info { last-claim-block: stacks-block-height }))
        (ok rewards)
      )
    err-not-found
  )
)

(define-public (fund-rewards (amount uint))
  (stx-transfer-memo? amount tx-sender (as-contract tx-sender) 0x7265776172642066756e64696e67)
)
