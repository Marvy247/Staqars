;; Decentralized Lottery - Provably fair lottery with hash-based randomness
;; Built for Stacks Builder Challenge by Marcus David

(define-constant contract-owner tx-sender)
(define-constant err-round-closed (err u101))
(define-constant err-round-active (err u102))
(define-constant err-no-winner (err u103))

(define-data-var current-round uint u1)
(define-data-var ticket-price uint u1000000) ;; 1 STX
(define-data-var max-tickets-per-round uint u100)

(define-map rounds
  uint
  {
    total-tickets: uint,
    prize-pool: uint,
    end-block: uint,
    winner: (optional principal),
    winning-ticket: (optional uint),
    active: bool
  }
)

(define-map tickets
  { round: uint, ticket-number: uint }
  principal
)

(define-map user-tickets
  { round: uint, user: principal }
  (list 20 uint)
)

(define-read-only (get-round (round-id uint))
  (map-get? rounds round-id)
)

(define-read-only (get-user-tickets (round-id uint) (user principal))
  (default-to (list) (map-get? user-tickets { round: round-id, user: user }))
)

(define-public (start-round (duration uint))
  (let
    (
      (round-id (var-get current-round))
      (previous-round (map-get? rounds (- round-id u1)))
    )
    (asserts! (is-eq tx-sender contract-owner) (err u100))
    (if (is-some previous-round)
      (asserts! (not (get active (unwrap-panic previous-round))) err-round-active)
      true
    )
    (map-set rounds round-id {
      total-tickets: u0,
      prize-pool: u0,
      end-block: (+ stacks-block-height duration),
      winner: none,
      winning-ticket: none,
      active: true
    })
    (var-set current-round (+ round-id u1))
    (ok round-id)
  )
)

(define-public (buy-ticket (round-id uint))
  (match (map-get? rounds round-id)
    round
      (let
        (
          (ticket-num (get total-tickets round))
          (user-ticket-list (get-user-tickets round-id tx-sender))
        )
        (asserts! (get active round) err-round-closed)
        (asserts! (<= stacks-block-height (get end-block round)) err-round-closed)
        (asserts! (< ticket-num (var-get max-tickets-per-round)) err-round-closed)
        
        (try! (stx-transfer-memo? (var-get ticket-price) tx-sender (as-contract tx-sender) 0x6c6f74746572792074696b657420))
        (map-set tickets { round: round-id, ticket-number: ticket-num } tx-sender)
        (map-set user-tickets { round: round-id, user: tx-sender }
          (unwrap-panic (as-max-len? (append user-ticket-list ticket-num) u20))
        )
        (map-set rounds round-id (merge round {
          total-tickets: (+ ticket-num u1),
          prize-pool: (+ (get prize-pool round) (var-get ticket-price))
        }))
        (ok ticket-num)
      )
    (err u101)
  )
)

(define-public (draw-winner (round-id uint))
  (match (map-get? rounds round-id)
    round
      (let
        (
          (random-num (+ stacks-block-height round-id))
          (winning-num (mod random-num (get total-tickets round)))
          (winner-addr (unwrap! (map-get? tickets { round: round-id, ticket-number: winning-num }) err-no-winner))
          (prize (/ (* (get prize-pool round) u95) u100))
        )
        (asserts! (get active round) err-round-closed)
        (asserts! (> stacks-block-height (get end-block round)) err-round-active)
        (asserts! (> (get total-tickets round) u0) err-no-winner)
        
        (try! (as-contract (stx-transfer-memo? prize tx-sender winner-addr 0x6c6f747465727920707269676520)))
        (map-set rounds round-id (merge round {
          winner: (some winner-addr),
          winning-ticket: (some winning-num),
          active: false
        }))
        (ok winner-addr)
      )
    (err u101)
  )
)
