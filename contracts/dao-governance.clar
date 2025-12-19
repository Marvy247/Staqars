;; DAO Governance Contract
;; Decentralized autonomous organization with proposal voting and treasury management
;; Built for Stacks Builder Challenge by Marcus David

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-proposal-closed (err u104))
(define-constant err-proposal-active (err u105))
(define-constant err-insufficient-votes (err u106))

;; Proposal states
(define-constant state-active u1)
(define-constant state-passed u2)
(define-constant state-rejected u3)
(define-constant state-executed u4)

;; Governance parameters
(define-data-var proposal-threshold uint u1000) ;; Min tokens to create proposal
(define-data-var quorum-percentage uint u20) ;; 20% quorum required
(define-data-var voting-period uint u1440) ;; ~10 days in blocks
(define-data-var next-proposal-id uint u1)

;; Data maps
(define-map proposals
  uint
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    amount: uint,
    recipient: (optional principal),
    start-block: uint,
    end-block: uint,
    yes-votes: uint,
    no-votes: uint,
    state: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

(define-map member-tokens principal uint)
(define-data-var total-tokens uint u0)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-member-tokens (member principal))
  (default-to u0 (map-get? member-tokens member))
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: voter }))
)

;; Public functions
(define-public (mint-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set member-tokens recipient 
      (+ (get-member-tokens recipient) amount))
    (var-set total-tokens (+ (var-get total-tokens) amount))
    (ok true)
  )
)

(define-public (create-proposal 
    (title (string-ascii 100)) 
    (description (string-ascii 500))
    (amount uint)
    (recipient (optional principal)))
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (member-balance (get-member-tokens tx-sender))
    )
    (asserts! (>= member-balance (var-get proposal-threshold)) err-insufficient-votes)
    
    (map-set proposals proposal-id {
      proposer: tx-sender,
      title: title,
      description: description,
      amount: amount,
      recipient: recipient,
      start-block: stacks-block-height,
      end-block: (+ stacks-block-height (var-get voting-period)),
      yes-votes: u0,
      no-votes: u0,
      state: state-active
    })
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (vote-yes bool))
  (match (map-get? proposals proposal-id)
    proposal
      (let
        ((voter-tokens (get-member-tokens tx-sender)))
        (asserts! (> voter-tokens u0) err-unauthorized)
        (asserts! (is-eq (get state proposal) state-active) err-proposal-closed)
        (asserts! (<= stacks-block-height (get end-block proposal)) err-proposal-closed)
        (asserts! (not (has-voted proposal-id tx-sender)) err-already-voted)
        
        (map-set votes 
          { proposal-id: proposal-id, voter: tx-sender }
          { vote: vote-yes, weight: voter-tokens }
        )
        
        (map-set proposals proposal-id
          (if vote-yes
            (merge proposal { yes-votes: (+ (get yes-votes proposal) voter-tokens) })
            (merge proposal { no-votes: (+ (get no-votes proposal) voter-tokens) })
          )
        )
        (ok true)
      )
    err-not-found
  )
)

(define-public (finalize-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
      (let
        (
          (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
          (quorum-needed (/ (* (var-get total-tokens) (var-get quorum-percentage)) u100))
        )
        (asserts! (is-eq (get state proposal) state-active) err-proposal-closed)
        (asserts! (> stacks-block-height (get end-block proposal)) err-proposal-active)
        
        (if (and 
              (>= total-votes quorum-needed)
              (> (get yes-votes proposal) (get no-votes proposal)))
          (begin
            (map-set proposals proposal-id (merge proposal { state: state-passed }))
            (ok true)
          )
          (begin
            (map-set proposals proposal-id (merge proposal { state: state-rejected }))
            (ok false)
          )
        )
      )
    err-not-found
  )
)

(define-public (execute-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
      (begin
        (asserts! (is-eq (get state proposal) state-passed) err-proposal-closed)
        
        (match (get recipient proposal)
          recipient-addr
            (try! (as-contract (stx-transfer-memo? (get amount proposal) tx-sender recipient-addr 0x70726f706f73616c20657865637574696f6e)))
          true
        )
        
        (map-set proposals proposal-id (merge proposal { state: state-executed }))
        (ok true)
      )
    err-not-found
  )
)

(define-public (deposit-treasury (amount uint))
  (stx-transfer-memo? amount tx-sender (as-contract tx-sender) 0x7472656173757279206465706f736974)
)
