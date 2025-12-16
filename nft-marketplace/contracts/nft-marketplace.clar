;; NFT Marketplace - SIP-009 compliant trading with royalties
;; Built for Stacks Builder Challenge by Marcus David

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-listing-exists (err u103))
(define-constant err-insufficient-payment (err u104))

(define-data-var platform-fee-rate uint u250) ;; 2.5%
(define-data-var next-listing-id uint u1)

(define-map listings
  uint
  {
    seller: principal,
    nft-contract: principal,
    token-id: uint,
    price: uint,
    royalty-percent: uint,
    creator: principal,
    active: bool
  }
)

(define-map nft-listings { nft-contract: principal, token-id: uint } uint)

(define-read-only (get-listing (listing-id uint))
  (map-get? listings listing-id)
)

(define-public (list-nft 
    (nft-contract principal)
    (token-id uint)
    (price uint)
    (royalty-percent uint)
    (creator principal))
  (let ((listing-id (var-get next-listing-id)))
    (asserts! (<= royalty-percent u1000) err-unauthorized)
    (map-set listings listing-id {
      seller: tx-sender,
      nft-contract: nft-contract,
      token-id: token-id,
      price: price,
      royalty-percent: royalty-percent,
      creator: creator,
      active: true
    })
    (map-set nft-listings { nft-contract: nft-contract, token-id: token-id } listing-id)
    (var-set next-listing-id (+ listing-id u1))
    (ok listing-id)
  )
)

(define-public (buy-nft (listing-id uint))
  (match (map-get? listings listing-id)
    listing
      (let
        (
          (platform-fee (/ (* (get price listing) (var-get platform-fee-rate)) u10000))
          (royalty (/ (* (get price listing) (get royalty-percent listing)) u10000))
          (seller-amount (- (- (get price listing) platform-fee) royalty))
        )
        (asserts! (get active listing) err-not-found)
        (try! (stx-transfer? (get price listing) tx-sender (as-contract tx-sender)))
        (try! (as-contract (stx-transfer? seller-amount tx-sender (get seller listing))))
        (try! (as-contract (stx-transfer? royalty tx-sender (get creator listing))))
        (try! (as-contract (stx-transfer? platform-fee tx-sender contract-owner)))
        (map-set listings listing-id (merge listing { active: false }))
        (ok true)
      )
    err-not-found
  )
)

(define-public (cancel-listing (listing-id uint))
  (match (map-get? listings listing-id)
    listing
      (begin
        (asserts! (is-eq tx-sender (get seller listing)) err-unauthorized)
        (map-set listings listing-id (merge listing { active: false }))
        (ok true)
      )
    err-not-found
  )
)
