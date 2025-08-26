;; BreedingGame Protocol
;; A collectible creature breeding game with genetic algorithms and marketplace trading
;; Implements creature breeding mechanics and marketplace functionality

;; Define the NFT for creatures
(define-non-fungible-token creature uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-creature-owner (err u101))
(define-constant err-creature-not-found (err u102))
(define-constant err-invalid-price (err u103))
(define-constant err-insufficient-payment (err u104))
(define-constant err-breeding-cooldown (err u105))
(define-constant err-invalid-creature-id (err u106))

;; Data variables
(define-data-var next-creature-id uint u1)
(define-data-var breeding-fee uint u1000000) ;; 1 STX in microSTX

;; Creature data structure
(define-map creatures uint {
  genes: uint,           ;; Genetic code represented as uint
  generation: uint,      ;; Generation number (0 for genesis)
  birth-time: uint,      ;; Block height when created
  last-breed-time: uint, ;; Last breeding timestamp
  parent1: (optional uint), ;; Parent 1 ID
  parent2: (optional uint)  ;; Parent 2 ID
})

;; Marketplace listings
(define-map marketplace-listings uint {
  seller: principal,
  price: uint,
  listed-at: uint
})

;; Breeding cooldown period (in blocks)
(define-constant breeding-cooldown-blocks u144) ;; ~24 hours

;; Function 1: Breed two creatures to create offspring
(define-public (breed-creatures (parent1-id uint) (parent2-id uint))
  (let (
    (parent1-data (unwrap! (map-get? creatures parent1-id) err-creature-not-found))
    (parent2-data (unwrap! (map-get? creatures parent2-id) err-creature-not-found))
    (current-block block-height)
    (new-creature-id (var-get next-creature-id))
    (parent1-owner (unwrap! (nft-get-owner? creature parent1-id) err-creature-not-found))
    (parent2-owner (unwrap! (nft-get-owner? creature parent2-id) err-creature-not-found))
  )
  (begin
    ;; Check ownership of both parents
    (asserts! (or (is-eq tx-sender parent1-owner) (is-eq tx-sender parent2-owner)) err-not-creature-owner)
    
    ;; Check breeding cooldown for parent1
    (asserts! (>= current-block (+ (get last-breed-time parent1-data) breeding-cooldown-blocks)) err-breeding-cooldown)
    
    ;; Check breeding cooldown for parent2
    (asserts! (>= current-block (+ (get last-breed-time parent2-data) breeding-cooldown-blocks)) err-breeding-cooldown)
    
    ;; Pay breeding fee
    (try! (stx-transfer? (var-get breeding-fee) tx-sender contract-owner))
    
    ;; Generate offspring genes using simple genetic algorithm
    ;; Combines parent genes with some randomization
    (let (
      (offspring-genes (+ 
        (/ (+ (get genes parent1-data) (get genes parent2-data)) u2)
        (mod (+ current-block new-creature-id) u1000000))) ;; Add some randomness
      (max-generation (max (get generation parent1-data) (get generation parent2-data)))
      (offspring-generation (+ max-generation u1))
    )
    
    ;; Create new creature
    (try! (nft-mint? creature new-creature-id tx-sender))
    
    ;; Store creature data
    (map-set creatures new-creature-id {
      genes: offspring-genes,
      generation: offspring-generation,
      birth-time: current-block,
      last-breed-time: u0,
      parent1: (some parent1-id),
      parent2: (some parent2-id)
    })
    
    ;; Update parent breeding times
    (map-set creatures parent1-id 
      (merge parent1-data {last-breed-time: current-block}))
    (map-set creatures parent2-id 
      (merge parent2-data {last-breed-time: current-block}))
    
    ;; Increment next creature ID
    (var-set next-creature-id (+ new-creature-id u1))
    
    (ok new-creature-id)))))

;; Function 2: List creature on marketplace or buy listed creature
(define-public (marketplace-transaction (creature-id uint) (action (string-ascii 10)) (price uint))
  (let (
    (creature-owner (unwrap! (nft-get-owner? creature creature-id) err-creature-not-found))
    (listing-data (map-get? marketplace-listings creature-id))
    (current-block block-height)
  )
  (if (is-eq action "list")
    ;; List creature for sale
    (begin
      (asserts! (is-eq tx-sender creature-owner) err-not-creature-owner)
      (asserts! (> price u0) err-invalid-price)
      (map-set marketplace-listings creature-id {
        seller: tx-sender,
        price: price,
        listed-at: current-block
      })
      (ok true))
    
    ;; Buy creature from marketplace
    (if (is-eq action "buy")
      (let (
        (listing (unwrap! listing-data err-creature-not-found))
        (seller (get seller listing))
        (listing-price (get price listing))
      )
      (begin
        (asserts! (>= price listing-price) err-insufficient-payment)
        
        ;; Transfer payment to seller
        (try! (stx-transfer? listing-price tx-sender seller))
        
        ;; Transfer creature to buyer
        (try! (nft-transfer? creature creature-id seller tx-sender))
        
        ;; Remove from marketplace
        (map-delete marketplace-listings creature-id)
        
        (ok true)))
      
      ;; Unlist creature
      (if (is-eq action "unlist")
        (begin
          (asserts! (is-eq tx-sender creature-owner) err-not-creature-owner)
          (map-delete marketplace-listings creature-id)
          (ok true))
        (err u999))))) ;; Invalid action

;; Read-only functions

;; Get creature data
(define-read-only (get-creature (creature-id uint))
  (map-get? creatures creature-id))

;; Get marketplace listing
(define-read-only (get-marketplace-listing (creature-id uint))
  (map-get? marketplace-listings creature-id))

;; Get creature owner
(define-read-only (get-creature-owner (creature-id uint))
  (nft-get-owner? creature creature-id))

;; Get breeding fee
(define-read-only (get-breeding-fee)
  (ok (var-get breeding-fee)))

;; Get next creature ID
(define-read-only (get-next-creature-id)
  (ok (var-get next-creature-id)))

;; Check if creature can breed (cooldown check)
(define-read-only (can-breed (creature-id uint))
  (match (map-get? creatures creature-id)
    creature-data 
      (ok (>= block-height (+ (get last-breed-time creature-data) breeding-cooldown-blocks)))
    (ok false)))

;; Admin function to create genesis creatures (only owner)
(define-public (create-genesis-creature (recipient principal) (genes uint))
  (let (
    (new-creature-id (var-get next-creature-id))
  )
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Mint genesis creature
    (try! (nft-mint? creature new-creature-id recipient))
    
    ;; Store creature data
    (map-set creatures new-creature-id {
      genes: genes,
      generation: u0,
      birth-time: block-height,
      last-breed-time: u0,
      parent1: none,
      parent2: none
    })
    
    ;; Increment next creature ID
    (var-set next-creature-id (+ new-creature-id u1))
    
    (ok new-creature-id))))