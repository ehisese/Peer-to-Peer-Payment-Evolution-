
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_PAYMENT_NOT_FOUND (err u103))
(define-constant ERR_PAYMENT_ALREADY_COMPLETED (err u104))
(define-constant ERR_PAYMENT_EXPIRED (err u105))
(define-constant ERR_INVALID_RECIPIENT (err u106))
(define-constant ERR_ESCROW_LOCKED (err u107))
(define-constant ERR_DISPUTE_PERIOD_ACTIVE (err u108))
(define-constant ERR_INVALID_SCHEDULE (err u109))
(define-constant ERR_GROUP_FULL (err u110))

;; Time-based constants (using block increments)
(define-constant BLOCKS_PER_DAY u144) ;; Approximate blocks per day
(define-constant BLOCKS_PER_WEEK u1008) ;; Approximate blocks per week
(define-constant DEFAULT_EXPIRY_BLOCKS u1008) ;; Default 1 week expiry

;; ============================================================================
;; DATA STRUCTURES
;; ============================================================================

;; Payment request structure
(define-map payment-requests
  { payment-id: uint }
  {
    sender: principal,
    recipient: principal,
    amount: uint,
    memo: (string-utf8 256),
    created-block: uint,
    expires-block: uint,
    status: (string-ascii 20), ;; "pending", "completed", "expired", "cancelled"
    payment-type: (string-ascii 20) ;; "instant", "scheduled", "escrow"
  }
)
;; Escrow payments with dispute resolution
(define-map escrow-payments
  { payment-id: uint }
  {
    arbiter: principal,
    release-condition: (string-utf8 256),
    dispute-deadline: uint,
    is-disputed: bool,
    dispute-reason: (string-utf8 512),
    funds-locked: uint
  }
)

;; User profiles and reputation system
(define-map user-profiles
  { user: principal }
  {
    total-sent: uint,
    total-received: uint,
    transaction-count: uint,
    reputation-score: uint,
    is-verified: bool,
    registration-block: uint
  }
)
;; Recurring payment schedules
(define-map recurring-payments
  { schedule-id: uint }
  {
    payer: principal,
    recipient: principal,
    amount: uint,
    frequency-blocks: uint,
    next-payment-block: uint,
    total-payments: uint,
    completed-payments: uint,
    is-active: bool,
    created-block: uint
  }
)

;; Payment groups for split payments
(define-map payment-groups
  { group-id: uint }
  {
    creator: principal,
    total-amount: uint,
    paid-amount: uint,
    participant-count: uint,
    deadline-block: uint,
    is-completed: bool,
    created-block: uint
  }
)

;; Group participant contributions
(define-map group-participants
  { group-id: uint, participant: principal }
  {
    amount-owed: uint,
    amount-paid: uint,
    has-paid: bool,
    payment-block: uint
  }
)
;; Transaction history for audit trail
(define-map transaction-history
  { tx-id: uint }
  {
    sender: principal,
    recipient: principal,
    amount: uint,
    transaction-type: (string-ascii 20),
    block-number: uint,
    fee-paid: uint
  }
)

;; ============================================================================
;; DATA VARIABLES
;; ============================================================================

(define-data-var payment-counter uint u0)
(define-data-var schedule-counter uint u0)
(define-data-var group-counter uint u0)
(define-data-var transaction-counter uint u0)
(define-data-var platform-fee-rate uint u25) ;; 0.25% = 25 basis points
(define-data-var min-payment-amount uint u1000000) ;; 1 STX minimum
(define-data-var max-payment-amount uint u1000000000000) ;; 1M STX maximum
(define-data-var contract-nonce uint u0)

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

(define-private (get-current-block)
  ;; Since block-height is not available, we'll use a counter-based approach
  ;; This simulates block progression for testing purposes
  (let ((current-nonce (var-get contract-nonce)))
    (var-set contract-nonce (+ current-nonce u1))
    current-nonce
  )
)

(define-private (get-next-payment-id)
  (let ((current-id (var-get payment-counter)))
    (var-set payment-counter (+ current-id u1))
    current-id
  )
)

(define-private (get-next-schedule-id)
  (let ((current-id (var-get schedule-counter)))
    (var-set schedule-counter (+ current-id u1))
    current-id
  )
)

(define-private (get-next-group-id)
  (let ((current-id (var-get group-counter)))
    (var-set group-counter (+ current-id u1))
    current-id
  )
)

(define-private (get-next-transaction-id)
  (let ((current-id (var-get transaction-counter)))
    (var-set transaction-counter (+ current-id u1))
    current-id
  )
)

(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

(define-private (is-valid-amount (amount uint))
  (and 
    (>= amount (var-get min-payment-amount))
    (<= amount (var-get max-payment-amount))
  )
)

(define-private (create-user-profile-if-not-exists (user principal))
  (let ((current-block (get-current-block)))
    (if (is-none (map-get? user-profiles { user: user }))
      (map-set user-profiles { user: user }
        { 
          total-sent: u0, 
          total-received: u0, 
          transaction-count: u0, 
          reputation-score: u100, 
          is-verified: false, 
          registration-block: current-block 
        }
      )
      false
    )
  )
)

(define-private (update-user-profile (user principal) (amount uint) (is-sender bool))
  (let ((profile (default-to 
                   { total-sent: u0, total-received: u0, transaction-count: u0, 
                     reputation-score: u100, is-verified: false, registration-block: (get-current-block) }
                   (map-get? user-profiles { user: user }))))
    (if is-sender
      ;; Update sender profile
      (map-set user-profiles { user: user }
        (merge profile {
          total-sent: (+ (get total-sent profile) amount),
          transaction-count: (+ (get transaction-count profile) u1)
        })
      )
      ;; Update receiver profile
      (map-set user-profiles { user: user }
        (merge profile {
          total-received: (+ (get total-received profile) amount),
          transaction-count: (+ (get transaction-count profile) u1)
        })
      )
    )
  )
)

(define-private (record-transaction (sender principal) (recipient principal) (amount uint) (tx-type (string-ascii 20)) (fee uint))
  (let ((tx-id (get-next-transaction-id))
        (current-block (get-current-block)))
    (map-set transaction-history { tx-id: tx-id }
      {
        sender: sender,
        recipient: recipient,
        amount: amount,
        transaction-type: tx-type,
        block-number: current-block,
        fee-paid: fee
      }
    )
    tx-id
  )
)

;; ============================================================================
;; BASIC PAYMENT FUNCTIONS
;; ============================================================================

;; Create a payment request
(define-public (create-payment-request 
  (recipient principal) 
  (amount uint) 
  (memo (string-utf8 256))
  (expires-in-blocks uint))
  
  (let ((payment-id (get-next-payment-id))
        (current-block (get-current-block)))
    (asserts! (is-valid-amount amount) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient tx-sender)) ERR_INVALID_RECIPIENT)
    
    ;; Create user profiles if they don't exist
    (create-user-profile-if-not-exists tx-sender)
    (create-user-profile-if-not-exists recipient)
    
    (map-set payment-requests { payment-id: payment-id }
      {
        sender: tx-sender,
        recipient: recipient,
        amount: amount,
        memo: memo,
        created-block: current-block,
        expires-block: (+ current-block expires-in-blocks),
        status: "pending",
        payment-type: "instant"
      }
    )
    
    (ok payment-id)
  )
)

;; Execute instant payment
(define-public (pay-instant (recipient principal) (amount uint) (memo (string-utf8 256)))
  (let ((platform-fee (calculate-platform-fee amount))
        (net-amount (- amount platform-fee)))
    
    (asserts! (is-valid-amount amount) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient tx-sender)) ERR_INVALID_RECIPIENT)
    
    ;; Create user profiles if they don't exist
    (create-user-profile-if-not-exists tx-sender)
    (create-user-profile-if-not-exists recipient)
    
    ;; Transfer net amount to recipient
    (try! (stx-transfer? net-amount tx-sender recipient))
    
    ;; Transfer platform fee to contract owner
    (try! (stx-transfer? platform-fee tx-sender CONTRACT_OWNER))
    
    ;; Update user profiles
    (update-user-profile tx-sender amount true)
    (update-user-profile recipient net-amount false)
    
    ;; Record transaction
    (let ((tx-id (record-transaction tx-sender recipient net-amount "instant" platform-fee)))
      (ok { payment-id: (get-next-payment-id), net-amount: net-amount, transaction-id: tx-id })
    )
  )
)

;; Complete a payment request
(define-public (complete-payment-request (payment-id uint))
  (let ((payment-data (unwrap! (map-get? payment-requests { payment-id: payment-id }) 
                               ERR_PAYMENT_NOT_FOUND))
        (current-block (get-current-block)))
    
    (asserts! (is-eq tx-sender (get recipient payment-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status payment-data) "pending") ERR_PAYMENT_ALREADY_COMPLETED)
    (asserts! (< current-block (get expires-block payment-data)) ERR_PAYMENT_EXPIRED)
    
    (let ((amount (get amount payment-data))
          (sender (get sender payment-data))
          (platform-fee (calculate-platform-fee amount))
          (net-amount (- amount platform-fee)))
      
      ;; Transfer from sender to recipient
      (try! (stx-transfer? net-amount sender tx-sender))
      (try! (stx-transfer? platform-fee sender CONTRACT_OWNER))
      
      ;; Update payment status
      (map-set payment-requests { payment-id: payment-id }
        (merge payment-data { status: "completed" })
      )
      
      ;; Update user profiles
      (update-user-profile sender amount true)
      (update-user-profile tx-sender net-amount false)
      
      ;; Record transaction
      (let ((tx-id (record-transaction sender tx-sender net-amount "request" platform-fee)))
        (ok { net-amount: net-amount, transaction-id: tx-id })
      )
    )
  )
)

;; ============================================================================
;; ESCROW PAYMENT FUNCTIONS
;; ============================================================================

;; Create escrow payment with manual block parameter
(define-public (create-escrow-payment 
  (recipient principal) 
  (amount uint) 
  (arbiter principal)
  (release-condition (string-utf8 256))
  (dispute-period-blocks uint)
  (memo (string-utf8 256))
  (current-block-num uint))
  
  (let ((payment-id (get-next-payment-id)))
    (asserts! (is-valid-amount amount) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient tx-sender)) ERR_INVALID_RECIPIENT)
    (asserts! (not (is-eq arbiter tx-sender)) ERR_INVALID_RECIPIENT)
    
    ;; Lock funds in contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Create user profiles if they don't exist
    (create-user-profile-if-not-exists tx-sender)
    (create-user-profile-if-not-exists recipient)
    (create-user-profile-if-not-exists arbiter)
    
    ;; Create payment request
    (map-set payment-requests { payment-id: payment-id }
      {
        sender: tx-sender,
        recipient: recipient,
        amount: amount,
        memo: memo,
        created-block: current-block-num,
        expires-block: (+ current-block-num dispute-period-blocks),
        status: "pending",
        payment-type: "escrow"
      }
    )
    
    ;; Create escrow details
    (map-set escrow-payments { payment-id: payment-id }
      {
        arbiter: arbiter,
        release-condition: release-condition,
        dispute-deadline: (+ current-block-num dispute-period-blocks),
        is-disputed: false,
        dispute-reason: u"",
        funds-locked: amount
      }
    )
    
    (ok payment-id)
  )
)

;; Release escrow payment
(define-public (release-escrow (payment-id uint) (current-block-num uint))
  (let ((payment-data (unwrap! (map-get? payment-requests { payment-id: payment-id }) 
                               ERR_PAYMENT_NOT_FOUND))
        (escrow-data (unwrap! (map-get? escrow-payments { payment-id: payment-id }) 
                              ERR_PAYMENT_NOT_FOUND)))
    
    (asserts! (or (is-eq tx-sender (get sender payment-data))
                  (is-eq tx-sender (get recipient payment-data))
                  (is-eq tx-sender (get arbiter escrow-data))) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status payment-data) "pending") ERR_PAYMENT_ALREADY_COMPLETED)
    (asserts! (not (get is-disputed escrow-data)) ERR_DISPUTE_PERIOD_ACTIVE)
    
    (let ((amount (get amount payment-data))
          (recipient (get recipient payment-data))
          (platform-fee (calculate-platform-fee amount))
          (net-amount (- amount platform-fee)))
      
      ;; Transfer from contract to recipient
      (try! (as-contract (stx-transfer? net-amount tx-sender recipient)))
      (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
      
      ;; Update payment status
      (map-set payment-requests { payment-id: payment-id }
        (merge payment-data { status: "completed" })
      )
      
      ;; Update user profiles
      (update-user-profile (get sender payment-data) amount true)
      (update-user-profile recipient net-amount false)
      
      ;; Record transaction
      (let ((tx-id (record-transaction (get sender payment-data) recipient net-amount "escrow" platform-fee)))
        (ok { net-amount: net-amount, transaction-id: tx-id })
      )
    )
  )
)

;; Dispute escrow payment
(define-public (dispute-escrow (payment-id uint) (dispute-reason (string-utf8 512)))
  (let ((payment-data (unwrap! (map-get? payment-requests { payment-id: payment-id }) 
                               ERR_PAYMENT_NOT_FOUND))
        (escrow-data (unwrap! (map-get? escrow-payments { payment-id: payment-id }) 
                              ERR_PAYMENT_NOT_FOUND)))
    
    (asserts! (or (is-eq tx-sender (get sender payment-data))
                  (is-eq tx-sender (get recipient payment-data))) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status payment-data) "pending") ERR_PAYMENT_ALREADY_COMPLETED)
    (asserts! (not (get is-disputed escrow-data)) ERR_DISPUTE_PERIOD_ACTIVE)
    
    ;; Update escrow with dispute
    (map-set escrow-payments { payment-id: payment-id }
      (merge escrow-data {
        is-disputed: true,
        dispute-reason: dispute-reason
      })
    )
    
    (ok true)
  )
)

;; ============================================================================
;; RECURRING PAYMENT FUNCTIONS
;; ============================================================================

;; Setup recurring payment
(define-public (setup-recurring-payment 
  (recipient principal) 
  (amount uint) 
  (frequency-blocks uint) 
  (total-payments uint)
  (start-block uint))
  
  (let ((schedule-id (get-next-schedule-id)))
    (asserts! (is-valid-amount amount) ERR_INVALID_AMOUNT)
    (asserts! (not (is-eq recipient tx-sender)) ERR_INVALID_RECIPIENT)
    (asserts! (> frequency-blocks u0) ERR_INVALID_SCHEDULE)
    (asserts! (> total-payments u0) ERR_INVALID_SCHEDULE)
    
    ;; Create user profiles if they don't exist
    (create-user-profile-if-not-exists tx-sender)
    (create-user-profile-if-not-exists recipient)
    
    (map-set recurring-payments { schedule-id: schedule-id }
      {
        payer: tx-sender,
        recipient: recipient,
        amount: amount,
        frequency-blocks: frequency-blocks,
        next-payment-block: start-block,
        total-payments: total-payments,
        completed-payments: u0,
        is-active: true,
        created-block: start-block
      }
    )
    
    (ok schedule-id)
  )
)

;; Execute recurring payment
(define-public (execute-recurring-payment (schedule-id uint) (current-block-num uint))
  (let ((schedule-data (unwrap! (map-get? recurring-payments { schedule-id: schedule-id }) 
                                ERR_PAYMENT_NOT_FOUND)))
    
    (asserts! (get is-active schedule-data) ERR_PAYMENT_ALREADY_COMPLETED)
    (asserts! (>= current-block-num (get next-payment-block schedule-data)) ERR_PAYMENT_EXPIRED)
    (asserts! (< (get completed-payments schedule-data) (get total-payments schedule-data)) 
              ERR_PAYMENT_ALREADY_COMPLETED)
    
    (let ((amount (get amount schedule-data))
          (payer (get payer schedule-data))
          (recipient (get recipient schedule-data))
          (platform-fee (calculate-platform-fee amount))
          (net-amount (- amount platform-fee))
          (new-completed (+ (get completed-payments schedule-data) u1)))
      
      ;; Execute payment
      (try! (stx-transfer? net-amount payer recipient))
      (try! (stx-transfer? platform-fee payer CONTRACT_OWNER))
      
      ;; Update schedule
      (map-set recurring-payments { schedule-id: schedule-id }
        (merge schedule-data {
          next-payment-block: (+ current-block-num (get frequency-blocks schedule-data)),
          completed-payments: new-completed,
          is-active: (< new-completed (get total-payments schedule-data))
        })
      )
      
      ;; Update user profiles
      (update-user-profile payer amount true)
      (update-user-profile recipient net-amount false)
      
      ;; Record transaction
      (let ((tx-id (record-transaction payer recipient net-amount "recurring" platform-fee)))
        (ok { completed-payments: new-completed, transaction-id: tx-id })
      )
    )
  )
)

;; ============================================================================
;; GROUP PAYMENT FUNCTIONS
;; ============================================================================

;; Create payment group (bill splitting)
(define-public (create-payment-group 
  (total-amount uint) 
  (participant-list (list 10 principal))
  (deadline-blocks uint)
  (current-block-num uint))
  
  (let ((group-id (get-next-group-id))
        (participant-count (len participant-list))
        (amount-per-person (/ total-amount participant-count)))
    
    (asserts! (is-valid-amount total-amount) ERR_INVALID_AMOUNT)
    (asserts! (> participant-count u0) ERR_INVALID_AMOUNT)
    (asserts! (<= participant-count u10) ERR_GROUP_FULL)
    
    ;; Create user profile for creator
    (create-user-profile-if-not-exists tx-sender)
    
    (map-set payment-groups { group-id: group-id }
      {
        creator: tx-sender,
        total-amount: total-amount,
        paid-amount: u0,
        participant-count: participant-count,
        deadline-block: (+ current-block-num deadline-blocks),
        is-completed: false,
        created-block: current-block-num
      }
    )
    
    ;; Add participants using fold
    (fold add-participant-to-group participant-list 
          { group-id: group-id, amount-per-person: amount-per-person })
    
    (ok group-id)
  )
)

;; Helper function to add participants using fold
(define-private (add-participant-to-group (participant principal) (data { group-id: uint, amount-per-person: uint }))
  (begin
    (create-user-profile-if-not-exists participant)
    (map-set group-participants 
      { group-id: (get group-id data), participant: participant }
      {
        amount-owed: (get amount-per-person data),
        amount-paid: u0,
        has-paid: false,
        payment-block: u0
      }
    )
    data
  )
)

;; Pay group contribution
(define-public (pay-group-contribution (group-id uint) (current-block-num uint))
  (let ((group-data (unwrap! (map-get? payment-groups { group-id: group-id }) 
                             ERR_PAYMENT_NOT_FOUND))
        (participant-data (unwrap! (map-get? group-participants { group-id: group-id, participant: tx-sender }) 
                                   ERR_UNAUTHORIZED)))
    
    (asserts! (not (get has-paid participant-data)) ERR_PAYMENT_ALREADY_COMPLETED)
    (asserts! (< current-block-num (get deadline-block group-data)) ERR_PAYMENT_EXPIRED)
    
    (let ((amount (get amount-owed participant-data))
          (creator (get creator group-data))
          (platform-fee (calculate-platform-fee amount))
          (net-amount (- amount platform-fee))
          (new-paid-amount (+ (get paid-amount group-data) amount)))
      
      ;; Transfer payment to group creator
      (try! (stx-transfer? net-amount tx-sender creator))
      (try! (stx-transfer? platform-fee tx-sender CONTRACT_OWNER))
      
      ;; Update participant status
      (map-set group-participants { group-id: group-id, participant: tx-sender }
        (merge participant-data {
          amount-paid: amount,
          has-paid: true,
          payment-block: current-block-num
        })
      )
      
      ;; Update group totals
      (map-set payment-groups { group-id: group-id }
        (merge group-data {
          paid-amount: new-paid-amount,
          is-completed: (>= new-paid-amount (get total-amount group-data))
        })
      )
      
      ;; Update user profiles
      (update-user-profile tx-sender amount true)
      (update-user-profile creator net-amount false)
      
      ;; Record transaction
      (let ((tx-id (record-transaction tx-sender creator net-amount "group" platform-fee)))
        (ok { net-amount: net-amount, transaction-id: tx-id })
      )
    )
  )
)

;; ============================================================================
;; QUERY FUNCTIONS
;; ============================================================================

;; Get payment request details
(define-read-only (get-payment-request (payment-id uint))
  (map-get? payment-requests { payment-id: payment-id })
)

;; Get escrow payment details
(define-read-only (get-escrow-payment (payment-id uint))
  (map-get? escrow-payments { payment-id: payment-id })
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get recurring payment schedule
(define-read-only (get-recurring-schedule (schedule-id uint))
  (map-get? recurring-payments { schedule-id: schedule-id })
)

;; Get payment group details
(define-read-only (get-payment-group (group-id uint))
  (map-get? payment-groups { group-id: group-id })
)

;; Get group participant details
(define-read-only (get-group-participant (group-id uint) (participant principal))
  (map-get? group-participants { group-id: group-id, participant: participant })
)

;; Get transaction history
(define-read-only (get-transaction (tx-id uint))
  (map-get? transaction-history { tx-id: tx-id })
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-payments: (var-get payment-counter),
    total-schedules: (var-get schedule-counter),
    total-groups: (var-get group-counter),
    total-transactions: (var-get transaction-counter),
    platform-fee-rate: (var-get platform-fee-rate),
    min-payment: (var-get min-payment-amount),
    max-payment: (var-get max-payment-amount),
    current-nonce: (var-get contract-nonce)
  }
)

;; ============================================================================
;; ADMIN FUNCTIONS
;; ============================================================================

;; Update platform fee (owner only)
(define-public (update-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-rate u500) ERR_INVALID_AMOUNT) ;; Max 5%
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

;; Update payment limits (owner only)
(define-public (update-payment-limits (min-amount uint) (max-amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (< min-amount max-amount) ERR_INVALID_AMOUNT)
    (var-set min-payment-amount min-amount)
    (var-set max-payment-amount max-amount)
    (ok true)
  )
)

;; Emergency pause functionality (owner only)
(define-data-var contract-paused bool false)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)