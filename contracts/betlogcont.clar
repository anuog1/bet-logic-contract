;; bet-logic-contract 
;; A decentralized betting contract for STX price predictions within specified time frames 
;; Users can bet on whether STX price will rise or drop within a given duration 
;; constants 
(define-constant CONTRACT_OWNER tx-sender) 
(define-constant ERR_UNAUTHORIZED (err u100)) 
(define-constant ERR_INVALID_BET_AMOUNT (err u101)) 
(define-constant ERR_INVALID_DURATION (err u102)) 
(define-constant ERR_BET_NOT_FOUND (err u103)) 
(define-constant ERR_BET_ALREADY_RESOLVED (err u104)) 
(define-constant ERR_BET_NOT_EXPIRED (err u105)) 
(define-constant ERR_INSUFFICIENT_BALANCE (err u106)) 
(define-constant ERR_INVALID_PREDICTION (err u107)) 
(define-constant ERR_ORACLE_ERROR (err u108)) 
(define-constant ERR_WITHDRAWAL_FAILED (err u109)) 
;; Betting parameters 
(define-constant MIN_BET_AMOUNT u100000) ;; 0.1 STX in microSTX 
(define-constant MAX_BET_AMOUNT u100000000000) ;; 100,000 STX in microSTX 
(define-constant MIN_DURATION u3600) ;; 1 hour in seconds 
(define-constant MAX_DURATION u2592000) ;; 30 days in seconds 
(define-constant HOUSE_EDGE u300) ;; 3% house edge (300 basis points) 
(define-constant BASIS_POINTS u10000) ;; 100% = 10,000 basis points 
;; Bet outcomes 
(define-constant BET_OUTCOME_PENDING u0) 
(define-constant BET_OUTCOME_WIN u1) 
(define-constant BET_OUTCOME_LOSE u2) 
(define-constant BET_OUTCOME_DRAW u3) 
;; Prediction types 
(define-constant PREDICTION_RISE u1) 
(define-constant PREDICTION_DROP u2) 
;; Oracle and timing 
(define-constant ORACLE_TOLERANCE u300) ;; 5 minutes tolerance for price fetching 
(define-constant RESOLUTION_WINDOW u86400) ;; 24 hours window to resolve expired bets 
;; data maps and vars 
;; Global state variables 
(define-data-var bet-id-nonce uint u0) ;; Auto-incrementing bet ID generator 
(define-data-var total-bets-created uint u0) ;; Total number of bets ever created 
(define-data-var total-volume uint u0) ;; Total betting volume in microSTX 
(define-data-var house-balance uint u0) ;; Contract's accumulated house edge fees 
(define-data-var contract-paused bool false) ;; Emergency pause mechanism 
(define-data-var oracle-address (optional principal) none) ;; Authorized oracle address 
 
;; Core bet data structure 
(define-map bets 
  { bet-id: uint } 
  { 
    bettor: principal,           ;; Address of the bettor 
    amount: uint,                ;; Bet amount in microSTX 
    prediction: uint,            ;; PREDICTION_RISE or PREDICTION_DROP 
    start-price: uint,           ;; STX price when bet was placed (in cents) 
    target-price: (optional uint), ;; Final STX price for resolution (in cents) 
    start-time: uint,            ;; Block height when bet was created 
    end-time: uint,              ;; Block height when bet expires 
    duration: uint,              ;; Duration in seconds 
    outcome: uint,               ;; BET_OUTCOME_* constants 
    resolved: bool,              ;; Whether bet has been resolved 
    resolved-at: (optional uint), ;; Block height when resolved 
    payout: uint                 ;; Calculated payout amount 
  } 
) 
 
;; User statistics and history 
(define-map user-stats 
  { user: principal } 
  { 
    total-bets: uint,            ;; Number of bets placed by user 
    total-wagered: uint,         ;; Total amount wagered 
    total-won: uint,             ;; Total amount won 
    total-lost: uint,            ;; Total amount lost 
    win-streak: uint,            ;; Current winning streak 
    best-streak: uint,           ;; Best winning streak ever 
    last-bet-time: uint          ;; Last time user placed a bet 
  } 
) 
 
;; User's active bets (for quick lookup) 
(define-map user-active-bets 
  { user: principal, bet-id: uint } 
  { active: bool } 
) 
 
;; Price oracle data 
(define-map price-data 
  { timestamp: uint } 
  { 
    price: uint,                 ;; STX price in cents 
    source: principal,           ;; Oracle that provided the price 
    block-height: uint,          ;; Block when price was recorded 
    confidence: uint             ;; Confidence level (0-10000 basis points) 
  } 
) 
 
;; Betting pool data for risk management 
(define-map daily-pools 
  { date: uint }                 ;; Unix timestamp (day start) 
  { 
    total-rise-bets: uint,       ;; Total amount bet on price rise 
    total-drop-bets: uint,       ;; Total amount bet on price drop 
    total-volume: uint,          ;; Total daily volume 
    bet-count: uint              ;; Number of bets for the day 
  } 
) 
 
;; Contract configuration (admin settable) 
(define-map contract-config 
  { key: (string-ascii 32) } 
  { value: uint } 
) 
 
;; Withdrawal requests (for large payouts) 
(define-map withdrawal-requests 
  { request-id: uint } 
  { 
    user: principal, 
    amount: uint, 
    bet-id: uint, 
    requested-at: uint, 
    processed: bool 
  } 
) 
 
;; Emergency pause reasons 
(define-map pause-reasons 
  { reason-id: uint } 
  { 
    reason: (string-ascii 256), 
    paused-at: uint, 
    paused-by: principal 
  } 
) 
 
;; private functions 
 
;; Input validation functions 
(define-private (is-valid-bet-amount (amount uint)) 
  (and (>= amount MIN_BET_AMOUNT) (<= amount MAX_BET_AMOUNT))) 
 
(define-private (is-valid-duration (duration uint)) 
  (and (>= duration MIN_DURATION) (<= duration MAX_DURATION))) 
 
(define-private (is-valid-prediction (prediction uint)) 
  (or (is-eq prediction PREDICTION_RISE) (is-eq prediction PREDICTION_DROP))) 
 
(define-private (is-contract-active) 
  (not (var-get contract-paused))) 
 
;; Access control functions 
(define-private (is-contract-owner) 
  (is-eq tx-sender CONTRACT_OWNER)) 
 
(define-private (is-authorized-oracle) 
  (match (var-get oracle-address) 
    oracle-addr (is-eq tx-sender oracle-addr) 
    false)) 
 
;; Betting calculation functions 
(define-private (calculate-payout (bet-amount uint) (won bool)) 
  (if won 
    ;; Winner gets back their bet + profit minus house edge 
    (let ((gross-payout (* bet-amount u2))) ;; 2x multiplier for winners 
      (- gross-payout (/ (* gross-payout HOUSE_EDGE) BASIS_POINTS))) 
    u0)) ;; Losers get nothing 
 
(define-private (calculate-house-fee (bet-amount uint)) 
  (/ (* bet-amount HOUSE_EDGE) BASIS_POINTS)) 
 
(define-private (determine-bet-outcome (start-price uint) (end-price uint) (prediction uint)) 
  (let ((price-diff (if (> end-price start-price)  
                      (- end-price start-price)  
                      (- start-price end-price)))) 
    (if 
      ;; If prices are essentially the same (within 1% tolerance), it's a draw 
      (< price-diff (/ start-price u100))  
      BET_OUTCOME_DRAW 
      ;; Check if prediction matches actual price movement 
      (if (and (is-eq prediction PREDICTION_RISE) (> end-price start-price))  
        BET_OUTCOME_WIN 
        (if (and (is-eq prediction PREDICTION_DROP) (< end-price start-price))  
          BET_OUTCOME_WIN 
          ;; Otherwise it's a loss 
          BET_OUTCOME_LOSE))))) 
 
;; Time and block management 
(define-private (get-current-timestamp) 
  ;; Convert block height to approximate timestamp 
  ;; Stacks blocks are ~10 minutes apart on average 
  (* block-height u600)) 
 
(define-private (is-bet-expired (end-time uint)) 
  (> (get-current-timestamp) end-time)) 
 
(define-private (calculate-end-time (duration uint)) 
  (+ (get-current-timestamp) duration)) 
 
;; Price oracle helper functions 
(define-private (get-latest-price) 
  (let ((current-time (get-current-timestamp))) 
    (map-get? price-data { timestamp: current-time }))) 
 
(define-private (is-price-data-fresh (timestamp uint)) 
  (let ((current-time (get-current-timestamp))) 
    (<= (- current-time timestamp) ORACLE_TOLERANCE))) 
 
(define-private (store-price-data (price uint) (timestamp uint)) 
  (begin 
    (map-set price-data  
      { timestamp: timestamp } 
      { 
        price: price, 
        source: tx-sender, 
        block-height: block-height, 
        confidence: u10000 ;; Full confidence for now 
      }) 
    true)) 
 
;; User statistics management 
(define-private (update-user-stats-on-bet (user principal) (amount uint)) 
  (let ((current-stats (default-to  
                         { total-bets: u0, total-wagered: u0, total-won: u0,  
                           total-lost: u0, win-streak: u0, best-streak: u0,  
                           last-bet-time: u0 } 
                         (map-get? user-stats { user: user })))) 
    (map-set user-stats { user: user } 
      (merge current-stats { 
        total-bets: (+ (get total-bets current-stats) u1), 
        total-wagered: (+ (get total-wagered current-stats) amount), 
        last-bet-time: (get-current-timestamp) 
      })) 
    true)) 
 
(define-private (update-user-stats-on-resolution (user principal) (outcome uint) (payout uint) 
(bet-amount uint)) 
  (let ((current-stats (unwrap-panic (map-get? user-stats { user: user })))) 
    (if (is-eq outcome BET_OUTCOME_WIN) 
      ;; Update for win 
      (let ((new-streak (+ (get win-streak current-stats) u1))) 
        (map-set user-stats { user: user } 
          (merge current-stats { 
            total-won: (+ (get total-won current-stats) payout), 
            win-streak: new-streak, 
            best-streak: (if (> new-streak (get best-streak current-stats))  
                           new-streak  
                           (get best-streak current-stats)) 
          }))) 
      ;; Update for loss or draw 
      (map-set user-stats { user: user } 
        (merge current-stats { 
          total-lost: (+ (get total-lost current-stats) bet-amount), 
          win-streak: u0 ;; Reset streak on loss/draw 
        }))) 
    true)) 
 
;; Daily pool management for risk balancing 
(define-private (get-today-timestamp) 
  ;; Get start of current day timestamp 
  (let ((current-time (get-current-timestamp))) 
    (- current-time (mod current-time u86400)))) 
 
(define-private (update-daily-pool (prediction uint) (amount uint)) 
  (let ((today (get-today-timestamp)) 
        (current-pool (default-to  
                        { total-rise-bets: u0, total-drop-bets: u0,  
                          total-volume: u0, bet-count: u0 } 
                        (map-get? daily-pools { date: today })))) 
    (map-set daily-pools { date: today } 
      (if (is-eq prediction PREDICTION_RISE) 
        (merge current-pool { 
          total-rise-bets: (+ (get total-rise-bets current-pool) amount), 
          total-volume: (+ (get total-volume current-pool) amount), 
          bet-count: (+ (get bet-count current-pool) u1) 
        }) 
        (merge current-pool { 
          total-drop-bets: (+ (get total-drop-bets current-pool) amount), 
          total-volume: (+ (get total-volume current-pool) amount), 
          bet-count: (+ (get bet-count current-pool) u1) 
        }))) 
    true)) 
 
;; Bet management helpers 
(define-private (generate-bet-id) 
  (let ((current-nonce (var-get bet-id-nonce))) 
    (var-set bet-id-nonce (+ current-nonce u1)) 
    current-nonce)) 
 
(define-private (add-user-active-bet (user principal) (bet-id uint)) 
  (begin 
    (map-set user-active-bets { user: user, bet-id: bet-id } { active: true }) 
    true)) 
 
(define-private (remove-user-active-bet (user principal) (bet-id uint)) 
  (begin 
    (map-delete user-active-bets { user: user, bet-id: bet-id }) 
    true)) 
 
;; Contract state management 
(define-private (update-global-stats (amount uint)) 
  (begin 
    (var-set total-bets-created (+ (var-get total-bets-created) u1)) 
    (var-set total-volume (+ (var-get total-volume) amount)) 
    true)) 
 
(define-private (add-to-house-balance (amount uint)) 
  (begin 
    (var-set house-balance (+ (var-get house-balance) amount)) 
    true)) 
 
;; Validation helper for bet resolution 
(define-private (can-resolve-bet (bet-data (tuple (bettor principal) (amount uint) (prediction 
uint)  
                                                 (start-price uint) (target-price (optional uint))  
                                                 (start-time uint) (end-time uint) (duration uint)  
                                                 (outcome uint) (resolved bool) (resolved-at (optional uint))  
                                                 (payout uint)))) 
  (and  
    (not (get resolved bet-data)) 
    (is-eq (get outcome bet-data) BET_OUTCOME_PENDING) 
    (is-bet-expired (get end-time bet-data)))) 
 
;; Risk management - check if bet would create imbalance 
(define-private (is-bet-safe-for-pool (prediction uint) (amount uint)) 
  (let ((today (get-today-timestamp)) 
        (current-pool (default-to  
                        { total-rise-bets: u0, total-drop-bets: u0,  
                          total-volume: u0, bet-count: u0 } 
                        (map-get? daily-pools { date: today })))) 
    ;; Ensure no single prediction type exceeds 80% of daily volume 
    (if (is-eq prediction PREDICTION_RISE) 
      (let ((new-rise-total (+ (get total-rise-bets current-pool) amount)) 
            (new-total-volume (+ (get total-volume current-pool) amount))) 
        (< new-rise-total (/ (* new-total-volume u8) u10))) 
      (let ((new-drop-total (+ (get total-drop-bets current-pool) amount)) 
            (new-total-volume (+ (get total-volume current-pool) amount))) 
        (< new-drop-total (/ (* new-total-volume u8) u10)))))) 
 
;; public functions 
 
;; Core betting functions 
 
;; Place a new bet on STX price prediction 
(define-public (place-bet (amount uint) (prediction uint) (duration uint) (current-price uint)) 
  (let ((bet-id (generate-bet-id)) 
        (end-time (calculate-end-time duration))) 
    (asserts! (is-contract-active) ERR_UNAUTHORIZED) 
    (asserts! (is-valid-bet-amount amount) ERR_INVALID_BET_AMOUNT) 
    (asserts! (is-valid-duration duration) ERR_INVALID_DURATION) 
    (asserts! (is-valid-prediction prediction) ERR_INVALID_PREDICTION) 
    (asserts! (is-bet-safe-for-pool prediction amount) ERR_INVALID_BET_AMOUNT) 
     
    ;; Transfer STX from user to contract 
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender))) 
     
    ;; Store the bet 
    (map-set bets { bet-id: bet-id } 
      { 
        bettor: tx-sender, 
        amount: amount, 
        prediction: prediction, 
        start-price: current-price, 
        target-price: none, 
        start-time: (get-current-timestamp), 
        end-time: end-time, 
        duration: duration, 
        outcome: BET_OUTCOME_PENDING, 
        resolved: false, 
        resolved-at: none, 
        payout: u0 
      }) 
     
    ;; Update various tracking data 
    (update-user-stats-on-bet tx-sender amount) 
    (update-daily-pool prediction amount) 
    (update-global-stats amount) 
    (add-user-active-bet tx-sender bet-id) 
     
    ;; Store current price data 
    (store-price-data current-price (get-current-timestamp)) 
     
    (ok bet-id))) 
 
;; Resolve an expired bet 
(define-public (resolve-bet (bet-id uint) (final-price uint)) 
  (let ((bet-data (unwrap! (map-get? bets { bet-id: bet-id }) ERR_BET_NOT_FOUND))) 
    (asserts! (can-resolve-bet bet-data) ERR_BET_NOT_EXPIRED) 
     
    (let ((outcome (determine-bet-outcome (get start-price bet-data) final-price (get prediction 
bet-data))) 
          (bet-amount (get amount bet-data)) 
          (bettor (get bettor bet-data))) 
       
      (let ((payout (if (is-eq outcome BET_OUTCOME_WIN) 
                      (calculate-payout bet-amount true) 
                      (if (is-eq outcome BET_OUTCOME_DRAW) 
                        bet-amount ;; Return original bet on draw 
                        u0)))) 
         
        ;; Update bet with resolution data 
        (map-set bets { bet-id: bet-id } 
          (merge bet-data { 
            target-price: (some final-price), 
            outcome: outcome, 
            resolved: true, 
            resolved-at: (some (get-current-timestamp)), 
            payout: payout 
          })) 
         
        ;; Update user stats 
        (update-user-stats-on-resolution bettor outcome payout bet-amount) 
         
        ;; Remove from active bets 
        (remove-user-active-bet bettor bet-id) 
         
        ;; Handle payouts and house edge 
        (if (> payout u0) 
          (begin 
            ;; Pay the user 
            (try! (as-contract (stx-transfer? payout tx-sender bettor))) 
            ;; Add house edge to balance (only on wins, not draws) 
            (if (is-eq outcome BET_OUTCOME_WIN) 
              (add-to-house-balance (calculate-house-fee bet-amount)) 
              true)) 
          ;; On loss, entire bet goes to house 
          (add-to-house-balance bet-amount)) 
         
        ;; Store final price data 
        (store-price-data final-price (get-current-timestamp)) 
         
        (ok outcome))))) 
 
