;; Decentralized Autonomous Organization Merger Protocol
;; 
;; A comprehensive smart contract protocol that enables secure and democratic mergers
;; between two DAOs through governance voting, asset consolidation, and automated
;; execution. This contract facilitates the complete merger lifecycle from proposal
;; creation to final execution with proper quorum validation and member consensus.
;;
;; Key Features:
;; - Multi-DAO registration and management system
;; - Democratic voting mechanism with weighted voting power
;; - Automated asset consolidation and treasury management
;; - Secure proposal lifecycle with time-bound voting periods
;; - Emergency pause functionality for contract security
;; - Comprehensive audit trail and state management

;; ERROR CONSTANTS
(define-constant ERR-UNAUTHORIZED-ACCESS-VIOLATION (err u100))
(define-constant ERR-INVALID-PROPOSAL-DATA-FORMAT (err u101))
(define-constant ERR-VOTING-PERIOD-ALREADY-EXPIRED (err u102))
(define-constant ERR-VOTING-PERIOD-STILL-ACTIVE (err u103))
(define-constant ERR-DUPLICATE-VOTING-ATTEMPT (err u104))
(define-constant ERR-MERGER-PROPOSAL-NOT-APPROVED (err u105))
(define-constant ERR-MERGER-ALREADY-EXECUTED (err u106))
(define-constant ERR-INSUFFICIENT-VOTE-PARTICIPATION (err u107))
(define-constant ERR-INVALID-DAO-IDENTIFIER-REFERENCE (err u108))
(define-constant ERR-MERGER-PROPOSAL-ALREADY-EXISTS (err u109))
(define-constant ERR-INVALID-VOTING-DURATION-PERIOD (err u110))
(define-constant ERR-CONTRACT-IN-EMERGENCY-PAUSE-STATE (err u111))
(define-constant ERR-EMPTY-ORGANIZATION-NAME-PROVIDED (err u112))
(define-constant ERR-ZERO-VOTING-POWER-ALLOCATION (err u113))
(define-constant ERR-IDENTICAL-DAO-MERGER-ATTEMPT (err u114))
(define-constant ERR-INACTIVE-DAO-OPERATIONAL-STATE (err u115))

;; PROTOCOL CONFIGURATION CONSTANTS
(define-constant contract-administrator tx-sender)
(define-constant minimum-voting-duration-in-blocks u144)    ;; Approximately 1 day in blocks
(define-constant maximum-voting-duration-in-blocks u1008)   ;; Approximately 1 week in blocks
(define-constant required-quorum-participation-percentage u51)         ;; 51% quorum requirement
(define-constant maximum-dao-organization-name-length u50)
(define-constant maximum-asset-type-category-entries u10)
(define-constant maximum-treasury-balance-threshold u1000000000000)
(define-constant maximum-individual-voting-power-limit u1000000)

;; CORE DATA STRUCTURES
;; Comprehensive DAO registry with metadata and operational state
(define-map dao-organization-registry-database
  { dao-organization-identifier: uint }
  {
    organization-name: (string-ascii 50),
    governance-token-contract-principal: principal,
    current-treasury-balance: uint,
    active-member-count: uint,
    is-operational-status: bool,
    registration-block-height: uint,
    last-activity-block-height: uint
  }
)

;; Detailed merger proposal tracking and management
(define-map merger-proposal-registry-database
  { merger-proposal-identifier: uint }
  {
    source-dao-organization-identifier: uint,
    target-dao-organization-identifier: uint,
    proposal-creator-principal: principal,
    merged-organization-name: (string-ascii 50),
    voting-start-block-height: uint,
    voting-end-block-height: uint,
    total-affirmative-votes: uint,
    total-negative-votes: uint,
    total-eligible-voting-power: uint,
    is-execution-completed: bool,
    is-proposal-approved: bool,
    proposal-creation-block-height: uint
  }
)

;; Individual member voting records and history
(define-map member-voting-history-database
  { merger-proposal-identifier: uint, member-principal-address: principal }
  {
    vote-decision-choice: bool,
    member-voting-power-weight: uint,
    vote-submission-block-height: uint,
    member-dao-organization-identifier: uint
  }
)

;; DAO membership registry with voting power allocation
(define-map dao-membership-database
  { dao-organization-identifier: uint, member-principal-address: principal }
  {
    allocated-voting-power: uint,
    membership-start-block-height: uint,
    is-membership-active: bool,
    last-voting-activity-block-height: uint
  }
)

;; Post-merger asset consolidation tracking
(define-map consolidated-merger-assets-database
  { merger-proposal-identifier: uint }
  {
    total-combined-treasury-value: uint,
    total-consolidated-member-count: uint,
    new-merged-dao-organization-identifier: uint,
    asset-breakdown-by-category: (list 10 { asset-category-name: (string-ascii 20), asset-total-value: uint }),
    merger-execution-block-height: uint
  }
)

;; CONTRACT STATE VARIABLES
(define-data-var next-dao-organization-identifier uint u1)
(define-data-var next-merger-proposal-identifier uint u1)
(define-data-var is-emergency-pause-activated bool false)
(define-data-var total-registered-dao-organizations uint u0)
(define-data-var total-completed-merger-transactions uint u0)

;; READ-ONLY UTILITY FUNCTIONS 
(define-read-only (get-dao-organization-details-by-identifier (dao-organization-identifier uint))
  (map-get? dao-organization-registry-database { dao-organization-identifier: dao-organization-identifier })
)

(define-read-only (get-merger-proposal-details-by-identifier (merger-proposal-identifier uint))
  (map-get? merger-proposal-registry-database { merger-proposal-identifier: merger-proposal-identifier })
)

(define-read-only (get-member-voting-record-by-identifiers (merger-proposal-identifier uint) (member-principal-address principal))
  (map-get? member-voting-history-database { merger-proposal-identifier: merger-proposal-identifier, member-principal-address: member-principal-address })
)

(define-read-only (get-dao-membership-details-by-identifiers (dao-organization-identifier uint) (member-principal-address principal))
  (map-get? dao-membership-database { dao-organization-identifier: dao-organization-identifier, member-principal-address: member-principal-address })
)

(define-read-only (get-consolidated-merger-assets-by-identifier (merger-proposal-identifier uint))
  (map-get? consolidated-merger-assets-database { merger-proposal-identifier: merger-proposal-identifier })
)

(define-read-only (get-emergency-pause-status)
  (var-get is-emergency-pause-activated)
)

(define-read-only (get-protocol-comprehensive-statistics)
  {
    total-registered-dao-organizations: (var-get total-registered-dao-organizations),
    total-completed-merger-transactions: (var-get total-completed-merger-transactions),
    next-dao-organization-identifier: (var-get next-dao-organization-identifier),
    next-merger-proposal-identifier: (var-get next-merger-proposal-identifier),
    is-protocol-paused: (var-get is-emergency-pause-activated)
  }
)

;; Calculate member's effective voting power across DAOs
(define-read-only (calculate-member-effective-voting-power (dao-organization-identifier uint) (member-principal-address principal))
  (match (get-dao-membership-details-by-identifiers dao-organization-identifier member-principal-address)
    membership-details 
    (if (get is-membership-active membership-details)
      (get allocated-voting-power membership-details)
      u0
    )
    u0
  )
)

;; Validate if proposal is within active voting period
(define-read-only (is-proposal-voting-period-active (merger-proposal-identifier uint))
  (match (get-merger-proposal-details-by-identifier merger-proposal-identifier)
    proposal-details
    (and
      (>= stacks-block-height (get voting-start-block-height proposal-details))
      (<= stacks-block-height (get voting-end-block-height proposal-details))
      (not (get is-execution-completed proposal-details))
    )
    false
  )
)

;; Comprehensive analysis of proposal voting outcomes
(define-read-only (analyze-merger-proposal-voting-results (merger-proposal-identifier uint))
  (match (get-merger-proposal-details-by-identifier merger-proposal-identifier)
    proposal-details
    (let
      (
        (total-affirmative-vote-count (get total-affirmative-votes proposal-details))
        (total-negative-vote-count (get total-negative-votes proposal-details))
        (total-votes-cast (+ total-affirmative-vote-count total-negative-vote-count))
        (total-eligible-voting-power (get total-eligible-voting-power proposal-details))
        (is-quorum-threshold-met (>= (* total-votes-cast u100) (* total-eligible-voting-power required-quorum-participation-percentage)))
        (is-majority-consensus-achieved (> total-affirmative-vote-count total-negative-vote-count))
        (is-voting-period-concluded (> stacks-block-height (get voting-end-block-height proposal-details)))
      )
      (some {
        proposal-identifier: merger-proposal-identifier,
        is-voting-currently-active: (is-proposal-voting-period-active merger-proposal-identifier),
        total-affirmative-vote-count: total-affirmative-vote-count,
        total-negative-vote-count: total-negative-vote-count,
        total-votes-cast: total-votes-cast,
        is-quorum-threshold-satisfied: is-quorum-threshold-met,
        is-majority-consensus-reached: is-majority-consensus-achieved,
        is-final-approval-granted: (and is-quorum-threshold-met is-majority-consensus-achieved is-voting-period-concluded),
        is-execution-completed: (get is-execution-completed proposal-details),
        is-voting-period-ended: is-voting-period-concluded
      })
    )
    none
  )
)

;; DAO ORGANIZATION REGISTRATION AND MANAGEMENT
(define-public (register-new-dao-organization 
  (organization-name (string-ascii 50)) 
  (governance-token-contract-principal principal) 
  (initial-treasury-balance uint))
  (let
    (
      (new-dao-organization-identifier (var-get next-dao-organization-identifier))
    )
    (asserts! (not (var-get is-emergency-pause-activated)) ERR-CONTRACT-IN-EMERGENCY-PAUSE-STATE)
    (asserts! (> (len organization-name) u0) ERR-EMPTY-ORGANIZATION-NAME-PROVIDED)
    (asserts! (<= (len organization-name) maximum-dao-organization-name-length) ERR-INVALID-PROPOSAL-DATA-FORMAT)
    (asserts! (not (is-eq governance-token-contract-principal (as-contract tx-sender))) ERR-INVALID-PROPOSAL-DATA-FORMAT)
    (asserts! (<= initial-treasury-balance maximum-treasury-balance-threshold) ERR-INVALID-PROPOSAL-DATA-FORMAT)
    
    (map-set dao-organization-registry-database
      { dao-organization-identifier: new-dao-organization-identifier }
      {
        organization-name: organization-name,
        governance-token-contract-principal: governance-token-contract-principal,
        current-treasury-balance: initial-treasury-balance,
        active-member-count: u0,
        is-operational-status: true,
        registration-block-height: stacks-block-height,
        last-activity-block-height: stacks-block-height
      }
    )
    
    (var-set next-dao-organization-identifier (+ new-dao-organization-identifier u1))
    (var-set total-registered-dao-organizations (+ (var-get total-registered-dao-organizations) u1))
    (ok new-dao-organization-identifier)
  )
)

(define-public (add-member-to-dao-organization 
  (dao-organization-identifier uint) 
  (new-member-principal-address principal) 
  (member-voting-power-allocation uint))
  (let
    (
      (dao-organization-details (unwrap! (get-dao-organization-details-by-identifier dao-organization-identifier) ERR-INVALID-DAO-IDENTIFIER-REFERENCE))
    )
    (asserts! (not (var-get is-emergency-pause-activated)) ERR-CONTRACT-IN-EMERGENCY-PAUSE-STATE)
    (asserts! (get is-operational-status dao-organization-details) ERR-INACTIVE-DAO-OPERATIONAL-STATE)
    (asserts! (> member-voting-power-allocation u0) ERR-ZERO-VOTING-POWER-ALLOCATION)
    (asserts! (not (is-eq new-member-principal-address tx-sender)) ERR-UNAUTHORIZED-ACCESS-VIOLATION)
    (asserts! (not (is-eq new-member-principal-address (as-contract tx-sender))) ERR-INVALID-PROPOSAL-DATA-FORMAT)
    (asserts! (<= member-voting-power-allocation maximum-individual-voting-power-limit) ERR-INVALID-PROPOSAL-DATA-FORMAT)
    (asserts! (is-none (get-dao-membership-details-by-identifiers dao-organization-identifier new-member-principal-address)) ERR-DUPLICATE-VOTING-ATTEMPT)
    
    (map-set dao-membership-database
      { dao-organization-identifier: dao-organization-identifier, member-principal-address: new-member-principal-address }
      {
        allocated-voting-power: member-voting-power-allocation,
        membership-start-block-height: stacks-block-height,
        is-membership-active: true,
        last-voting-activity-block-height: u0
      }
    )
    
    (map-set dao-organization-registry-database
      { dao-organization-identifier: dao-organization-identifier }
      (merge dao-organization-details { 
        active-member-count: (+ (get active-member-count dao-organization-details) u1),
        last-activity-block-height: stacks-block-height
      })
    )
    
    (ok true)
  )
)

;; MERGER PROPOSAL LIFECYCLE MANAGEMENT
(define-public (create-merger-proposal-between-dao-organizations 
  (source-dao-organization-identifier uint) 
  (target-dao-organization-identifier uint) 
  (merged-organization-name (string-ascii 50)) 
  (voting-duration-in-blocks uint))
  (let
    (
      (new-merger-proposal-identifier (var-get next-merger-proposal-identifier))
      (source-dao-organization-details (unwrap! (get-dao-organization-details-by-identifier source-dao-organization-identifier) ERR-INVALID-DAO-IDENTIFIER-REFERENCE))
      (target-dao-organization-details (unwrap! (get-dao-organization-details-by-identifier target-dao-organization-identifier) ERR-INVALID-DAO-IDENTIFIER-REFERENCE))
      (voting-start-block-height stacks-block-height)
      (voting-end-block-height (+ stacks-block-height voting-duration-in-blocks))
      (combined-eligible-voting-power (+ (get active-member-count source-dao-organization-details) (get active-member-count target-dao-organization-details)))
    )
    (asserts! (not (var-get is-emergency-pause-activated)) ERR-CONTRACT-IN-EMERGENCY-PAUSE-STATE)
    (asserts! (not (is-eq source-dao-organization-identifier target-dao-organization-identifier)) ERR-IDENTICAL-DAO-MERGER-ATTEMPT)
    (asserts! (get is-operational-status source-dao-organization-details) ERR-INACTIVE-DAO-OPERATIONAL-STATE)
    (asserts! (get is-operational-status target-dao-organization-details) ERR-INACTIVE-DAO-OPERATIONAL-STATE)
    (asserts! (and (>= voting-duration-in-blocks minimum-voting-duration-in-blocks) 
                   (<= voting-duration-in-blocks maximum-voting-duration-in-blocks)) ERR-INVALID-VOTING-DURATION-PERIOD)
    (asserts! (> (len merged-organization-name) u0) ERR-EMPTY-ORGANIZATION-NAME-PROVIDED)
    
    ;; Verify proposer has membership in either DAO organization
    (asserts! 
      (or 
        (is-some (get-dao-membership-details-by-identifiers source-dao-organization-identifier tx-sender))
        (is-some (get-dao-membership-details-by-identifiers target-dao-organization-identifier tx-sender))
      ) 
      ERR-UNAUTHORIZED-ACCESS-VIOLATION
    )
    
    (map-set merger-proposal-registry-database
      { merger-proposal-identifier: new-merger-proposal-identifier }
      {
        source-dao-organization-identifier: source-dao-organization-identifier,
        target-dao-organization-identifier: target-dao-organization-identifier,
        proposal-creator-principal: tx-sender,
        merged-organization-name: merged-organization-name,
        voting-start-block-height: voting-start-block-height,
        voting-end-block-height: voting-end-block-height,
        total-affirmative-votes: u0,
        total-negative-votes: u0,
        total-eligible-voting-power: combined-eligible-voting-power,
        is-execution-completed: false,
        is-proposal-approved: false,
        proposal-creation-block-height: stacks-block-height
      }
    )
    
    (var-set next-merger-proposal-identifier (+ new-merger-proposal-identifier u1))
    (ok new-merger-proposal-identifier)
  )
)

(define-public (submit-merger-proposal-vote (merger-proposal-identifier uint) (vote-decision-choice bool))
  (let
    (
      (merger-proposal-details (unwrap! (get-merger-proposal-details-by-identifier merger-proposal-identifier) ERR-INVALID-PROPOSAL-DATA-FORMAT))
      (source-dao-organization-identifier (get source-dao-organization-identifier merger-proposal-details))
      (target-dao-organization-identifier (get target-dao-organization-identifier merger-proposal-details))
      (source-dao-voting-power (calculate-member-effective-voting-power source-dao-organization-identifier tx-sender))
      (target-dao-voting-power (calculate-member-effective-voting-power target-dao-organization-identifier tx-sender))
      (total-member-voting-power (+ source-dao-voting-power target-dao-voting-power))
      (member-primary-dao-affiliation (if (> source-dao-voting-power u0) source-dao-organization-identifier target-dao-organization-identifier))
    )
    (asserts! (not (var-get is-emergency-pause-activated)) ERR-CONTRACT-IN-EMERGENCY-PAUSE-STATE)
    (asserts! (is-proposal-voting-period-active merger-proposal-identifier) ERR-VOTING-PERIOD-ALREADY-EXPIRED)
    (asserts! (> total-member-voting-power u0) ERR-UNAUTHORIZED-ACCESS-VIOLATION)
    (asserts! (is-none (get-member-voting-record-by-identifiers merger-proposal-identifier tx-sender)) ERR-DUPLICATE-VOTING-ATTEMPT)
    
    (map-set member-voting-history-database
      { merger-proposal-identifier: merger-proposal-identifier, member-principal-address: tx-sender }
      {
        vote-decision-choice: vote-decision-choice,
        member-voting-power-weight: total-member-voting-power,
        vote-submission-block-height: stacks-block-height,
        member-dao-organization-identifier: member-primary-dao-affiliation
      }
    )
    
    (let
      (
        (updated-affirmative-votes (if vote-decision-choice 
          (+ (get total-affirmative-votes merger-proposal-details) total-member-voting-power) 
          (get total-affirmative-votes merger-proposal-details)))
        (updated-negative-votes (if vote-decision-choice 
          (get total-negative-votes merger-proposal-details) 
          (+ (get total-negative-votes merger-proposal-details) total-member-voting-power)))
      )
      (map-set merger-proposal-registry-database
        { merger-proposal-identifier: merger-proposal-identifier }
        (merge merger-proposal-details {
          total-affirmative-votes: updated-affirmative-votes,
          total-negative-votes: updated-negative-votes
        })
      )
    )
    
    ;; Update member's last voting activity timestamp
    (map-set dao-membership-database
      { dao-organization-identifier: member-primary-dao-affiliation, member-principal-address: tx-sender }
      (merge 
        (unwrap-panic (get-dao-membership-details-by-identifiers member-primary-dao-affiliation tx-sender))
        { last-voting-activity-block-height: stacks-block-height }
      )
    )
    
    (ok true)
  )
)

;; MERGER EXECUTION AND FINALIZATION
(define-public (execute-approved-merger-proposal (merger-proposal-identifier uint))
  (let
    (
      (merger-proposal-details (unwrap! (get-merger-proposal-details-by-identifier merger-proposal-identifier) ERR-INVALID-PROPOSAL-DATA-FORMAT))
      (source-dao-organization-details (unwrap! (get-dao-organization-details-by-identifier (get source-dao-organization-identifier merger-proposal-details)) ERR-INVALID-DAO-IDENTIFIER-REFERENCE))
      (target-dao-organization-details (unwrap! (get-dao-organization-details-by-identifier (get target-dao-organization-identifier merger-proposal-details)) ERR-INVALID-DAO-IDENTIFIER-REFERENCE))
      (merger-voting-analysis (unwrap! (analyze-merger-proposal-voting-results merger-proposal-identifier) ERR-INVALID-PROPOSAL-DATA-FORMAT))
    )
    (asserts! (not (var-get is-emergency-pause-activated)) ERR-CONTRACT-IN-EMERGENCY-PAUSE-STATE)
    (asserts! (not (is-proposal-voting-period-active merger-proposal-identifier)) ERR-VOTING-PERIOD-STILL-ACTIVE)
    (asserts! (not (get is-execution-completed merger-proposal-details)) ERR-MERGER-ALREADY-EXECUTED)
    (asserts! (get is-final-approval-granted merger-voting-analysis) ERR-MERGER-PROPOSAL-NOT-APPROVED)
    
    (let
      (
        (total-consolidated-treasury-balance (+ (get current-treasury-balance source-dao-organization-details) 
                                               (get current-treasury-balance target-dao-organization-details)))
        (total-consolidated-member-count (+ (get active-member-count source-dao-organization-details) 
                                          (get active-member-count target-dao-organization-details)))
        (new-merged-dao-organization-identifier (var-get next-dao-organization-identifier))
      )
      
      ;; Create the new merged DAO organization entity
      (try! (register-new-dao-organization 
        (get merged-organization-name merger-proposal-details) 
        (get governance-token-contract-principal source-dao-organization-details) 
        total-consolidated-treasury-balance))
      
      ;; Record comprehensive merger asset consolidation information
      (map-set consolidated-merger-assets-database
        { merger-proposal-identifier: merger-proposal-identifier }
        {
          total-combined-treasury-value: total-consolidated-treasury-balance,
          total-consolidated-member-count: total-consolidated-member-count,
          new-merged-dao-organization-identifier: new-merged-dao-organization-identifier,
          asset-breakdown-by-category: (list 
            { asset-category-name: "treasury-funds", asset-total-value: total-consolidated-treasury-balance }
            { asset-category-name: "member-base", asset-total-value: total-consolidated-member-count }
          ),
          merger-execution-block-height: stacks-block-height
        }
      )
      
      ;; Deactivate the source and target DAO organizations
      (map-set dao-organization-registry-database
        { dao-organization-identifier: (get source-dao-organization-identifier merger-proposal-details) }
        (merge source-dao-organization-details { is-operational-status: false, last-activity-block-height: stacks-block-height })
      )
      
      (map-set dao-organization-registry-database
        { dao-organization-identifier: (get target-dao-organization-identifier merger-proposal-details) }
        (merge target-dao-organization-details { is-operational-status: false, last-activity-block-height: stacks-block-height })
      )
      
      ;; Mark merger proposal as executed and approved
      (map-set merger-proposal-registry-database
        { merger-proposal-identifier: merger-proposal-identifier }
        (merge merger-proposal-details { is-execution-completed: true, is-proposal-approved: true })
      )
      
      (var-set total-completed-merger-transactions (+ (var-get total-completed-merger-transactions) u1))
      (ok new-merged-dao-organization-identifier)
    )
  )
)

;; ADMINISTRATIVE FUNCTIONS AND CONTROLS
(define-public (activate-emergency-pause-protocol)
  (begin
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS-VIOLATION)
    (var-set is-emergency-pause-activated true)
    (ok true)
  )
)

(define-public (deactivate-emergency-pause-protocol)
  (begin
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS-VIOLATION)
    (var-set is-emergency-pause-activated false)
    (ok true)
  )
)

(define-public (update-dao-organization-treasury-balance (dao-organization-identifier uint) (new-treasury-balance uint))
  (let
    (
      (dao-organization-details (unwrap! (get-dao-organization-details-by-identifier dao-organization-identifier) ERR-INVALID-DAO-IDENTIFIER-REFERENCE))
    )
    (asserts! (not (var-get is-emergency-pause-activated)) ERR-CONTRACT-IN-EMERGENCY-PAUSE-STATE)
    (asserts! (get is-operational-status dao-organization-details) ERR-INACTIVE-DAO-OPERATIONAL-STATE)
    (asserts! 
      (or 
        (is-eq tx-sender contract-administrator)
        (is-some (get-dao-membership-details-by-identifiers dao-organization-identifier tx-sender))
      ) 
      ERR-UNAUTHORIZED-ACCESS-VIOLATION
    )
    
    (map-set dao-organization-registry-database
      { dao-organization-identifier: dao-organization-identifier }
      (merge dao-organization-details { 
        current-treasury-balance: new-treasury-balance,
        last-activity-block-height: stacks-block-height
      })
    )
    
    (ok true)
  )
)

(define-public (deactivate-dao-member-membership (dao-organization-identifier uint) (member-principal-address principal))
  (let
    (
      (dao-organization-details (unwrap! (get-dao-organization-details-by-identifier dao-organization-identifier) ERR-INVALID-DAO-IDENTIFIER-REFERENCE))
      (member-details (unwrap! (get-dao-membership-details-by-identifiers dao-organization-identifier member-principal-address) ERR-UNAUTHORIZED-ACCESS-VIOLATION))
    )
    (asserts! (not (var-get is-emergency-pause-activated)) ERR-CONTRACT-IN-EMERGENCY-PAUSE-STATE)
    (asserts! (get is-operational-status dao-organization-details) ERR-INACTIVE-DAO-OPERATIONAL-STATE)
    (asserts! (get is-membership-active member-details) ERR-INACTIVE-DAO-OPERATIONAL-STATE)
    (asserts! 
      (or 
        (is-eq tx-sender contract-administrator)
        (is-eq tx-sender member-principal-address)
      ) 
      ERR-UNAUTHORIZED-ACCESS-VIOLATION
    )
    
    (map-set dao-membership-database
      { dao-organization-identifier: dao-organization-identifier, member-principal-address: member-principal-address }
      (merge member-details { is-membership-active: false })
    )
    
    (map-set dao-organization-registry-database
      { dao-organization-identifier: dao-organization-identifier }
      (merge dao-organization-details { 
        active-member-count: (- (get active-member-count dao-organization-details) u1),
        last-activity-block-height: stacks-block-height
      })
    )
    
    (ok true)
  )
)