;; Carbon Sequestration Contract
;; Quantifies climate benefits of forests

(define-data-var admin principal tx-sender)

;; Carbon project data structure
(define-map carbon-projects
  { project-id: uint }
  {
    forest-plot-id: uint,
    owner: principal,
    start-date: uint,
    end-date: uint,
    baseline-carbon-tons: uint,
    current-carbon-tons: uint,
    verification-status: bool,
    methodology: (string-utf8 100)
  }
)

;; Carbon credit tokens
(define-fungible-token carbon-credit)

;; Carbon measurement records
(define-map carbon-measurements
  { measurement-id: uint }
  {
    project-id: uint,
    measurement-date: uint,
    carbon-tons: uint,
    verifier: principal,
    methodology-details: (string-utf8 200)
  }
)

(define-data-var next-project-id uint u1)
(define-data-var next-measurement-id uint u1)

;; Register a new carbon project
(define-public (register-carbon-project
    (forest-plot-id uint)
    (end-date uint)
    (baseline-carbon-tons uint)
    (methodology (string-utf8 100)))
  (let ((new-id (var-get next-project-id)))
    (begin
      (map-set carbon-projects
        { project-id: new-id }
        {
          forest-plot-id: forest-plot-id,
          owner: tx-sender,
          start-date: block-height,
          end-date: end-date,
          baseline-carbon-tons: baseline-carbon-tons,
          current-carbon-tons: baseline-carbon-tons,
          verification-status: false,
          methodology: methodology
        }
      )
      (var-set next-project-id (+ new-id u1))
      (ok new-id)
    )
  )
)

;; Record a carbon measurement
(define-public (record-carbon-measurement
    (project-id uint)
    (carbon-tons uint)
    (methodology-details (string-utf8 200)))
  (let ((new-id (var-get next-measurement-id))
        (project (unwrap! (map-get? carbon-projects { project-id: project-id }) (err u404))))
    (begin
      (asserts! (or (is-eq tx-sender (var-get admin)) (is-eq tx-sender (get owner project))) (err u403))
      (map-set carbon-measurements
        { measurement-id: new-id }
        {
          project-id: project-id,
          measurement-date: block-height,
          carbon-tons: carbon-tons,
          verifier: tx-sender,
          methodology-details: methodology-details
        }
      )

      ;; Update the project's current carbon tons
      (map-set carbon-projects
        { project-id: project-id }
        (merge project { current-carbon-tons: carbon-tons })
      )

      (var-set next-measurement-id (+ new-id u1))
      (ok new-id)
    )
  )
)

;; Verify a carbon project
(define-public (verify-carbon-project (project-id uint) (verification-status bool))
  (let ((project (unwrap! (map-get? carbon-projects { project-id: project-id }) (err u404))))
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) (err u403))
      (map-set carbon-projects
        { project-id: project-id }
        (merge project { verification-status: verification-status })
      )
      (ok true)
    )
  )
)

;; Mint carbon credits based on sequestered carbon
(define-public (mint-carbon-credits (project-id uint))
  (let ((project (unwrap! (map-get? carbon-projects { project-id: project-id }) (err u404)))
        (sequestered-carbon (- (get current-carbon-tons project) (get baseline-carbon-tons project))))
    (begin
      (asserts! (is-eq tx-sender (get owner project)) (err u403))
      (asserts! (get verification-status project) (err u403))
      (asserts! (> sequestered-carbon u0) (err u400))

      ;; Mint carbon credits equal to the sequestered carbon
      (ft-mint? carbon-credit sequestered-carbon (get owner project))
    )
  )
)

;; Transfer carbon credits
(define-public (transfer-carbon-credits (amount uint) (recipient principal))
  (ft-transfer? carbon-credit amount tx-sender recipient)
)

;; Read-only functions
(define-read-only (get-carbon-project (project-id uint))
  (map-get? carbon-projects { project-id: project-id })
)

(define-read-only (get-carbon-measurement (measurement-id uint))
  (map-get? carbon-measurements { measurement-id: measurement-id })
)

(define-read-only (get-carbon-credit-balance (owner principal))
  (ft-get-balance carbon-credit owner)
)

;; Calculate sequestered carbon for a project
(define-read-only (calculate-sequestered-carbon (project-id uint))
  (let ((project (unwrap-panic (map-get? carbon-projects { project-id: project-id }))))
    (- (get current-carbon-tons project) (get baseline-carbon-tons project))
  )
)

;; Transfer ownership of admin role
(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u403))
    (var-set admin new-admin)
    (ok true)
  )
)
