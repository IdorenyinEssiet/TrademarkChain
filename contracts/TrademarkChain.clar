;; TrademarkChain: Trademark Registration System
;; Version: 1.0.0

(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-TRADEMARK-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-REGISTERED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-CLASS-COUNT (err u5))
(define-constant ERR-INVALID-TRADEMARK-CLASS (err u6))
(define-constant ERR-INVALID-REGISTRATION-TYPE (err u7))
(define-constant ERR-INVALID-TRADEMARK-NAME (err u8))
(define-constant ERR-INVALID-DESCRIPTION (err u9))

(define-constant MIN-CLASS-COUNT u1)

(define-data-var next-trademark-id uint u1)

(define-map trademark-registry
    uint
    {
        brand-owner: principal,
        trademark-name: (string-utf8 50),
        description: (string-utf8 200),
        trademark-class: (string-utf8 15),
        registration-type: (string-utf8 10),
        registration-status: (string-utf8 15),
        class-count: uint
    })

(define-private (validate-trademark-class (trademark-class (string-utf8 15)))
    (or 
        (is-eq trademark-class u"Goods")
        (is-eq trademark-class u"Services")
        (is-eq trademark-class u"Collective")
        (is-eq trademark-class u"Certification")
        (is-eq trademark-class u"Figurative")
        (is-eq trademark-class u"Descriptive")
    ))

(define-private (validate-registration-type (registration-type (string-utf8 10)))
    (or 
        (is-eq registration-type u"Standard")
        (is-eq registration-type u"Madrid")
        (is-eq registration-type u"Regional")
        (is-eq registration-type u"National")
        (is-eq registration-type u"Renewal")
    ))

(define-private (validate-text-input (text (string-utf8 200)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    ))

(define-public (register-trademark 
    (trademark-name (string-utf8 50))
    (description (string-utf8 200))
    (trademark-class (string-utf8 15))
    (registration-type (string-utf8 10))
    (class-count uint))
    (let
        (
            (trademark-id (var-get next-trademark-id))
        )
        (asserts! (validate-text-input trademark-name u3 u50) ERR-INVALID-TRADEMARK-NAME)
        (asserts! (validate-text-input description u10 u200) ERR-INVALID-DESCRIPTION)
        (asserts! (>= class-count MIN-CLASS-COUNT) ERR-INVALID-CLASS-COUNT)
        (asserts! (validate-trademark-class trademark-class) ERR-INVALID-TRADEMARK-CLASS)
        (asserts! (validate-registration-type registration-type) ERR-INVALID-REGISTRATION-TYPE)
        
        (map-set trademark-registry trademark-id {
            brand-owner: tx-sender,
            trademark-name: trademark-name,
            description: description,
            trademark-class: trademark-class,
            registration-type: registration-type,
            registration-status: u"pending",
            class-count: class-count
        })
        (var-set next-trademark-id (+ trademark-id u1))
        (ok trademark-id)
    ))

(define-public (approve-trademark (trademark-id uint))
    (let
        (
            (trademark (unwrap! (map-get? trademark-registry trademark-id) ERR-TRADEMARK-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get brand-owner trademark)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get registration-status trademark) u"pending") ERR-INVALID-STATUS)
        (ok (map-set trademark-registry trademark-id (merge trademark { registration-status: u"registered" })))
    ))

(define-read-only (get-trademark (trademark-id uint))
    (ok (map-get? trademark-registry trademark-id)))

(define-read-only (get-brand-owner (trademark-id uint))
    (ok (get brand-owner (unwrap! (map-get? trademark-registry trademark-id) ERR-TRADEMARK-NOT-FOUND))))

(define-read-only (get-total-trademarks)
    (ok (- (var-get next-trademark-id) u1)))

(define-read-only (get-trademark-status (trademark-id uint))
    (ok (get registration-status (unwrap! (map-get? trademark-registry trademark-id) ERR-TRADEMARK-NOT-FOUND))))