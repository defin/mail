(in-package mime)

;; mostly defined in rfc2046 except where noted

(define-media-type (text plain)
    (charset))

(define-media-type (text enriched)) ;rfc1896

(define-media-type (image jpeg))

(define-media-type (image gif))

(define-media-type (audio basic))

(define-media-type (video mpeg))

(define-media-type (application octet-stream)
    (type padding))

(define-media-type (application postscript))

(define-media-type (multipart mixed)
    (boundary))

(define-media-type (multipart alternative)
    (boundary))

(define-media-type (multipart related) ; rfc2387
    )

(define-media-type (multipart parallel)
    (boundary))

(define-media-type (multipart digest)
    (boundary))

(define-media-type (message rfc822))

(define-media-type (message partial)
    (id number total))

(define-media-type (message external-body)
    (access-type expiration size permission name site directory mode server subject))

