(in-package mime)

(defmethod decode-entity-body ((entity t))
  "")

(defmethod decode-entity-body ((entity base-mime-entity))
  "")

(defmethod decode-entity-body ((entity base-discrete-mime-entity))
  (with-slots (body content-transfer-encoding) entity
    (content-transfer-encoding-decoder body content-transfer-encoding)))

(defmethod content-transfer-encoding-decoder ((body string) (encoding (eql :7bit)))
  body)

(defmethod content-transfer-encoding-decoder ((body string) (encoding (eql :8bit)))
  body)

(defmethod content-transfer-encoding-decoder ((body string) (encoding (eql :quoted-printable)))
  (decode-quoted-printable body))

(defmethod content-transfer-encoding-decoder ((body string) (encoding (eql :base64)))
  (base64-string-to-string body))

(defmethod content-transfer-encoding-encoder ((body string) (encoding (eql :base64)))
  (string-to-base64-string body))

