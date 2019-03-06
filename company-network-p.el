(defun get-ip-address-first-octet (dev)
  (aref (cdr (assoc dev (network-interface-list))) 0))

(defun private-network-first-octet-p (first-octet)
  (if (= first-octet 192) t nil)) ; 192 private

(defun company-network-first-octet-p (first-octet)
  (if (= first-octet company-network-first-octet) t nil)) ; 43 company

(defun company-network-p ()
  (cond
   ((eq system-type 'windows-nt)
    (if (company-network-first-octet-p (get-ip-address-first-octet "eth0")) t
      (if (company-network-first-octet-p (get-ip-address-first-octet "eth1")) t
	(if (company-network-first-octet-p (get-ip-address-first-octet "wlan1")) t nil)))
    )
   ((eq system-type 'darwin)
    (if (company-network-first-octet-p (get-ip-address-first-octet "en0")) t nil)
    ;; ommit the checking other inteface
    )
   ((eq system-type 'gnu/linux)
    (if (company-network-first-octet-p (get-ip-address-first-octet "eno1")) t nil)
    ;; ommit the checking other inteface
    )
   )
  )
