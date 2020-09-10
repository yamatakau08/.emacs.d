(defun get-ip-address-first-octet (dev)
  (aref (cdr (assoc dev (network-interface-list))) 0))

(defun my-get-network-type ()
  (let (folist '()) ; folist: first octet list
    (dolist (iip (network-interface-list)) ; iip: interface ip
      (add-to-list 'folist (aref (cdr iip) 0)))
    (cond ((memq company-ip-first-octet folist) 'company)
	  ((memq 192                    folist) 'private)
	  (t                                     nil))))

;;(defun xget-ip-address-first-octet (dev)
;;  (if (assoc dev (network-interface-list))
;;      (aref (cdr (assoc dev (network-interface-list))) 0)
;;    nil))

(defun private-network-first-octet-p (first-octet)
  (if (= first-octet 192) t nil)) ; 192 private

(defun company-network-first-octet-p (first-octet)
  (if (= first-octet company-ip-first-octet) t nil))

(defun company-network-p ()
  (cond
   ((eq system-type 'windows-nt)
    (or (company-network-first-octet-p (get-ip-address-first-octet "eth0"))
	(company-network-first-octet-p (get-ip-address-first-octet "eth1"))
	(company-network-first-octet-p (get-ip-address-first-octet "wlan1"))))
   ((eq system-type 'darwin)
    (or (company-network-first-octet-p (get-ip-address-first-octet "en0"))))
   ((eq system-type 'gnu/linux)
    (or (company-network-first-octet-p (get-ip-address-first-octet "eno1"))))))
