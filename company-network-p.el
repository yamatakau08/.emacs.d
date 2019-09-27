; (setq x '(("en0" . [192 168 0 18 0]) ("lo0" . [127 0 0 1 0])))
; (setq x '(("lo" . [127 0 0 1 0]) ("eth1" . [0 0 0 0 0]) ("eth0" . [0 0 0 0 0]) ("wlan1" . [43 31 208 49 0]) ("wlan0" . [0 0 0 0 0])))

(defun get-ip-address-first-octet (dev)
  (aref (cdr (assoc dev (network-interface-list))) 0))

(defun my-get-network-type ()
  (let (folist '()) ; folist: first octet list
    (dolist (iip (network-interface-list)) ; iip: interface ip
      (add-to-list 'folist (aref (cdr iip) 0)))
    (cond ((memq company-ip-first-octet folist) 'company)
	  ((memq 192                    folist) 'private)
	  (t                                     nil))))

(defun xget-ip-address-first-octet (dev)
  (if (assoc dev (network-interface-list))
      (aref (cdr (assoc dev (network-interface-list))) 0)
    nil))

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
