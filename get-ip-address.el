;;; https://gist.github.com/tkhoa2711/ef99938c8752ca3e52c2

(defun get-ip-address (&optional dev)
  "Get the IP-address for device DEV (default: eth0) of the current machine."
  (let ((dev (if dev dev "eth0")))
    (format-network-address (car (network-interface-info dev)) t)))

;(defun get-ip-address-first-octet (&optional dev)
;  (let ((dev (if dev dev "eth0")))
;    (car (split-string (get-ip-address dev) "\\."))))

(defun get-ip-address-first-octet (&optional dev)
  (let* ((dev (if dev dev "eth0"))
	 (ip-address (get-ip-address dev)))
    (if ip-address
	(car (split-string ip-address "\\."))
      nil)))
    
(defun private-network-p (first-octet)
  (if (string= first-octet "192") t nil)) ; "192" private

(defun check-private-network ()
  (cond 
   ((eq system-type 'windows-nt)
    (or (private-network-p (get-ip-address-first-octet "eth0"))
	(private-network-p (get-ip-address-first-octet "wlan0"))))
   ((eq system-type 'darwin) 
    (private-network-p (get-ip-address-first-octet "en0")))
   ((eq system-type 'gnu/linux) ; Linux Mint
    (private-network-p (get-ip-address-first-octet "en0")))
   ))



  
