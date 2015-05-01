(require 'socks)

;; (setq socks-server (list "My socks server" "localhost" 8118 4))

(setq url-proxy-services
			'(("no_proxy" . "^\\(localhost\\|10.*\\)")
				("http" . "localhost:8118")
				("https" . "localhost:8118")))

;; (setq socks-override-functions 1)
;; (setq erc-server-connect-function 'socks-open-network-stream)
