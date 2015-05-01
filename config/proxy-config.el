(require 'socks)

(setq url-proxy-services
			'(("no_proxy" . "^\\(localhost\\|10.*\\)")
				("http" . "localhost:8118")
				("https" . "localhost:8118")))

(provide 'proxy-config)
