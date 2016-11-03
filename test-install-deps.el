;; This file is used to automatically install js2-mode during CI testing
;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(package-install 'js2-mode)

;;; test-install-deps.el ends here
