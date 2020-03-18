;;; init-ui.el --- Theme and modeline -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; restore windows layout
(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))

(provide 'init-ui)

;;; init-ui.el ends here
