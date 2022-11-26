;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-guess-indent-offset-verbose nil))

(use-package python-mode
  :ensure t
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

;; python -m venv ENV_DIR
(use-package pyvenv
  :ensure t
  :commands pyvenv-deactivate pyvenv-deactivate
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs"))
  (advice-add 'pyvenv-activate :around #'(lambda (orig-func DIRECTORY)
                                           (funcall orig-func DIRECTORY)
                                           (lsp-restart-workspace) ;; 重启lsp
                                           (doom-modeline-env-update-python) ;; 刷新状态栏
                                           )))

(provide 'init-python)
;;; init-python.el ends here
