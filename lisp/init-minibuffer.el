;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  ;; :custom
  ;; (vertico-sort-function nil) ;;设置为nil历史命令无法生效
)

(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-o"     . embark-act)
         ("C-c C-c" . embark-export)
         ("C-c C-o" . embark-collect)
         ("C-c C-e" . embark-export-write))
  :config
  (define-key embark-file-map (kbd "E") #'open-directory-externally))

(use-package consult
  :ensure t
  :bind (([remap imenu]                  . consult-imenu)
         ([remap goto-line]              . consult-goto-line)
         ([remap bookmark-jump]          . consult-bookmark)
         ([remap evil-show-marks]        . consult-mark)
         ([remap recentf-open-files]     . consult-recent-file)
         ([remap repeat-complex-command] . consult-complex-command)
         ("C-x b"                        . consult-buffer))
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       ;; :preview-key nil ;; 禁止预览
                       ))
  ;; Windows设置locate为everything，需添加命令行程序到环境变量
  (if (and (eq system-type 'windows-nt)
	 (fboundp 'w32-shell-execute))
    (progn
  (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
  (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
  ))
  :custom
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after embark consult)

;; 支持拼音搜索
(use-package pyim
  :ensure t)

(defun eh-orderless-regexp (orig_func component)
  (let ((result (funcall orig_func component)))
      (require 'pyim)
      (pyim-cregexp-build result)))

(defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
    (advice-add 'orderless-regexp :around #'eh-orderless-regexp)
        (advice-remove 'orderless-regexp #'eh-orderless-regexp)))

(defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
	(advice-remove 'orderless-regexp #'eh-orderless-regexp)))

;; (advice-add 'exit-minibuffer :after #'disable-py-search)
(add-hook 'minibuffer-exit-hook 'disable-py-search)
(global-set-key (kbd "s-p") 'toggle-chinese-search)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
