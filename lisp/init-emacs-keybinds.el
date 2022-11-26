;;; init-emacs-keybinds.el --- emacs mode keybinds

;;; Commentary:
;;

;;; Code:

(global-set-key (kbd "s-a") 'mark-whole-buffer) ;;对应Windows上面的Ctrl-a 全选
(global-set-key (kbd "s-c") 'kill-ring-save) ;;对应Windows上面的Ctrl-c 复制
(global-set-key (kbd "s-s") 'save-buffer) ;; 对应Windows上面的Ctrl-s 保存
(global-set-key (kbd "s-v") 'yank) ;对应Windows上面的Ctrl-v 粘贴
(global-set-key (kbd "s-x") 'kill-region) ;对应Windows上面的Ctrol-x 剪切

(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "M-m c f") 'open-config)

(global-set-key (kbd "<f11>") 'toggle-frame-maximized)

(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(provide 'init-emacs-keybinds)
;;; init-emacs-keybinds.el ends here
