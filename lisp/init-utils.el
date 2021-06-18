;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-and-compile
  (require 'thingatpt))

(defconst ydcv-buffer-name "*ydcv*")

;;;###autoload
(defun ydcv-dwim (word)
  "Call `ydcv' on WORD."
  (interactive (list (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (word-at-point))))
  (let ((max-mini-window-height 0)
        (buf (get-buffer-create ydcv-buffer-name)))
    (shell-command (format "ydcv %s" word) buf)
    (with-current-buffer buf
      (view-mode +1))))

(defconst qrcode-buffer-name "*qrcode*")

;;;###autoload
(defun qrencode-on-region (start end)
  "Call `qrencode' from START to END."
  (interactive "r")
  (let ((buf (get-buffer-create qrcode-buffer-name))
        (coding-system-for-read 'raw-text)
        (inhibit-read-only t))
    (shell-command-on-region start end "qrencode -o -" buf)
    (with-current-buffer buf
      (image-mode))
    (switch-to-buffer buf)))

(provide 'init-utils)

;;; init-utils.el ends here
