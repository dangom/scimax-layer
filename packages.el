;;; packages.el --- scimax layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Daniel P. Gomez <gomez.danp@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;; "The list of Lisp packages required by the scimax layer.

;;; Code:

(defconst scimax-layer-packages
  '(org
    ox-latex
    org-agenda))

(defun scimax/post-init-org ()

  (with-eval-after-load 'org-src
    (require 'org-id)
    (add-to-list 'org-structure-template-alist
                 '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"
                   "<src lang=\"emacs-lisp\">\n?\n</src>"))
    (add-to-list 'org-structure-template-alist
                 '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC"
                   "<src lang=\"shell\">\n?\n</src>"))

    (setq org-src-window-setup 'current-window)

    (add-to-list 'org-structure-template-alist
                 '("lh" "#+latex_header: " ""))
    (add-to-list 'org-structure-template-alist
                 '("lc" "#+latex_class: " ""))
    (add-to-list 'org-structure-template-alist
                 '("lco" "#+latex_class_options: " ""))
    (add-to-list 'org-structure-template-alist
                 '("ao" "#+attr_org: " ""))
    (add-to-list 'org-structure-template-alist
                 '("al" "#+attr_latex: " ""))
    (add-to-list 'org-structure-template-alist
                 '("ca" "#+caption: " ""))
    (add-to-list 'org-structure-template-alist
                 '("ti" "#+title: " ""))
    (add-to-list 'org-structure-template-alist
                 '("tn" "#+tblname: " ""))

    (add-to-list 'org-speed-commands-user (cons "m" 'org-mark-subtree))
    (add-to-list 'org-speed-commands-user (cons "P" 'org-set-property))

    )

  (setq org-catch-invisible-edits 'smart
        org-special-ctrl-a/e t
        org-image-actual-width '(400)
        org-list-allow-alphabetical t
        org-use-speed-commands t
        org-return-follows-link t
        org-edit-src-content-indentation 0)

  (setq org-confirm-babel-evaluate nil
        org-confirm-elisp-link-function nil
        org-confirm-shell-link-function nil)

  ;; These next configurations change the way exports behave, not only how
  ;; org-mode behaves while editing. That means they can exporting the same org
  ;; file from different computers may yield different results if these
  ;; settings here aren't identical and aren't overwritten on a per file basis.
  (with-eval-after-load 'ox
    (require 'ox-extra)
    ;; Patch this function to work with the latest org.
    (defun org-latex-header-blocks-filter (backend)
  (when (org-export-derived-backend-p backend 'latex)
    (let ((blocks
	   (org-element-map (org-element-parse-buffer 'greater-element nil) 'export-block
	     (lambda (block)
	       (when (and (string= (org-element-property :type block) "LATEX")
			  (string= (org-export-read-attribute
				    :header block :header)
				   "yes"))
		 (list (org-element-property :begin block)
		       (org-element-property :end block)
		       (org-element-property :value block)))))))
      (mapc (lambda (block)
	      (goto-char (nth 0 block))
	      (let ((contents-lines (split-string (nth 2 block) "\n" t)))
                (delete-region (nth 0 block) (nth 1 block))
                (dolist (line contents-lines)
                  (insert (concat "#+latex_header: "
                                  (replace-regexp-in-string "\\` *" "" line)
                                  "\n")))))
	    ;; go in reverse, to avoid wrecking the numeric blocks
	    ;; earlier in the file
	    (reverse blocks)))))
    (ox-extras-activate '(ignore-headlines latex-header-blocks)))

  (setq org-export-with-sub-superscripts '{}
        org-html-checkbox-type 'html)

  (defun scimax/align-result-table ()
    "Align tables in the subtree."
    (save-restriction
      (save-excursion
        (unless (org-before-first-heading-p) (org-narrow-to-subtree))
        (org-element-map (org-element-parse-buffer) 'table
          (lambda (tbl)
            (goto-char (org-element-property :begin tbl))
            (while (not (looking-at "|")) (forward-line))
            (org-table-align))))))

  (add-hook 'org-babel-after-execute-hook
            'scimax/align-result-table)

  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images)

  (with-eval-after-load 'org
    ;; This function overwrites the org-src function to make src blocks be colored again.
    (defun org-src-font-lock-fontify-block (lang start end)
      "Fontify code block.
  LANG is the language of the block.  START and END are positions of
  the block.  This function is called by Emacs automatic
  fontification, as long as `org-src-fontify-natively' is non-nil."
      (let ((lang-mode (org-src--get-lang-mode lang)))
        (when (fboundp lang-mode)
          (let ((string (buffer-substring-no-properties start end))
                (modified (buffer-modified-p))
                (org-buffer (current-buffer))
                (block-faces (let ((face-name (intern (format "org-block-%s" lang))))
                               (append (and (facep face-name) (list face-name))
                                       '(org-block)))))
            (remove-text-properties start end '(face nil))
            (with-current-buffer
                (get-buffer-create
                 (format " *org-src-fontification:%s*" lang-mode))
              (erase-buffer)
              (insert string " ") ;; so there's a final property change
              (unless (eq major-mode lang-mode) (funcall lang-mode))
              (org-font-lock-ensure)
              (let ((pos (point-min)) next)
                (while (setq next (next-single-property-change pos 'face))
                  (let ((new-face (get-text-property pos 'face)))
                    (put-text-property
                     (+ start (1- pos)) (1- (+ start next)) 'face
                     (list :inherit (append (and new-face (list new-face))
                                            block-faces))
                     org-buffer))
                  (setq pos next))
                ;; Add the face to the remaining part of the font.
                (put-text-property (1- (+ start pos))
                                   end 'face
                                   (list :inherit block-faces) org-buffer)))
            (add-text-properties
             start end
             '(font-lock-fontified t fontified t font-lock-multiline t))
            (set-buffer-modified-p modified)))))

    (defun org-fontify-meta-lines-and-blocks-1 (limit)
      "Fontify #+ lines and blocks."
      (let ((case-fold-search t))
        (if (re-search-forward
             "^\\([ \t]*#\\(\\(\\+[a-zA-Z]+:?\\| \\|$\\)\\(_\\([a-zA-Z]+\\)\\)?\\)[ \t]*\\(\\([^ \t\n]*\\)[ \t]*\\(.*\\)\\)\\)"
             limit t)
            (let ((beg (match-beginning 0))
                  (block-start (match-end 0))
                  (block-end nil)
                  (lang (match-string 7))
                  (beg1 (line-beginning-position 2))
                  (dc1 (downcase (match-string 2)))
                  (dc3 (downcase (match-string 3)))
                  end end1 quoting block-type ovl)
              (cond
               ((and (match-end 4) (equal dc3 "+begin"))
                ;; Truly a block
                (setq block-type (downcase (match-string 5))
                      quoting (member block-type org-protecting-blocks))
                (when (re-search-forward
                       (concat "^[ \t]*#\\+end" (match-string 4) "\\>.*")
                       nil t)  ;; on purpose, we look further than LIMIT
                  (setq end (min (point-max) (match-end 0))
                        end1 (min (point-max) (1- (match-beginning 0))))
                  (setq block-end (match-beginning 0))
                  (when quoting
                    (org-remove-flyspell-overlays-in beg1 end1)
                    (remove-text-properties beg end
                                            '(display t invisible t intangible t)))
                  (add-text-properties
                   beg end '(font-lock-fontified t font-lock-multiline t))
                  (add-text-properties beg beg1 '(face org-meta-line))
                  (org-remove-flyspell-overlays-in beg beg1)
                  (add-text-properties  ; For end_src
                   end1 (min (point-max) (1+ end)) '(face org-meta-line))
                  (org-remove-flyspell-overlays-in end1 end)
                  (cond
                   ((and lang (not (string= lang "")) org-src-fontify-natively)
                    (org-src-font-lock-fontify-block lang block-start block-end)
                    (add-text-properties beg1 block-end (list 'src-block t 'lang (substring-no-properties lang))))
                   (quoting
                    (add-text-properties beg1 (min (point-max) (1+ end1))
                                         (let ((face-name (intern (format "org-block-%s" lang))))
                                           (append (and (facep face-name) (list face-name))
                                                   '(face org-block))))) ; end of source block
                   ((not org-fontify-quote-and-verse-blocks))
                   ((string= block-type "quote")
                    (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-quote)))
                   ((string= block-type "verse")
                    (add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-verse))))
                  (add-text-properties beg beg1 '(face org-block-begin-line))
                  (add-text-properties (min (point-max) (1+ end)) (min (point-max) (1+ end1))
                                       '(face org-block-end-line))
                  t))
               ((member dc1 '("+title:" "+author:" "+email:" "+date:"))
                (org-remove-flyspell-overlays-in
                 (match-beginning 0)
                 (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
                (add-text-properties
                 beg (match-end 3)
                 (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
                     '(font-lock-fontified t invisible t)
                   '(font-lock-fontified t face org-document-info-keyword)))
                (add-text-properties
                 (match-beginning 6) (min (point-max) (1+ (match-end 6)))
                 (if (string-equal dc1 "+title:")
                     '(font-lock-fontified t face org-document-title)
                   '(font-lock-fontified t face org-document-info))))
               ((equal dc1 "+caption:")
                (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
                (remove-text-properties (match-beginning 0) (match-end 0)
                                        '(display t invisible t intangible t))
                (add-text-properties (match-beginning 1) (match-end 3)
                                     '(font-lock-fontified t face org-meta-line))
                (add-text-properties (match-beginning 6) (+ (match-end 6) 1)
                                     '(font-lock-fontified t face org-block))
                t)
               ((member dc3 '(" " ""))
                (org-remove-flyspell-overlays-in beg (match-end 0))
                (add-text-properties
                 beg (match-end 0)
                 '(font-lock-fontified t face font-lock-comment-face)))
               (t ;; just any other in-buffer setting, but not indented
                (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
                (remove-text-properties (match-beginning 0) (match-end 0)
                                        '(display t invisible t intangible t))
                (add-text-properties beg (match-end 0)
                                     '(font-lock-fontified t face org-meta-line))
                t))))))

    (defface org-block-emacs-lisp
      `((t (:background "LightCyan1")))
      "Face for elisp src blocks")

    (defface org-block-python
      `((t (:background "DarkSeaGreen1")))
      "Face for python blocks")

    (defface org-block-ipython
      `((t (:background "ivory")))
      "Face for ipython blocks")

    (defface org-block-jupyter-hy
      `((t (:background "AntiqueWhite1")))
      "Face for hylang blocks")

    (defface org-block-sh
      `((t (:background "light goldenrod yellow")))
      "Face for sh blocks")

    (defface org-block-shell
      `((t (:background "light goldenrod yellow")))
      "Face for shell blocks")
    )
  )

(defun scimax/post-init-org-agenda ()
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t
        org-agenda-todo-ignore-timestamp t
        org-agenda-todo-ignore-with-date t
        org-agenda-start-on-weekday nil))


(defun scimax/post-init-ox-latex ()
  (setq org-latex-title-command ""
        org-latex-prefer-user-labels t))

;;; packages.el ends here
