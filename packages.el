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

(defconst scimax-packages
  '((org :location built-in)
    (ox-latex :location built-in)
    (org-agenda :location built-in)
    org-ref))

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
    (ox-extras-activate '(ignore-headlines)))

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
