;;; funcs.el --- Scimax Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Daniel P. Gomez <gomez.danp@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar scimax--tlmgr-installed-packages nil
  "Cached list of installed LaTeX packages.")


(defun scimax//tlmgr-installed (&optional refresh)
  "Get a list of installed LaTeX packages. Uses a cached value if
possible unless REFRESH is non-nil."
  (unless (or scimax--tlmgr-installed-packages refresh)
    (setq scimax--tlmgr-installed-packages
          (mapcar (lambda (s)
                    (split-string (substring s 2) ":" t))
                  (split-string
                   (shell-command-to-string "tlmgr list --only-installed") "\n" t))))
  scimax--tlmgr-installed-packages)


(defun scimax/texdoc (package)
  "Run texdoc on the PACKAGE."
  (interactive (list (completing-read "Package: " (scimax//tlmgr-installed))))
  (shell-command (format "texdoc %s" package)))


(defun scimax/kpsewhich (symbol)
  "Run kpsewhich on SYMBOL."
  (interactive "sSymbol: ")
  (message (shell-command-to-string (format "kpsewhich %s" symbol))))


(defun scimax/latex-setup ()
  "Display buffer with LaTeX setup information."
  (interactive)
  (message "Please wait while I gather some information. This can take a while.")
  (with-current-buffer (get-buffer-create "*scimax-latex-setup*")
    (erase-buffer)
    (org-mode)
    (insert (s-format "#+TITLE: LaTeX setup
This file describes how LaTeX is setup on your computer.
* Executables
latex: ${(executable-find \"latex\")}
pdflatex: ${(executable-find \"pdflatex\")}
bibtex: ${(executable-find \"bibtex\")}
biber: ${(executable-find \"biber\")}
tlmgr: ${(executable-find \"tlmgr\")}
kpsewhich: ${(executable-find \"kpsewhich\")}
texdoc: ${(executable-find \"texdoc\")}
Configuration:
${(shell-command-to-string \"tlmgr conf texmf\")}
* Latex classes org-mode knows about
Here are some relevant variables
help:org-format-latex-header
help:org-latex-default-packages-alist
help:org-latex-packages-alist
help:org-latex-pdf-process
Note: Not every class has a corresponding style file. Click on the texdoc link to learn more about the class.
Missing files should be installed in the TEXMFHOME directory listed above. See https://en.wikibooks.org/wiki/LaTeX/Installing_Extra_Packages for help.
"
                      (lambda (arg &optional extra)
                        (eval (read arg)))))
    (loop for (org-name header-string cls) in
          (-uniq (loop for latex-class in org-latex-classes
                       collect
                       (list (car latex-class)
                             (nth 1 latex-class)
                             (let ((header-string (nth 1 latex-class)))
                               (when (string-match "documentclass.*?{\\(.*?\\)}" header-string)
                                 (match-string 1 header-string))))))
          do
          (let ((cls-path (s-trim (shell-command-to-string (format "kpsewhich %s.cls" cls))))
                (sty-path (s-trim (shell-command-to-string (format "kpsewhich %s.sty" cls)))))
            (insert (s-format "
** ${org-name} creates documents with this LaTeX documentclass: ${cls}
This is the header that is expanded.
${header-string}
LaTeX path for class: [[${cls-path}]]
 [[elisp:(shell-command \"texdoc ${cls}\"][texdoc ${cls}]]
Latex style path: [[${sty-path}]]

"
                              (lambda (arg &optional extra)
                                (eval (read arg)))))))

    (insert "* org-mode default latex packages\n\n")
    (loop for (options package snippet compilers) in org-latex-default-packages-alist
          do
          (insert (s-format "- ${package} (options=${options}) [[elisp:(shell-command \"texdoc ${package}\"][texdoc ${package}]]\n"
                            (lambda (arg &optional extra)
                              (eval (read arg))))))

    (insert "\n* org-mode defined latex packages\n\n")
    (loop for (options package snippet compilers) in org-latex-packages-alist
          do
          (insert (s-format "- ${package} [${options}] [[elisp:(shell-command \"texdoc ${package}\"][texdoc ${package}]]\n"
                            (lambda (arg &optional extra)
                              (eval (read arg))))))

    (insert "\n\n* org-mode LaTeX compiling setup\n\n")
    (insert (format "org-latex-pdf-process = \"%s\"\n" org-latex-pdf-process))
    (if (functionp org-latex-pdf-process)
        (insert "%s" (describe-function org-latex-pdf-process))))

  (switch-to-buffer "*scimax-latex-setup*")
  (goto-char (point-min)))

(defun scimax/org-return (&optional ignore)
    "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
    (interactive "P")
    (if ignore
        (org-return)
      (cond

       ((eq 'line-break (car (org-element-context)))
        (org-return-indent))

       ;; Open links like usual, unless point is at the end of a line.
       ;; and if at beginning of line, just press enter.
       ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
            (bolp))
        (org-return))

       ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
       ;; Johansson!
       ((org-inlinetask-in-task-p)
        (org-return))

       ;; checkboxes too
       ((org-at-item-checkbox-p)
        (if (org-element-property :contents-begin
                                  (org-element-context))
            ;; we have content so add a new checkbox
            (org-insert-todo-heading nil)
          ;; no content so delete it
          (setf (buffer-substring (line-beginning-position) (point)) "")
          (org-return)))

       ;; lists end with two blank lines, so we need to make sure we are also not
       ;; at the beginning of a line to avoid a loop where a new entry gets
       ;; created with only one blank line.
       ((org-in-item-p)
        (if (save-excursion
              (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
            (org-insert-item)
          (beginning-of-line)
          (delete-region (line-beginning-position) (line-end-position))
          (org-return)))

       ;; org-heading
       ((org-at-heading-p)
        (if (not (string= "" (org-element-property :title (org-element-context))))
            (progn
              ;; Go to end of subtree suggested by Pablo GG on Disqus post.
              (org-end-of-subtree)
              (org-insert-heading-respect-content)
              (outline-show-entry))
          ;; The heading was empty, so we delete it
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")))

       ;; tables
       ((org-at-table-p)
        (if (-any?
             (lambda (x) (not (string= "" x)))
             (nth
              (- (org-table-current-dline) 1)
              (remove 'hline (org-table-to-lisp))))
            (org-return)
          ;; empty row
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")
          (org-return)))

       ;; fall-through case
       (t
        (org-return)))))
