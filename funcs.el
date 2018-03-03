;;; funcs.el --- Scimax Layer functions File for Spacemacs
;;
;;
;; Author: Daniel P. Gomez <gomez.danp@gmail.com>
;; URL: https://github.com/dangom/spacescimax.git
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

;; The following monkey-patches ox to add support for
;; a :absolute-paths t toggle on the #+INCLUDE derivative.
(with-eval-after-load 'ox
  (defun org-export-expand-include-keyword (&optional included dir footnotes)
  "Expand every include keyword in buffer.
Optional argument INCLUDED is a list of included file names along
with their line restriction, when appropriate.  It is used to
avoid infinite recursion.  Optional argument DIR is the current
working directory.  It is used to properly resolve relative
paths.  Optional argument FOOTNOTES is a hash-table used for
storing and resolving footnotes.  It is created automatically."
  (let ((includer-file (buffer-file-name (buffer-base-buffer)))
	(case-fold-search t)
	(file-prefix (make-hash-table :test #'equal))
	(current-prefix 0)
	(footnotes (or footnotes (make-hash-table :test #'equal)))
	(include-re "^[ \t]*#\\+INCLUDE:"))
    ;; If :minlevel is not set the text-property
    ;; `:org-include-induced-level' will be used to determine the
    ;; relative level when expanding INCLUDE.
    ;; Only affects included Org documents.
    (goto-char (point-min))
    (while (re-search-forward include-re nil t)
      (put-text-property (line-beginning-position) (line-end-position)
			 :org-include-induced-level
			 (1+ (org-reduced-level (or (org-current-level) 0)))))
    ;; Expand INCLUDE keywords.
    (goto-char (point-min))
    (while (re-search-forward include-re nil t)
      (unless (org-in-commented-heading-p)
	(let ((element (save-match-data (org-element-at-point))))
	  (when (eq (org-element-type element) 'keyword)
	    (beginning-of-line)
	    ;; Extract arguments from keyword's value.
	    (let* ((value (org-element-property :value element))
		   (ind (org-get-indentation))
		   location
		   (file
		    (and (string-match
			  "^\\(\".+?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)" value)
			 (prog1
			     (save-match-data
			       (let ((matched (match-string 1 value)))
				 (when (string-match "\\(::\\(.*?\\)\\)\"?\\'"
						     matched)
				   (setq location (match-string 2 matched))
				   (setq matched
					 (replace-match "" nil nil matched 1)))
				 (expand-file-name
				  (org-unbracket-string "\"" "\"" matched)
				  dir)))
			   (setq value (replace-match "" nil nil value)))))
		   (only-contents
		    (and (string-match ":only-contents *\\([^: \r\t\n]\\S-*\\)?"
				       value)
			 (prog1 (org-not-nil (match-string 1 value))
			   (setq value (replace-match "" nil nil value)))))
		   (lines
		    (and (string-match
			  ":lines +\"\\(\\(?:[0-9]+\\)?-\\(?:[0-9]+\\)?\\)\""
			  value)
			 (prog1 (match-string 1 value)
			   (setq value (replace-match "" nil nil value)))))
		   (env (cond
			 ((string-match "\\<example\\>" value) 'literal)
			 ((string-match "\\<export\\(?: +\\(.*\\)\\)?" value)
			  'literal)
			 ((string-match "\\<src\\(?: +\\(.*\\)\\)?" value)
			  'literal)))
		   ;; Minimal level of included file defaults to the
		   ;; child level of the current headline, if any, or
		   ;; one.  It only applies is the file is meant to be
		   ;; included as an Org one.
		   (minlevel
		    (and (not env)
			 (if (string-match ":minlevel +\\([0-9]+\\)" value)
			     (prog1 (string-to-number (match-string 1 value))
			       (setq value (replace-match "" nil nil value)))
			   (get-text-property (point)
					      :org-include-induced-level))))
		   (args (and (eq env 'literal) (match-string 1 value)))
		   (block (and (string-match "\\<\\(\\S-+\\)\\>" value)
			       (match-string 1 value))))
	      ;; Remove keyword.
	      (delete-region (point) (line-beginning-position 2))
	      (cond
	       ((not file) nil)
	       ((not (file-readable-p file))
		(error "Cannot include file %s" file))
	       ;; Check if files has already been parsed.  Look after
	       ;; inclusion lines too, as different parts of the same
	       ;; file can be included too.
	       ((member (list file lines) included)
		(error "Recursive file inclusion: %s" file))
	       (t
		(cond
		 ((eq env 'literal)
		  (insert
		   (let ((ind-str (make-string ind ?\s))
			 (arg-str (if (stringp args) (format " %s" args) ""))
			 (contents
			  (org-escape-code-in-string
			   (org-export--prepare-file-contents file lines))))
		     (format "%s#+BEGIN_%s%s\n%s%s#+END_%s\n"
			     ind-str block arg-str contents ind-str block))))
		 ((stringp block)
		  (insert
		   (let ((ind-str (make-string ind ?\s))
			 (contents
			  (org-export--prepare-file-contents file lines)))
		     (format "%s#+BEGIN_%s\n%s%s#+END_%s\n"
			     ind-str block contents ind-str block))))
		 (t
		  (insert
		   (with-temp-buffer
		     (let ((org-inhibit-startup t)
			   (lines
			    (if location
				(org-export--inclusion-absolute-lines
				 file location only-contents lines)
			      lines)))
		       (org-mode)
		       (insert
			(org-export--prepare-file-contents
			 file lines ind minlevel
			 (or
			  (gethash file file-prefix)
			  (puthash file (cl-incf current-prefix) file-prefix))
			 footnotes
			 includer-file)))
		     (org-export-expand-include-keyword
		      (cons (list file lines) included)
		      (file-name-directory file)
		      footnotes)
		     (buffer-string)))))
		;; Expand footnotes after all files have been
		;; included.  Footnotes are stored at end of buffer.
		(unless included
		  (org-with-wide-buffer
		   (goto-char (point-max))
		   (maphash (lambda (k v)
			      (insert (format "\n[fn:%s] %s\n" k v)))
			    footnotes))))))))))))

  (defun org-export--prepare-file-contents
    (file &optional lines ind minlevel id footnotes includer-file)
  "Prepare contents of FILE for inclusion and return it as a string.

When optional argument LINES is a string specifying a range of
lines, include only those lines.

Optional argument IND, when non-nil, is an integer specifying the
global indentation of returned contents.  Since its purpose is to
allow an included file to stay in the same environment it was
created (e.g., a list item), it doesn't apply past the first
headline encountered.

Optional argument MINLEVEL, when non-nil, is an integer
specifying the level that any top-level headline in the included
file should have.

Optional argument ID is an integer that will be inserted before
each footnote definition and reference if FILE is an Org file.
This is useful to avoid conflicts when more than one Org file
with footnotes is included in a document.

Optional argument FOOTNOTES is a hash-table to store footnotes in
the included document."
  (with-temp-buffer
    (insert-file-contents file)
    ;; Adapt all file links within the included document that
    ;; contain relative paths in order to make these paths
    ;; relative to the base document, or absolute
    (goto-char (point-min))
    (while (re-search-forward org-any-link-re nil t)
      (let ((link (save-excursion
		    (backward-char)
		    (org-element-context))))
	(when (string= "file" (org-element-property :type link))
	  (let* ((old-path (org-element-property :path link))
		 (new-path (expand-file-name old-path (file-name-directory file))))
	    (insert (let ((new (org-element-copy link)))
		      (org-element-put-property
		       new :path
		       (if includer-file
			   (file-relative-name
			    new-path (file-name-directory includer-file))
			 new-path))
		      (when (org-element-property :contents-begin link)
			(org-element-adopt-elements
			 new
			 (buffer-substring
			  (org-element-property :contents-begin link)
			  (org-element-property :contents-end link))))
		      (delete-region (org-element-property :begin link)
				     (org-element-property :end link))
		      (org-element-interpret-data new)))))))
    (when lines
      (let* ((lines (split-string lines "-"))
	     (lbeg (string-to-number (car lines)))
	     (lend (string-to-number (cadr lines)))
	     (beg (if (zerop lbeg) (point-min)
		    (goto-char (point-min))
		    (forward-line (1- lbeg))
		    (point)))
	     (end (if (zerop lend) (point-max)
		    (goto-char (point-min))
		    (forward-line (1- lend))
		    (point))))
	(narrow-to-region beg end)))
    ;; Remove blank lines at beginning and end of contents.  The logic
    ;; behind that removal is that blank lines around include keyword
    ;; override blank lines in included file.
    (goto-char (point-min))
    (org-skip-whitespace)
    (beginning-of-line)
    (delete-region (point-min) (point))
    (goto-char (point-max))
    (skip-chars-backward " \r\t\n")
    (forward-line)
    (delete-region (point) (point-max))
    ;; If IND is set, preserve indentation of include keyword until
    ;; the first headline encountered.
    (when (and ind (> ind 0))
      (unless (eq major-mode 'org-mode)
	(let ((org-inhibit-startup t)) (org-mode)))
      (goto-char (point-min))
      (let ((ind-str (make-string ind ?\s)))
	(while (not (or (eobp) (looking-at org-outline-regexp-bol)))
	  ;; Do not move footnote definitions out of column 0.
	  (unless (and (looking-at org-footnote-definition-re)
		       (eq (org-element-type (org-element-at-point))
			   'footnote-definition))
	    (insert ind-str))
	  (forward-line))))
    ;; When MINLEVEL is specified, compute minimal level for headlines
    ;; in the file (CUR-MIN), and remove stars to each headline so
    ;; that headlines with minimal level have a level of MINLEVEL.
    (when minlevel
      (unless (eq major-mode 'org-mode)
	(let ((org-inhibit-startup t)) (org-mode)))
      (org-with-limited-levels
       (let ((levels (org-map-entries
		      (lambda () (org-reduced-level (org-current-level))))))
	 (when levels
	   (let ((offset (- minlevel (apply #'min levels))))
	     (unless (zerop offset)
	       (when org-odd-levels-only (setq offset (* offset 2)))
	       ;; Only change stars, don't bother moving whole
	       ;; sections.
	       (org-map-entries
		(lambda ()
		  (if (< offset 0) (delete-char (abs offset))
		    (insert (make-string offset ?*)))))))))))
    ;; Append ID to all footnote references and definitions, so they
    ;; become file specific and cannot collide with footnotes in other
    ;; included files.  Further, collect relevant footnote definitions
    ;; outside of LINES, in order to reintroduce them later.
    (when id
      (let ((marker-min (point-min-marker))
	    (marker-max (point-max-marker))
	    (get-new-label
	     (lambda (label)
	       ;; Generate new label from LABEL by prefixing it with
	       ;; "-ID-".
	       (format "-%d-%s" id label)))
	    (set-new-label
	     (lambda (f old new)
	       ;; Replace OLD label with NEW in footnote F.
	       (save-excursion
		 (goto-char (+ (org-element-property :begin f) 4))
		 (looking-at (regexp-quote old))
		 (replace-match new))))
	    (seen-alist))
	(goto-char (point-min))
	(while (re-search-forward org-footnote-re nil t)
	  (let ((footnote (save-excursion
			    (backward-char)
			    (org-element-context))))
	    (when (memq (org-element-type footnote)
			'(footnote-definition footnote-reference))
	      (let* ((label (org-element-property :label footnote)))
		;; Update the footnote-reference at point and collect
		;; the new label, which is only used for footnotes
		;; outsides LINES.
		(when label
		  (let ((seen (cdr (assoc label seen-alist))))
		    (if seen (funcall set-new-label footnote label seen)
		      (let ((new (funcall get-new-label label)))
			(push (cons label new) seen-alist)
			(org-with-wide-buffer
			 (let* ((def (org-footnote-get-definition label))
				(beg (nth 1 def)))
			   (when (and def
				      (or (< beg marker-min)
					  (>= beg marker-max)))
			     ;; Store since footnote-definition is
			     ;; outside of LINES.
			     (puthash new
				      (org-element-normalize-string (nth 3 def))
				      footnotes))))
			(funcall set-new-label footnote label new)))))))))
	(set-marker marker-min nil)
	(set-marker marker-max nil)))
    (org-element-normalize-string (buffer-string)))))
