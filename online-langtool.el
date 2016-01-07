;;; online-langtool.el --- Online grammar checker for natural languages.

;; Version 0.1 ; 11/26/2015
;; Copyright (C) 2015-2025, Xuelei Li
;; License LGPR (following `languagetool.org')

;; Maintainer/Author: Xuelei Li(lixuelei86@gmail.com)

;; This stand-alone module serves as a handy tool for checking partial
;; contents(mostly a marked region specified by the user, rather than
;; the whole buffer, which may contain elements from programming
;; languages ) of buffer under editing. It is powered by
;; `http://www.languagetool.org' and each time of checking must allow
;; url-request to be done normally, which always relies upon an
;; internet connection.

;; For local grammar checking based upon LanguageTool's local java
;; engine, check out module `langtool.el'. This module is powerful,
;; but until 11/26/2015 it still lacks of some flexibility and
;; efficiency.

;; You can contribute to making grammar rules for multiple languages
;; on the platform of `http://www.languagetool.org'.

;; Thank to the supporting built-in module `overlay', the third-party
;; module `popup' and some further utilities for handling
;; url-requests.  They significantly simply implementations of
;; functionalities in this module.

;; The key map for this module is setup a little bit weird since it
;; tries to avoid the extremely scattered key-map definitions with in
;; TeX-mode, specifically, TeX-mode supplied by
;; AUCTeX(`auctex-devel@gnu.org').

;; Code:


;; Variables on module level.

(defvar --lang-req-pat
  "https://languagetool.org:8081/?language=%s&text=%s"
  "The pattern of url-request for sending yet to be checked text.")

(defvar --online-langtool-default-language
  "en-US"
  "Active language environment for checking.")

(defvar --online-langtool-active-overlays
  nil
  "The cache stack to store unhandled grammar errors.")

(defvar --online-langtool-language-list
  '("en-US" "de-DE" "cn-CN")
  "A list of usual languages environment available for checking.")


;; Face for hinting words/sentences with errors.

(defface online-langtool-error-face
  '((t (:foreground "black" :background "pink")))
  "Face for errored words.")


;; Private utility functions.

(defun --online-langtool-encode (str)
  "Encode a sentence or paragraph `str' into a https-request."
  (let* ((str (replace-regexp-in-string "[\n\t ]+$" "" str))
	 (req (format --lang-req-pat
		      --online-langtool-default-language str))
	 (enc (url-encode-url req)))
    enc))

(defun --online-langtool-request (str)
  "Request for checking with given argument `str'.
Returns a string as parsed xml as a list. "
  (let* ((enc (--online-langtool-encode str))
	 (buf1 (url-retrieve-synchronously enc)))
    (with-current-buffer buf1
      (goto-char url-http-end-of-headers)
      (xml-parse-region (point) (point-max)))))

(defun --online-langtool-check-errors (str)
  "Return a list of error information objects as a list of
lists. "
  (let* ((x (--online-langtool-request str))
	 (ms (assoc 'matches x)))
    (if ms
	(let ((errs nil))
	  (dolist (e ms)
	    (when (and (listp e) (equal (car e) 'error))
	      (push (cadr e) errs)))
	  errs)
      (error "Failed request."))))

(defun --online-langtool-gen-overlays (str beg end)
  "Generate overlays for each lexical/syntax error and cache them
into `--online-langtool-active-overlays' for further looping."
  (save-excursion
    (let* ((errs (--online-langtool-check-errors str)))
      (dolist (e errs --online-langtool-active-overlays)
	;; Use `offset' other than `fromx', `tox' to refer positions.
	(let* ((obeg (+ beg (string-to-int (cdr (assoc 'offset e)))))
	       (oend (+ obeg (string-to-int (cdr (assoc 'errorlength e)))))
	       (o (make-overlay obeg oend))
	       (msg (cdr (assoc 'msg e)))
	       (rplst (split-string (cdr (assoc 'replacements e)) "#"))
	       (rps (replace-regexp-in-string "#" "\n* " (cdr (assoc 'replacements e))))
	       (hint (concat msg (if (zerop (length rps))
				     nil
				   (concat "\nReplacements: \n* " rps)))))
	  (overlay-put o 'face 'online-langtool-error-face)
	  (overlay-put o 'help-echo hint)
	  (overlay-put o 'hint hint)
	  (overlay-put o 'msg msg)
	  (overlay-put o 'replacements rplst)
	  (push o --online-langtool-active-overlays))))))

(defvar --online-langtool-popup-menu-keymap 
  ;; FIXME: How to define the key with decent manner?
  (let ((map (make-sparse-keymap)))
    (define-key map "<ESC>" 'popup-close)
    (define-key map "<ENTER>" 'popup-select)
    (define-key map "<SPC>" 'popup-select)
    (define-key map "\C-n" 'popup-next)
    (define-key map [down] 'popup-next)
    (define-key map "\C-p" 'popup-previous)
    (define-key map [up] 'popup-previous)
    map)
  "Keymap for using `popup-menu*' to correct words.")


;; Final interactive functions for external use.

(defun online-langtool-set-language (arg)
  "Prompt user to choose a default language for grammar
checking."
  (interactive
   (list (completing-read
	  "Choose language: "
	  --online-langtool-language-list)))
  (when (member arg --online-langtool-language-list)
    (setq --online-langtool-default-language arg)))

(defun online-langtool-clear-all ()
  "Clear all current error information by both destructing the
overlays and clear the stack of overlays."
  (interactive)
  (while --online-langtool-active-overlays
    (pop --online-langtool-active-overlays))
  (remove-overlays))

(defun online-langtool-check-region (begin end)
  "Perform grammar check for marked region. Clear all results
before. A sentence which is too short is not to be checked."
  (interactive "r")
  (online-langtool-clear-all)
  (save-excursion
    (if (> (- end begin) 5)
	(let* ((str (buffer-substring-no-properties begin end)))
	  (--online-langtool-gen-overlays str begin end))
      (message "Sentence too short."))))

(defun online-langtool-check-paragraph ()
  (interactive)
  (save-excursion
    (deactivate-mark)
    (mark-paragraph)
    (call-interactively 'online-langtool-check-region)))

(defun online-langtool-loop-overlays ()
  "Loop through the overlays and get hints for correction."
  (interactive)
  (require 'popup nil t)
  (if --online-langtool-active-overlays
      (let* ((o0 (pop --online-langtool-active-overlays))
	     (ht (overlay-get o0 'hint))
	     (msg (overlay-get o0 'msg))
	     (rps (overlay-get o0 'replacements))
	     (beg (overlay-start o0))
	     (end (overlay-end o0)))
	(goto-char beg)
	;; Delete overlay object since it is to be checked only once.
	(remove-overlays beg end)
	(if (fboundp 'popup-menu*)
	    (if (and rps
		     (> (length (car rps)) 0))
		(let ((sel (popup-menu* rps
					:prompt msg
					;; :isearch t :keymap
					;; --online-langtool-popup-menu-keymap
					:initial-index 0)))
		  (when (and (stringp sel)
			     (> (length sel) 0))
		    ;; Delete wrong word.
		    (delete-region beg end)
		    ;; Insert selected word at current position.
		    (insert sel)))
	      ;; Not using `popup-tip', thus keeping style
	      ;; consistent with `popup-menu'.
	      (message msg))
	  ;; If `popup' not available, use naive `message' instead.
	  (message ht)))
    (message "No active grammar error. Recheck or give up. ")))


;; Package minor mode declaration.

(defvar online-langtool-keymap
  (let ((mp (make-sparse-keymap)))
    (define-key mp (kbd "C-c M-r") 'online-langtool-check-region)
    (define-key mp (kbd "C-c M-c") 'online-langtool-clear-all)
    (define-key mp (kbd "C-c M-n") 'online-langtool-loop-overlays)
    (define-key mp (kbd "C-c M-l") 'online-langtool-set-language)
    mp)
  "Keymap for using checking functionalities.")

(define-minor-mode online-langtool-mode
  "Use real-time online grammar checking for natural languages powered
by `www.langtool.org'. "
  :lighter " LangTool"
  :keymap online-langtool-keymap)


(provide 'online-langtool-mode)

;;; `online-langtool.el' ends here.
