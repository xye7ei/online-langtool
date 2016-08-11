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

(defgroup online-langtool nil
  "Minor mode for online LangTool unilities."
  :prefix "online-langtool"
  :group 'languages)

(defvar online-langtool--request-pattern-ginger
  "http://services.gingersoftware.com/Ginger/correct/json/GingerTheText?lang=US&clientVersion=2.0&apiKey=6ae0c3a0-afdc-4532-a810-82ded0054236&text=%s."
  "The pattern of url-request for sending yet to be checked text.")

(defvar online-langtool--request-pattern
  "https://languagetool.org:8081/?language=%s&text=%s"
  "The pattern of url-request for sending yet to be checked text.")

(defvar online-langtool--default-language
  "en-US"
  "Active language environment for checking.")

(defvar online-langtool--active-overlays
  nil
  "The cache stack to store unhandled grammar errors.")

(defvar online-langtool--language-list
  '("en-US" "de-DE" "cn-CN")
  "A list of usual languages environment available for checking.")


;; Face for hinting words/sentences with errors.

(defface online-langtool-error-face
  '((t (:foreground "black" :background "pink")))
  "Face for errored words.")


;; Private utility functions.

(defun online-langtool--request-xml (str)
  "Request for checking with given argument `str'.
Returns a string as parsed xml as a list. "
  (let* ((str (replace-regexp-in-string "[\n\t ]+$" "" str)) ; Cleanup tailing spaces.
	 (req (format online-langtool--request-pattern
		      online-langtool--default-language str))
	 (enc (online-langtool--encode req))
	 (buf1 (url-retrieve-synchronously enc))) ; A buffer is retrieved.
    (with-current-buffer buf1
      (goto-char url-http-end-of-headers)
      (xml-parse-region (point) (point-max)))))

(defun online-langtool--request-json-langtool (text)
  "New version."
  (let ((url-request-method "POST")
	(url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")
				     ("Accept" . "application/json")))
	(url-request-data
	 (format "text=%s&language=en-US&enabledOnly=false"
		 (url-encode-url text))))
    (with-current-buffer (url-retrieve-synchronously "https://languagetool.org/api/v2/check")
      (goto-char url-http-end-of-headers)
      (json-read-from-string (buffer-substring (point) (point-max))))))

(defun online-langtool--request-json-ginger (text)
  (let ((u (format
	    "http://services.gingersoftware.com/Ginger/correct/json/GingerTheText?lang=US&clientVersion=2.0&apiKey=6ae0c3a0-afdc-4532-a810-82ded0054236&text=%s"
	    (url-encode-url text))))
    (with-current-buffer (url-retrieve-synchronously u)
      (goto-char url-http-end-of-headers)
      (json-read-from-string (buffer-substring (point) (point-max))))))

;; (defvar online-langtool--engine "languagetool")
;; ;; (setq online-langtool--engine "ginger") 
;; (defun online-langtool--msg-rpls (text)
;;   (cond ((equal online-langtool--engine "languagetool")
;; 	 (let* ((js (online-langtool--request-json-langtool text))
;; 		(infos (cdr (assoc 'matches js))))
;; 	   ;; `(,(cdr (assoc 'message infos))
;; 	   ;;   ,(elt (cdr (assoc 'replacements infos)) 0))
;; 	   infos))
;; 	((equal online-langtool--engine "ginger")
;; 	 (let* ((js (online-langtool--request-json-ginger text))
;; 		 (infos (elt (cdr (car js)) 0)))
;; 	   `(,(format "Confidence level %s" (cdr (assoc 'Confidence infos)))
;; 	     ,())))
;; 	(t (error "No such egine."))))

(defun online-langtool--request-check (str)
  "Return a list of error information objects as a list of
lists. "
  (let* ((x (online-langtool--request-xml str))
	 (ms (assoc 'matches x)))
    (if ms
	(let ((errs nil))
	  (dolist (e ms)
	    (when (and (listp e) (equal (car e) 'error))
	      (push (cadr e) errs)))
	  errs)
      (error "Failed request."))))

(defun online-langtool--gen-overlays (str beg end)
  "Generate overlays for each lexical/syntactic error and cache
them into `online-langtool--active-overlays' for further
looping."
  (save-excursion
    (let* ((errs (online-langtool--request-check str)))
      (dolist (e errs online-langtool--active-overlays)
	;; Use `offset' rather than `fromx', `tox' to refer positions.
	(let* ((obeg (+ beg (string-to-int (cdr (assoc 'offset e)))))
	       (oend (+ obeg (string-to-int (cdr (assoc 'errorlength e)))))
	       (o (make-overlay obeg oend))
	       (msg (cdr (assoc 'msg e)))
	       (rplst (split-string (cdr (assoc 'replacements e)) "#"))
	       (rps (replace-regexp-in-string "#" "\n* " (cdr (assoc 'replacements e)))))
	  (overlay-put o 'face 'online-langtool-error-face)
	  (overlay-put o 'msg msg)
	  (overlay-put o 'replacements rplst)
	  (push o online-langtool--active-overlays))))))

(defvar online-langtool--popup-menu-keymap 
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

;;;###autoload
(defun online-langtool-set-language (arg)
  "Prompt user to choose a default language for grammar
checking."
  (interactive
   (list (completing-read
	  "Choose language: "
	  online-langtool--language-list)))
  (when (member arg online-langtool--language-list)
    (setq online-langtool--default-language arg)))

;;;###autoload
(defun online-langtool-clear-all ()
  "Clear all current error information by both destructing the
overlays and clear the stack of overlays."
  (interactive)
  (while online-langtool--active-overlays
    (pop online-langtool--active-overlays))
  (remove-overlays))

;;;###autoload
(defun online-langtool-check-region (begin end)
  "Perform grammar check for marked region. Clear all results
before. A sentence which is too short is not to be checked."
  (interactive "r")
  (online-langtool-clear-all)
  (save-excursion
    (if (> (- end begin) 5)
	(let* ((str (buffer-substring-no-properties begin end)))
	  (online-langtool--gen-overlays str begin end))
      (message "Sentence too short.")))
  (deactivate-mark)
  (when (< (length online-langtool--active-overlays) 1)
    (message "No syntactic error found (Maybe unable to find).")))

;;;###autoload
(defun online-langtool-check-paragraph ()
  (interactive)
  (save-excursion
    (deactivate-mark)
    (mark-paragraph)
    (call-interactively 'online-langtool-check-region)))

;;;###autoload
(defun online-langtool-loop-overlays ()
  "Loop through the overlays and get hints for correction."
  (interactive)
  (require 'popup nil t)
  (if online-langtool--active-overlays
      (let* ((o0 (pop online-langtool--active-overlays))
	     (msg (overlay-get o0 'msg))
	     (rps (overlay-get o0 'replacements))
	     (beg (overlay-start o0))
	     (end (overlay-end o0)))
	(goto-char beg)
	;; Delete overlay object since it is to be checked only once.
	(remove-overlays beg end)
	;; ;; Style 1: completing-read
	(let ((w (if (> (length rps) 1)
		     (completing-read (concat msg " (TAB to show suggestions): ")
				      rps)
		   (completing-read (concat msg " : ")
				    rps nil nil (nth 0 rps)))))
	  (delete-region beg end)
	  (insert w))
	;; ;; Style 2: popup
	;; (if (fboundp 'popup-menu*)
	;;     (if (and rps
	;; 	     (> (length (car rps)) 0))
	;; 	(let ((sel (popup-menu* rps
	;; 				:prompt msg
	;; 				;; :isearch t :keymap
	;; 				;; online-langtool--popup-menu-keymap
	;; 				:initial-index 0)))
	;; 	  (when (and (stringp sel)
	;; 		     (> (length sel) 0))
	;; 	    ;; Delete wrong word.
	;; 	    (delete-region beg end)
	;; 	    ;; Insert selected word at current position.
	;; 	    (insert sel)))
	;;       ;; Not using `popup-tip', thus keeping style
	;;       ;; consistent with `popup-menu'.
	;;       (message msg))
	;;   ;; If `popup' not available, use naive `message' instead.
	;;   (message ht))
	)
    (message "No active grammar error. Recheck or give up.")))


;; Package minor mode declaration.

(defvar online-langtool-keymap
  (let ((mp (make-sparse-keymap)))
    (define-key mp (kbd "C-c M-r") 'online-langtool-check-region)
    (define-key mp (kbd "C-c M-h") 'online-langtool-check-paragraph)
    (define-key mp (kbd "C-c M-c") 'online-langtool-clear-all)
    (define-key mp (kbd "C-c M-n") 'online-langtool-loop-overlays)
    (define-key mp (kbd "C-c M-l") 'online-langtool-set-language)
    mp)
  "Keymap for using checking functionalities.")

;;;###autoload
(define-minor-mode online-langtool-mode
  "Use real-time online grammar checking for natural languages powered
by `www.langtool.org'. "
  :group 'online-langtool
  :lighter " LangTool"
  :keymap online-langtool-keymap)


(provide 'online-langtool-mode)

;;; `online-langtool.el' ends here.
