;;; helm-ja-wikipedia.el --- Wikipedia suggestions -*- lexical-binding: t -*-
(require 'json)

(require 'helm-net)

(defcustom helm-ja-wikipedia-suggest-url
  "https://ja.wikipedia.org/w/api.php?action=opensearch&search=%s"
  "Url used for looking up Wikipedia suggestions.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-ja-wikipedia-summary-url
  "https://ja.wikipedia.org/w/api.php?action=query&format=json&prop=extracts&titles=%s&exintro=1&explaintext=1&redirects=1"
  "URL for getting the summary of a Wikipedia topic.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-ja-wikipedia-input-idle-delay 0.6
  "`helm-input-idle-delay' used for helm-wikipedia."
  :type 'float
  :group 'helm-net)

(declare-function json-read-from-string "json" (string))
(defun helm-ja-wikipedia-suggest-fetch ()
  "Fetch Wikipedia suggestions and return them as a list."
  (require 'json)
  (let ((request (format helm-ja-wikipedia-suggest-url
                         (url-hexify-string helm-pattern))))
    (helm-net--url-retrieve-sync
     request #'helm-ja-wikipedia--parse-buffer)))

(defun helm-ja-wikipedia--parse-buffer ()
  "Parse wikipedia buffer."
  (goto-char (point-min))
  (when (re-search-forward "^\\[.+\\[\\(.*\\)\\]\\]" nil t)
    (cl-loop for i across (aref (json-read-from-string (match-string 0)) 1)
             collect i into result
             finally return (or result
                                (append
                                 result
                                 (list (cons (format "Search for '%s' on wikipedia"
                                                     helm-pattern)
                                             helm-pattern)))))))

(defvar helm-ja-wikipedia--summary-cache (make-hash-table :test 'equal)
  "A temporary cache for wikipedia summary.")
(defun helm-ja-wikipedia-show-summary (input)
  "Show Wikipedia summary for INPUT in new buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*helm wikipedia summary*"))
        (summary (helm-ja-wikipedia--get-summary input)))
    (with-current-buffer buffer
      (visual-line-mode)
      (erase-buffer)
      (insert summary)
      (pop-to-buffer (current-buffer))
      (goto-char (point-min)))))

(defun helm-ja-wikipedia-persistent-action (candidate)
  "Run PA on CANDIDATE for wikipedia source."
  (unless (string= (format "Search for '%s' on wikipedia"
                           helm-pattern)
                   (helm-get-selection nil t))
    (message "Fetching summary from Wikipedia...")
    (let ((buf (get-buffer-create "*helm wikipedia summary*"))
          (result (helm-ja-wikipedia--get-summary candidate)))
      (with-current-buffer buf
        (erase-buffer)
        (setq cursor-type nil)
        (insert result)
        (fill-region (point-min) (point-max))
        (goto-char (point-min)))
      (display-buffer buf))))

(defun helm-ja-wikipedia--get-summary (input)
  "Return Wikipedia summary for INPUT as string.
Follows any redirections from Wikipedia, and stores results in
`helm-ja-wikipedia--summary-cache'."
  (let (result)
    (while (progn
             (setq result (or (gethash input helm-ja-wikipedia--summary-cache)
                              (puthash input
                                       (helm-ja-wikipedia--fetch-summary input)
                                       helm-ja-wikipedia--summary-cache)))
             (when (and result
                        (listp result))
               (setq input (cdr result))
               (message "Redirected to %s" input)
               t)))
    (unless result
      (error "Error when getting summary"))
    result))

(defun helm-ja-wikipedia--fetch-summary (input)
  "Fetch wikipedia summary matching INPUT."
  (let* ((request (format helm-ja-wikipedia-summary-url
                          (url-hexify-string input))))
    (helm-net--url-retrieve-sync
     request #'helm-ja-wikipedia--parse-summary)))

(defun helm-ja-wikipedia--parse-summary ()
  "Return plain-text rendering of article summary.
Read from JSON in HTTP response buffer.  Should be called in
`url-retrieve' response buffer."
  (goto-char (point-min))
  (re-search-forward "\n\n" nil t)
  (let* ((json (json-read))
         (pages (let-alist json
                  .query.pages)))
    (alist-get 'extract (nth 0 pages))))

(defvar helm-ja-wikipedia-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "<C-return>") 'helm-ja-wikipedia-show-summary-action)
    map)
  "Keymap for `helm-ja-wikipedia-suggest'.")

(defcustom helm-search-suggest-action-ja-wikipedia-url
  "https://ja.wikipedia.org/wiki/Special:Search?search=%s"
  "The Wikipedia search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defvar helm-source-ja-wikipedia-suggest
  (helm-build-sync-source "Wikipedia Suggest"
    :candidates #'helm-ja-wikipedia-suggest-fetch
    :action '(("Wikipedia" . (lambda (candidate)
                               (helm-search-suggest-perform-additional-action
                                helm-search-suggest-action-ja-wikipedia-url
                                candidate)))
              ("Show summary in new buffer (C-RET)" . helm-ja-wikipedia-show-summary))
    :persistent-action #'helm-ja-wikipedia-persistent-action
    :persistent-help "show summary"
    :match-dynamic t
    :keymap helm-ja-wikipedia-map
    :requires-pattern 3))

(defun helm-ja-wikipedia-show-summary-action ()
  "Exit Helm buffer and call `helm-ja-wikipedia-show-summary' with selected candidate."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-ja-wikipedia-show-summary)))

;;;###autoload
(defun helm-ja-wikipedia-suggest ()
  "Preconfigured `helm' for Wikipedia lookup with Wikipedia suggest."
  (interactive)
  (let ((helm-input-idle-delay helm-ja-wikipedia-input-idle-delay)) 
    (helm :sources 'helm-source-ja-wikipedia-suggest
          :buffer "*helm wikipedia*"
          :input-idle-delay (max 0.4 helm-input-idle-delay))))


(provide 'helm-ja-wikipedia)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-ja-wikipedia.el ends here
