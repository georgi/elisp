;; browser-help.el - open documentation in browser
;;

(require 'url)

(defun search-site-url (site url keyword)
  (concat "http://www.google.com/"
          (format "search?q=%s+site:%s+inurl:%s&btnI"  
                  (url-hexify-string keyword)  
                  (url-hexify-string site)  
                  (url-hexify-string url))))

(defun html-help ()
  "Open htmlhelp documentation word under the point"
  (interactive)
  (browse-url (search-site-url  "htmlhelp.com" "reference" 
                                (thing-at-point 'symbol))))

(defun javascript-help ()
  "Open javascript documentation for word under the point"
  (interactive)
  (browse-url (search-site-url  "developer.mozilla.org" "en/docs" 
                                (thing-at-point 'symbol))))

(defun ruby-help ()
  "Open ruby documentation for word under the point"
  (interactive)
  (browse-url (search-site-url  "ruby-doc.org" "/" 
                                (thing-at-point 'symbol))))

(defun rails-help ()
  "Open rails documentation for word under the point"
  (interactive)
  (browse-url (search-site-url  "api.rubyonrails.org" "/" 
                                (thing-at-point 'symbol))))


;; ********************************************************************************
;; Dictionary lookup

(defun dictionary-lookup ()
  (interactive)
  (browse-url
   (format "http://dict.leo.org?search=%s"
           (url-hexify-string (read-string "Search word: " (current-word))))))
