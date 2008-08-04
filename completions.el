;; ********************************************************************************
;; Completion
;;

(defun he-word-beginning ()
  (let ((p
         (save-excursion
           (backward-word 1)
           (point))))
    p))

(defun try-expand-collection (old collection)
  (unless  old
    (he-init-string (he-word-beginning) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string collection) 'string-lessp)))
  (while (and he-expand-list (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))

  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())

    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

(defun try-expand-tag (old)
  (if (not tags-file-name)
      (setq tags-file-name (find-tags-file)))
  (try-expand-collection old 'tags-complete-tag))

(defun try-expand-css-property (old)
  (try-expand-collection old cssm-properties))

(defun try-expand-javascript-dom-function (old)
  (try-expand-collection old javascript-dom-function-names))

(defvar javascript-dom-function-names
  '("isId"
    "appendData"
    "deleteData"
    "insertData"
    "replaceData"
    "substringData"
    "createAttribute"
    "createAttributeNS"
    "createCDATASection"
    "createComment"
    "createDocumentFragment"
    "createElement"
    "createElementNS"
    "createEntityReference"
    "createProcessingInstruction"
    "createTextNode"
    "getElementById"
    "getElementsByTagName"
    "getElementsByTagNameNS"
    "importNode"
    "load"
    "loadHTML"
    "loadHTMLFile"
    "loadXML"
    "normalizeDocument"
    "registerNodeClass"
    "relaxNGValidate"
    "relaxNGValidateSource"
    "save"
    "saveHTML"
    "saveHTMLFile"
    "saveXML"
    "schemaValidate"
    "schemaValidateSource"
    "validate"
    "xinclude"
    "appendXML"
    "getAttribute"
    "getAttributeNode"
    "getAttributeNodeNS"
    "getAttributeNS"
    "getElementsByTagName"
    "getElementsByTagNameNS"
    "hasAttribute"
    "hasAttributeNS"
    "removeAttribute"
    "removeAttributeNode"
    "removeAttributeNS"
    "setAttribute"
    "setAttributeNode"
    "setAttributeNodeNS"
    "setAttributeNS"
    "setIdAttribute"
    "setIdAttributeNode"
    "setIdAttributeNS"
    "createDocument"
    "createDocumentType"
    "hasFeature"
    "getNamedItem"
    "getNamedItemNS"
    "item"
    "appendChild"
    "cloneNode"
    "hasAttributes"
    "hasChildNodes"
    "insertBefore"
    "isDefaultNamespace"
    "isSameNode"
    "isSupported"
    "lookupNamespaceURI"
    "lookupPrefix"
    "normalize"
    "removeChild"
    "replaceChild"
    "item"
    "isWhitespaceInElementContent"
    "splitText"
    "evaluate"
    "query"
    "registerNamespace"))


(defun indent-and-complete ()
  "Indent line and complete"
  (interactive)

  (cond
   ((and (boundp 'snippet) snippet)
    (snippet-next-field))

   ((looking-at "\\_>")
    (hippie-expand nil))

   ((indent-for-tab-command))))

(defun try-expand-abbrev (old)
  (expand-abbrev))
