--- lisp/mh-e/mh-e.el-ORIG	2022-03-11 16:04:21.000000000 +0900
+++ lisp/mh-e/mh-e.el	2022-04-04 21:31:48.335014000 +0900
@@ -1966,6 +1966,12 @@
   :group 'mh-letter
   :package-version '(MH-E . "8.0"))
 
+(defcustom-mh mh-letter-use-charset t
+  "use the charset parameter in the user specified Content-Type header field,
+by preprending it to the option `mm-coding-system-priorities'."
+  :type 'boolean
+  :group 'mh-letter)
+
 ;;; Ranges (:group 'mh-ranges)
 
 (defcustom-mh mh-interpret-number-as-range-flag t
--- lisp/mh-e/mh-comp.el-ORIG	2022-03-11 16:04:21.000000000 +0900
+++ lisp/mh-e/mh-comp.el	2022-04-04 21:31:48.331851000 +0900
@@ -272,6 +272,11 @@
 of the delivery; this output can be found in a buffer called \"*MH-E
 Mail Delivery*\".
 
+When the customization option `mh-letter-use-charset' is non-nil,
+the charset parameter in the Content-Type header field specifies
+prefered charset for outgoing message,
+i.e, preprendig it to the option `mm-coding-system-priorities'.
+
 The hook `mh-before-send-letter-hook' is run at the beginning of
 this command. For example, if you want to check your spelling in
 your message before sending, add the function `ispell-message'.
@@ -295,10 +300,45 @@
            (goto-char (point-min)))
       (if (not (y-or-n-p "Auto fields inserted, send? "))
           (error "Send aborted")))
-  (cond ((mh-mh-directive-present-p)
-         (mh-mh-to-mime))
-        ((or (mh-mml-tag-present-p) (not (mh-ascii-buffer-p)))
-         (mh-mml-to-mime)))
+
+  ;; fetch "Content-Type" field and get "charset=xxx".
+  ;; if exists, set mm-coding-system-priorities accordingly,
+  ;; and replace "Content-Type" field without "charset=xxx".
+  (let* ((mm-coding-system-priorities mm-coding-system-priorities)
+	 (case-fold-search t)
+	 (content-type
+	  (save-restriction
+	    (message-narrow-to-headers)
+	    (mail-fetch-field "content-type")))
+	 new-content-type charset)
+    (when (and content-type
+	       (string-match "charset=" content-type))
+      (setq new-content-type
+	    (apply #'concat
+		   (mapcar
+		    #'(lambda (x)
+			(cond
+			 ((stringp x) x)	;; first element
+			 ((consp x)	;; parameter (symbol . value-string)
+			  (if (not (eq (car x) 'charset))
+			      (format "; %s=%s" (car x) (cdr x))
+			    (setq charset (intern (downcase (cdr x))))
+			    ""))
+			 (t "")))
+		    (mail-header-parse-content-type content-type))))
+      (save-restriction
+	(message-narrow-to-headers)
+	(goto-char (point-min)) 
+	(and (search-forward content-type nil t nil)
+	     (replace-match new-content-type)))
+      (when (and mh-letter-use-charset charset (coding-system-p charset))
+	(setq mm-coding-system-priorities
+	      (cons charset mm-coding-system-priorities))))
+    (cond ((mh-mh-directive-present-p)
+           (mh-mh-to-mime))
+          ((or (mh-mml-tag-present-p) (not (mh-ascii-buffer-p)))
+           (mh-mml-to-mime)))
+    )
   (save-buffer)
   (message "Sending...")
   (let ((draft-buffer (current-buffer))
