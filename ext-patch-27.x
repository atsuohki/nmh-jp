--- lisp/mh-e/mh-comp.el-ORIG	2022-04-01 13:01:56.520310000 +0900
+++ lisp/mh-e/mh-comp.el	2022-04-01 13:01:56.519771000 +0900
@@ -296,10 +296,45 @@
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
+      (when (and charset (coding-system-p charset))
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
