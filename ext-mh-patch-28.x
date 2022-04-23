--- lisp/gnus/mm-uu.el-ORIG	2022-04-19 08:37:18.340351000 +0900
+++ lisp/gnus/mm-uu.el	2022-04-22 09:34:27.766050000 +0900
@@ -29,6 +29,7 @@
 (require 'mm-decode)
 (require 'mailcap)
 (require 'mml2015)
+(require 'sendmail)
 (eval-when-compile (require 'cl-lib))
 
 (autoload 'uudecode-decode-region "uudecode")
@@ -497,8 +498,30 @@
 
 (defvar gnus-newsgroup-charset)
 
-(defun mm-uu-pgp-signed-extract-1 (_handles _ctl)
-  (let ((buf (mm-uu-copy-to-buffer (point-min) (point-max))))
+(defun mm-uu-pgp-signed-extract-1 (handles _ctl)
+  (let ((buf (mm-uu-copy-to-buffer (point-min) (point-max)))
+	(text-plain-type mm-uu-text-plain-type))
+
+    ;; extract charset and set text-plain-type
+    (if handles
+	(setq text-plain-type (car (cdr (car handles))))
+      (let* ((case-fold-search t)
+	     (ct
+	      (save-restriction
+		(widen)
+		(narrow-to-region (point-min) (mail-header-end))
+		(ignore-errors (mail-header-parse-content-type
+				(message-fetch-field "Content-Type" t)))))
+	     (charset (mail-content-type-get ct 'charset)))
+	(if (and (consp ct)
+		 (stringp (car ct))
+		 (string= (car ct) "text/plain")
+		 (stringp charset)
+		 (setq charset (intern (downcase charset)))
+		 (coding-system-p charset))
+	    (setq text-plain-type
+		  (list "text/plain" (cons 'charset charset))))))
+
     (with-current-buffer buf
       (if (mm-uu-pgp-signed-test)
 	  (progn
@@ -516,7 +539,7 @@
 	   (format-message
 	    "Clear verification not supported by `%s'.\n" mml2015-use)))
 	(mml2015-extract-cleartext-signature))
-      (list (mm-make-handle buf mm-uu-text-plain-type)))))
+      (list (mm-make-handle buf text-plain-type)))))
 
 (defun mm-uu-pgp-signed-extract ()
   (let ((mm-security-handle (list (substring "multipart/signed"))))
@@ -546,6 +569,7 @@
 
 (defun mm-uu-pgp-encrypted-extract-1 (_handles _ctl)
   (let ((buf (mm-uu-copy-to-buffer (point-min) (point-max)))
+	(text-plain-type mm-uu-text-plain-type)
 	(first t)
 	charset)
     ;; Make sure there's a blank line between header and body.
@@ -562,6 +586,10 @@
       (save-restriction
 	(narrow-to-region (point-min) (point))
 	(setq charset (mail-fetch-field "charset")))
+      (unless (and (stringp charset)
+		   (setq charset (intern (downcase charset)))
+		   (coding-system-p charset))
+	     (setq charset nil))
       (if (and (mm-uu-pgp-encrypted-test)
 	       (progn
 		 (mml2015-clean-buffer)
@@ -570,20 +598,14 @@
 							   'gnus-info)
 			"OK")))
 	  (progn
-	    ;; Decode charset.
-	    (if (and (or charset
-			 (setq charset gnus-newsgroup-charset))
-		     (setq charset (mm-charset-to-coding-system charset))
-		     (not (eq charset 'ascii)))
-		;; Assume that buffer's multibyteness is turned off.
-		;; See `mml2015-pgg-clear-decrypt'.
-		(insert (decode-coding-string (prog1
-						  (buffer-string)
-						(erase-buffer)
-						(mm-enable-multibyte))
-					      charset))
-	      (mm-enable-multibyte))
-	    (list (mm-make-handle buf mm-uu-text-plain-type)))
+	    ;; Decode charset. --- leave it to caller 
+	    ;; charset defaults to utf-8, but detect it for safety
+	    (unless charset
+	      (setq charset
+		    (detect-coding-region (point-min) (point-max) t)))
+	    (setq text-plain-type
+		  (list "text/plain" (cons 'charset charset)))
+	    (list (mm-make-handle buf text-plain-type)))
 	(list (mm-make-handle buf '("application/pgp-encrypted")))))))
 
 (defun mm-uu-pgp-encrypted-extract ()
--- lisp/mh-e/mh-mime.el-ORIG	2022-03-11 16:04:21.000000000 +0900
+++ lisp/mh-e/mh-mime.el	2022-04-23 15:21:16.229919000 +0900
@@ -514,6 +514,33 @@
 Optional argument, PRE-DISSECTED-HANDLES is a list of MIME
 handles. If present they are displayed otherwise the buffer is
 parsed and then displayed."
+
+  ;; when the message is only text/plain and base64/quoted-printable,
+  ;; decode it first for easy handling.
+  (unless pre-dissected-handles
+    (save-restriction
+     (let (ct cte)
+       (widen)
+       (narrow-to-region (point-min) (mh-mail-header-end))
+       (setq ct (ignore-errors (mail-header-parse-content-type
+                                (message-fetch-field "Content-Type" t)))
+             cte (message-fetch-field "Content-Transfer-Encoding" t))
+       (widen)
+       (when (stringp cte)
+         (setq cte (intern (downcase (mail-header-strip-cte cte)))))
+       (when (and (consp ct) (equal (car ct) "text/plain")
+                  (or (eq cte 'quoted-printable)
+                      (eq cte 'base64)))
+           (let ((case-fold-search t))
+             ;; fake Content-Transfer-Encoding
+             (goto-char (point-min))
+             (re-search-forward "^Content-Transfer-Encoding"
+                                (mh-mail-header-end) t)
+             (goto-char (line-beginning-position))
+             (insert "X-"))
+           (narrow-to-region (1+ (mh-mail-header-end)) (point-max))
+           (mm-decode-content-transfer-encoding cte "text/plain")))))
+
   (let ((handles ())
         (folder mh-show-folder-buffer)
         (raw-message-data (buffer-string)))
@@ -577,7 +604,7 @@
       (save-restriction
         (narrow-to-region (min (1+ (mh-mail-header-end)) (point-max))
                           (point-max))
-        (mm-decode-body charset
+        (mm-decode-body (or charset 'undecided)
                         (and cte (intern (downcase cte)))
                         (car ct))))))
 
@@ -1054,7 +1081,8 @@
      (when (and function (eolp))
        (backward-char))
      (unwind-protect (and function (funcall function data))
-       (set-buffer-modified-p nil)))))
+       (set-buffer-modified-p nil))))
+  (mh-show-addr))
 
 (defun mh-push-button (event)
   "Click MIME button for EVENT.
