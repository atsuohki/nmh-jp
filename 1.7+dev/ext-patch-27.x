--- lisp/gnus/mml.el-ORIG	2021-01-29 02:52:38.000000000 +0900
+++ lisp/gnus/mml.el	2022-03-30 22:35:25.259063000 +0900
@@ -483,6 +483,19 @@
 	(options message-options))
     (if (not cont)
 	nil
+      ;; `charset=xxx' will be inserted unconditionally -- so, remove it
+      (if (and content-type
+	       (string-match "charset=" content-type))
+	  (setq content-type
+		(apply #'concat
+		       (mapcar #'(lambda (x)
+				   (cond
+				    ((stringp x) x)
+				    ((atom x) (symbol-name x))
+				    ((and (consp x) (not (eq (car x) 'charset)))
+				     (format "; %s=%s" (car x) (cdr x)))
+				    (t "")))
+			       (mail-header-parse-content-type content-type)))))
       (when (and (consp (car cont))
 		 (= (length cont) 1)
 		 content-type)
