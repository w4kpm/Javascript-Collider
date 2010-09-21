(require 'drakma)
(use-package 'drakma)

(defparameter *browser* "http://localhost:8204")

(defun browser-request (location parameters)
  (let ((results
	 (multiple-value-list (http-request (concatenate 'string *browser*  location) 
		       :method :post 
		       :content-length t
		       :parameters parameters))))
	(if (eql (second results) 200)
	    (first results)
	    results)
	)  )


    (defun nav (url)
      (browser-request "/loadurl" `(("url" . ,url))))

    (defun reload ()
      (browser-request "/reload-browser" ()))

    (defun run-javascript (code)
      (browser-request "/run-javascript" `(("code" . ,code))))

    (defun run-parenscript (code)
      (browser-request "/run-parenscript" `(("code" . ,(format nil "~S" code)))))


    (defun load-javascript-file (url)
      (browser-request "/load-javascript-file" `(("url" . ,url))))



    (defun load-js-uri (uri)
      (load-javascript-file uri))

    (defun js-exe (code)
      (run-javascript code))


    (defun goto-uri (uri)
      (nav uri))

