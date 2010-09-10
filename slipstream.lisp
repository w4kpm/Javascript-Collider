(defpackage slipstream
  (:use :common-lisp ))
(in-package slipstream)
(require 'parenscript)
(shadowing-import '(parenscript:false parenscript:true parenscript:@ parenscript:void parenscript:var))
(use-package 'parenscript)
(use-package 'ccl)
(require 'hunchentoot)
(shadowing-import '(hunchentoot:shutdown hunchentoot:remote-port))
(use-package 'hunchentoot)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (objc:load-framework "WebKit" :webkit))


(defun pathname-to-file-url (pathname)
  ;; NATIVE-TRANSLATED-NAMESTRING returns a simple string that can be
  ;; passed to a filesystem function.  (It may be exactly the same as
  ;; what NAMESTRING returns, or it may differ if special characters
  ;; were escaped in NAMESTRING's result.)
  (ccl::with-autorelease-pool
    (#/retain
     (#/fileURLWithPath: ns:ns-url (ccl::%make-nsstring
                                    (native-translated-namestring pathname))))))

(defun url-from-string (s)
  (ccl::with-autorelease-pool
    (#/retain (#/URLWithString: ns:ns-url (ccl::%make-nsstring (string s))))))
		  
(defparameter *jserrstate* nil)

(defclass javascript-error (ns:ns-object)
  ((jserror :foreign-type :id
	    :accessor jserror)
   (jserrdict :foreign-type :id
	      :accessor jserrdict))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/webView:addMessageToConsole: :void)
    ((self javascript-error) (wv :id) (dict :id))
  (setf *jserrstate* 1)
  (with-slots (jserror jserrdict) self

    (#/retain dict)
    (setf jserror (#/retain (#/objectForKey: dict (ccl::%make-nsstring "message"))))
    (setf jserrdict dict)))

(defparameter *jserrcatch* (make-instance 'javascript-error))




(defmacro run-in-other-thread (func &rest params)
  `(let* ((ip ccl::*initial-process*))
    (if (eq ccl::*current-process* ip)
	(,func ,@params)
      (let* ((s (make-semaphore))
	     retval
            )
        (process-interrupt ip (lambda ()
				(setf retval (,func ,@params))
                                (signal-semaphore s)))
        (wait-on-semaphore s)
        retval))))
(defun %browser-window (urlspec)
  (gui::assume-cocoa-thread)
  ;; Content rect for window, bounds rect for view.
  (ns:with-ns-rect (r 100.0 100.0 800.0 600.0)
    (ccl::with-autorelease-pool 
      (let* ((url (if (typep urlspec 'pathname)
                    (pathname-to-file-url urlspec)
                    (url-from-string urlspec)))
             ;; Create a window with titlebar, close & iconize buttons
             (w (make-instance
                 'ns:ns-window
                 :with-content-rect r
                 :style-mask (logior #$NSTitledWindowMask
                                     #$NSClosableWindowMask
                                     #$NSMiniaturizableWindowMask
                                     #$NSResizableWindowMask)
                 ;; Backing styles other than #$NSBackingStoreBuffered
                 ;; don't work at all in Cocoa.
                 :backing #$NSBackingStoreBuffered
                 :defer t)))
        (#/setTitle: w (#/absoluteString url))
        ;; Create a web-view instance,
        (let* ((v (make-instance
                   'ns:web-view
                   :with-frame r
                   :frame-name #@"frame" ; could be documented a bit better ...
                   :group-name #@"group"))) ; as could this
          ;; Make the view be the window's content view.
          (#/setContentView: w v)
          ;; Start a URL request.  The request is processed
          ;; asynchronously, but apparently needs to be initiated
          ;; from the event-handling thread.
          (let* ((webframe (#/mainFrame v))
                 (request (#/requestWithURL: ns:ns-url-request url)))
            ;; Failing to wait until the main thread has
            ;; initiated the request seems to cause
            ;; view-locking errors.  Maybe that's just
            ;; an artifact of some other problem.
            (#/loadRequest: webframe request)
            ;; Make the window visible & activate it
            ;; The view knows how to draw itself and respond
            ;; to events.
            (#/makeKeyAndOrderFront: w +null-ptr+))
          v)))))

(defun %reload (v)
  (gui::assume-cocoa-thread)
  ;; Content rect for window, bounds rect for view.
    (ccl::with-autorelease-pool 
          (let ((webframe (#/mainFrame v)))
	    (#/reload webframe))))


(defun %nav (v urlspec)
  (gui::assume-cocoa-thread)
  ;; Content rect for window, bounds rect for view.
    (ccl::with-autorelease-pool 
      (let* ((url (if (typep urlspec 'pathname)
                    (pathname-to-file-url urlspec)
                    (url-from-string urlspec))))
          (let* ((webframe (#/mainFrame v))
                 (request (#/requestWithURL: ns:ns-url-request url)))
            ;; Failing to wait until the main thread has
            ;; initiated the request seems to cause
            ;; view-locking errors.  Maybe that's just
            ;; an artifact of some other problem.
            (#/loadRequest: webframe request)))))

 

 (defun %js (v javascript)
   (let (output)
     (gui::assume-cocoa-thread)
     ;; Content rect for window, bounds rect for view.
     (ccl::with-autorelease-pool       
	(setf output (ccl::lisp-string-from-nsstring (#/stringByEvaluatingJavaScriptFromString: v (ccl::%make-nsstring javascript))))
	(if *jserrstate*
	    (progn 
	      (setf output (ccl::lisp-string-from-nsstring (jserror *jserrcatch*)))
	      (setf *jserrstate* nil)))	  	
	output)))




(defun loadingp (v)
    (#/isLoading v))



(defun wait-loading ()
  (loop while (loadingp *browser*)       
       do (progn 
	    (sleep .1))))

(defun reload-wait ()
  (reload)
  (wait-loading)
)

(defun nav-wait (url)
  (nav url)
  (wait-loading)
)


(defun js-exe (code)
;  (format t "in~%")
  (let (( x (run-in-other-thread %js *browser* code)))    
  x))

(defun load-js-uri (uri)
  (let ((js (format nil "
j=document.createElement(\"script\");
j.src=\"~A\";
j.type='text/javascript';
document.getElementsByTagName(\"HEAD\")[0].appendChild(j);
" uri)))
    (js-exe js)))

(defun nav (uri)
  (run-in-other-thread %nav *browser* uri))


(defun reload ()
  (run-in-other-thread %reload *browser* ))

(defun browser-window (urlspec)
  (setf *browser* (run-in-other-thread %browser-window  urlspec)))

(browser-window "http://www.mcdonald-consulting.net")

(#/setUIDelegate: *browser* *jserrcatch*)



(defvar *slipstream-server* nil)

(defun start-slipstream ()
 (setf *slipstream-server* (start (make-instance 'acceptor  :port 8204))))
(defun stop-slipstream ()
  (if *slipstream-server*
      (stop *slipstream-server*)))



(setq *dispatch-table*
      (nconc
       (list 'dispatch-easy-handlers
	     (create-prefix-dispatcher "/loadurl" 'loadurl)
	     (create-prefix-dispatcher "/reload-browser" 'reload-browser)
	     (create-prefix-dispatcher "/load-javascript-file" 'load-javascript-file)
	     (create-prefix-dispatcher "/run-javascript" 'run-javascript)
	     (create-prefix-dispatcher "/run-parenscript" 'run-parenscript)
	     
       (list #'default-dispatcher))))


(defun loadurl ()
  (let ((url (parameter "url")))
    (format t "~A" url)
    (nav-wait url)
    (concatenate 'string "loaded url: " url)))

(defun reload-browser ()
  (reload-wait)
  "browser reloaded"
  )


(defun load-javascript-file ()
  (let ((url (parameter "url")))
    (load-js-uri url)
    (concatenate 'string "loaded url: " url)))
(defun run-javascript ()
  (let ((code (parameter "code")))
    (format t "code was ~A~%" code)
    (js-exe code)))

(defun run-parenscript ()
  (let* ((code (parameter "code"))
	 (ps-form (read-from-string code))
	 (javascript (ps* ps-form)))
    (format t "parenscript code was ~A~%" javascript )   
    (js-exe javascript)))  


(start-slipstream)


