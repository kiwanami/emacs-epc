;;; epc.el --- Emacs Procedure Call

;; Copyright (C) 2011  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile (require 'cl))
(require 'concurrent)



;;==================================================
;; Utility 

(defvar epc:debug-out nil)
(defvar epc:debug-buffer "*epc log*")

(setq epc:debug-out t)

(defun epc:log-init ()
  (when (get-buffer epc:debug-buffer)
    (kill-buffer epc:debug-buffer)))

(defun epc:log (&rest args)
  (when epc:debug-out
    (with-current-buffer
        (get-buffer-create epc:debug-buffer)
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n"))))

(defun epc:make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))


;;==================================================
;; Low Level Interface

(defvar epc:uid 1)

(defun epc:uid ()
  (incf epc:uid))

;; epc:connection structure
;;   name  : 
;;   process : 
;;   buffer : 
;;   channel  :

(defstruct epc:connection name process buffer channel)

(defun epc:connect (host port)
  "[internal] Connect the server, initialize the process and
return epc:connection object."
  (epc:log ">> Connection start: %s:%s" host port)
  (lexical-let* ((connection-id (epc:uid))
                 (connection-name (format "epc con %s" connection-id))
                 (connection-buf (epc:make-procbuf (format "*%s*" connection-name)))
                 (connection-process
                  (open-network-stream connection-name connection-buf host port))
                 (channel (cc:signal-channel connection-name))
                 (connection (make-epc:connection 
                              :name connection-name
                              :process connection-process
                              :buffer connection-buf
                              :channel channel)))
    (epc:log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process 
                        (lambda (p m)
                          (epc:process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e) 
                            (epc:process-sentinel connection p e)))
    connection))

(defun epc:process-sentinel (connection process msg)
  (epc:log "!! Process Sentinel [%s] : %S : %S"  
           (epc:connection-name connection) process msg)
  (epc:disconnect connection))

(defun epc:net-send (connection sexp)
  (let* ((msg (encode-coding-string 
               (concat (epc:prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (epc:net-encode-length (length msg)) msg))
         (proc (epc:connection-process connection)))
    (epc:log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun epc:disconnect (connection)
  (lexical-let
      ((process (epc:connection-process connection))
       (buf (epc:connection-buffer connection))
       (name (epc:connection-name connection)))
    (epc:log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (epc:log "!! Disconnected finished [%s]" name)))

(defun epc:process-filter (connection process message)
  (epc:log "INCOMING: [%s] [%S]" (epc:connection-name connection) message)
  (with-current-buffer (epc:connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (epc:process-available-input connection process)))

(defun epc:process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (epc:net-have-input-p)
      (let ((event (epc:net-read-or-lose process))
            (ok nil))
        (epc:log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'cc:signal-send 
                         (cons (epc:connection-channel connection) event))
                  (setq ok t))
              ('error (epc:log "MsgError: %S / <= %S" err event)))
          (unless ok
            (epc:run-when-idle 'epc:process-available-input process)))))))

(defun epc:net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (epc:net-decode-length))))

(defun epc:run-when-idle (function &rest args)
  "Call FUNCTION as soon as Emacs is idle."
  (apply #'run-at-time 
         (if (featurep 'xemacs) itimer-short-interval 0) 
         nil function args))

(defun epc:net-read-or-lose (process)
  (condition-case error
      (epc:net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun epc:net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (epc:net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length)) content)
    (assert (plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string 
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun epc:net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun epc:net-encode-length (n)
  "Encode an integer into a 24-bit hex string."
  (format "%06x" n))

(defun epc:prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length 
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))


;;==================================================
;; High Level Interface

;; epc:manager
;;   port       : port number
;;   server-process : process object for the peer
;;   connection : epc:connection instance
;;   methods    : alist of method (name . function)
;;   sessions   : alist of session (id . deferred)
(defstruct epc:manager server-process port connection methods sessions)

;; epc:method
;;   name       : method name (symbol)   ex: 'test
;;   task       : method function (function with one argument)
;;   arg-specs  : arg-specs (one string) ex: "(A B C D)"
;;   docstring  : docstring (one string) ex: "A test function. Return sum of A,B,C and D"
(defstruct epc:method name task docstring arg-specs)

(defvar epc:live-connections nil
  "[internal] A list of `epc:manager' objects those currently connect to the epc peer. 
This variable is for debug purpose.")

(defun epc:start-epc (server-prog server-args)
  "Start the epc server program and return an epc:manager object."
  (let ((mngr (epc:start-server server-prog server-args)))
    (epc:init-epc-layer mngr)
    mngr))

(defun epc:start-server (server-prog server-args)
  "[internal] Start a peer server and return an epc:manager instance which is set up partially."
  (let* ((process-name (format "epc:server:%s" (epc:uid)))
         (process-buffer (get-buffer-create (format " *%s*" process-name)))
         (process (apply 'start-process
                         process-name process-buffer 
                         server-prog server-args))
         (cont 1) port)
    (while cont
      (accept-process-output process 0.1)
      (let ((port-str (with-current-buffer process-buffer
                          (buffer-string))))
        (cond
         ((string-match "^[0-9]+$" port-str)
          (setq port (string-to-number port-str)
                cont nil))
         ((< 0 (length port-str))
          (error "Server may raise an error : %s" port-str))
         ((not (eq 'run (process-status process)))
          (setq cont nil))
         (t
          (incf cont)
          (when (< 30 cont) ; timeout 3 seconds
            (error "Timeout server response."))))))
    (make-epc:manager :server-process process
                      :port port
                      :connection (epc:connect "localhost" port))))

(defun epc:stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (epc:manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (epc:disconnect (epc:manager-connection mngr))
    (when buf (kill-buffer buf))))

(defun epc:start-epc-debug (port)
  "[internal] Return an epc:manager instance which is set up partially."
  (epc:init-epc-layer
   (make-epc:manager :server-process nil
                     :port port
                     :connection (epc:connect "localhost" port))))

(defun epc:args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond 
   ((atom args) args)
   (t (cadr args))))

(defun epc:init-epc-layer (mngr)
  "[internal] Connect to the server program and return an epc:connection instance."
  (lexical-let*
      ((mngr mngr)
       (conn (epc:manager-connection mngr))
       (channel (epc:connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (loop for (method . body) in
          `((call 
             . (lambda (args)
                 (epc:log "SIG CALL: %S" args)
                 (apply 'epc:handler-called-method ,mngr (epc:args args))))
            (return
             . (lambda (args)
                 (epc:log "SIG RET: %S" args)
                 (apply 'epc:handler-return ,mngr (epc:args args))))
            (return-error
             . (lambda (args)
                 (epc:log "SIG RET-ERROR: %S" args)
                 (apply 'epc:handler-return-error ,mngr (epc:args args))))
            (epc-error
             . (lambda (args)
                 (epc:log "SIG EPC-ERROR: %S" args)
                 (apply 'epc:handler-epc-error ,mngr (epc:args args))))
            (methods
             . (lambda (args)
                 (epc:log "SIG METHODS: %S" args)
                 (epc:handler-methods ,mngr (caadr args))))
            ) do
              (cc:signal-connect channel method body))
    mngr))



(defun epc:manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (epc:manager-connection mngr)))
    (epc:net-send conn (cons method messages))))

(defun epc:manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (loop for i in (epc:manager-methods mngr)
        if (eq method-name (epc:method-name i))
        do (return i)))

(defun epc:handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (loop for i in (epc:manager-methods mngr)
               collect 
               (list
                (epc:method-name i) 
                (or (epc:method-arg-specs i) "")
                (or (epc:method-docstring i) "")))))
    (epc:manager-send mngr 'return uid info)))
        
(defun epc:handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (lexical-let ((mngr mngr) (uid uid))
    (let* ((methods (epc:manager-methods mngr))
           (method (epc:manager-get-method mngr name)))
      (cond
       ((null method)
        (epc:log "ERR: No such method : %s" name)
        (epc:manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (epc:method-task method))
                   (ret (funcall f args)))
              (cond
               ((deferred-p ret)
                (deferred:nextc ret
                  (lambda (xx) (epc:manager-send mngr 'return uid xx))))
               (t (epc:manager-send mngr 'return uid ret))))
            (error 
             (epc:log "ERROR : %S" err)
             (epc:manager-send mngr 'return-error uid err))))))))

(defun epc:manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (loop with ret = nil
        for pair in (epc:manager-sessions mngr)
        unless (eq uid (car pair)) 
        do (push pair ret)
        finally 
        do (setf (epc:manager-sessions mngr) ret)))

(defun epc:handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (epc:manager-sessions mngr))))
    (cond
     (pair
      (epc:log "RET: id:%s [%S]" uid args)
      (epc:manager-remove-session mngr uid)
      (deferred:callback (cdr pair) args))
     (t ; error
      (epc:log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun epc:handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (epc:manager-sessions mngr))))
    (cond
     (pair
      (epc:log "RET-ERR: id:%s [%S]" uid args)
      (epc:manager-remove-session mngr uid)
      (deferred:errorback (cdr pair) (format "%S" args)))
     (t ; error
      (epc:log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun epc:handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (epc:manager-sessions mngr))))
    (cond
     (pair
      (epc:log "RET-EPC-ERR: id:%s [%S]" uid args)
      (epc:manager-remove-session mngr uid)
      (deferred:errorback (cdr pair) (list 'epc-error args)))
     (t ; error
      (epc:log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))



(defun epc:call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (epc:uid))
        (sessions (epc:manager-sessions mngr))
        (d (deferred:new)))
    (push (cons uid d) sessions)
    (setf (epc:manager-sessions mngr) sessions)
    (epc:manager-send mngr 'call uid method-name args)
    d))

(defun epc:define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-epc:method 
                  :name method-name :task task 
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (epc:manager-methods mngr))))
    (setf (epc:manager-methods mngr) methods)
    method))

(defun epc:query-methods-deferred (mngr)
  "Return a list of information for the peer's methods. 
The list is consisted of lists of strings: 
 (name arg-specs docstring)."
  (let ((uid (epc:uid))
        (sessions (epc:manager-sessions mngr))
        (d (deferred:new)))
    (push (cons uid d) sessions)
    (setf (epc:manager-sessions mngr) sessions)
    (epc:manager-send mngr 'methods uid)
    d))

(defun epc:call-method-sync (mngr method-name args)
  "Call peer's method with args synchronously, and return a result."
  (lexical-let ((result 'epc:nothing))
    (save-current-buffer
      (accept-process-output (epc:manager-server-process
                             sec msec t)))
    
  ))

(provide 'epc)
;;; epc.el ends here
