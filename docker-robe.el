(require 'cl-lib)
(require 'inf-ruby)
(require 'robe)
(require 'advice)


(defvar docker-robe:container nil)
(defvar docker-robe:enabled nil)
(defvar docker-robe:source-map nil)


(defun docker-robe::exec (func cmd)
  (if (not (executable-find "docker"))
      (error "Not found 'docker' command. You sure docker has been installed.")
    (funcall func (format "docker %s" cmd))))

(defun docker-robe::select-container ()
  (let* ((cmd "ps --format='{{.Names}} {{.ID}}'")
         (container-id-of (cl-loop for line in (split-string (docker-robe::exec 'shell-command-to-string cmd) "\n")
                                   if (> (length line) 1)
                                   collect (apply 'cons (split-string line " "))))
         (container-names (mapc 'car container-id-of))
         (container-name (completing-read "select container: " container-names nil t nil '())))
    (assoc-default container-name container-id-of)))

(defun docker-robe::select-port (container)
  (let* ((cmd (format "inspect --format='{{range $conf := .NetworkSettings.Ports}}{{(index $conf 0).HostPort}} {{end}}' %s" container))
         (ports (split-string (docker-robe::exec 'shell-command-to-string cmd) " ")))
    (cond
     ((= (length ports) 0)
      (error "Not any ports bound to %s" container))
     ((= (length ports) 1)
      (nth 0 ports))
     ((> (length ports) 1)
      (completing-read "select port: " ports nil t nil '())))))

(defun docker-robe::source-map (container)
  (let* ((cmd (format "inspect --format='{{range $conf := .Mounts}}{{$conf.Destination}}:{{end}}' %s" container)))
    (cl-loop for remote-path in (split-string (docker-robe::exec 'shell-command-to-string cmd) ":")
             for local-path = (when (and (string-match "/" remote-path)
                                         (y-or-n-p (format "Local source is mounted on %s" remote-path)))
                                (directory-file-name (read-directory-name "Local source path : " nil nil t default-directory)))
             if local-path
             collect (cons remote-path local-path))))

(defun docker-robe::ensure-library (container)
  (let ((robe-library-dir (file-name-directory robe-ruby-path)))
    (docker-robe::exec 'shell-command (format "exec -it %s mkdir -p '%s'" container robe-library-dir))
    (docker-robe::exec 'shell-command (format "cp '%s' '%s:%s'" robe-ruby-path container robe-library-dir))))

(defun docker-robe::filter-response (endpoint res)
  (cond
    ((string= endpoint "class_locations")
     (let ((source-map (buffer-local-value 'docker-robe:source-map (inf-ruby-buffer))))
       (mapcar
        (lambda (remote-location)
          (cl-loop for (remote-path . local-path) in source-map
                   for re = (rx-to-string `(and bos ,remote-path))
                   for local-location = (replace-regexp-in-string re local-path remote-location)
                   if (not (string= remote-location local-location))
                   return local-location
                   finally return remote-location))
        res)))
    (t
     res)))


(defun docker-robe:dockerize (activate)
  (if activate
      (progn
        (ad-enable-regexp "docker-robe:dockerize")
        (ad-activate-regexp "docker-robe:dockerize"))
    (ad-disable-regexp "docker-robe:dockerize")
    (ad-deactivate-regexp "docker-robe:dockerize")))


(defadvice robe-start (around docker-robe:dockerize disable)
  (cond
   ((and robe-running
         (not (ad-get-arg 0))
         (get-buffer-process (inf-ruby-buffer)))
    ad-do-it)
   (t
    (let* ((docker-robe:enabled t)
           (docker-robe:container (docker-robe::select-container))
           (robe-host nil)
           (robe-port (docker-robe::select-port docker-robe:container))
           (source-map (docker-robe::source-map docker-robe:container)))
      (docker-robe::ensure-library docker-robe:container)
      ad-do-it
      (with-current-buffer (inf-ruby-buffer)
        (set (make-local-variable 'robe-port) robe-port)
        (set (make-local-variable 'docker-robe:source-map) source-map))))))

(defadvice robe-request (around docker-robe:dockerize disable)
  (let ((robe-host "127.0.0.1")
        (robe-port (buffer-local-value 'robe-port (inf-ruby-buffer))))
    ad-do-it
    (setq ad-return-value (docker-robe::filter-response (ad-get-arg 0) ad-return-value))))

(defadvice inf-ruby-console-run (around docker-robe:dockerize disable)
  (when (and docker-robe:enabled
             docker-robe:container)
    (ad-set-arg 0 (format "docker exec -it %s %s" docker-robe:container (ad-get-arg 0))))
  ad-do-it)


(provide 'docker-robe)
;;; docker-robe.el ends here
