;;; docker-robe.el --- Let robe work under docker container

;; Copyright (C) 2017  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: ruby convenience docker
;; URL: https://github.com/aki2o/emacs-docker-robe
;; Version: 0.3.0
;; Package-Requires: ((inf-ruby "2.5.0") (robe "0.7.9") (docker-tramp "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; You'll be able to develop using robe under docker container in the following conditions.
;; 
;;  - Developing in host with volume mounted to the docker container which runs the product code.
;;  - The docker container expose a port for docker-robe.el
;;  - Host configures a port forwarding to the docker container port.
;;  - It's able to copy `robe-ruby-path' into the docker container same path using docker command.
;; 
;; For more infomation, see <https://github.com/aki2o/emacs-docker-robe/blob/master/README.md>

;;; Dependencies:
;; 
;; - inf-ruby.el ( see <https://github.com/nonsequitur/inf-ruby> )
;; - robe.el ( see <https://github.com/dgutov/robe> )
;; - docker-tramp.el ( see <https://github.com/emacs-pe/docker-tramp.el> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'docker-robe)
;; (docker-robe:activate)

;;; Configuration:
;; 
;; Nothing

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "docker-robe:" :docstring t)
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'macro :prefix "docker-robe:" :docstring t)
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "docker-robe:" :docstring t)
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "docker-robe:" :docstring t)
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.5.1 (x86_64-apple-darwin14.5.0, NS apple-appkit-1348.17) of 2016-06-16 on 192.168.102.190
;; - docker ... Docker version 1.12.6, build 78d1802
;; - inf-ruby.el ... Version 2.5.0
;; - robe.el ... Version 0.7.9
;; - docker-tramp.el ... Version 0.1


;; Enjoy!!!


;;; Code:
(eval-when-compile (require 'cl))
(require 'inf-ruby)
(require 'robe)
(require 'docker-tramp)
(require 'docker-tramp-compat)
(require 'projectile nil t)
(require 'advice)

(defgroup docker-robe nil
  "Let robe work under docker container."
  :group 'convenience
  :prefix "docker-robe:")

(defcustom docker-robe:project-cache-file (concat user-emacs-directory ".docker-robe-project")
  "Filepath stores project configuration."
  :type 'string
  :group 'docker-robe)

(defcustom docker-robe:project-root-detect-function 'projectile-project-root
  "Function detect project root path for `current-buffer'."
  :type 'symbol
  :group 'docker-robe)


(defvar docker-robe:enabled nil)
(defvar docker-robe:project-root nil)
(defvar docker-robe:project-cache-hash nil)
(defvar docker-robe:container nil)
(defvar docker-robe:volume-local-path-alist nil)
(defvar docker-robe:entrypoint nil)


;;;;;;;;;;;;;
;; Utility

(defun docker-robe::exec (func cmd)
  (if (not (executable-find "docker"))
      (error "Not found 'docker' command. You sure docker has been installed.")
    (funcall func (format "docker %s" cmd))))

(defun docker-robe::container-name (container)
  (let ((cmd (format "ps --format='{{.Names}}' -f 'id=%s'" container)))
    (replace-regexp-in-string "\n" "" (docker-robe::exec 'shell-command-to-string cmd))))

(defun docker-robe::container-id (container-name)
  (let ((cmd (format "ps --format='{{.ID}}' -f 'name=%s'" container-name)))
    (replace-regexp-in-string "\n" "" (docker-robe::exec 'shell-command-to-string cmd))))

(defun docker-robe::docker-tramp-path-prefix (container)
  (format "/docker:%s:" (if docker-tramp-use-names
                            (docker-robe::container-name container)
                          container)))


;;;;;;;;;;;
;; Cache

(defun docker-robe::project-cached-value (cache-name)
  (when docker-robe:project-root
    (plist-get (docker-robe::ensure-project-cache)
               cache-name)))

(defun docker-robe::ensure-project-cache ()
  (gethash docker-robe:project-root (docker-robe::ensure-project-cache-hash)))

(defun docker-robe::ensure-project-cache-hash ()
  (or docker-robe:project-cache-hash
      (setq docker-robe:project-cache-hash
            (or (docker-robe::load-project-cache-hash)
                (make-hash-table :test 'equal)))))

(defun docker-robe::load-project-cache-hash ()
  (when (file-exists-p docker-robe:project-cache-file)
    (read (with-temp-buffer
            (insert-file-contents docker-robe:project-cache-file)
            (buffer-string)))))

(defun* docker-robe::store-project-cache (&key container port volume-local-path-alist entrypoint)
  (when docker-robe:project-root
    (puthash docker-robe:project-root
             `(:container ,container :port ,port :volume-local-path-alist ,volume-local-path-alist, :entrypoint ,entrypoint)
             (docker-robe::ensure-project-cache-hash))
    (with-temp-buffer
      (insert (prin1-to-string docker-robe:project-cache-hash))
      (write-file docker-robe:project-cache-file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config Docker Environment

(defun* docker-robe::select-container (&key (use-cache nil))
  (or (when use-cache (docker-robe::project-cached-value :container))
      (let* ((cmd "ps --format='{{.Names}} {{.ID}}'")
             (res (docker-robe::exec 'shell-command-to-string cmd))
             (container-id-alist (loop for line in (split-string res "\n")
                                       if (> (length line) 1)
                                       collect (apply 'cons (split-string line " "))))
             (container-names (mapc 'car container-id-alist))
             (container-name (completing-read "select container: " container-names nil t nil '())))
        (assoc-default container-name container-id-alist))))

(defun* docker-robe::select-port (container &key (use-cache nil))
  (or (when use-cache (docker-robe::project-cached-value :port))
      (let* ((cmd (format "inspect --format='{{range $conf := .NetworkSettings.Ports}}{{(index $conf 0).HostPort}} {{end}}' %s" container))
             (ports (split-string (docker-robe::exec 'shell-command-to-string cmd) " ")))
        (cond
         ((= (length ports) 0)
          (error "Not any ports bound to %s" container))
         ((= (length ports) 1)
          (nth 0 ports))
         ((> (length ports) 1)
          (completing-read "select port: " ports nil t nil '()))))))

(defun* docker-robe::select-volume-local-path (container &key (use-cache nil))
  (or (when use-cache (docker-robe::project-cached-value :volume-local-path-alist))
      (let* ((cmd (format "inspect --format='{{range $conf := .Mounts}}{{if eq $conf.Type \"bind\"}}{{$conf.Destination}}:{{end}}{{end}}' %s" container)))
        (loop for remote-path in (split-string (docker-robe::exec 'shell-command-to-string cmd) ":")
              for local-path = (when (and (string-match "/" remote-path)
                                          (y-or-n-p (format "Local source is mounted on %s" remote-path)))
                                 (directory-file-name (read-directory-name "Local source path : " nil nil t default-directory)))
              if local-path
              collect (cons remote-path local-path)))))

(defun* docker-robe::select-entrypoint (container &key (use-cache nil))
  (or (when use-cache (docker-robe::project-cached-value :entrypoint))
      (let* ((docker-tramp-path-prefix (docker-robe::docker-tramp-path-prefix container))
             (selected-value (read-file-name "select entrypoint: " (concat docker-tramp-path-prefix "/"))))
        (when (and selected-value
                   (> (length selected-value) (length docker-tramp-path-prefix)))
          (substring selected-value (length docker-tramp-path-prefix))))))

(defun docker-robe::ensure-library (container)
  (let ((robe-library-dir (shell-quote-argument (file-name-directory robe-ruby-path))))
    (docker-robe::exec 'shell-command (format "exec -i %s mkdir -p %s" container robe-library-dir))
    (docker-robe::exec 'shell-command (format "cp %s %s:%s" (shell-quote-argument robe-ruby-path) container robe-library-dir))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling robe response

(defun docker-robe::filter-response (endpoint res)
  (if docker-robe:container
      (cond
       ((string= endpoint "class_locations")
        (mapcar 'docker-robe::find-accessible-path-for res))
       ((string= endpoint "method_targets")
        (mapcar
         (lambda (method-definition)
           (multiple-value-bind (classnm flg methodnm args path row-index) method-definition
             `(,classnm ,flg ,methodnm ,args ,(docker-robe::find-accessible-path-for path) ,row-index)))
         res))
       (t
        res))
    res))

(defun docker-robe::find-accessible-path-for (remote-path)
  (when remote-path
    (loop for (remote-map . local-map) in (buffer-local-value 'docker-robe:volume-local-path-alist (inf-ruby-buffer))
          for re = (rx-to-string `(and bos ,remote-map))
          for local-path = (replace-regexp-in-string re local-map remote-path)
          if (not (string= remote-path local-path))
          return local-path
          finally return (concat (docker-robe::docker-tramp-path-prefix docker-robe:container) remote-path))))


;;;;;;;;;;;;
;; Advice

(defadvice robe-start (around docker-robe:dockerize disable)
  (cond
   ((and robe-running
         (not (ad-get-arg 0))
         (get-buffer-process (inf-ruby-buffer)))
    ad-do-it)
   (t
    (let* ((docker-robe:enabled t)
           (docker-robe:project-root (when (functionp docker-robe:project-root-detect-function)
                                       (funcall docker-robe:project-root-detect-function)))
           (docker-robe:container (docker-robe::select-container :use-cache t))
           (robe-host nil)
           (robe-port (docker-robe::select-port docker-robe:container :use-cache t))
           (volume-local-path-alist (docker-robe::select-volume-local-path docker-robe:container :use-cache t))
           (docker-robe:entrypoint (docker-robe::select-entrypoint docker-robe:container :use-cache t)))
      (docker-robe::store-project-cache :container docker-robe:container
                                        :port robe-port
                                        :volume-local-path-alist volume-local-path-alist
                                        :entrypoint docker-robe:entrypoint)
      (docker-robe::ensure-library docker-robe:container)
      ad-do-it
      (with-current-buffer (inf-ruby-buffer)
        (set (make-local-variable 'docker-robe:container) docker-robe:container)
        (set (make-local-variable 'robe-port) robe-port)
        (set (make-local-variable 'docker-robe:volume-local-path-alist) volume-local-path-alist)
        (set (make-local-variable 'docker-robe:entrypoint) docker-robe:entrypoint))))))

(defadvice robe-request (around docker-robe:dockerize disable)
  (let ((robe-host "127.0.0.1")
        (robe-port (buffer-local-value 'robe-port (inf-ruby-buffer)))
        (docker-robe:container (buffer-local-value 'docker-robe:container (inf-ruby-buffer)))
        (docker-robe:entrypoint (buffer-local-value 'docker-robe:entrypoint (inf-ruby-buffer))))
    ad-do-it
    (setq ad-return-value (docker-robe::filter-response (ad-get-arg 0) ad-return-value))))

(defadvice inf-ruby-console-run (around docker-robe:dockerize disable)
  (when (and docker-robe:enabled
             docker-robe:container)
    (ad-set-arg 0 (format "docker exec -it %s %s %s" docker-robe:container (or docker-robe:entrypoint "") (ad-get-arg 0))))
  ad-do-it)


;;;;;;;;;;;;;;;;;;;
;; User Function

;;;###autoload
(defun docker-robe:activate ()
  "Activate docker-robe advices."
  (ad-enable-regexp "docker-robe:dockerize")
  (ad-activate-regexp "docker-robe:dockerize"))

;;;###autoload
(defun docker-robe:deactivate ()
  "Deactivate docker-robe advices."
  (ad-disable-regexp "docker-robe:dockerize")
  (ad-deactivate-regexp "docker-robe:dockerize"))

;;;###autoload
(defun docker-robe:configure-current-project ()
  "Configure project has `current-buffer'."
  (interactive)
  (let* ((docker-robe:project-root (or (when (functionp docker-robe:project-root-detect-function)
                                         (funcall docker-robe:project-root-detect-function))
                                       (error "Can't detect project root path for %s" (buffer-file-name))))
         (container (docker-robe::select-container))
         (port (docker-robe::select-port container))
         (volume-local-path-alist (docker-robe::select-volume-local-path container))
         (entrypoint (docker-robe::select-entrypoint container)))
    (docker-robe::store-project-cache :container container
                                      :port port
                                      :volume-local-path-alist volume-local-path-alist
                                      :entrypoint entrypoint)))


(provide 'docker-robe)
;;; docker-robe.el ends here
