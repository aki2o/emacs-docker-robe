;;; docker-robe.el --- Let robe work under docker container

;; Copyright (C) 2017  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: ruby convenience docker
;; URL: https://github.com/aki2o/emacs-docker-robe
;; Version: 0.0.1
;; Package-Requires: ((inf-ruby "2.5.0") (robe "0.7.9"))

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


;; Enjoy!!!


;;; Code:
(eval-when-compile (require 'cl))
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
         (container-id-of (loop for line in (split-string (docker-robe::exec 'shell-command-to-string cmd) "\n")
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
    (loop for remote-path in (split-string (docker-robe::exec 'shell-command-to-string cmd) ":")
          for local-path = (when (and (string-match "/" remote-path)
                                      (y-or-n-p (format "Local source is mounted on %s" remote-path)))
                             (directory-file-name (read-directory-name "Local source path : " nil nil t default-directory)))
          if local-path
          collect (cons remote-path local-path))))

(defun docker-robe::ensure-library (container)
  (let ((robe-library-dir (shell-quote-argument (file-name-directory robe-ruby-path))))
    (docker-robe::exec 'shell-command (format "exec -i %s mkdir -p %s" container robe-library-dir))
    (docker-robe::exec 'shell-command (format "cp %s %s:%s" (shell-quote-argument robe-ruby-path) container robe-library-dir))))

(defun docker-robe::local-path-mapped (remote-path)
  (loop for (remote-map . local-map) in docker-robe:source-map
        for re = (rx-to-string `(and bos ,remote-map))
        for local-path = (replace-regexp-in-string re local-map remote-path)
        if (not (string= remote-path local-path))
        return local-path
        finally return remote-path))

(defun docker-robe::filter-response (endpoint res)
  (cond
    ((string= endpoint "class_locations")
     (let ((docker-robe:source-map (buffer-local-value 'docker-robe:source-map (inf-ruby-buffer))))
       (mapcar 'docker-robe::local-path-mapped res)))
    ((string= endpoint "method_targets")
     (let ((docker-robe:source-map (buffer-local-value 'docker-robe:source-map (inf-ruby-buffer))))
       (mapcar
        (lambda (method-definition)
          (multiple-value-bind (classnm flg methodnm args path row-index) method-definition
            `(,classnm ,flg ,methodnm ,args ,(docker-robe::local-path-mapped path) ,row-index)))
        res)))
    (t
     res)))


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
