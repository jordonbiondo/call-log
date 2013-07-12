;;; trace-it.el --- Log a message and where in code it came from
;; 
;; Filename: call-log.el
;; Description: Log messages with information about where in the source the message came from.
;; Author: Jordon Biondo
;; Maintainer: Jordon Biondo <biondoj@mail.gvsu.edu>
;; Created: Fri Jul 12 12:57:34 2013 (-0400)
;; Version: .1
;; Last-Updated: Fri Jul 12 18:50:31 2013 (-0400)
;;           By: jorbi
;;     Update #: 3
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(defun clog/msg(msg &optional out-buffer)
  (clog/write (format "%s %s"
		     (clog/-color-text "clog/MSG:" 'font-lock-constant-face)
		     (clog/make-string msg)) out-buffer))

(defun clog/bug(msg &optional out-buffer)
  (clog/write (format "%s %s"
		     (clog/-color-text "clog/BUG:" 'font-lock-warning-face)
		     (clog/make-string msg)) out-buffer))

(defun clog/todo(msg &optional out-buffer)
  (clog/write (format "%s %s"
		     (clog/-color-text "clog/TODO:" 'font-lock-variable-name-face)
		     (clog/make-string msg)) out-buffer))


(defun clog/up-to-func()
  "Returns the nearest backtrace frame that is a direct function call."
  (let ((func-frame) (height 11)) ;; 10 is when the frames move outside this lib, ;; TODO make this less dumb
    (while (progn (setq func-frame (backtrace-frame height))
		  (not (clog/frame-is-direct-call-p func-frame)))
      (setq height (1+ height)))
    func-frame))

(defun clog/frame-is-direct-call-p(frame)
  "Returns true is a backtrace frame is top level function call."
  (if (> (length frame) 1)
      (and (equal (first frame) t)
	   (fboundp (second frame)))))

(defun clog/find-func-definition(frame)
  "Returns the name of the file containing the definition of the function that is being
called in the backtrace frame, FRAME."
  (symbol-file (clog/get-frame-func frame)))


(defun clog/get-frame-func(frame)
  "Returns the symbol name of the function being called in FRAME"
  (if (> (length frame) 1)
      (second frame)
    nil))

(defun clog/make-string(msg)
  "Returns a formated string with a given message, MSG to be logged."
  (let ((frame (clog/up-to-func)))
    (format "\"%s\"\n\t%s (%s)\n\t%s %s"
    	    msg
	    (clog/-color-text "func:" 'font-lock-keyword-face)
	    (clog/-color-text (symbol-name (clog/get-frame-func frame))
			     'font-lock-function-name-face)
	    (clog/-color-text "file:" 'font-lock-keyword-face)
	    (clog/-color-text (clog/find-func-definition frame) 'font-lock-string-face))))
			     
(defun clog/-color-text(msg face)
  (propertize msg 'face face))

(defun clog/write(msg &optional buffer)
  (if (not buffer) (setq buffer (get-buffer "*Messages*")))
  (if buffer
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert (format "\n%s\n" msg)))
    (princ "tracker buffer error")))



(provide 'call-log)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; trace-it.el ends here
