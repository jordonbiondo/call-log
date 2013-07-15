;;; Call-log.el --- Log a message and where in code it came from
;; 
;; Filename: call-log.el
;; Description: Log messages with information about where in the source the message came from.
;; Author: Jordon Biondo
;; Maintainer: Jordon Biondo <biondoj@mail.gvsu.edu>
;; Created: Fri Jul 12 12:57:34 2013 (-0400)
;; Version: .1
;; Last-Updated: Mon Jul 15 11:35:53 2013 (-0400)
;;           By: jorbi
;;     Update #: 4
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

;;---------------------------------------------------------------------------
;; Output buffer
;;---------------------------------------------------------------------------  
(defvar clog/output-buffer "*Messages*"
  "The name of the buffer where all clog messages will be logged.")

(defun clog/-get-output-buffer()
  "Internal function used to return the buffer specified by `clog/output-buffer', if it does not exist, it will be created .If `clog/output-buffer' is nil, returns the *Messages* buffer."
  (let ((buf (get-buffer clog/output-buffer)))
    (if (and buf (bufferp buf)) buf
      (if clog/output-buffer
	  (generate-new-buffer (generate-new-buffer-name clog/output-buffer))
	(get-buffer "*Messages*")))))

;;---------------------------------------------------------------------------
;; Time stamp
;;---------------------------------------------------------------------------  
(defvar clog/time-format "%Y-%m-%dT%T%z"
  "Format of the time stamp on each log message. See `format-time-string' for details.")

(defun clog/-time-string()
  "Internal function used to return a the formated time string specifed by `clog/time-format'.
If `clog/time-format is nil, return the default ISO 8601 format"
  (let (time-str (format-time-string clog/time-format))
    (if time-str time-str (format-time-string "%Y-%m-%dT%T%z"))))

;;---------------------------------------------------------------------------
;; Public use
;;---------------------------------------------------------------------------  
;;;###autoload
(defun clog/msg(msg)
  "Writes MSG with call information to the `clog/-output-buffer'."
  (clog/-write (format "%s %s"
		      (clog/-color-text "clog/MSG:" 'font-lock-constant-face)
		      (clog/-make-string msg))))
;;;###autoload
(defun clog/bug(msg)
  "Writes MSG denated as a bug with call information to the `clog/-output-buffer'."
  (setq msg (propertize msg 'face 'font-lock-warning-face))
  (clog/-write (format "%s %s"
		      (clog/-color-text "clog/BUG:" 'font-lock-warning-face)
		      (clog/-make-string msg))))
;;;###autoload
(defun clog/todo(msg)
  "Writes MSG denated as a todo with call information to the `clog/-output-buffer'."
  (clog/-write (format "%s %s"
		      (clog/-color-text "clog/TODO:" 'font-lock-variable-name-face)
		      (clog/-make-string msg))))

;;---------------------------------------------------------------------------
;; Backtracing
;;---------------------------------------------------------------------------  

(defun clog/-up-to-func()
  "Returns the nearest backtrace frame that is a direct function call."
  ;; TODO make this less dumb
  (let ((func-frame) (height 11)) ;; 11 is when the frames move outside this lib...
    (while (progn (setq func-frame (backtrace-frame height))
		  (not (clog/-frame-is-direct-call-p func-frame)))
      (setq height (1+ height)))
    func-frame))

(defun clog/-frame-is-direct-call-p(frame)
  "Returns true is a backtrace frame is top level function call."
  (if (> (length frame) 1)
      (and (equal (first frame) t)
	   (fboundp (second frame)))))

(defun clog/-find-func-definition(frame)
  "Returns the name of the file containing the definition of the function that is being
called in the backtrace frame, FRAME."
  (if (and (fboundp (clog/-get-frame-func frame))
		    (symbol-file (clog/-get-frame-func frame)))
      (symbol-file (clog/-get-frame-func frame))
    (format "Not found, current buffer: %s" (buffer-name))))

(defun clog/-get-frame-func(frame)
  "Returns the symbol name of the function being called in FRAME"
  (if (> (length frame) 1)
      (second frame)
    nil))

(defun clog/-frame-func-name(val)
  "Returns the `symbol-name' of VAL if it is a function else an error message."
  (if (fboundp val) (symbol-name val) "*none*"))

;;---------------------------------------------------------------------------
;; Writing and Coloring
;;---------------------------------------------------------------------------  

(defun clog/-color-text(msg face)
  "Return the string MSG with the text properties of FACE."
  (propertize msg 'face face))

(defun clog/-make-string(msg)
  "Returns a formated string with a given message, MSG to be logged."
  (let ((frame (clog/-up-to-func)))
    (format "\"%s\"\n\t%s (%s)\n\t%s %s\n\t%s %s"
    	    msg
	    (clog/-color-text "func:" 'font-lock-keyword-face)
	    (clog/-color-text (clog/-frame-func-name (clog/-get-frame-func frame))
			     'font-lock-function-name-face)
	    (clog/-color-text "file:" 'font-lock-keyword-face)
	    (clog/-color-text (clog/-find-func-definition frame) 'font-lock-string-face)
	    (clog/-color-text "time:" 'font-lock-keyword-face)
	    (clog/-color-text (clog/-time-string) 'default))))

(defun clog/-write(msg)
  "Writes the string MSG to the end of the `clog/-output-buffer'."
  (with-current-buffer (clog/-get-output-buffer)
    (goto-char (point-max))
    (insert (format "\n%s\n" msg))))

;;---------------------------------------------------------------------------
;; Provide it
;;---------------------------------------------------------------------------  
(provide 'call-log)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; call-log.el




;; Say we need a reminder to come back and work on a certain function
