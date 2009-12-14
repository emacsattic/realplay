;;; realplay.el --- Interface with Real Player

;;; Commentary:

;;; THANKS:
;; Annie Nightingagle, Bobby Friction and Nihal, Rob Da Bank and the
;; belated John Peel for great music

;;; BUGS:

;;; INSTALLATION:
;; Realplay support:
;; (add-hook 'w3m-load-hook 'realplay-w3m-init)
;; Open http://kanis.fr/radio.html in w3m for some stream sample

;;; TODO
;; implement set functions
;; resume play after quitting

;;; Code:

;; shut up compile warning
(eval-when-compile
  (defvar w3m-content-type-alist))

(defcustom realplay-executable "realplay"
  "The name of the Real Player executable
This can be either absolute or looked up on `exec-path'"
  :type 'file
  :group 'realplay)

(defcustom realplay-timeout 1
  "Timeout in seconds waiting for Real Player process output"
  :type 'number
  :group 'realplay)

;;;###autoload
(defvar realplay-map nil
  "Keymap containing bindings to realplay functions.
It is not bound to any key by default: to bind it
so that you have a bookmark prefix, just use `global-set-key' and bind a
key of your choice to `realplay-map'.")

;;;###autoload (define-prefix-command 'realplay-map)

;; Read the help on all of these functions for details...
;;;###autoload (define-key realplay-map "p" 'realplay-play-pause)
;;;###autoload (define-key realplay-map "f" 'realplay-fast-forward)
;;;###autoload (define-key realplay-map "b" 'realplay-rewind)
;;;###autoload (define-key realplay-map "q" 'realplay-shutdown)

(defvar realplay-skip 5
  "Number of minutes to skip when doing a rewind or fast forward")

(defconst realplay-process-name "realplay"
  "Name of the process")

(defconst realplay-process-buffer "*realplay*"
  "Name of the process buffer")

(defvar realplay-output nil
  "Output of process are stored here")

;;;###autoload
(defun realplay-url (url)
  "Play URL"
  (interactive "sPlease enter URL to play: ")
  (let ((process (get-buffer-process realplay-process-buffer)))
    (when process
      (realplay-stop)
      (delete-process process)))
  (realplay-start-process)
  (realplay-embed (concat "src=" url))
  (realplay-play))

;;;###autoload
(defun realplay-w3m-init ()
  "Initialize realplay in w3m."
  (realplay-w3m
   '(("audio/x-pn-realaudio-plugin" "\\.rpm\\'" realplay-url nil)
     ("audio/x-mpegurl" "\\.m3u\\'" realplay-url nil)
     ("audio/x-pn-realaudio" "\\.ram\\'" realplay-url nil)
     ("audio/x-scpls" "\\.pls\\'" realplay-url nil)))
  (message "Realplay in w3m initialized."))

(defun realplay-w3m (list)
  "Append LIST at the end of w3m-content-type-alist"
  (setq w3m-content-type-alist
        (append w3m-content-type-alist list)))

;;;###autoload
(defun realplay-fast-forward (&optional minute)
  "Fast forward MINUTE.
If MINUTE is not specified use `realplay-skip'"
  (interactive "P")
  (realplay-move-forward (* 60000 (or minute realplay-skip))))

;;;###autoload
(defun realplay-rewind (&optional minute)
  "Rewind by MINUTE.
If MINUTE is not specified use `realplay-skip'"
  (interactive "P")
  (realplay-move-backward (* 60000 (or minute realplay-skip))))

(defun realplay-move-forward (increment)
  "Fast forward by INCREMENT in millisecond"
  (realplay-seek
   (let ((position (+ (realplay-get-int "position") increment))
         (length (realplay-get-int "length")))
     (if (> position length) length position))))

(defun realplay-move-backward (increment)
  "Rewind by INCREMENT in 100th of a second"
  (realplay-seek
   (let ((position (- (realplay-get-int "position") increment)))
     (if (> position 0) position 0))))

;;;; Process functions

(defun realplay-start-process ()
  (start-process realplay-process-name
                 realplay-process-buffer
                 realplay-executable "-e" "1")
  (set-process-filter  (get-process realplay-process-name)
                       'realplay-process-filter))

(defun realplay-command (string)
  "Send command STRING returns output string, nil indicates failure"
  (setq realplay-output nil)
  (process-send-string realplay-process-buffer (concat string "\n"))
  (while (or (not (accept-process-output
                   (get-buffer-process realplay-process-buffer)
                   realplay-timeout))
             (eq realplay-output nil)))
  (if (and (stringp realplay-output)
           (eq (aref realplay-output 0) ?0))
      realplay-output nil))

(defun realplay-process-filter (proc string)
  (when (string-match "^[0-9-].*" string)
    (setq realplay-output (match-string 0 string)))
  ;; Insert the text, advancing the process marker.
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point))
        (if moving (goto-char (process-mark proc)))))))

;;;; Real Player functions

(defun realplay-embed (parameter)
  "Run Embed with PARAMETER string, return channel integer"
  (let ((channel (realplay-command (concat "Embed " parameter))))
    (when (stringp channel)
      (string-to-number channel))))

(defun realplay-send-command (command &optional channel arg)
  "Send COMMAND on CHANNEL, returns t on success
CHANNEL defaults to 0 if it is not specified."
  (unless channel
    (set 'channel 0))
  (let* ((argument (cond ((stringp arg) arg)
                         ((numberp arg) (int-to-string arg))
                         ((eq arg nil) "")
                         (t (error "Wrong argument"))))
         (output (realplay-command
                  (concat command " " (int-to-string channel)
                          " " argument))))
    (stringp output)))

(defun realplay-play (&optional channel)
  "Send Play command on CHANNEL, returns t on success"
  (realplay-send-command "Play" channel))

(defun realplay-seek (position &optional channel)
  "Execute Seek command on CHANNEL at POSITION, returns t on success"
  (realplay-send-command "Seek" channel position))

;;;###autoload
(defun realplay-play-pause (&optional channel)
  "Send PlayPause command on CHANNEL, returns t on success"
  (interactive)
  (realplay-send-command "PlayPause" channel))

(defun realplay-pause (&optional channel)
  "Send Pause command on CHANNEL, returns t on success"
  (realplay-send-command "Pause" channel))

;;;###autoload
(defun realplay-shutdown (&optional channel)
  (interactive "P")
  "Send Shutdown command on CHANNEL, returns t on success"
  (realplay-send-command "Shutdown" (or channel 0)))

(defun realplay-stop (&optional channel)
  "Send Stop command on CHANNEL, returns t on success"
  (realplay-send-command "Stop" channel))

(defun realplay-set-xid (xid &optional channel)
  "Send SetXID command on CHANNEL for X windows XID
You can embed realplayer control in a frame if emacs is compiled
with GTK"
  (realplay-send-command "SetXID" channel xid))

(defun realplay-set-window (socket &optional channel)
  "Set SOCKET id on CHANNEL, returns t on success"
  (realplay-send-command "SetWindow" channel socket))

(defun realplay-unset-window (&optional channel)
  "Send UnsetWindow command on CHANNEL, returns t on success"
  (realplay-send-command "UnsetWindow" channel))

(defun realplay-new-stream (id url mime-type length &optional channel)
  "NewStream on CHANNEL with arguments ID URL MIME-TYPE and LENGTH"
  (realplay-send-command "NewStream"
                         (concat (int-to-string id) " " url " "
                                 mime-type " " (int-to-string length))
                         channel))

(defun realplay-stream-data (id length &optional channel)
  "StreamData on CHANNEL with arguments ID and LENGTH"
  (realplay-send-command "StreamData"
                         (concat (int-to-string id) " "
                                 (int-to-string length)) channel))

(defun realplay-stream-done (id &optional channel)
  "StreamDone on CHANNEL with argument ID, returns t on success"
  (realplay-stream-done "StreamDone" id channel))

(defun realplay-version (version)
  "Send Version number, returns version number of ipcplayer"
  (let ((output (realplay-command (concat "Version "
                                          (int-to-string version)))))
    (when (stringp output)
      (string-to-number (cadr (split-string output ", "))))))

(defun realplay-get-property (command property &optional channel)
  "Return second value of COMMAND on PROPERTY in CHANNEL
CHANNEL defaults to 0 if it is not specified."
  (unless channel
    (set 'channel 0))
  (let ((output
         (realplay-command
          (concat command " " (int-to-string channel) " " property))))
    (when (stringp output)
      (cadr (split-string output ", ")))))

(defun realplay-get-bool (property &optional channel)
  "Return boolean PROPERTY in CHANNEL, nil indicates failure
List of boolean properties: aboutdlg autogotourl autostart
canpause canplay canplaypause canseek canstop center
consoleevents contextmenu donext dopause doplay doplaypause
doprev dostop doublesize enabledblsz enablefullscr enablemsgbox
enableorigsz errorevents fullscreen hasnext hasprev imagestatus
isplus keyevents live loop maintainaspect mouseevents mute nologo
originalsize prefetch prefsdlg shuffle statsdlg stereo"
  (realplay-get-int property channel))

(defun realplay-get-int (property &optional channel)
  "Return integer PROPERTY in CHANNEL, nil indicates failure
List of integer properties: bufelapsed bufremaining bwavg bwconn
bwcur countryid curentry errorrmacode errorseverity errorusercode
height langid length numentries numloops numsources pktsearly
pktslate pktsmissing pktsorder pktsrecv pktstotal position volume
width playstate"
  (let ((string (realplay-get-property
                 "GetPlayerUINT32Prop" property channel)))
    (when string (string-to-number string))))

(defun realplay-get-string (property &optional channel)
  "Return string PROPERTY in CHANNEL, nil indicates failure
CHANNEL defaults to 0 if it is not specified.
List of string properties: author backgroundcolor console
controls copyright entryabstract entryauthor entrycopyright
entrytitle errorrmastr errorurl erroruserstr langstr laststatus
region src srctrans title version"
  (let ((string (realplay-get-property
                 "GetPlayerStringProp" property channel)))
    ;; remove quotes
    (when string (substring string  1 -1))))

(provide 'realplay)

;; Copyright (C) 2007 Ivan Kanis
;; Author: Ivan Kanis
;; $Id: realplay.el 2443 2009-04-20 19:27:59Z ivan $
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

