;;; jabber-emoicon.el --- 
;; 
;; Filename: jabber-emoicon.el
;; Description: 
;; Author: Christian
;; Maintainer: 
;; Created: vie oct 28 00:52:49 2011 (-0300)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Loading this library makes no need to activate the minor mode.
;; This library loads everythings it is needed
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


					; JABBER Considerations
;; Conciderar Jabber!

(require 'emotion-iconous-mode)

;; Es necesario reterirar el [enter] del modo menor `emotion-iconous-mode'
;;(define-key emotion-iconous-mode-map [return] nil)

;; Usar C-Ret para enviar
(eval-and-compile
  (define-key jabber-chat-mode-map [C-return] 'jabber-chat-buffer-send)
  (add-hook 'jabber-alert-message-hooks 'emoicon-buffer-jabber)
  )

(defun emoicon-jabber-buffer ()
  "Replace every text into emoticon if possible. This is intended for Jabber-chat buffers!"
  (interactive)
    (let ((inhibit-read-only t))
      (emoicon-buffer)
      )
    )

(defun emoicon-buffer-jabber (from buffer text title)
  (with-current-buffer buffer
    (emoicon-jabber-buffer)
    )
  )

(defadvice jabber-chat-buffer-send (after jabber-chat-emoicon-buffer-send ())
  "Changes emoticons with `emoicon-jabber-buffer' as soon a message is sended..."
  (emoicon-jabber-buffer)
  )

(eval-and-compile
  (ad-activate 'jabber-chat-buffer-send)
  )


(provide 'jabber-emoicon)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jabber-emoicon.el ends here
