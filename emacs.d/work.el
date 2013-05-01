(defun work:connect-to-irc()
  "Connect to the REA IRC server"
  (interactive)
  (erc :server "ops01.int.realestate.com.au" :nick "dbayne" :full-name "Duncan Bayne"))

(global-set-key (kbd "C-x e") 'work:connect-to-irc)
