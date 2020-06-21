;;; vimh-git.el --- VIMH git -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; VIMH git
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'git-commit)
(require 'esi-core)
(require 'vimh-store)

(defcustom vimh-git-voice-max-length 30
  "Maximum length of audio file in seconds"
  :type 'number)

(defvar vimh-git-voice-instream nil
  "Flight mode style audio stream for git projects.")

(defun vimh-git--store-path ()
  (let ((repo-root (locate-dominating-file default-directory ".git")))
    (if (null repo-root)
        (error "Not in a git repository")
      (concat repo-root ".vimh"))))

(defun vimh-git-setup ()
  "Setup vimh store in the current git project."
  (vimh-store-setup (vimh-git--store-path)))

(defun vimh-git-stop-instream ()
  (when vimh-git-voice-instream
    (esi-core--stop-background-recording vimh-git-voice-instream)
    (setq vimh-git-voice-instream nil)))

(defun vimh-git-start-instream ()
  (vimh-git-stop-instream)
  (setq vimh-git-voice-instream (esi-core--start-background-recording 16000 vimh-git-voice-max-length)))

;; TODO: The connection between commit and this voice is lost
(defun vimh-git-hook-fn ()
  (let ((store-path (vimh-git--store-path))
        (audio-blob (esi-core--read-background-recording-buffer vimh-git-voice-instream)))
    (vimh-store-save store-path audio-blob "pcm")))

;;;###autoload
(defun vimh-git-insert ()
  (interactive)
  (let ((hash (vimh-git-hook-fn)))
    (insert "#vimh(" hash ")")))

;; TODO: This will go as a minor mode later on
;;;###autoload
(defun vimh-git-enable ()
  (interactive)
  (vimh-git-setup)
  (vimh-git-start-instream))

;;;###autoload
(defun vimh-git-disable ()
  (interactive)
  (vimh-git-stop-instream))

(provide 'vimh-git)

;;; vimh-git.el ends here
