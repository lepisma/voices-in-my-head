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

(require 'vimh-store)
(require 'vimh)

(defun vimh-git--store-path ()
  (let ((repo-root (locate-dominating-file default-directory ".git")))
    (if (null repo-root)
        (error "Not in a git repository")
      (concat repo-root ".vimh"))))

;;;###autoload
(defun vimh-git-setup ()
  "Setup vimh store in the current git project."
  (vimh-store-setup (vimh-git--store-path)))

(defun vimh-git-store-save ()
  (let ((store-path (vimh-git--store-path))
        (audio-blob (vimh-read-flight-stream)))
    (vimh-store-save store-path audio-blob "pcm")))

;;;###autoload
(defun vimh-git-insert ()
  (interactive)
  (let ((hash (vimh-git-store-save)))
    (insert ":vimh(\"" hash "\")")))

(provide 'vimh-git)

;;; vimh-git.el ends here
