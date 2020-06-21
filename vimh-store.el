;;; vimh-store.el --- Storage for vimh -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Storage for vimh
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

(defvar vimh-store-db-file-name "data.sqlite"
  "Default file name of the sqlite database.")

(defun vimh-store--setup-db (db-file-path)
  "Create sqlite db at given path."
  (call-process "sqlite3" nil nil nil db-file-path "CREATE TABLE IF NOT EXISTS voices (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    ext TEXT NOT NULL
  );"))

(defun vimh-store--voices-dir (store-directory)
  (concat (file-name-as-directory store-directory) "voices/"))

(defun vimh-store-setup (directory)
  "Create a new store in given DIRECTORY and return DIRECTORY."
  (make-directory (vimh-store--voices-dir directory) t)
  directory)

(defun vimh-store-save (store-directory audio-blob ext)
  "Save given AUDIO-BLOB in STORE-DIRECTORY with EXT extension.

Return name (hash) of the stored item for reference."
  (let* ((hash (secure-hash 'sha512 audio-blob))
         (target-file-path (concat (vimh-store--voices-dir store-directory) hash "." ext)))
    (if (file-exists-p target-file-path)
        (message "File already exists")
      (f-write-bytes audio-blob target-file-path))
    hash))

(defun vimh-store-load (store-directory name ext)
  "Load blob from store if item is found."
  (let ((target-file-path (concat (vimh-store--voices-dir store-directory) name "." ext)))
    (when (file-exists-p target-file-path)
      (f-read-bytes target-file-path))))

(provide 'vimh-store)

;;; vimh-store.el ends here
