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

(defun vimh-store-setup (directory)
  "Create a new store in given DIRECTORY."
  (make-directory (concat (file-name-as-directory directory) "voices"))
  (vimh-store--setup-db (concat (file-name-as-directory directory) vimh-store-db-file-name)))

(provide 'vimh-store)

;;; vimh-store.el ends here
