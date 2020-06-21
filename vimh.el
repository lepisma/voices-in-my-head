;;; vimh.el --- VIMH -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.4
;; Package-Requires: ((emacs "26") (esi "0.0.7"))
;; URL: https://github.com/lepisma/voices-in-my-head

;;; Commentary:

;; VIMH
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

(require 'esi-core)

(defcustom vimh-flight-max-duration 30
  "Maximum length of audio file in seconds for flight mode
recordings."
  :type 'number)

(defcustom vimh-sample-rate 44100
  "Sample rate for recordings."
  :type 'integer)

(defvar vimh-flight-stream nil
  "Flight mode style audio stream.")

(defun vimh--stop-flight-stream ()
  (when vimh-flight-stream
    (esi-core--stop-background-recording vimh-flight-stream)
    (setq vimh-flight-stream nil)))

(defun vimh--start-flight-stream ()
  (vimh--stop-flight-stream)
  (setq vimh-flight-stream (esi-core--start-background-recording vimh-sample-rate vimh-flight-max-duration)))

(defun vimh-read-flight-stream ()
  "Read audio blob from the flight stream."
  (if (null vimh-flight-stream)
      (error "Flight stream not active.")
    (esi-core--read-background-recording-buffer vimh-flight-stream)))

;;;###autoload
(define-minor-mode vimh-mode
  "Voices In My Head minor mode."
  :global t
  (if vimh-mode
      (vimh--start-flight-stream)
    (vimh--stop-flight-stream)))

(provide 'vimh)

;;; vimh.el ends here
