;;; vimh.el --- VIMH -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.3
;; Package-Requires: ((emacs "26") (esi "0.0.5") (magit "2.90.1"))
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

(defcustom vimh-voice-max-duration 30
  "Maximum length of audio file in seconds"
  :type 'number)

(defcustom vimh-voice-sample-rate 44100
  "Sample rate for recordings"
  :type 'integer)

(defvar vimh-voice-instream nil
  "Flight mode style audio stream.")

(defun vimh--stop-stream ()
  (when vimh-voice-instream
    (esi-core--stop-background-recording vimh-voice-instream)
    (setq vimh-voice-instream nil)))

(defun vimh--start-stream ()
  (vimh--stop-stream)
  (setq vimh-voice-instream (esi-core--start-background-recording vimh-voice-sample-rate vimh-voice-max-duration)))

;;;###autoload
(define-minor-mode vimh-mode
  "Voices In My Head minor mode."
  :global t
  (if vimh-mode
      (vimh--start-stream)
    (vimh--stop-stream)))

(provide 'vimh)

;;; vimh.el ends here
