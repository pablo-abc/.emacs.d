;;; flycheck-apib.el --- API Blueprint flycheck integration -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Josh Benner

;; Author: Josh Benner <joshbenner@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((flycheck "27") (apib-mode "0.1"))
;; Keywords: API Blueprint flycheck drafter
;; URL: https://github.com/joshbenner/flycheck-apib

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'flycheck)

(flycheck-define-checker
 apib-drafter
 "A syntax checker for API Blueprint using drafter."
 :command ("drafter" "-ul" source)
 :error-patterns ((warning line-start
                           "warning: (" (one-or-more digit) ")  " (message)
                           " - line " line ", column " column
                           line-end)
                  (error line-start
                         "error: (" (one-or-more digit) ")  " (message)
                         " - line " line ", column " column
                         line-end)
                  )
 :modes apib-mode)

(add-to-list 'flycheck-checkers 'apib-drafter)

(provide 'flycheck-apib)
;;; flycheck-apib ends here
