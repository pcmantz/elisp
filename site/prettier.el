;;; prettier.el --- Use the https://prettier.io/ formatter -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Paul C. Mantz

;; Author: Paul C. Mantz <pcmantz@mcpantz.org>

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <https://www.gnu.org/licenses/>.


(require 'reformatter)

(reformatter-define prettier-js
  :program "prettier"
  :args '("--stdin")
  :lighter " JSFmt"
  :group 'prettier-js)

(reformatter-define prettier-yaml
  :program "prettier"
  :args '("-l" "yml" "--stdin")
  :lighter "YAML"
  :group 'prettier-yaml)

(reformatter-define prettier-json
  :program "prettier"
  :args '("--stdin" "-l")
  :lighter " JSFmt"
  :group 'prettier-json)
