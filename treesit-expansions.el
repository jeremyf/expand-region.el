;;; treesit-er-expansions.el ---  -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Aleksandar Dimitrov


;; Author: Aleksandar Dimitrov <git@aleks.bg>
;; Created: 2023-03-13
;; Keywords: marking region

;; This file is not part of GNU Emacs.
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'treesit)
(require 'expand-region-core)


(defun er/treesit-mark--from-to (from to)
  (set-mark from)
  (goto-char to)
  (activate-mark))

(defun er/treesit--balanced-region-p (beg end)
  "Check if region between BEG and END is balanced by `scan-sexps'.
Note that `forward-sexp' isn't used because some grammars ignore
brakets, thus the functions doesn't move a over a balanced
\"sexp\".

See:
- https://github.com/wkirschbaum/elixir-ts-mode/issues/21."
  (save-excursion
    (condition-case err
        (cl-block
            nil (goto-char beg)
            (while t
              ;; skip spaces, since the region may contain trailing spaces while
              ;; another sexp lies beyond those
              (skip-chars-forward " \t\n\f" end)
              (if (or (eobp) (equal (point) end))
                  (cl-return t)
                (let* ((next-pos (scan-sexps (point) 1)))
                  (cond
                   ;; can't find any more sexps: balanced
                   ((null next-pos)
                    (cl-return t))
                   ;; there is a sexp begins in the region and ends outside the
                   ;; region: region not balanced
                   ((< end next-pos)
                    (cl-return nil))
                   (t
                    (goto-char next-pos)))))))
      (scan-error
       ;; error because being at the end of list: balanced
       (string-search "ends prematurely" (error-message-string err))))))

;;;###autoload
(defun er/treesit-mark-bigger-list-or-node ()
    "Expand the marked region by navigating Tree Sitter's produced syntax tree.

When the region is not active: mark the smallest node at point.

When the region is active:

- Find the smallest node that covers on the region, when its
  range is bigger (and includes) the region, mark that range.

- When that range is exactly the region: try to mark the
  \"inner\" region between the bigger ancestor node (that is
  usually just the immediate parent node)'s first and last named
  node:

        + This region is usually equivalent to
  `er/mark-inside-pairs' but also works for keyword-delimited
  blocks.

        + The first and last child nodes are unnamed, who are
  delimiters, there must be 2 or mode named nodes between those,
  the intended region includes all named nodes, excludes the
  mandatory preceding and trailing unnamed nodes.

        + The region must be balanced before marking, as some
  grammars let an opening bracket, eg. \"{\" (unnamed node after
  a named node) be a middle child and the closing one, eg. \"}\"
  be the last child, therefore those \"inner\" regions are not
  balanced as it has \"{\" but not \"}\", making it little sense
  to expand.

- When the above expansions can't be found, just mark the bigger
  ancestor node."
  (interactive)
  (cond
   ((not (use-region-p))
    (let* ((node@pt (treesit-node-at (point))))
      (er/treesit-mark--from-to
       (treesit-node-end node@pt) (treesit-node-start node@pt))))
   (t
    (let* ((beg0 (region-beginning))
           (end0 (region-end))
           ;; find the smallest node that covers the region
           (cover-node (treesit-node-on beg0 end0))
           (cover-beg (treesit-node-start cover-node))
           (cover-end (treesit-node-end cover-node)))
      ;; if found node is bigger than the region, mark it
      (if (or (< cover-beg beg0) (< end0 cover-end))
          (er/treesit-mark--from-to cover-end cover-beg)
        (let* ((super-node
                (treesit-parent-until
                 cover-node
                 (lambda (node)
                   (or (/= (treesit-node-start node) cover-beg)
                       (/= (treesit-node-end node) cover-end))))))
          (if-let* ((_
                     (and (< 1 (treesit-node-child-count super-node 'named))
                          (not
                           (treesit-node-check
                            (treesit-node-child super-node 0) 'named))
                          (not
                           (treesit-node-check
                            (treesit-node-child super-node -1) 'named))))
                    (list-beg
                     (treesit-node-start
                      (treesit-node-child super-node 0 'named)))
                    (list-end
                     (treesit-node-end
                      (treesit-node-child super-node -1 'named)))
                    (_
                     (and (<= list-beg cover-beg cover-end list-end)
                          (er/treesit--balanced-region-p list-beg list-end))))
            (er/treesit-mark--from-to list-end list-beg)
            (er/treesit-mark--from-to
             (treesit-node-end super-node)
             (treesit-node-start super-node)))))))))

;;;###autoload
(defun er/add-treesit-expansion ()
  "Add expansions for treesit-based modes."
  (when (treesit-parser-list)
    (set
     (make-local-variable 'er/try-expand-list)
     (append er/try-expand-list '(er/treesit-mark-bigger-list-or-node)))))

;;;###autoload
(progn
  (defun er/treesit-enable-modes-expansions ()
    (er/enable-mode-expansions 'prog-mode 'er/add-treesit-expansion)
    (er/enable-mode-expansions 'conf-mode 'er/add-treesit-expansion)
    (er/enable-mode-expansions 'text-mode 'er/add-treesit-expansion)))

(provide 'treesit-expansions)

;;; treesit-expansions.el ends here
