;;; org-ir.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Mikhail Skorzhisnkii
;;
;; Author: Mikhail Skorzhisnkii <http://github.com/mskorzhinskiy>
;; Maintainer: Mikhail Skorzhisnkii <mskorzhinskiy@eml.cc>
;; Created: August 16, 2020
;; Modified: August 16, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/mskorzhinskiy/org-ir
;; Package-Requires: ((emacs 27.0.91) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'org-ql)

(require 'seq)
(require 'cl-lib)

(defvar org-ir-month-report
  (list :query `(not (tags "meal"))
        :ignore-tags `(list "journal" "story"))
  "Format of monthly reports to generate by `org-ir-month'.")

(defvar org-ir-week-report
  (list :query `(not (tags "meal"))
        :ignore-tags `(list "journal" "story"))
  "Format of weekly report to generate by `org-ir-week'.")

(defcustom org-ir-setup-file ""
  "Line to the setupfile for the export engines.")

;; Functions

(defun org-ir-insert-clocktable (arguments)
  "Inserts mini-clocktable under the point."
  (insert "\n\n")
  (insert (format "#+BEGIN: clocktable :scope subtree :compact t %s"
                  arguments))
  (insert "\n")
  (insert "#+END: clocktable")
  (insert "\n"))

(defmacro org-ir-with-header (header &optional make-ct ct-format &rest body)
  "Executes BODY if CONDITION is true with narrowed HEADER."
  `(progn
     (unless (search-forward-regexp (concat ,header "$") nil t)
       (org-insert-subheading nil)
       (org-edit-headline ,header)
       (when ,make-ct
         (org-ir-insert-clocktable ,ct-format)))
     (save-restriction
       (org-narrow-to-subtree)
       ,@body
       (save-excursion
         (org-up-heading-safe)
         (when (save-excursion (org-up-heading-safe))
           (org-sort-entries nil ?a)
           (org-sort-entries nil ?T))))))

(defun org-ir-insert-new-item (headline tags link-only)
  "Insert headline with tags."
  (org-insert-subheading nil)
  (unless link-only
    (org-paste-subtree))
  (org-edit-headline headline)
  (org-set-tags tags))

(defun org-ir-insert-query (buffers-of-file query settings)
  "Function to insert the report."
  (let ((link-only (plist-get settings :link-only))
        (make-clock-table (plist-get settings :make-ct))
        (clock-table-format (plist-get settings :ct-format)))
    (save-restriction
      (org-narrow-to-subtree)
      (let ((origin (point-marker)))
        (dolist (element (org-ql-select buffers-of-file query
                           :action #'element-with-markers))
          (let* ((marker (plist-get (cadr element) :org-marker))
                 (buffer (marker-buffer marker))
                 (pos (marker-position marker)))
            (with-current-buffer buffer
              (goto-char pos)
              ;; Actual operations on the entry
              (org-copy-subtree 0 nil nil t)
              (let* ((all-tags (cl-sort (seq-difference (org-get-tags (point) nil)
                                                        (plist-get settings :remove-tags))
                                        #'string<))
                     (tags-formatted (if (length all-tags)
                                         (format "Tags: %s" (mapconcat 'identity all-tags ", "))
                                       "No tags"))
                     (link (org-store-link nil nil))
                     (path-formatted (org-format-outline-path (org-get-outline-path t nil)
                                                              250 nil " » ")))
                (save-excursion
                  (switch-to-buffer (marker-buffer origin))
                  (goto-char (point-min))
                  (org-ir-with-header
                   tags-formatted make-clock-table clock-table-format
                   (org-ir-insert-new-item link all-tags link-only))))))))
      ;; Update all clock tables in this subtree
      (org-dblock-update t))))

(defun org-ir-report-range (settings)
  (let ((date-beg (plist-get settings :begin))
        (date-end (plist-get settings :end))
        (step (plist-get settings :step))
        (remove-tags (plist-get settings :remove-tags))
        (query (plist-get settings :query)))
    (when (org-ql-select (org-agenda-files)
            `(and (or (closed :from ,date-beg :to ,date-end)
                      (clocked :from ,date-beg :to ,date-end))
                  ,query))
      ;; Insert clock report
      (org-insert-heading)
      (org-edit-headline "Clock table")
      (save-excursion
        (let* ((ct-format (format ":tstart \"%s\" :tend \"%s\"" date-beg date-end))
               (ct-block-format (format "%s :step %s" ct-format step)))
          (org-ir-insert-clocktable (concat ct-format " :maxlevel 2"))
          (org-ir-insert-query
           (org-agenda-files)
           `(and (clocked :from ,date-beg :to ,date-end) ,query)
           `(:make-ct t
             :remove-tags ,remove-tags
             :ct-format ,ct-block-format))))
      ;; Go to the top-most item
      (while (org-up-heading-safe))
      ;; Attach closed tasks
      (org-insert-heading)
      (org-edit-headline "Closed tasks")
      (org-ir-insert-query
       (org-agenda-files)
       `(and (closed :from ,date-beg :to ,date-end)
             (not (clocked :from ,date-beg :to ,date-end))
             ,query)
       `(:remove-tags ,remove-tags
         :link-only t))
      ;; Go to the top-most item
      (while (org-up-heading-safe))
      ;; Attach items that was worked
      (org-insert-heading)
      (org-edit-headline "Worked items")
      (org-ir-insert-query
       (org-agenda-files)
       `(and (ts-inactive :from ,date-beg :to ,date-end)
             (not (closed :from ,date-beg :to ,date-end))
             (not (clocked :from ,date-beg :to ,date-end))
             ,query)
       `(:remove-tags ,remove-tags
         :link-only t)))))

(defun org-ir-report-week (date-within-week tags-to-ignore query)
  (let* ((date (ts-parse date-within-week))
         (adjust-beg-day (+ (- (ts-dow date))))
         (adjust-end-day (+ (- 6 (ts-dow date))))
         (start (thread-last date
                  (ts-adjust 'day adjust-beg-day)
                  (ts-apply :hour 0 :minute 0 :second 0)))
         (end (thread-last date
                (ts-adjust 'day adjust-end-day)
                (ts-apply :hour 23 :minute 59 :second 59)))
         (start-string (ts-format start))
         (end-string (ts-format end)))
    (find-file (format "~/inbox/%s_%s_%s.org"
                       (ts-format "%YW%W" start)
                       (ts-format "%d.%m" start)
                       (ts-format "%d.%m" end)))
    (erase-buffer)
    (insert (format "#+TITLE: %s [%s - %s]\n"
                    (ts-format "%Y Week %W" start)
                    (ts-format "%d.%m" start)
                    (ts-format "%d.%m" end)))
    (insert (format "#+SETUPFILE: %s\n" org-ir-setup-file))
    (insert (format "* Overview\n\n\n"))
    (org-ir-report-range
     `(:begin ,start-string
       :end   ,end-string
       :step "day"
       :remove-tags ,tags-to-ignore
       :query ,query))))

(defun org-ir-report-month (date-within-month tags-to-ignore query)
  (let* ((date (ts-parse date-within-month))
         (last-day (calendar-last-day-of-month (ts-month-num date)
                                               (ts-year date)))
         (start (thread-last date
                  (ts-apply :day 1)
                  (ts-apply :hour 0 :minute 0 :second 0)))
         (end (thread-last date
                (ts-apply :day last-day)
                (ts-apply :hour 23 :minute 59 :second 59)))
         (start-string (ts-format start))
         (end-string (ts-format end)))
    (find-file (format "~/inbox/%s.org" (ts-format "%Y_%B" start)))
    (erase-buffer)
    (insert (format "#+TITLE: %s [%s - %s]\n"
                    (ts-format "%Y %B" start)
                    (ts-format "%d.%m" start)
                    (ts-format "%d.%m" end)))
    (insert (format "#+SETUPFILE: %s\n" org-ir-setup-file))
    (insert (format "* Overview\n\n\n"))
    (org-ir-report-range
     `(:begin ,start-string
       :end   ,end-string
       :step "week"
       :remove-tags ,tags-to-ignore
       :query ,query))))

(defun org-ir-week ()
  (interactive)
  (let ((date (org-read-date)))
    (org-ir-report-week
     date
     (cl-getf org-ir-week-report :ignore-tags)
     (cl-getf org-ir-week-report :query)))
  (flush-lines "^$" (point-min) (point-max))
  (org-set-startup-visibility))

(defun org-ir-month ()
  (interactive)
  (let ((date (org-read-date)))
    (org-ir-report-month
     date
     (cl-getf org-ir-week-report :ignore-tags)
     (cl-getf org-ir-month-report :query)))
  (flush-lines "^$" (point-min) (point-max))
  (org-set-startup-visibility))

(provide 'org-ir)
;;; org-ir.el ends here
