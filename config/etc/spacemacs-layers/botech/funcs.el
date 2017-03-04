(defun org-dblock-write:clockdaytable (params)
  "Write a modified clocktable with a per day resolution"
  (message "Requiring org clock")
  (require 'org-clock)
  (setq params (org-combine-plists org-clocktable-defaults params))
  (catch 'exit
    (let* ((p1 (copy-sequence params))
           (scope (plist-get params :scope))
           (block (plist-get params :block))
           (ts (plist-get params :tstart))
           (te (plist-get params :tend))
           (ws (plist-get params :wstart))
           (ms (plist-get params :mstart))
           (step 86400)
           (dump-data (plist-get params :dump-data))
           (formatter (or (plist-get params :formatter)
                          org-clock-clocktable-formatter
                          'org-clocktable-write-default))
           cc ipos one-file-with-archives scope-is-list tbls level)
      ;; Check if we need to do steps
      (when block
        ;; Get the range text for the header
        (setq cc (org-clock-special-range block nil t ws ms)
              ts (car cc)
              te (nth 1 cc)))

      (setq ipos (point)) ; remember the insertion position

      ;; Get the right scope
      (cond
       ((and scope (listp scope) (symbolp (car scope)))
        (setq scope (eval scope)))
       ((eq scope 'agenda)
        (setq scope (org-agenda-files t)))
       ((eq scope 'agenda-with-archives)
        (setq scope (org-agenda-files t))
        (setq scope (org-add-archive-files scope)))
       ((eq scope 'file-with-archives)
        (setq scope (and buffer-file-name
                         (org-add-archive-files (list buffer-file-name)))
              one-file-with-archives t)))
      (setq scope-is-list (and scope (listp scope)))

      (cond
       ((numberp ts)
        ;; If ts is a number, it's an absolute day number from org-agenda.
        (destructuring-bind (month day year) (calendar-gregorian-from-absolute ts)
          (setq ts (org-float-time (encode-time 0 0 0 day month year)))))
       (ts
        (setq ts (org-float-time
                  (apply 'encode-time (org-parse-time-string ts))))))
      (cond
       ((numberp te)
        ;; Likewise for te.
        (destructuring-bind (month day year) (calendar-gregorian-from-absolute te)
          (setq te (org-float-time (encode-time 0 0 0 day month year)))))
       (te
        (setq te (org-float-time
                  (apply 'encode-time (org-parse-time-string te))))))
      (setq tsb ts)
      (setq p1 (plist-put p1 :header ""))
      (setq p1 (plist-put p1 :step nil))
      (setq p1 (plist-put p1 :block nil))

      (while (< tsb te)
        (or (bolp) (insert "\n"))
        (setq p1 (plist-put p1 :tstart (format-time-string
                                        (org-time-stamp-format nil t)
                                        (seconds-to-time (max tsb ts)))))
        (setq p1 (plist-put p1 :tend (format-time-string
                                      (org-time-stamp-format nil t)
                                      (seconds-to-time (min te (setq tsb (+ tsb step)))))))

        (if scope-is-list
            ;; we collect from several files
            (let* ((files scope)
                   file)
              (org-agenda-prepare-buffers files)
              (while (setq file (pop files))
                (with-current-buffer (find-buffer-visiting file)
                  (save-excursion
                    (save-restriction
                      (push (org-clock-get-table-data (plist-get p1 :tstart) p1) tbls))))))
          ;; Just from the current file
          (save-restriction
            ;; get the right range into the restriction
            (org-agenda-prepare-buffers (list (or (buffer-file-name)
                                                  (current-buffer))))
            (cond
             ((not scope))  ; use the restriction as it is now
             ((eq scope 'file) (widen))
             ((eq scope 'subtree) (org-narrow-to-subtree))
             ((eq scope 'tree)
              (while (org-up-heading-safe))
              (org-narrow-to-subtree))
             ((and (symbolp scope) (string-match "^tree\\([0-9]+\\)$"
                                                 (symbol-name scope)))
              (setq level (string-to-number (match-string 1 (symbol-name scope))))
              (catch 'exit
                (while (org-up-heading-safe)
                  (looking-at org-outline-regexp)
                  (if (<= (org-reduced-level (funcall outline-level)) level)
                      (throw 'exit nil))))
              (org-narrow-to-subtree)))
            ;; do the table, with no file name.
            (setq tbl (org-clock-get-table-data (plist-get p1 :tstart) p1))
            (unless (= 0 (nth 1 tbl))
              (push tbl tbls)
              )
            )))

      ;; OK, at this point we tbls as a list of tables, one per file
      (setq tbls (nreverse tbls))

      ;; Skip days without any values
      ;; (setq tbls (seq-filter (lambda (x) (/= 0 (nth 1 x))) tbls))

      (setq params (plist-put params :multifile scope-is-list))
      (setq params (plist-put params :one-file-with-archives
                              one-file-with-archives))

      ;; (my/format-daily-clocktables ipos tbls params)
      (if dump-data
          (progn
           (goto-char ipos)
           (insert (pp-to-string tbls))
           )
        (funcall formatter ipos tbls params)
        )
      )))


(defun my/format-daily-clocktables (ipos tables params)
  "Write out a clock table at position IPOS in the current buffer.
TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.  PARAMS is the parameter property list obtained
from the dynamic block definition."
  ;; This function looks quite complicated, mainly because there are a
  ;; lot of options which can add or remove columns.  I have massively
  ;; commented this function, the I hope it is understandable.  If
  ;; someone wants to write their own special formatter, this maybe
  ;; much easier because there can be a fixed format with a
  ;; well-defined number of columns...
  (let* ((hlchars '((1 . "*") (2 . "/")))
         (lwords (assoc (or (plist-get params :lang)
                            (bound-and-true-p org-export-default-language)
                            "en")
                        org-clock-clocktable-language-setup))
         (multifile (plist-get params :multifile))
         (block (plist-get params :block))
         (sort (plist-get params :sort))
         (header (plist-get  params :header))
         (narrow (plist-get params :narrow))
         (ws (or (plist-get params :wstart) 1))
         (ms (or (plist-get params :mstart) 1))
         (link (plist-get params :link))
         (maxlevel (or (plist-get params :maxlevel) 3))
         (emph (plist-get params :emphasize))
         (level-p (plist-get params :level))
         (org-time-clocksum-use-effort-durations
          (plist-get params :effort-durations))
         (timestamp (plist-get params :timestamp))
         (properties (plist-get params :properties))
         (ntcol (max 1 (or (plist-get params :tcolumns) 100)))
         (rm-file-column (plist-get params :one-file-with-archives))
         (indent (plist-get params :indent))
         (case-fold-search t)
         range-text total-time tbl level hlc formula pcol
         file-time entries entry headline
         recalc content narrow-cut-p tcol)

    ;; Implement abbreviations
    (when (plist-get params :compact)
      (setq level nil indent t narrow (or narrow '40!) ntcol 1))

    ;; Some consistency test for parameters
    (unless (integerp ntcol)
      (setq params (plist-put params :tcolumns (setq ntcol 100))))

    (when block
      ;; Get the range text for the header
      (setq range-text (nth 2 (org-clock-special-range block nil t ws ms))))

    ;; Compute the total time
    (setq total-time (apply '+ (mapcar 'cadr tables)))

    ;; Now we need to output this tsuff
    (goto-char ipos)

    ;; Insert the table header line
    (insert-before-markers
     "|"                              ; table line starter
     (if multifile (concat (nth 1 lwords) "|") "")  ; file column, maybe
     (if level-p   (concat (nth 2 lwords) "|") "")  ; level column, maybe
     (if timestamp (concat (nth 3 lwords) "|") "")  ; timestamp column, maybe
     (if properties (concat (mapconcat 'identity properties "|") "|") "") ;properties columns, maybe
     (concat (nth 4 lwords) "|"
             (nth 5 lwords) "|\n"))                 ; headline and time columns

    ;; Insert the total time in the table
    (insert-before-markers
     "|-\n"                            ; a hline
     "|"                               ; table line starter
     (if multifile (concat "| " (nth 6 lwords) " ") "")
                                        ; file column, maybe
     (if level-p   "|"      "")        ; level column, maybe
     (if timestamp "|"      "")        ; timestamp column, maybe
     (if properties (make-string (length properties) ?|) "")  ; properties columns, maybe
     (concat (format org-clock-total-time-cell-format (nth 7 lwords))  "| ") ; instead of a headline
     (format org-clock-total-time-cell-format
             (org-minutes-to-clocksum-string (or total-time 0))) ; the time
     "|\n")                          ; close line

    ;; Now iterate over the tables and insert the data
    ;; but only if any time has been collected
    (when (and total-time (> total-time 0))

      (while (setq tbl (pop tables))
        ;; now tbl is the table resulting from one file.
        (setq file-time (nth 1 tbl))
        (when (or (and file-time (> file-time 0))
                  (not (plist-get params :fileskip0)))
          (insert-before-markers "|-\n")  ; a hline because a new file starts

          (setq tbl-day (nth 0 tbl))
          ;; Get the list of node entries and iterate over it
          (setq entries (nth 2 tbl))
          (while (setq entry (pop entries))
            (setq level (car entry)
                  headline (nth 1 entry)
                  hlc (if emph (or (cdr (assoc level hlchars)) "") "")
                  hlts (if (= 1 level) (concat " " tbl-day) ""))
            (when narrow-cut-p
              (if (and (string-match (concat "\\`" org-bracket-link-regexp
                                             "\\'")
                                     headline)
                       (match-end 3))
                  (setq headline
                        (format "[[%s][%s]]"
                                (match-string 1 headline)
                                (org-shorten-string (match-string 3 headline)
                                                    narrow)))
                (setq headline (org-shorten-string headline narrow))))
            (insert-before-markers
             "|"                      ; start the table line
             (if multifile "|" "")    ; free space for file name column?
             (if level-p (format "%d|" (car entry)) "")   ; level, maybe
             (if timestamp (concat (nth 2 entry) "|") "") ; timestamp, maybe
             (if properties
                 (concat
                  (mapconcat
                   (lambda (p) (or (cdr (assoc p (nth 4 entry))) ""))
                   properties "|") "|") "")  ;properties columns, maybe
             (if indent (org-clocktable-indent-string level) "") ; indentation
             hlc headline hlc hlts "|"                                ; headline
             (make-string (min (1- ntcol) (or (- level 1))) ?|)
                                        ; empty fields for higher levels
             hlc (org-minutes-to-clocksum-string (nth 3 entry)) hlc ; time
             "|\n"                                               ; close line
             )))))
    ;; When exporting subtrees or regions the region might be
    ;; activated, so let's disable ̀delete-active-region'
    (let ((delete-active-region nil)) (backward-delete-char 1))
    (if (setq formula (plist-get params :formula))
        (cond
         ((eq formula '%)
          ;; compute the column where the % numbers need to go
          (setq pcol (+ 2
                        (length properties)
                        (if multifile 1 0)
                        (if level-p 1 0)
                        (if timestamp 1 0)
                        (min maxlevel (or ntcol 100))))
          ;; compute the column where the total time is
          (setq tcol (+ 2
                        (length properties)
                        (if multifile 1 0)
                        (if level-p 1 0)
                        (if timestamp 1 0)))
          (insert
           (format
            "\n#+TBLFM: $%d='(org-clock-time%% @%d$%d $%d..$%d);%%.1f"
            pcol            ; the column where the % numbers should go
            (if (and narrow (not narrow-cut-p)) 3 2) ; row of the total time
            tcol            ; column of the total time
            tcol (1- pcol)  ; range of columns where times can be found
            ))
          (setq recalc t))
         ((stringp formula)
          (insert "\n#+TBLFM: " formula)
          (setq recalc t))
         (t (error "Invalid formula in clocktable")))
      ;; Should we rescue an old formula?
      (when (stringp (setq content (plist-get params :content)))
        (when (string-match "^\\([ \t]*#\\+tblfm:.*\\)" content)
          (setq recalc t)
          (insert "\n" (match-string 1 (plist-get params :content)))
          (beginning-of-line 0))))
    ;; Back to beginning, align the table, recalculate if necessary
    (goto-char ipos)
    (skip-chars-forward "^|")
    (org-table-align)
    (when org-hide-emphasis-markers
      ;; we need to align a second time
      (org-table-align))
    (when sort
      (save-excursion
        (org-table-goto-line 3)
        (org-table-goto-column (car sort))
        (org-table-sort-lines nil (cdr sort))))
    (when recalc
      (if (eq formula '%)
          (save-excursion
            (if (and narrow (not narrow-cut-p)) (beginning-of-line 2))
            (org-table-goto-column pcol nil 'force)
            (insert "%")))
      (org-table-recalculate 'all))
    (when rm-file-column
      ;; The file column is actually not wanted
      (forward-char 1)
      (org-table-delete-column))
    total-time))
