(defun org-dblock-write:clockdaytable (params)
  "Write a modified clocktable with a per day resolution"
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
                      (push (botech//org-clock-get-table-data (plist-get p1 :tstart) p1) tbls))))))
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
            (setq tbl (botech//org-clock-get-table-data (plist-get p1 :tstart) p1))
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


(defun botech/format-daily-clocklines (ipos tables params)
  "Write out a clock lines table at position IPOS in the current buffer.

TABLES is a list of tables with clocking data as produced by
`botech//org-clock-get-table-data'.  PARAMS is the parameter property list obtained
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
         clocklines clockline
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
     "|Date|Issue|Start|End|Duration|Activity|Employee"
     "|\n")                 ; headline and time columns

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
                  hlts (if (= 1 level) (concat " " tbl-day) "")
                  clocklines (get-text-property 0 :botech-clocklines headline))
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
            (while (setq clockline (pop clocklines))
              (insert-before-markers
               ;; start the table line
               "|"
               (if multifile "|" "")    ; free space for file name column?
               ;; day
               (concat (format-time-string "%y-%m-%d" (nth 0 clockline)) "|")
               ;; issue
               (or (cdr (assoc "issue" (nth 4 entry))) "") "|"
               ;; start - only time
               (concat (format-time-string "%H:%M:%S" (nth 0 clockline)) "|")
               ;; end - only time
               (concat (format-time-string "%H:%M:%S" (nth 1 clockline)) "|")
               ;; duration
               (concat (format-seconds "%.2h:%.2m:%.2s" (nth 2 clockline)) "|")
               ;; activity
               (or (cdr (assoc "work_type" (nth 4 entry))) "") "|"
               ;; user
               (or (cdr (assoc "user" (nth 4 entry))) "") "|"
               "\n"                                               ; close line
               ))))))
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



(defun botech//org-clock-get-table-data (file params)
  "Get the clocktable data for file FILE, with parameters PARAMS.

MODIFICATIONS:

 - Call botech//org-clock-sum instead of org-clock-sum

FILE is only for identification - this function assumes that
the correct buffer is current, and that the wanted restriction is
in place.
The return value will be a list with the file name and the total
file time (in minutes) as 1st and 2nd elements.  The third element
of this list will be a list of headline entries.  Each entry has the
following structure:

  (LEVEL HEADLINE TIMESTAMP TIME)

LEVEL:     The level of the headline, as an integer.  This will be
           the reduced level, so 1,2,3,... even if only odd levels
           are being used.
HEADLINE:  The text of the headline.  Depending on PARAMS, this may
           already be formatted like a link.
TIMESTAMP: If PARAMS require it, this will be a time stamp found in the
           entry, any of SCHEDULED, DEADLINE, NORMAL, or first inactive,
           in this sequence.
TIME:      The sum of all time spend in this tree, in minutes.  This time
           will of cause be restricted to the time block and tags match
           specified in PARAMS."
  (let* ((maxlevel (or (plist-get params :maxlevel) 3))
	 (timestamp (plist-get params :timestamp))
	 (ts (plist-get params :tstart))
	 (te (plist-get params :tend))
	 (ws (plist-get params :wstart))
	 (ms (plist-get params :mstart))
	 (block (plist-get params :block))
	 (link (plist-get params :link))
	 (tags (plist-get params :tags))
	 (properties (plist-get params :properties))
	 (inherit-property-p (plist-get params :inherit-props))
	 (matcher (and tags (cdr (org-make-tags-matcher tags))))
	 cc st p time level hdl props tsp tbl)

    (setq org-clock-file-total-minutes nil)
    (when block
      (setq cc (org-clock-special-range block nil t ws ms)
	    ts (car cc)
	    te (nth 1 cc)))
    (when (integerp ts) (setq ts (calendar-gregorian-from-absolute ts)))
    (when (integerp te) (setq te (calendar-gregorian-from-absolute te)))
    (when (and ts (listp ts))
      (setq ts (format "%4d-%02d-%02d" (nth 2 ts) (car ts) (nth 1 ts))))
    (when (and te (listp te))
      (setq te (format "%4d-%02d-%02d" (nth 2 te) (car te) (nth 1 te))))
    ;; Now the times are strings we can parse.
    (if ts (setq ts (org-matcher-time ts)))
    (if te (setq te (org-matcher-time te)))
    (save-excursion
      (botech//org-clock-sum ts te
		     (when matcher
		       `(lambda ()
			  (let* ((tags-list (org-get-tags-at))
				 (org-scanner-tags tags-list)
				 (org-trust-scanner-tags t))
			    (funcall ,matcher nil tags-list nil)))))
      (goto-char (point-min))
      (setq st t)
      (while (or (and (bobp) (prog1 st (setq st nil))
		      (get-text-property (point) :org-clock-minutes)
		      (setq p (point-min)))
		 (setq p (next-single-property-change
			  (point) :org-clock-minutes)))
	(goto-char p)
	(when (setq clocklines (get-text-property p :botech-clocklines)
              time (get-text-property p :org-clock-minutes))
	  (save-excursion
	    (beginning-of-line 1)
	    (when (and (looking-at "\\(\\*+\\)[ \t]+\\(.*?\\)\\([ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$")
		       (setq level (org-reduced-level
				    (- (match-end 1) (match-beginning 1))))
		       (<= level maxlevel))
	      (setq hdl (if (not link)
			    (match-string 2)
			  (org-make-link-string
			   (format "file:%s::%s"
				   (buffer-file-name)
				   (save-match-data
				     (match-string 2)))
			   (org-make-org-heading-search-string
			    (replace-regexp-in-string
			     org-bracket-link-regexp
			     (lambda (m) (or (match-string 3 m)
					(match-string 1 m)))
			     (match-string 2)))))
		    tsp (when timestamp
			  (setq props (org-entry-properties (point)))
			  (or (cdr (assoc "SCHEDULED" props))
			      (cdr (assoc "DEADLINE" props))
			      (cdr (assoc "TIMESTAMP" props))
			      (cdr (assoc "TIMESTAMP_IA" props))))
		    props (when properties
			    (remove nil
				    (mapcar
				     (lambda (p)
				       (when (org-entry-get (point) p inherit-property-p)
					 (cons p (org-entry-get (point) p inherit-property-p))))
				     properties))))
	      (when (> time 0) (push (list level hdl tsp time props) tbl))))))
      (setq tbl (nreverse tbl))
      (list file org-clock-file-total-minutes tbl))))


(defun botech//org-clock-sum (&optional tstart tend headline-filter propname)
  "Sum the times for each subtree.

MODIFICATIONS:

 - Store each clock line in a property :org-clock-lines

Puts the resulting times in minutes as a text property on each headline.
TSTART and TEND can mark a time range to be considered.
HEADLINE-FILTER is a zero-arg function that, if specified, is called for
each headline in the time range with point at the headline.  Headlines for
which HEADLINE-FILTER returns nil are excluded from the clock summation.
PROPNAME lets you set a custom text property instead of :org-clock-minutes."
  (org-with-silent-modifications
   (let* ((re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
		      org-clock-string
		      "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
	  (lmax 30)
	  (ltimes (make-vector lmax 0))
	  (t1 0)
	  (level 0)
	  ts te dt ts_time te_time
	  time
    clocklines)
     (if (stringp tstart) (setq tstart (org-time-string-to-seconds tstart)))
     (if (stringp tend) (setq tend (org-time-string-to-seconds tend)))
     (if (consp tstart) (setq tstart (float-time tstart)))
     (if (consp tend) (setq tend (float-time tend)))
     (remove-text-properties (point-min) (point-max)
			     `(,(or propname :org-clock-minutes) t
			       :org-clock-force-headline-inclusion t))
     (save-excursion
       (goto-char (point-max))
       (while (re-search-backward re nil t)
	 (cond
	  ((match-end 2)
	   ;; Two time stamps
	   (setq ts (match-string 2)
           te (match-string 3)
           ts_time (apply #'encode-time (org-parse-time-string ts))
           ts (float-time ts_time)
           te_time (apply #'encode-time (org-parse-time-string te))
           te (float-time te_time)
           ts (if tstart (max ts tstart) ts)
           te (if tend (min te tend) te)
           ts_time (seconds-to-time ts)
           te_time (seconds-to-time te)
           dt (- te ts)
           t1 (if (> dt 0) (+ t1 (floor (/ dt 60))) t1))
     ;; Remember each clockline
     (when (> dt 0)
       (push (list ts_time te_time dt) clocklines))
     )
	  ((match-end 4)
	   ;; A naked time
	   (setq dt (+ (string-to-number (match-string 5))
                 (* 60 (string-to-number (match-string 4))))
           t1 (+ t1 dt ))
     ;; Add a clockline which only has a duration
     (push (list nil nil dt) clocklines)
     )
	  (t ;; A headline
	   ;; Add the currently clocking item time to the total
	   (when (and org-clock-report-include-clocking-task
		      (equal (org-clocking-buffer) (current-buffer))
		      (equal (marker-position org-clock-hd-marker) (point))
		      tstart
		      tend
		      (>= (float-time org-clock-start-time) tstart)
		      (<= (float-time org-clock-start-time) tend))
	     (let ((time (floor (- (float-time)
				   (float-time org-clock-start-time))
				60)))
	       (setq t1 (+ t1 time))))
	   (let* ((headline-forced
		   (get-text-property (point)
				      :org-clock-force-headline-inclusion))
		  (headline-included
		   (or (null headline-filter)
		       (save-excursion
			 (save-match-data (funcall headline-filter))))))
	     (setq level (- (match-end 1) (match-beginning 1)))
	     (when (>= level lmax)
	       (setq ltimes (vconcat ltimes (make-vector lmax 0)) lmax (* 2 lmax)))
	     (when (or (> t1 0) (> (aref ltimes level) 0))
	       (when (or headline-included headline-forced)
		 (if headline-included
		     (cl-loop for l from 0 to level do
			      (aset ltimes l (+ (aref ltimes l) t1))))
		 (setq time (aref ltimes level))
		 (goto-char (match-beginning 0))
		 (put-text-property (point) (point-at-eol)
				    (or propname :org-clock-minutes) time)
     (put-text-property (point) (point-at-eol) :botech-clocklines clocklines)
     ;; reset clocklines, so that we can start to collect clocklines
     ;; for the next headline
     (setq clocklines nil)
		 (when headline-filter
		   (save-excursion
		     (save-match-data
		       (while (org-up-heading-safe)
			 (put-text-property
			  (point) (line-end-position)
			  :org-clock-force-headline-inclusion t))))))
	       (setq t1 0)
	       (cl-loop for l from level to (1- lmax) do
			(aset ltimes l 0)))))))
       (setq org-clock-file-total-minutes (aref ltimes 0))))))
