
(setc garbage-collection-messages nil)

(setq gc-cons-threshold (* 8 100 1000 1000))
(setq gc-cons-percentage 10.0)

(defun ol-garbage-collect (&optional quiet)
  (interactive)
  (let ((secs-before (float-time)))
    (unless quiet
      (message (format "Start garbage-collect at %s" (current-time-string))))
    (garbage-collect)
    (let* ((secs-after (float-time))
           (time-diff (- secs-after secs-before)))
      (unless quiet
        (message (format "Finish garbage-collect at: %s.    Took %g seconds."
                         (current-time-string)
                         time-diff))))))

(defvar ol-last-gc nil)

(defun ol-frame-out-of-focus ()
  (unless (frame-focus-state)
    ;; this function is called 4 times whenever I alt-tab, so to make
    ;; sure gc is not invoked right after it has already been done,
    ;; have some margin.
    (when (or (null ol-last-gc) (> (- (float-time) ol-last-gc) 5.0))
      (setq ol-last-gc (float-time))
      (ol-garbage-collect 'quiet))))

(add-function :after after-focus-change-function 'ol-frame-out-of-focus)

(setq read-process-output-max (* 1024 1024)) ;; 1 MB

(provide 'ol-gc)
