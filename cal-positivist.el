;;; cal-positivist.el --- calendar functions for the Positivist calendar.

;; Author: Andy Craze <accraze@gmail.com>
;; Version: 1.0
;; Keywords: calendar
;; Human-Keywords: positivist, calendar
;; Package: calendar

;;; Code:

(require 'calendar)

(defconst calendar-positivist-month-name-array
  ["Moses" "Homer" "Aristotle" "Archimedes" "Caesar" "St. Paul" "Charlemagne" "Dante"
   "Gutenberg" "Shakespeare" "Descartes" "Frederic" "Bichat"]
  "Array of the month names in the Positivist calendar.")

(defconst calendar-positivist-mosum
'(0 31 59 90 120 151 181 212 243 273 304 334))

(defconst calendar-positivist-weekday-name-array
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

(defconst calendar-positivist-epoch (calendar-absolute-from-gregorian '(1 1 1789))
  "Absolute date of start of Positive calendar = Jan 1, 1789 AD.")

(defun calendar-positivist-to-absolute (date)
  "Compute absolute date from Positivist date DATE.
TODO: The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  )

(defun calendar-positivist-from-absolute (date)
  "Positivist date (month day year) corresponding to the absolute DATE."
  (if (< date calendar-positivist-epoch)
      (list 0 0 0)                      ; pre-positivist date
    (let* ((greg (calendar-gregorian-from-absolute date))
           (gmonth (calendar-extract-month greg))
           (gday (calendar-extract-day greg))
           (year (if (< (calendar-extract-year greg) 2000)(+ (calendar-extract-year greg)1900)(calendar-extract-year greg)))
           (leapyear (if
                         (or (and (= (% year 4) 0) (/= (% year 100) 0)) (=(% year 400) 0))
                         t
                     nil))
           (doyy (+ (calendar-extract-day greg) (elt calendar-positivist-mosum gmonth)))
           (doy (if (and (bound-and-true-p leapyear) (> gmonth 1)) (+ doyy 1) doyy))
           (pmonth (aref calendar-positivist-month-name-array (- (truncate (/(+ doy 27)28)) 2)))
           (pweekday (aref calendar-positivist-weekday-name-array (% doy 7)))
           (ppdate (% doy 28))
           (pdate (if (= ppdate 0) 28 ppdate))
           (pyear (- year 1788))
           (pcal
            (if (< doy 365)
                (concat pweekday ", " pmonth " " (number-to-string  pdate) ", " (number-to-string pyear))
              (if(= doy 365)
                  (concat "the Festival of All the Dead," pyear)
                (concat "the Festival of Holy Women" pyear)))))
           (list pmonth pdate pyear))
  ))

;;;###cal-autoload
(defun calendar-positivist-date-string (&optional date)
  "String of Positivist date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((positivist-date (calendar-positivist-from-absolute
                           (calendar-absolute-from-gregorian
                            (or date (calendar-current-date)))))
         (y (calendar-extract-year positivist-date)))
    (if (< y 1)
        ""                              ; pre-positivist
      (format "%s %s, %s" (calendar-extract-month positivist-date)(calendar-extract-day positivist-date)(calendar-extract-year positivist-date))
      )))

;;;###cal-autoload
(defun calendar-positivist-print-date ()
  "Show the positivist calendar equivalent of the selected date."
  (interactive)
  (let ((s (calendar-positivist-date-string (calendar-cursor-to-date t))))
    (if (string-equal s "")
        (message "Date is pre-Positivist")
      (message "Positivist date: %s" s))))

(provide 'cal-positivist)

;;; cal-positivist.el ends here
