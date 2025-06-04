(in-package #:libre-pool)

;;; The sensors are measuring
;;; Channel 0: pH
;;; Channel 1: ORP
;;; Channel 7: 50% of VREF (test only)

;;; Sensors are interfaced using Linux IIO subsystems
;;; Example: /sys/bus/iio/devices/iio\:device1/in_voltage0_raw

(defparameter *socket* nil)

(defparameter *graphite-udp-port* 8125)
(defparameter *graphite-tcp-port* 8125)
(defparameter *graphite-ip* "192.168.2.204")

;;; Sensor Calibrations
(defparameter *mcp3008-scale* (/ 3.3 1024))
(defparameter *pH-low-point* 4.0)
(defparameter *pH-high-point* 7.0)
(defparameter *pH-low-count* 18)
(defparameter *pH-high-count* 480)

(defun timestamp ()
  (string-trim '(#\Space #\Newline) (with-output-to-string (stream)
				      #+ecl (ext:run-program "date" '("+%s") :wait t :output stream)
				      #+sbcl (sb-ext:run-program "/usr/bin/date" '("+%s") :wait t :output stream))))

(defun read-sensor-channel (channel)
  "Return raw data"
  (let ((channel-filename (format nil "/sys/bus/iio/devices/iio:device1/in_voltage~A_raw" channel)))
    (with-open-file (in channel-filename)
      (parse-integer (read-line in)))))

;;; Using statsd convention
(defun send-datum-udp (name datum &optional (timestamp (timestamp)))
  (let ((string (format nil "~A:~A|g" name datum)))
    ;;(format t "~A~%" string)
    (usocket:socket-send *socket* (flexi-streams:string-to-octets string) (length string))))

(defun calibrate (count scale bias)
  (* (+ count bias) scale))

(defun lerp (x x0 y0 x1 y1)
  (/ (+ (* y0 (- x1 x)) (* y1 (- x x0))) (- x1 x0)))

(defun cal-ph (pH-counts)
  (lerp pH-counts *pH-low-count* *ph-low-point* *pH-high-count* *ph-high-point*))

(defun collect-data ()
  (let ((mcp3008-data (mapcar #'read-sensor-channel (loop for i below 8 collect i)))
	(timestamp (timestamp)))
    ;; Send raw channel data
    (loop for i below 8 do
      (progn (send-datum-udp (format nil "pool.mcp3008.ch~A.counts" i) (nth i mcp3008-data) timestamp)
	     (send-datum-udp (format nil "pool.mcp3008.ch~A.voltage" i) (calibrate (nth i mcp3008-data) *mcp3008-scale* 0) timestamp)))
    ;; Send pH
    (send-datum-udp "pool.measurements.pH" (cal-ph (nth 0 mcp3008-data)))
    (send-datum-udp "pool.measurements.ORP" (calibrate (nth 1 mcp3008-data) *mcp3008-scale* 0))))

(defun reset () 
  (setf *socket* (usocket:socket-connect *graphite-ip* *graphite-udp-port* :protocol :datagram)))

(defun collection-loop ()
  (rm:with-timer-period ()
    (let ((p (rm:make-timer-period)))
      (do () (nil nil)
	(rm:period p :ms 10000)
	(collect-data)))))

(defun main ()
  (reset)
  (collection-loop))
