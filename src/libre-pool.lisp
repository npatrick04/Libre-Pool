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
;;; Calibration 7/20/2025
(defparameter *pH-cal-counts* '( 144 601 1023))
(defparameter *pH-cal-points* '(4.0 7.0 10.01))

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

(defun my-lerp (x x0 y0 x1 y1)
  (/ (+ (* y0 (- x1 x)) (* y1 (- x x0))) (- x1 x0)))

(defun lerps (x xs ys)
  (do ((xs xs (cdr xs))
       (ys ys (cdr ys)))
      ;; Either the x value is less than first xs OR
      ;;                    is > first xs and < second xs OR
      ;;                    We're all out of more stages
      ((or (<= x (car xs))
	   (and (> x (car xs)) (< x (cadr xs)))
	   (= 2 (length xs)))
       (my-lerp x (first xs) (first ys) (second xs) (second ys)))))

(defun cal-ph (pH-counts)
  (lerps pH-counts *pH-cal-counts* *pH-cal-points*))

;;; https://www.phidgets.com/?prodid=1181#Measuring_Oxidation/Reduction_Potential_(ORP)
(defun cal-orp (ORP-counts)
  "Calculate ORP per Phidget documentation"
  (let ((voltage (calibrate ORP-counts *mcp3008-scale* 0)))
    (/ (- 2.5 voltage) 1.037)))

(defun read-water-temp ()
  "Get water temp in degrees Celsius"
  (with-open-file (in "/sys/bus/w1/devices/28-00000a006453/temperature" :if-does-not-exist nil)
    ;; DS18B20 with w1-gpio reads temp in milliCelcius
    (when in
      (let ((temp (parse-integer (read-line in nil))))
	(/ temp 1000.0)))))

(defun read-proc-temp ()
  (with-open-file (in "/sys/class/thermal/thermal_zone0/temp")
    (let ((temp (parse-integer (read-line in nil))))
      (/ temp 1000.0))))

(defun read-fan-speed ()
  (with-open-file (in "/sys/class/hwmon/hwmon1/fan1_input")
    (parse-integer (read-line in nil))))

(defun collect-data ()
  (let ((mcp3008-data (mapcar #'read-sensor-channel (loop for i below 8 collect i)))
	(water-temp (read-water-temp))
	(proc-temp-c (read-proc-temp))
	(timestamp (timestamp)))
    ;; Send raw channel data
    (loop for i below 8 do
      (progn (send-datum-udp (format nil "pool.mcp3008.ch~A.counts" i) (nth i mcp3008-data) timestamp)
	     (send-datum-udp (format nil "pool.mcp3008.ch~A.voltage" i) (calibrate (nth i mcp3008-data) *mcp3008-scale* 0) timestamp)))
    ;; Send pH
    (send-datum-udp "pool.measurements.pH" (cal-ph (nth 0 mcp3008-data)))
    (send-datum-udp "pool.measurements.ORP" (cal-orp (nth 1 mcp3008-data)))

    ;; Water Temperature
    (when water-temp
      (send-datum-udp "pool.measurements.waterTempC" water-temp timestamp)
      (send-datum-udp "pool.measurements.waterTempF" (+ (* water-temp 9/5) 32) timestamp))

    ;; Processor Temperature
    (send-datum-udp "pool.processor.temp-c" proc-temp-c)
    (send-datum-udp "pool.processor.temp-f" (+ (* proc-temp-c 9/5) 32))

    ;; Fan Speed
    (send-datum-udp "pool.processor.fan-speed" (read-fan-speed))))

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
