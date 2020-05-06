(require 'request)

(defgroup my-livedoor-weather-hacks nil
  "Livedoor weather hacks"
  :group 'tools)

(defcustom my-livedoor-weather-hacks-city-id 130010
  "Livedoor weather hacks city id defalut is 東京 130010
city id: http://weather.livedoor.com/forecast/rss/primary_area.xml
e.g.
東京: 130010
横浜: 140010"
  :group 'my-livedoor-weather-hacks
  :type  'number)

(defun my-livedoor-weather-hacks-fetch (city-id)
  "return weather forecast info with :dateLabel \"今日\" from livedoor weather hacks specified by arg.
live door weather hack: http://weather.livedoor.com/weather_hacks/webservice
city id               : http://weather.livedoor.com/forecast/rss/primary_area.xml
return forecast info  : (:dateLabel \"今日\" :telop \"曇時々雨\" :date \"2020-05-06\" :temperature (:min nil :max (:celsius \"22\" :fahrenheit \"71.6\")) :image (:width 50 :url \"http://weather.livedoor.com/img/icon/10.gif\" :title \"曇時々雨\" :height 31))"

  (interactive "ncity-id: ")

  (let (forecast)
    (request
      (format "http://weather.livedoor.com/forecast/webservice/json/v1?city=%s" city-id)
      ;; "http://weather.livedoor.com/forecast/webservice/json/v1?city=1410011" ; 横浜市北部 area id is not available
      :sync t

      ;;  pass
      ;; :parser 'json-read ; request return value is hash #s(request-response 200 nil ((pinpointLocations

      ;; pass
      :parser (lambda ()
		(let ((json-object-type 'plist)) ;; convert JSON objects to plist
		  (setq forecast (aref (plist-get (json-read) :forecasts) 0)))))

    (when (called-interactively-p 'any)
      (message "%s" forecast))
    forecast))

(defun my-livedoor-weather-hacks-fetch-telop ()
  "display telop e.g \"晴れ\" on mode line"
  (plist-get (my-livedoor-weather-hacks-fetch my-livedoor-weather-hacks-city-id) :telop))

(provide 'my-livedoor-weather-hack)
;;; my-livedoor-weather-hacks ends here
