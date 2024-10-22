;;; init-waka.el --- Config wakatime -*- lexical-binding: t; -*-
;;
;; Self-host wakapi(https://github.com/muety/wakapi) and setup wakatime-mode.


;; 1. Install wakapi locally. Wakapi is written by Go, so downlaod the released
;; single binary and put it in PATH.
;; 2. Install and require `wakatime-mode' in Emacs.
;; 3. Download the wakatime-cli(https://github.com/wakatime/wakatime-cli), and
;; put the single binary in a custom path, then make `wakatime-mode' know it.
(setq wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli"))

;; 4. Start up wakapi from command line, or run it as a service.
;; It is recommended to run it as a SystemD service, but I am running it with
;; pebble. Run `pebble run' in command line.
;;
;; Before launch, download the sample wakapi.yml from github.
;; Open the website and create an account.


;; 5. After login on the website, get the api key. There are two places need the
;; api key:
;; - init-secrets.el: make `wakatime-mode' know the api key.
;; - ~/.wakatime.cfg: make wakapi know the api key.
(require 'init-secrets)
;; Load the api from init-secrets.el
(setq wakatime-api-key my-wakapi-key)

(provide 'init-waka)
