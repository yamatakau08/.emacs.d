;;; spotify-playlist.el ---                   -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <0000910700@JPC20165182>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'seml-mode)
(require 'helm)

(defun helm-spotify-playlist (&optional dummy)
  "list spotify playlist"
  (interactive)
  (helm :sources (helm-build-sync-source "Spotify playlist"
		   :candidates (spotify-playlist--toppage-playlist)
		   :candidate-number-limit 10000
		   :migemo t
		   :action (helm-make-actions
			    "playlist" #'spotify-playlist--titles)
		   :persistent-action #'ignore)))

(defun spotify-playlist--toppage-playlist ()
  "get playlist of spotify toppage https://open.spotify.com"
  (let (entity items)
    (setq entity (nth 2 (nth 2 (seml-xpath '(html body script)
				 (seml-encode-string-from-html
				  (with-temp-buffer
				    (url-insert-file-contents "https://open.spotify.com")
				    (buffer-string)))))))
    (string-match "Spotify.Entity = \\(.*\\)" entity)
    (setq items (let-alist (aref (let-alist (json-read-from-string (match-string 1 entity)) .content.items) 0) .content.items))
    (mapcar (lambda (item)
	      ;; remove "" , one playlist has control code character in description
	      ;;`(,(replace-regexp-in-string "" "" (let-alist item .description)) . ,item) ; list description
	      `(,(let-alist item .name) . ,item)) items)))

(defun spotify-playlist--titles (playlist)
  "list titles of the candidate playlist"
  (let ((playlist-name (let-alist playlist .name))
	(playlist-url  (let-alist playlist .external_urls.spotify)))
    (helm :sources (helm-build-sync-source playlist-name
		     :candidates (spotify-playlist--list-titles playlist-url)
		     :candidate-number-limit 10000
		     :migemo t
		     :action (helm-make-actions
			      "open track" #'helm-spotify--open-track
			      "playlist"   #'helm-spotify-playlist)))))

(defun helm-spotify--open-track (track)
  "open track with browser"
  (browse-url-default-browser (let-alist track .track.external_urls.spotify)))

(defun spotify-playlist--list-titles (playlist-url)
  "get titles of playlist-url"
  (let (entity items)
    (setq entity
	  (nth 2 (nth 2 (seml-xpath '(html body script)
			  (seml-encode-string-from-html
			   (with-temp-buffer
			     (url-insert-file-contents playlist-url)
			     (buffer-string)))))))
    (string-match "Spotify.Entity = \\(.*\\)" entity)
    (setq items (let-alist (json-read-from-string (match-string 1 entity)) .tracks.items))
    (seq-filter
     (lambda (elt) (if (car elt) elt)) ; remove the element which car is nil, because one playlist has nil track list.
     (mapcar (lambda (item)
	       (let ((track-name (let-alist item .track.name)))
		 (cons (if track-name
			   (concat
			    track-name
			    " --- "
			    (mapconcat (lambda (artists)
					 (let-alist artists .name)) (let-alist item .track.artists) ",")))
		       item))) items))))

;; sample function for scraping
(defun spotify-playlist ()
  "scraping spotify global top song from script"
  (interactive)
  (let* ((url "https://open.spotify.com/playlist/37i9dQZEVXbNG2KDcFcKOF")
	 ;(url "https://open.spotify.com/playlist/37i9dQZF1DX4UkKv8ED8jp")
	 ;(url "https://open.spotify.com/playlist/37i9dQZF1EtgC7xMnl7idD")
	 ;(url "https://open.spotify.com/playlist/37i9dQZF1DX2wavB60DxMA")
	 ;(url "https://open.spotify.com/artist/5Vo1hnCRmCM6M4thZCInCj")
	 ;(url "https://open.spotify.com/playlist/37i9dQZF1DX0MkpDFqXa80")
	 (scraping-name "spotify-global-top-song")
	 (buffer-html   (format "%s.html"   scraping-name))
	 (buffer-output (format "%s.output" scraping-name)))
    (with-output-to-temp-buffer buffer-html
      ;; Since url-insert-file-contents puts out the contents in current buffer,
      ;; change current buffer to buffer-html
      (set-buffer buffer-html)
      (url-insert-file-contents url))

    (with-output-to-temp-buffer buffer-output
      (let* ((seml (seml-encode-buffer-from-html buffer-html))
	     (entity-cell (nth 2 (nth 2 (seml-xpath '(html body script) seml))))
	     entity
	     json
	     play-list-name
	     play-list-url
	     tracks
	     (rank 1))
	(string-match "Spotify.Entity = \\(.*\\)" entity-cell)
	(setq entity (match-string 1 entity-cell))
	(setq json (json-read-from-string entity))
	(setq play-list-name (alist-get 'name json))
	(setq play-list-url (let-alist json .external_urls.spotify))

	(pp play-list-name)
	(terpri)
	(pp play-list-url)
	(terpri)

	(setq tracks (or (let-alist json .tracks.items)
			 (let-alist json .top_tracks)))

	(mapc (lambda (track)
		(let* ((album_name        (or (let-alist track .track.album.name)
					      (let-alist track .album.name)))
		       (track_artists     (or (let-alist track .track.artists)
					      (let-alist track .artists)))
		       (track_artist_name (let-alist (aref track_artists 0) .name))
		       (track_name        (or (let-alist track .track.name)
					      (let-alist track .name))))
		  (pp (list rank track_name track_artist_name album_name)))
		(setq rank (1+ rank))) tracks)
	))))

(defun spotify-playlist-with-pcase ()
  "scraping spotify global top song from script
It happens that some songs are not listed, because structre of that songs are not matched pcase pattern.
"
  (interactive)
  (let* ((url "https://open.spotify.com/playlist/37i9dQZEVXbNG2KDcFcKOF") ; some song are not listed
	 ;(url "https://open.spotify.com/playlist/37i9dQZF1DX4UkKv8ED8jp") ; some song are not listed
	 ;(url "https://open.spotify.com/playlist/37i9dQZF1EtgC7xMnl7idD") ; some song are not listed
	 ;(url "https://open.spotify.com/playlist/37i9dQZF1DX2wavB60DxMA") ; some song are not listed
	 (scraping-name "spotify-global-top-song")
	 (buffer-html   (format "%s.html"   scraping-name))
	 (buffer-output (format "%s.output" scraping-name)))
    (with-output-to-temp-buffer buffer-html
      ;; Since url-insert-file-contents puts out the contents in current buffer,
      ;; change current buffer to buffer-html
      (set-buffer buffer-html)
      (url-insert-file-contents url))

    (with-output-to-temp-buffer buffer-output
      (let* ((seml (seml-encode-buffer-from-html buffer-html))
	     (script (nth 2 (nth 2 (seml-xpath '(html body script) seml))))
	     entity
	     json
	     tracks
	     items
	     (rank 1)) ; since json data doesn't have rank data, generate it with this function
	(string-match "Spotify.Entity = \\(.*\\)" script)
	(setq entity (match-string 1 script))
	(setq json (json-read-from-string entity))
	(setq tracks (pcase json
		       (`(,collaborative ,description ,external_urls ,followers ,href ,id ,images ,name ,owner ,primary_color ,public ,snapshot_id ,tracks ,type ,uri ,availabe_markets)
			tracks)))
	(setq items
	      (cdr (pcase tracks
		     (`(,tracks ,href ,items ,limit ,next ,offset ,previous ,total) items))))

	(mapc (lambda (item)
		(pcase item
		  (`(,added_at
		     ,added_by
		     ,is_local0
		     ,primary_color
		     (track
		      (album ,album_type ,album_artists ,album_external_urls ,album_href ,album_id ,album_images (name . ,album_name) . ,album_other)
		      (artists . [(,_ . (,href0 ,id0 (name . ,artist_name) ,type0 ,artist_uri))])
		      ,disc_number ,duration_ms ,episode ,explicit ,external_ids ,external_urls ,href ,id . (,is_local1 ,is_playable (name . ,track_name) ,popularity ,preview_url ,track ,track_number ,type ,track_uri))
		     ,video_thumbnail)
		   (pp (list rank track_name artist_name album_name))
		   (terpri)))
		(setq rank (1+ rank))) items)
	))))

(provide 'spotify-playlist)
;;; spotify-playlist.el ends here
