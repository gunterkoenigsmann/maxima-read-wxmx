;; maxima_read_wxmx.lisp -- load Maxima code from wxmx file
;; copyright 2016 Robert Dodier
;; I release this work under terms of the GNU General Public License
;; Quick-lisp-less version [uncompressed wxmx only] by Gunter Königsmann in 2018 


(defun $load_stream (in)
  (catch 'macsyma-quit (continue in t)))

;; Convert wxmx back to ASCII
(defun wxmx-unquote-xml (x)
  (let* ((tmp-x (string-substitute #\&        "&amp;" x))
	 (tmp-x (string-substitute #\<        "&lt;"  tmp-x))
	 (tmp-x (string-substitute #\>        "&gt;"  tmp-x))
	 (tmp-x (string-substitute #\Return   "&#13;" tmp-x))
	 (tmp-x (string-substitute #\Linefeed "&#13;" tmp-x))
	 (tmp-x (string-substitute #\Newline  "&#13;" tmp-x)))))

;; returns a string containing the Maxima code from the content.xml entry in FILE-NAME-WXMX
;;
;; Todo: An error message in the case that no<wxMaximaDocument and no </wxMaximaDocument>
;; are found.
(defun $read_content_xml (filename)
  (with-open-file (stream filename)
		  ;; Skip to the first line of the wxmx file's contents
		  (loop for line = (read-line stream nil)
			while (and
			       line
			       (not
				(search "<wxMaximaDocument" line))))

		  ;; Extract all lines containing a <line> and a </line> tag.
		  (loop for line = (read-line stream nil)
			while (and
			       line
			       (not
				(search "</wxMaximaDocument>" line)))
			do (
			    let (
				 (linestart (search "<line>"  line))
				 (lineend   (search "</line>" line))
				 )
			     (if (and linestart lineend)
				 (collect
				  (wxmx-unquote-xml
				   (subseq line
					   (+ linestart 6)
					   (- lineend 0)))))))))

		  

(defun $load_wxmx (file-name-wxmx)
  ($load_stream (make-string-input-stream ($read_content_xml file-name-wxmx))))

(defun $parse_content_xml (file-name-wxmx)
  (declare (special *mread-prompt*))
  (let*
    ((content-string ($read_content_xml file-name-wxmx))
     (input-stream (make-string-input-stream content-string))
     (*mread-prompt*))
    (cons '(mlist) (loop for x = (mread input-stream) while x collect (third x)))))
