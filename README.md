# WaChatConverter

WhatsApp conversations can be exported from the app in a .txt format.
However, the .txt files are not easy to read. This tool takes the .txt
file, parses it, generates a HTML page with CSS styling,
and compiles the page into a pdf using `wkhtmltopdf`.

---

This project is built with `stack`. Make sure you have `stack` and `wkhtmltopdf`
installed. The application is run with `stack run [path to chat.txt]`.

## Parsing doesn't work

WhatsApp might change the format of the .txt file in the future,
in which case you might have to fiddle with the parse functions in `Lib.hs`.
