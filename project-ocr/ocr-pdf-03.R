#install.packages("tesseract")
library(tesseract)
library(pdftools)


eng <- tesseract("eng")
text <- ocr("http://jeroen.github.io/images/testocr.png", engine = eng)
cat(text)

pngfile <- pdf_convert('https://jeroen.github.io/images/ocrscan.pdf', dpi = 600)

text <- ocr(pngfile)
cat(text)