#! /bin/bash
#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# generate help files with pandoc

vers=`git rev-list HEAD --count`
css="style/pandoc-bootstrap-template-master/template.css"
htmlTemplate="style/pandoc-bootstrap-template-master/template.html"
htmlOutput="html/accessmod_user_manual.html"
pdfOutput="pdf/accessmod_user_manual.pdf"
src="source/accessmod.md"



# html files
pandoc "$src" -o "$htmlOutput" --template "$htmlTemplate" --css "$css" --self-contained --toc --toc-depth 1
pandoc "$src" -o "$pdfOutput" --toc --toc-depth 2


