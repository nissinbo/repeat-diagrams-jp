# デザインダイアグラム作成ツール

デザインダイアグラムを作成するためのShinyアプリ[repeat-diagrams](https://presc.sdu.dk/repeat-diagrams/)の日本語入力に対応し、pptx出力の機能を追加したもの。

- [Shinyapps](https://nissinbo.shinyapps.io/repeat-diagrams-jp/)
- [元ツールのソースコード](https://source.coderefinery.org/lcl/repeat-diagrams)

## 環境とセットアップについて

- 以下、元ツールからの引用

Prerequisites:
* Linux server
* Installed Shiny backend (see https://docs.rstudio.com/shiny-server/#installation)
* R 3.5.1 or higher (compatibility has only been tested using 3.5.1)
	* ggplot2 library
	* ggfittext library
	* shiny library
In R run install.packages(c("ggplot2", "ggfittext", "shiny"))

Move app.R and make-diagram.R to a separate folder in /srv/shiny-server/repeat-diagrams/,
e.g. /srv/shiny-server/repeat-diagrams/.

Create a www directory in the folder, i.e. /srv/shiny-server/repeat-diagrams/www/.
Ensure that the "shiny" user has read and write permission for the www folder.

Navigate to http://shiny.your-server.org/repeat-diagrams/ and ensure that everything works as expected.