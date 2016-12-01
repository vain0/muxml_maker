# MuxmlMaker
[TypeingMania 5](http://www.sightseekerstudio.com/typingmania) や [IshoTyping](https://sites.google.com/site/ishotyping) で使用できる音楽 XML ファイルの作成をサポートするツール。

## 初期設定
* MuxmlMaker.yaml の ``StoragePath: ``の行に、出力ファイルを保存するためのディレクトリを書く。
  * このディレクトリの下に、 `info`, `lrc`, `xml` という名前のディレクトリを作っておく。
  * 記述例:

```
StoragePath: D:/IshoTyping/storage
```

## 使い方
* 音楽ファイル (.mp3 など) と歌詞ファイル (.lrc, UTF-8) を用意する。
* [RhythmicaLyrics](http://suwa.6.ql.bz/RhythmicaLyrics.html) などを用いて、歌詞ファイルにタグを付ける。
* 歌詞ファイルの冒頭に、以下のようにメタデータを追加する。

```
@name = 曲のタイトル
@music = 音楽ファイルのパス
@video = 動画ファイルのパス (省略可)
@pic = 背景画像ファイルのパス (省略可)
@genre = ジャンル (省略可)
@artist = アーティスト (省略可)
[mm:ss:xx]歌詞1行目[mm:ss:xx]
...
```

* MuxmlMaker があるフォルダのコンソールを起動して、以下のコマンドを打つ。

```
MuxmlMaker 歌詞ファイルのパス
```

* MuxmlMaker が起動し、「読み入力モード」になる。
  * 歌詞が1行ごとに表示されるので、読み仮名を入力していく。
* 読み入力が完了したら、StoragePath 直下の `info`, `lrc`, `xml` ディレクトリに、結果のファイルが出力される。

## 権利
All in this repository which **vain0** individually reserves all rights for are under public domain.
