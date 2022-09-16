# 第9章 コマンドラインツールの作成

## memo
* 書籍ではHUnitを`hjq.cabal`に追記と記載しているが、ビルドやテスト実行時に消えてしまうので`package.yaml`に追記
* Invalid characterと言われたらchcp 65001
* サンプル実行コマンド：`echo '[ { "age": 25, "name": "Y", "tel-number": "111-1111" }, { "age": 26, "name": "֓Ԏq", "tel-number": "222-2222" }, {
 "age": 27, "name": "RcY", "tel-number": "333-3333" } ]' | stack run -- '{\"name\":.[2].name,\"tel-numer\":.[2].tel-number}'`
    * ※日本語が文字化けしてしまうので要対応（[参考サイト](https://haskell.jp/blog/posts/2019/unicode-show.html)）
