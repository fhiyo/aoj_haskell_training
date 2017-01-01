# AOJの問題をHaskellで解く

AOJのHaskellの問題を解くための環境．

## 構成

```
.
├── README.md
├── hs.sh
├── src
│   ├── 0000
│   │   └── 0000.hs
│   ├── 0001
│   │   └── 0001.hs
│   └── 0002
│       └── 0002.hs
└── test
    ├── 0000
    │   ├── input
    │   │   └── 0.txt
    │   └── output
    │       └── 0.txt
    ├── 0001
    │   ├── input
    │   │   ├── 1.txt
    │   │   └── 2.txt
    │   └── output
    │       ├── 1.txt
    │       └── 2.txt
    └── 0002
        ├── input
        │   └── 1.txt
        └── output
            └── 1.txt
```

## サイクル
1. 解きたいAOJの問題を決める
1. `./hs.sh --make-env <問題番号>`
1. test/<問題番号>/{input,output}/ の下に同じファイル名で入力と出力のファイルを用意する (test用の入力/出力値) (複数テストケースを置ける)
1. `vim src/<問題番号>/<問題番号>.hs`でソースコードを書く
1. `./hs.sh --test <問題番号>`でテスト
1. テストをPassしたらコードをAOJに提出する

## その他
- `./hs.sh --lint <問題番号>`でhlintを使ったコーディングチェックをする
- `./hs.sh --clean`でビルド時に生成したファイルを削除
- `./hs.sh --copy <問題番号>`で書いたコードをクリップボードにコピー

## TODO
- http://qiita.com/stone_725/items/4290dc12a05ef6f824f0  
を参考にしてPython (?) で書いたコードをそのまま自動提出するスクリプトを組む

- stackを使った開発サイクルに切り替える
