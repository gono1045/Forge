#!/bin/bash

# このファイルの場所に移動
cd "$(dirname "$0")"

echo "=============================="
echo "   Forge Game 起動"
echo "=============================="
echo ""

# dataフォルダがなければ作成
mkdir -p data

# 実行
chmod +x bin/game
./bin/game

echo ""
echo "=============================="
echo "   ゲーム終了"
echo "=============================="

# ウィンドウ閉じないように待機
read -p "Enterキーで閉じる..."
exit
