  IDENTIFICATION DIVISION.
  PROGRAM-ID. MAIN.

  ENVIRONMENT DIVISION.
  CONFIGURATION SECTION.
  REPOSITORY.
      PROGRAM ORE
      PROGRAM REFINE
      PROGRAM SELL
      PROGRAM UPGRADE-WEAPON
      PROGRAM UPGRADE-ARMOR
      PROGRAM ADVENTURE
      PROGRAM SAVEGAME
      PROGRAM LOADGAME
      PROGRAM SHOP.

  INPUT-OUTPUT SECTION.

  DATA DIVISION.

  WORKING-STORAGE SECTION.
  01 ORE-STOCK      PIC 9(5) VALUE 100.
  01 INGOT-STOCK    PIC 9(5) VALUE 0.
  01 MONEY          PIC 9(6) VALUE 1000.
  01 COMMAND        PIC X(2) VALUE " ".
  01 GAME-DAY       PIC 9(4) VALUE 1.
  01 INGOT-PRICE    PIC 9(4) VALUE 500.
  01 WEAPON-LV      PIC 9 VALUE 0.
  01 ARMOR-LV       PIC 9 VALUE 0.
  01 PLAYER-HP      PIC S9(4) VALUE 100 SIGN TRAILING SEPARATE.
  01 PLAYER-ATK     PIC S9(3) VALUE 10.
  01 PLAYER-DEF     PIC S9(3) VALUE 5.
  01 DAY-PASS       PIC 9 VALUE 0.
  01 MONSTER-HP     PIC S9(4) SIGN TRAILING SEPARATE.
  01 MONSTER-ATK    PIC S9(3).
  01 DAMAGE         PIC S9(4) SIGN TRAILING SEPARATE. 
  01 BATTLE-COMMAND PIC 9.
  01 DEFENSE-MODE   PIC 9 VALUE 0.
  01 I              PIC 9.
  01 PLAYER-ITEM.
    05 ITEM-STOCK OCCURS 5 PIC 9(3) VALUE 0.
  01 ACTION-FLG     PIC 9 VALUE 0.
  COPY ITEMTABLE.

  PROCEDURE DIVISION.
    *> ====================
    *>  メイン処理
    *> ====================
    MAIN.
      DISPLAY "PROGRAM START"
      PERFORM GAME-LOOP
      STOP RUN.
    
    *> ==============================
    *>  ゲームメインループ
    *> ==============================
    GAME-LOOP.
      PERFORM UNTIL COMMAND = 0
        PERFORM SHOW-MENU
        PERFORM INPUT-COMMAND
        PERFORM EXECUTE-MENU
      END-PERFORM.
    
    *> ==============================
    *>  メニュー表示
    *> ==============================
    SHOW-MENU.
        DISPLAY "----------------------------"
        DISPLAY " DAY：" GAME-DAY
        DISPLAY "インゴット価格：" INGOT-PRICE
        DISPLAY "----------------------------"
        DISPLAY "1：鉱石購入"
        DISPLAY "2：インゴット精錬"
        DISPLAY "3：インゴット売却"
        DISPLAY "4：武器強化"
        DISPLAY "5：防具強化"
        DISPLAY "6：状態確認"
        DISPLAY "7：冒険"
        DISPLAY "8：セーブ"
        DISPLAY "9：ロード"
        DISPLAY "10：ショップ"
        DISPLAY "0：終了".

    *> ==============================
    *>  コマンド処理
    *> ==============================
    INPUT-COMMAND.
      ACCEPT COMMAND.

    EXECUTE-MENU.
      MOVE 0 TO DAY-PASS
      EVALUATE COMMAND
        WHEN "1"
          CALL "ORE"
          USING MONEY ORE-STOCK
        WHEN "2"
          CALL "REFINE"
          USING ORE-STOCK INGOT-STOCK
          MOVE 1 TO DAY-PASS
        WHEN "3"
          CALL "SELL"
          USING MONEY INGOT-STOCK INGOT-PRICE
        WHEN "4"
          CALL "UPGRADE-WEAPON"
          USING INGOT-STOCK WEAPON-LV PLAYER-ATK
          MOVE 1 TO DAY-PASS
        WHEN "5"
          CALL "UPGRADE-ARMOR"
          USING INGOT-STOCK ARMOR-LV PLAYER-DEF
          MOVE 1 TO DAY-PASS
        WHEN "6"
          PERFORM SHOW-STATUS
        WHEN "7"
          MOVE 0 TO ACTION-FLG
          CALL "ADVENTURE"
          USING PLAYER-HP
                PLAYER-ATK
                PLAYER-DEF
                MONEY
                ORE-STOCK
                PLAYER-ITEM
                ACTION-FLG
                LK-ITEM-TABLE
          IF ACTION-FLG = 1
            MOVE 1 TO DAY-PASS
          END-IF
        WHEN "8"
          CALL "SAVEGAME"
            USING MONEY
                  ORE-STOCK
                  INGOT-STOCK
                  GAME-DAY
                  INGOT-PRICE
                  WEAPON-LV
                  ARMOR-LV
                  PLAYER-ATK
                  PLAYER-DEF
                  PLAYER-ITEM
        WHEN "9"
          CALL "LOADGAME"
            USING MONEY
                  ORE-STOCK
                  INGOT-STOCK
                  GAME-DAY
                  INGOT-PRICE
                  WEAPON-LV
                  ARMOR-LV
                  PLAYER-ATK
                  PLAYER-DEF
                  PLAYER-ITEM
        WHEN "10"
          CALL "SHOP"
            USING MONEY PLAYER-HP PLAYER-ITEM LK-ITEM-TABLE
        WHEN "0"
          DISPLAY "ゲーム終了"
        WHEN OTHER
          DISPLAY "無効なコマンドです"
      END-EVALUATE
      
      IF DAY-PASS = 1
        ADD 1 TO GAME-DAY
        PERFORM UPDATE-PRICE
      END-IF.

    *> ==============================
    *>  ステータス確認
    *> ==============================
    SHOW-STATUS.
      DISPLAY "------ PLAYER ------"
      DISPLAY "HP：" PLAYER-HP
      DISPLAY "ATK：" PLAYER-ATK
      DISPLAY "DEF：" PLAYER-DEF
      DISPLAY "武器Lv：" WEAPON-LV
      DISPLAY "防具Lv：" ARMOR-LV

      DISPLAY "------ MATERIAL ------"
      DISPLAY "所持金：" MONEY
      DISPLAY "鉱石在庫：" ORE-STOCK
      DISPLAY "インゴット：" INGOT-STOCK
      DISPLAY "現在価格：" INGOT-PRICE

      DISPLAY "------ ITEM ------"
      PERFORM VARYING I FROM 1 UNTIL I > 5
        IF ITEM-STOCK(I) > 0
          DISPLAY "アイテム" I "：" ITEM-STOCK(I)
        END-IF
      END-PERFORM.
  
    *> ==============================
    *>  価格更新
    *> ==============================
    UPDATE-PRICE.
      COMPUTE INGOT-PRICE = FUNCTION INTEGER(FUNCTION RANDOM * 400) + 300.
