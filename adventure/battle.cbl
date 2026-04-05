  IDENTIFICATION DIVISION.
  PROGRAM-ID. BATTLE.

  DATA DIVISION.

  WORKING-STORAGE SECTION.
  01 DAMAGE         PIC S9(4).
  01 BATTLE-COMMAND PIC 99.
  01 DEFENSE-MODE   PIC 9 VALUE 0.
  01 I              PIC 9.
  01 RUN-SUCCESS    PIC 9 VALUE 0.
  01 HAS-ITEM       PIC 9 VALUE 0.
  01 REWARD-MONEY   PIC 9(4).
  01 MONSTER-TABLE.
    05 MONSTER-DATA OCCURS 3.
      10 M-NAME     PIC X(20).
      10 M-HP       PIC 9(4).
      10 M-ATK      PIC 9(3).
      10 M-REWARD   PIC 9(4). 
  01 MONSTER-INDEX  PIC 9.
  01 CRITICAL-RATE  PIC 9(3).
  01 ITEM-COUNT     PIC 9 VALUE 5.

  LINKAGE SECTION.
  01 LK-PLAYER-HP  PIC S9(4).
  01 LK-PLAYER-ATK PIC S9(3).
  01 LK-PLAYER-DEF PIC S9(3).
  01 LK-MONEY      PIC 9(6).
  01 LK-ORE-STOCK  PIC 9(5).
  01 LK-ITEM.
    05 LK-ITEM-STOCK OCCURS 5 PIC 9(3).
  01 LK-RESULT     PIC 9.
  01 LK-MONSTER.
    05 LK-MONSTER-NAME   PIC X(20).
    05 LK-MONSTER-HP   PIC S9(4).
    05 LK-MONSTER-ATK  PIC S9(3).
  COPY ITEMTABLE.

  PROCEDURE DIVISION USING
    LK-PLAYER-HP
    LK-PLAYER-ATK
    LK-PLAYER-DEF
    LK-MONEY
    LK-ORE-STOCK
    LK-ITEM
    LK-RESULT
    LK-MONSTER
    LK-ITEM-TABLE.

    MOVE "スライム" TO M-NAME(1)
    MOVE 50 TO M-HP(1)
    MOVE 5 TO M-ATK(1)
    MOVE 100 TO M-REWARD(1)

    MOVE "ゴブリン" TO M-NAME(2)
    MOVE 80 TO M-HP(2)
    MOVE 10 TO M-ATK(2)
    MOVE 150 TO M-REWARD(2)

    MOVE "オーク" TO M-NAME(3)
    MOVE 120 TO M-HP(3)
    MOVE 15 TO M-ATK(3)
    MOVE 250 TO M-REWARD(3)

    COMPUTE MONSTER-INDEX = FUNCTION INTEGER(FUNCTION RANDOM * 3) + 1

    MOVE M-NAME(MONSTER-INDEX) TO LK-MONSTER-NAME
    MOVE M-HP(MONSTER-INDEX) TO LK-MONSTER-HP
    MOVE M-ATK(MONSTER-INDEX) TO LK-MONSTER-ATK

    DISPLAY FUNCTION TRIM(LK-MONSTER-NAME) "が現れた！"

    MOVE 0 TO RUN-SUCCESS

    PERFORM UNTIL LK-MONSTER-HP <= 0 OR LK-PLAYER-HP <= 0

      PERFORM PLAYER-TURN

      *> 逃げたら終了
      IF RUN-SUCCESS = 1
        EXIT PERFORM
      END-IF

      *> アイテムで戻った場合はやり直し
      IF BATTLE-COMMAND = 3
        CONTINUE
      ELSE
        IF LK-MONSTER-HP > 0
          PERFORM MONSTER-TURN
        END-IF
      END-IF

    END-PERFORM

    IF RUN-SUCCESS = 1
      MOVE 3 TO LK-RESULT
      GOBACK
    END-IF

    IF LK-PLAYER-HP <= 0
      MOVE 2 TO LK-RESULT
    ELSE
      MOVE 1 TO LK-RESULT
    END-IF

    GOBACK.

    *> ==============================
    *>  プレイヤーターン
    *> ============================== 
    PLAYER-TURN.
      MOVE 0 TO BATTLE-COMMAND
      PERFORM UNTIL BATTLE-COMMAND >= 1 AND BATTLE-COMMAND <= 4
        DISPLAY "------------------"
        DISPLAY "あなたのターン"
        DISPLAY "1：攻撃"
        DISPLAY "2：防御"
        DISPLAY "3：アイテム"
        DISPLAY "4：逃げる"
        DISPLAY "------------------"

        ACCEPT BATTLE-COMMAND

        IF BATTLE-COMMAND < 1 OR BATTLE-COMMAND > 4
          DISPLAY "無効なコマンドです"
        END-IF
      END-PERFORM


      EVALUATE BATTLE-COMMAND
        WHEN 1
          PERFORM PLAYER-ATTACK
        WHEN 2
          PERFORM PLAYER-DEFENSE
        WHEN 3
          PERFORM USE-ITEM
        WHEN 4
          PERFORM RUN-AWAY
      END-EVALUATE.
    
    *> ==============================
    *>  攻撃コマンド
    *> ============================== 
    PLAYER-ATTACK.
      DISPLAY "あなたの攻撃！"

      COMPUTE CRITICAL-RATE = FUNCTION INTEGER(FUNCTION RANDOM * 100)

      IF CRITICAL-RATE < 20
        DISPLAY "クリティカルヒット!"
        COMPUTE DAMAGE = LK-PLAYER-ATK * 2
      ELSE
        MOVE LK-PLAYER-ATK TO DAMAGE
      END-IF

      DISPLAY FUNCTION TRIM(LK-MONSTER-NAME) "に" DAMAGE "のダメージ!"
      SUBTRACT DAMAGE FROM LK-MONSTER-HP

      IF LK-MONSTER-HP < 0
        MOVE 0 TO LK-MONSTER-HP
      END-IF

      DISPLAY FUNCTION TRIM(LK-MONSTER-NAME) "：" LK-MONSTER-HP
      IF LK-MONSTER-HP <= 0
        DISPLAY FUNCTION TRIM(LK-MONSTER-NAME) "を倒した！"
        PERFORM BATTLE-REWARD
      END-IF.

    *> ==============================
    *>  防御コマンド
    *> ============================== 
    PLAYER-DEFENSE.
      DISPLAY "守りを固めた！"
      MOVE 1 TO DEFENSE-MODE.

    *> ==============================
    *>  アイテムコマンド
    *> ============================== 
    USE-ITEM.
      DISPLAY "------ アイテム ------"

      MOVE 0 TO HAS-ITEM
      PERFORM VARYING I  FROM 1 UNTIL I > 5
        IF LK-ITEM-STOCK(I) > 0
          MOVE 1 TO HAS-ITEM
          DISPLAY I "：" 
            FUNCTION TRIM(LK-ITEM-NAME(I))
            "(所持数：" LK-ITEM-STOCK(I) ")"
        END-IF
      END-PERFORM

      IF HAS-ITEM = 0
        DISPLAY "アイテムを持っていない"
        MOVE 3 TO BATTLE-COMMAND
        EXIT PARAGRAPH
      END-IF

      DISPLAY "0：戻る"

      ACCEPT BATTLE-COMMAND

      IF BATTLE-COMMAND = 0
        MOVE 3 TO BATTLE-COMMAND
        EXIT PARAGRAPH
      END-IF

      IF BATTLE-COMMAND < 1 OR BATTLE-COMMAND > ITEM-COUNT
        DISPLAY "無効なコマンドです"
        MOVE 3 TO BATTLE-COMMAND
        EXIT PARAGRAPH
      END-IF

      IF LK-ITEM-STOCK(BATTLE-COMMAND) > 0
        SUBTRACT 1 FROM LK-ITEM-STOCK(BATTLE-COMMAND)

        ADD LK-ITEM-HEAL(BATTLE-COMMAND) TO LK-PLAYER-HP

        IF LK-PLAYER-HP > 100
          MOVE 100 TO LK-PLAYER-HP
        END-IF

        DISPLAY FUNCTION TRIM(LK-ITEM-NAME(BATTLE-COMMAND))
          "を使用してHPを"
          LK-ITEM-HEAL(BATTLE-COMMAND)
          "回復！"
      ELSE
        DISPLAY "無効なコマンドです"
      END-IF.

    *> ==============================
    *>  逃げるコマンド
    *> ============================== 
    RUN-AWAY.
      IF FUNCTION RANDOM < 0.7
        DISPLAY "うまく逃げられた！"
        MOVE 1 TO RUN-SUCCESS
      ELSE
        DISPLAY "逃げられなかった！"
      END-IF.

    *> ==============================
    *>  モンスターターン
    *> ==============================
    MONSTER-TURN.
      DISPLAY FUNCTION TRIM(LK-MONSTER-NAME) "の攻撃！"
        COMPUTE DAMAGE = LK-MONSTER-ATK - LK-PLAYER-DEF

        IF DAMAGE <= 0
          MOVE 1 TO DAMAGE
        END-IF

        IF DEFENSE-MODE = 1
          DIVIDE DAMAGE BY 2 GIVING DAMAGE
          DISPLAY "防御でダメージ半減！"
          MOVE 0 TO DEFENSE-MODE
        END-IF

        SUBTRACT DAMAGE FROM LK-PLAYER-HP

        IF LK-PLAYER-HP < 0
          MOVE 0 TO LK-PLAYER-HP
        END-IF

        DISPLAY "あなたのHP：" LK-PLAYER-HP.
  
    *> ==============================
    *>  報酬
    *> ============================== 
    BATTLE-REWARD.
      MOVE M-REWARD(MONSTER-INDEX) TO REWARD-MONEY
      ADD REWARD-MONEY TO LK-MONEY
      ADD 1 TO LK-ORE-STOCK
      DISPLAY "鉱石 +1"
      DISPLAY "獲得金額：" REWARD-MONEY
      DISPLAY "所持金：" LK-MONEY.
