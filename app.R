library(shiny)

# --- 定数（対戦相手データ） ---
OPPONENTS <- list(
  list(name = "のみ003", title = "第1回大会福岡代表", hp=1, atk=1, def=0, spd=98),
  list(name = "ラッシュやまのて", title = "第1回大会準優勝", hp=34, atk=33, def=33, spd=0),
  list(name = "しらはまのすな", title = "第2回大会準優勝", hp=28, atk=48, def=12, spd=12),
  list(name = "いかりのひでよし", title = "第1回大会優勝", hp=26, atk=36, def=36, spd=2),
  list(name = "うっ☆マンボ", title = "第2回大会優勝", hp=26, atk=38, def=32, spd=4)
)

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      input[type=number]::-webkit-inner-spin-button, 
      input[type=number]::-webkit-outer-spin-button { -webkit-appearance: none; margin: 0; }
      input[type=number] { -moz-appearance: textfield; }
      .status-warning { color: red; font-weight: bold; margin-bottom: 10px; font-size: 0.85em; }
      .battle-log { background-color: #f8f9fa; border: 1px solid #ddd; padding: 15px; height: 500px; overflow-y: auto; font-family: 'Courier New', monospace; white-space: pre-wrap; }
      .description-text { line-height: 1.2; margin-bottom: 20px; font-size: 1em; }
    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        setInterval(function() { $('.battle-log').each(function() { this.scrollTop = this.scrollHeight; }); }, 200);
      });
    "))
  ),
  
  h1("JBSクエスト・トリビュート"),
  
  # --- ヘッダー ---
  div(class = "description-text",
      "任意のヒットポイント（HP），攻撃力（ATK），守備力（DEF），素早さ（SPD）のキャラクターを生成してバトルさせるゲームです．", br(),
      "それぞれのステータスの最小値は0，最大値は100ですが，4つのステータスの合計値は100以下でなければなりません．", br(),
      "モード1では，あなたのキャラクターが5人の強豪と戦って，5連勝したらクリアです．", br(),
      "モード2では，任意のステータスをもつ2つのキャラクターを自由に対戦させることができます．", br(),
      "すべて入力し終えたら，「チャレンジ開始!」ボタンまたは「対戦開始!」ボタンを押してください．「次のターンへ」ボタンを押すと，戦闘の様子が1ターンずつ表示されます．", br(),
      "注意事項：", "JBSクエストのソースコードはマル秘なので，本ゲームはJBSクエストを完全再現したものではありません．本ゲームは，製作者がR言語で独自に作成したものです．", br(),
      "製作者：", tags$a(href="https://researchmap.jp/mtakaha", "高橋 将宜"), br(),
      "参考資料：", tags$a(href="https://jbsmemorial.sakura.ne.jp/etc/quest1.html", "JBSクエストの記録1"), "，", tags$a(href="https://jbsmemorial.sakura.ne.jp/etc/quest2.html", "JBSクエストの記録2")
  ),
  
  hr(),
  
  tabsetPanel(
    tabPanel("モード1: 5連勝チャレンジ",
             sidebarLayout(
               sidebarPanel(width = 4,
                            textInput("m1_name", "あなたのキャラクターの名前", value = ""),
                            numericInput("m1_hp", "ヒットポイント", value = NA, min = 0),
                            numericInput("m1_atk", "攻撃力", value = NA, min = 0),
                            numericInput("m1_def", "守備力", value = NA, min = 0),
                            numericInput("m1_spd", "素早さ", value = NA, min = 0),
                            uiOutput("warning_m1"),
                            hr(),
                            actionButton("start_m1", "チャレンジ開始！", class = "btn-primary", width = "100%"),
                            uiOutput("next_btn_ui_m1")
               ),
               mainPanel(width = 8, div(class = "battle-log", verbatimTextOutput("log_m1")))
             )
    ),
    tabPanel("モード2: 自由対戦",
             sidebarLayout(
               sidebarPanel(width = 4,
                            fluidRow(
                              column(6, textInput("m2_n1", "キャラ1の名前", ""), numericInput("m2_h1", "ヒットポイント", NA), numericInput("m2_a1", "攻撃力", NA), numericInput("m2_d1", "守備力", NA), numericInput("m2_s1", "素早さ", NA)),
                              column(6, textInput("m2_n2", "キャラ2の名前", ""), numericInput("m2_h2", "ヒットポイント", NA), numericInput("m2_a2", "攻撃力", NA), numericInput("m2_d2", "守備力", NA), numericInput("m2_s2", "素早さ", NA))
                            ),
                            uiOutput("warning_m2"),
                            hr(),
                            actionButton("start_m2", "対戦開始！", class = "btn-success", width = "100%"),
                            uiOutput("next_btn_ui_m2")
               ),
               mainPanel(width = 8, div(class = "battle-log", verbatimTextOutput("log_m2")))
             )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  battle_state <- reactiveValues(active = FALSE, mode = 0, c1 = NULL, c2 = NULL, turn = 1, opponent_idx = 1, log = "", finished = FALSE)
  
  check_stats <- function(n, h, a, d, s) {
    if (n == "" || any(is.na(c(h, a, d, s)))) return("全項目入力が必要です")
    if (sum(c(h, a, d, s)) > 100) return(paste0("合計100を超えています (現在:", sum(c(h, a, d, s)), ")"))
    if (h < 0 || a < 0 || d < 0 || s < 0) return("ステータスに負の値は入力できません")
    return(NULL)
  }
  
  output$warning_m1 <- renderUI({ msg <- check_stats(input$m1_name, input$m1_hp, input$m1_atk, input$m1_def, input$m1_spd); if(!is.null(msg)) div(class="status-warning", msg) })
  output$warning_m2 <- renderUI({ 
    msg1 <- check_stats(input$m2_n1, input$m2_h1, input$m2_a1, input$m2_d1, input$m2_s1)
    msg2 <- check_stats(input$m2_n2, input$m2_h2, input$m2_a2, input$m2_d2, input$m2_s2)
    msg <- if(!is.null(msg1)) msg1 else msg2
    if(!is.null(msg)) div(class="status-warning", msg) 
  })
  
  fmt_stats <- function(unit) {
    paste0("[", unit$name, "] HP:", unit$hp, " ATK:", unit$atk, " DEF:", unit$def, " SPD:", unit$spd)
  }
  
  execute_turn <- function() {
    c1 <- battle_state$c1; c2 <- battle_state$c2
    
    # --- ターン開始時にHPが0ならば攻撃処理をスキップ ---
    if (c1$hp > 0 && c2$hp > 0) {
      msg <- paste0("[ターン ", battle_state$turn, "]\n")
      order <- if ((c1$spd * runif(1)) >= (c2$spd * runif(1))) list(c1, c2) else list(c2, c1)
      
      for (i in 1:2) {
        atk_unit <- order[[i]]; def_unit <- if(i==1) order[[2]] else order[[1]]
        if (atk_unit$hp <= 0) next
        evasion_rate <- max(0, def_unit$spd - atk_unit$spd)
        if (runif(1, 0, 100) < evasion_rate) {
          action_msg <- "しかし かわされた！"
        } else {
          base_dmg <- (atk_unit$atk - (def_unit$def / 2)) / 2
          damage <- round(base_dmg + runif(1, -abs(base_dmg)/16, abs(base_dmg)/16))
          if (damage <= 0 && atk_unit$atk > 0) {
            damage <- if (runif(1) < 0.5) 1 else 0
          }
          def_unit$hp <- max(0, def_unit$hp - damage)
          action_msg <- paste0(damage, " のダメージを与えた！")
        }
        if (def_unit$id == "c1") c1$hp <- def_unit$hp else c2$hp <- def_unit$hp
        msg <- paste0(msg, atk_unit$name, " の攻撃！ ", action_msg, " (", c1$name, ":", c1$hp, ", ", c2$name, ":", c2$hp, ")\n")
        if (c1$hp <= 0 || c2$hp <= 0) break
      }
      battle_state$c1 <- c1; battle_state$c2 <- c2
      battle_state$log <- paste0(battle_state$log, msg, "\n"); battle_state$turn <- battle_state$turn + 1
    }
    
    # --- 終了処理（HPが0で始まった場合もここを通過して終了する） ---
    if (c1$hp <= 0 || c2$hp <= 0) {
      battle_state$finished <- TRUE
      winner <- if(c1$hp > 0) c1$name else c2$name
      battle_state$log <- paste0(battle_state$log, ">>>> ", winner, " の勝利！\n")
      if (battle_state$mode == 1 && winner == c1$name && battle_state$opponent_idx == 5) {
        battle_state$log <- paste0(battle_state$log, "\n祝！！ 5連勝達成！ あなたが真の王者です！\n")
      } else if (battle_state$mode == 1 && winner != c1$name) {
        battle_state$log <- paste0(battle_state$log, "\n敗北しました... チャレンジ失敗です。\n")
      }
    }
  }
  
  observeEvent(input$start_m1, {
    req(is.null(check_stats(input$m1_name, input$m1_hp, input$m1_atk, input$m1_def, input$m1_spd)))
    battle_state$active <- TRUE; battle_state$mode <- 1; battle_state$opponent_idx <- 1; battle_state$turn <- 1; battle_state$finished <- FALSE
    battle_state$c1 <- list(id="c1", name=input$m1_name, hp=input$m1_hp, atk=input$m1_atk, def=input$m1_def, spd=input$m1_spd)
    opp <- OPPONENTS[[1]]; opp$id <- "c2"
    battle_state$c2 <- opp
    battle_state$log <- paste0("=== チャレンジ開始 vs ", opp$name, " ===\n",
                               fmt_stats(battle_state$c1), "\n",
                               fmt_stats(opp), " (", opp$title, ")\n\n")
  })
  
  observeEvent(input$start_m2, {
    req(is.null(check_stats(input$m2_n1, input$m2_h1, input$m2_a1, input$m2_d1, input$m2_s1)))
    battle_state$active <- TRUE; battle_state$mode <- 2; battle_state$turn <- 1; battle_state$finished <- FALSE
    battle_state$c1 <- list(id="c1", name=input$m2_n1, hp=input$m2_h1, atk=input$m2_a1, def=input$m2_d1, spd=input$m2_s1)
    battle_state$c2 <- list(id="c2", name=input$m2_n2, hp=input$m2_h2, atk=input$m2_a2, def=input$m2_d2, spd=input$m2_s2)
    battle_state$log <- paste0("--- 自由対戦開始 ---\n",
                               fmt_stats(battle_state$c1), "\n",
                               fmt_stats(battle_state$c2), "\n\n")
  })
  
  observeEvent(input$next_turn, {
    if (!battle_state$finished) {
      execute_turn()
    } else if (battle_state$mode == 1 && battle_state$c1$hp > 0 && battle_state$opponent_idx < 5) {
      battle_state$opponent_idx <- battle_state$opponent_idx + 1
      c1 <- battle_state$c1; c1$hp <- input$m1_hp; battle_state$c1 <- c1
      opp <- OPPONENTS[[battle_state$opponent_idx]]; opp$id <- "c2"
      battle_state$c2 <- opp
      battle_state$turn <- 1; battle_state$finished <- FALSE
      battle_state$log <- paste0(battle_state$log, "=== 第", battle_state$opponent_idx, "戦 vs ", opp$name, " ===\n",
                                 fmt_stats(battle_state$c1), "\n",
                                 fmt_stats(opp), " (", opp$title, ")\n\n")
    }
  })
  
  output$log_m1 <- renderText({ req(battle_state$mode == 1); battle_state$log })
  output$log_m2 <- renderText({ req(battle_state$mode == 2); battle_state$log })
  
  btn_ui <- function(mode) {
    if(!battle_state$active || battle_state$mode != mode) return(NULL)
    if(battle_state$finished && (mode == 2 || (mode == 1 && (battle_state$c1$hp <= 0 || battle_state$opponent_idx == 5)))) return(NULL)
    label <- if(battle_state$finished) "次の試合へ進む" else "次のターンへ"
    actionButton("next_turn", label, class = "btn-warning", style="margin-top:10px; width:100%;")
  }
  output$next_btn_ui_m1 <- renderUI({ btn_ui(1) })
  output$next_btn_ui_m2 <- renderUI({ btn_ui(2) })
}

shinyApp(ui = ui, server = server)