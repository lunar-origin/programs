library(tidyverse)

# グループ分けを生成するための窓口の関数
generate_groupings = function(chars) {
  
  # 初期状態で全ての文字を残し，空のグループを持つ
  all_results = group_recursive(chars, list(), character())
  
  # 再帰処理の結果を整理して重複を削除
  all_results = unique(all_results)
  all_results = sort(all_results)
  
  return(all_results)
}


# グループ分けを生成するための再帰関数
group_recursive = function(remaining_chars, current_groups, all_results) {
  
  # 再帰の終点処理
  if (length(remaining_chars) == 0) {
    # 各グループの要素をソートし，グループ全体を標準化
    standardized_groups = sapply(current_groups, function(group) {
      paste(sort(strsplit(group, NULL)[[1]]), collapse = "")
    })
    result = paste(sort(standardized_groups), collapse = "+")
    
    # 重複を削除
    all_results = unique(c(all_results, result))
    return(all_results)
  }
  
  for (i in 1:length(remaining_chars)) {
    new_char = remaining_chars[i]
    new_remaining = remaining_chars[-i]
    
    # 現在の文字を新しいグループとして追加して再帰
    new_current_groups = c(current_groups, new_char)
    all_results = group_recursive(new_remaining, new_current_groups, all_results)
    
    # 既存のグループに現在の文字を追加して再帰
    if (length(current_groups) > 0) {
      for (j in 1:length(current_groups)) {
        updated_group = paste(c(strsplit(current_groups[[j]], NULL)[[1]], new_char), collapse = "")
        updated_groups = current_groups
        updated_groups[j] = updated_group
        all_results = group_recursive(new_remaining, updated_groups, all_results)
      }
    }
  }
  
  return(all_results)
}


# グループ分けを処理する関数
apply_grouping = function(data, grouping) {
  groups = unlist(strsplit(grouping, "\\+"))
  new_data = sapply(data, function(x) {
    for (group in groups) {
      if (x %in% unlist(strsplit(group, ""))) {
        return(group)
      }
    }
    return(x)  # 万が一，どのグループにも該当しない場合は元の値を返す
  })
  return(new_data)
}


# 1要因参加者間分散分析のモデル関数
between.model = function(df) {
  
  # グループ分けのパターンを生成
  elements = unique(df$Factor)
  groupings = generate_groupings(elements)
  
  # 結果を格納するリスト
  results_list = list()
  
  # 各グループ分けに対して処理を実施
  for (grouping in groupings) {
    # グループ分けを適用
    transformed_col = apply_grouping(df$Factor, grouping)
    
    # データフレームに変換列を追加
    df_transformed = df %>%
      mutate(!!paste0("", gsub("\\+", "_", grouping)) := as.factor(transformed_col))
    
    # モデル式の設定
    column_name = paste0("", gsub("\\+", "_", grouping))
    formula = if ("+" %in% strsplit(grouping, "")[[1]]) {
      as.formula(paste("Value ~", column_name))
    } else {
      as.formula("Value ~ 1")
    }
    
    # 適応する列が存在するか確認
    if (length(df_transformed[[column_name]]) > 0) {
      # NA を除去
      df_transformed_no_na = na.omit(df_transformed)
      
      # モデル適用とAIC抽出
      model = lm(formula, data = df_transformed_no_na)
      aic = AIC(model)
      
      # 結果をリストに保存
      results_list[[paste0("Grouping_", gsub("\\+", "_", grouping))]] = 
        c("Grouping" = column_name, "AIC" = aic)
    }
  }
  
  # 結果のデータフレームを作成
  results_df = bind_rows(results_list)
  results_df$AIC = as.numeric(results_df$AIC)
  
  # AICに基づいてソート
  results_df_sorted = results_df %>% arrange(AIC)
  
  # 結果の表示 (tibble型をデータフレームとして表示)
  print(as.data.frame(results_df_sorted))
}


# 1要因参加者内分散分析のモデル関数
within.model = function(df) {
  
  # グループ分けのパターンを生成
  elements = unique(df$Factor)
  groupings = generate_groupings(elements)
  
  # 結果を格納するリスト
  results_list = list()
  
  # 各グループ分けに対して処理を実施
  for (grouping in groupings) {
    # グループ分けを適用
    transformed_col = apply_grouping(df$Factor, grouping)
    
    # データフレームに変換列を追加
    df_transformed = df %>%
      mutate(!!paste0("", gsub("\\+", "_", grouping)) := as.factor(transformed_col))
    
    # モデル式の設定
    column_name = paste0("", gsub("\\+", "_", grouping))
    formula = if ("+" %in% strsplit(grouping, "")[[1]]) {
      as.formula(paste("Value ~ Participant + ", column_name))
    } else {
      as.formula("Value ~ 1")
    }
    
    # 適応する列が存在するか確認
    if (length(df_transformed[[column_name]]) > 0) {
      # NA を除去
      df_transformed_no_na = na.omit(df_transformed)
      
      # モデル適用とAIC抽出
      model = aov(formula, data = df_transformed_no_na)
      aic = AIC(model)
      
      # 結果をリストに保存
      results_list[[paste0("Grouping_", gsub("\\+", "_", grouping))]] = 
        c("Grouping" = column_name, "AIC" = aic)
    }
  }
  
  # 結果のデータフレームを作成
  results_df = bind_rows(results_list)
  results_df$AIC = as.numeric(results_df$AIC)
  
  # AICに基づいてソート
  results_df_sorted = results_df %>% arrange(AIC)
  
  # 結果の表示 (tibble型をデータフレームとして表示)
  print(as.data.frame(results_df_sorted))
}
