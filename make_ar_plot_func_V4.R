


library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(purrr)


make_ar_plot_func(
    "RFL_USPR",
    "UTICOM SYSTEM INC",
    "/apps/input_files/MAT/RFL/RFL_Inputs/2025-06-30/L3_AR/AR_TS_FINAL_0706.csv",
    "/apps/input_files/MAT/RFL/RFL_Inputs/2025-06-30/L3_AR",
    "/apps/input_files/MAT/RFL/RFL_Inputs/2025-06-30/L3_AR/AR_SUMMARY_0706.csv",
    y_pretty_n        = 5,
    y_pretty_padding  = 0.05,
    width_px          = 1200,
    height_px         = 800,
    units             = "px",
    dpi               = 100
)


make_ar_plot_func <- function(
    target_entity,
    target_account,
    input_csv,
    out_dir,
    cv_csv,
    y_pretty_n        = 5,
    y_pretty_padding  = 0.05,
    width_px          = 700,
    height_px         = 1200,
    units             = "px",
    dpi               = 100
) {
  
  # read the summary‐of‐CV file here
  cv_df <- fread("/apps/input_files/MAT/RFL/RFL_Inputs/2025-06-30/L3_AR/AR_SUMMARY_0706.csv")
  
  # custom y-scale generator
  y_pretty_fun <- function(n = y_pretty_n, data_vector = NULL, padding = y_pretty_padding) {
    if (!is.null(data_vector)) {
      mn <- min(data_vector, na.rm = TRUE); mx <- max(data_vector, na.rm = TRUE)
      if (mn >= 0 && mx >= 0) {
        lower <- min(0, mn * (1 - padding)); upper <- mx * (1 + padding)
      } else if (mn < 0 && mx > 0) {
        lower <- mn * (1 + padding); upper <- mx * (1 + padding)
      } else {
        lower <- mn * (1 + padding); upper <- max(0, mx * (1 - padding))
      }
      lims <- c(lower, upper)
    } else {
      lims <- NULL
    }
    scale_y_continuous(
      position = "right",
      labels   = label_comma(accuracy = 1),
      breaks   = pretty_breaks(n = n),
      limits   = lims,
      oob      = scales::squish
    )
  }
  
  # read & prep data, include TARGET_DSO for p5
  df <- fread(input_csv)[-1] %>%
    filter(account_name == target_account, entity ==target_entity) %>%
    transmute(
      Date                = as.Date(date),
      Balance             = round(as.numeric(calc_balance), 2),
      Target_Balance      = round(as.numeric(TARGET_BALANCE), 2),
      TTM_Debit           = round(as.numeric(TTM_DEBIT), 2),
      TTM_DSO             = round(as.numeric(TTM_DSO), 2),
      Target_DSO          = round(as.numeric(TARGET_DSO), 2),
      debit               = as.numeric(debit),
      credit              = as.numeric(credit),
      Debit_Intermittency = if("Debit_Intermittency" %in% names(.)) Debit_Intermittency else NA,
      Credit_Intermittency = if("Credit_Intermittency" %in% names(.)) Credit_Intermittency else NA,
      Net_Balance         = Balance - Target_Balance,
      over                = Balance > Target_Balance
    ) %>%
    arrange(Date) %>%
    mutate(run = cumsum(over != lag(over, default = first(over))))
  
  # common scales & theme
  date_scale <- scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  zero_line  <- geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.2)
  line_scale <- scale_color_manual(
    name   = NULL,
    values = c(
      "TTM Sales"       = "#297A3F",
      "TTM DSO"         = "#43799F",
      "Actual Balance"  = "#CD3E43",
      "Target Balance"  = "#2A7B43"
    )
  )
  base_theme <- theme_minimal(base_family = "Times", base_linewidth = 12) +
    theme(
      axis.title.x     = element_blank(),
      plot.background  = element_rect(fill = "#f3cdb6", colour = NA),
      panel.background = element_rect(fill = "#f3cdb6", colour = NA),
      axis.line.y      = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid.major = element_line(colour = "#e8a074", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      plot.margin      = unit(c(15, 10, 10, 40), "pt")   # <-- instead of margin()
    )
  
  # panels p1-p4 unchanged
  p1 <- ggplot(df, aes(Date, TTM_Debit, color = "TTM Sales")) +
    zero_line + geom_line(linewidth = 0.5) + line_scale + y_pretty_fun() + date_scale + base_theme +
    theme(legend.position = "left",
          panel.grid      = element_blank(),
          axis.title.y.right = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(l = 5))) +
    labs(y = "USD")
  p2 <- ggplot(df, aes(Date, TTM_DSO, color = "TTM DSO")) +
    zero_line + geom_line(linewidth = 0.5) + line_scale + y_pretty_fun() + date_scale + base_theme +
    theme(legend.position = "left",
          panel.grid      = element_blank(),
          axis.title.y.right = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(l = 5))) +
    labs(y = "Days")
  p3 <- ggplot(df, aes(Date)) +
    zero_line +
    geom_ribbon(aes(ymin = ifelse(over, Target_Balance, Balance),
                    ymax = ifelse(over, Balance, Target_Balance),
                    fill = over, group = run),
                alpha = 0.3, show.legend = FALSE) +
    scale_fill_manual(values = c(`TRUE` = "#CD3E43", `FALSE` = "#2A7B43")) +
    geom_line(aes(y = Balance, color = "Actual Balance"), linewidth = 0.5) +
    geom_line(aes(y = Target_Balance, color = "Target Balance"), linewidth = 0.5) +
    line_scale + y_pretty_fun() + date_scale + base_theme +
    theme(legend.position = "left",
          panel.grid      = element_blank(),
          axis.title.y.right = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(l = 5))) +
    labs(y = "USD")
  p4 <- ggplot(df, aes(Date, Net_Balance, fill = Net_Balance > 0)) +
    zero_line + geom_col() +
    scale_fill_manual(name = NULL,
                      values = c(`TRUE` = "#CD3E43", `FALSE` = "#2A7B43"),
                      labels = c(`TRUE` = "Leak", `FALSE` = "Gain")) +
    y_pretty_fun(data_vector = df$Net_Balance) + date_scale + base_theme +
    theme(legend.position = "left",
          panel.grid      = element_blank(),
          axis.title.y.right = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(l = 5))) +
    labs(y = "USD")
  
  # p5: density of TTM DSO with dynamic fill depending on median <= target
  target_val <- unique(df$Target_DSO)
  median_val <- median(df$TTM_DSO, na.rm = TRUE)
  fill_color <- if (median_val <= target_val) "forestgreen" else "firebrick"
  
  p5 <- ggplot(df %>% filter(!is.na(TTM_DSO), is.finite(TTM_DSO)), aes(x = TTM_DSO)) +
    geom_density(fill = fill_color, alpha = 0.5, na.rm = TRUE) +
    geom_vline(aes(xintercept = target_val, colour = "Target DSO", linetype = "Target DSO"), linewidth = 0.5) +
    geom_vline(aes(xintercept = median_val, colour = "Median TTM DSO", linetype = "Median TTM DSO"), linewidth = 0.5) +
    scale_colour_manual(
      values = c("Target DSO" = "forestgreen", "Median TTM DSO" = "firebrick"),
      name   = NULL
    ) +
    scale_linetype_manual(
      values = c("Target DSO" = "dashed", "Median TTM DSO" = "solid"),
      name   = NULL
    ) +
    scale_y_continuous(
      position = "right",
      labels   = scales::percent_format(accuracy = 1)
    ) +
    labs(x = "TTM DSO (Days)", y = "TTM DSO\nFrequency\nDistribution") +
    base_theme +
    theme(
      legend.position      = "left",
      panel.grid           = element_blank(),
      axis.title.y.right   = element_text(angle = 0, vjust = 0.5, hjust = 0.5, margin = margin(l = 5))
    )
  
  
  combined <- p1 / p2 / p3 / p4 / p5 +
    plot_layout(ncol = 1, guides = "keep") &
    theme(
      # increase the gap between the axis ticks/labels and the right‐side title:
      axis.title.y.right = element_text(
        angle  = 0,
        vjust  = 0.5,
        hjust  = 0.5,
        margin = margin(l = 20)    # ← bump this number up or down to taste
      ),
      
      # (optional) tuck your legend neatly on the left with a bit of key–label spacing
      legend.position  = "left",
      legend.spacing.x = unit(0.2, "cm")
    )
  
  combined_tight <- combined +
    plot_annotation(
      title = target_account,
      theme = theme(
        plot.title      = element_text(family = "Times", face = "bold", linewidth = 16, hjust = 0.5),
        plot.background = element_rect(fill = "#f3cdb6", color = NA)
      )
    )
  
  # render to file
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_file <- file.path(out_dir, paste0(gsub("[^[:alnum:] _.-]", "_", target_account), ".png"))
  png(filename = out_file, width = width_px, height = height_px, units = units, res = dpi)
  print(combined_tight)
  dev.off()
  
  invisible(out_file)
}



# READ ME 

# FORMAT OF THE TIME SERIES FILE in "/apps/input_files/MAT/RFL/RFL_Inputs/2025-06-30/L3_AR/AR_TS_FINAL_0706.csv"
# > str(AR_TS)
# 'data.frame':	170792 obs. of  30 variables:
#   $ X.1                 : int  1 2 3 4 5 6 7 8 9 10 ...
# $ X                   : int  396 397 398 399 400 401 402 403 404 405 ...
# $ entity              : chr  "RFLINKS2_US" "RFLINKS2_US" "RFLINKS2_US" "RFLINKS2_US" ...
# $ account_name        : chr  "ARROW INK LLC-NEW" "ARROW INK LLC-NEW" "ARROW INK LLC-NEW" "ARROW INK LLC-NEW" ...
# $ date                : chr  "2024-01-13" "2024-01-14" "2024-01-15" "2024-01-16" ...
# $ account_number      : int  180041 180041 180041 180041 180041 180041 180041 180041 180041 180041 ...
# $ category            : chr  "carry" "carry" "carry" "carry" ...
# $ debit               : num  0 0 0 0 6499 ...
# $ credit              : num  0 0 0 0 0 ...
# $ calc_balance        : num  10986 10986 10986 10986 17485 ...
# $ CheckIn             : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
# $ TTM_DEBIT           : num  346213 346213 346213 346213 349476 ...
# $ TTM_CREDIT          : num  341282 335506 335506 335506 335506 ...
# $ TTM_MEAN_BALANCE    : num  17534 17561 17588 17615 17650 ...
# $ TTM_TURNS           : num  31.5 31.5 31.5 31.5 20 ...
# $ TTM_DSO             : num  11.6 11.6 11.6 11.6 18.3 ...
# $ TARGET_DSO          : int  30 30 30 30 30 30 30 30 30 30 ...
# $ TARGET_TURNS        : num  12.2 12.2 12.2 12.2 12.2 ...
# $ TARGET_BALANCE      : num  28448 28448 28448 28448 28716 ...
# $ TARGET_DEBIT        : num  133704 133704 133704 133704 212797 ...
# $ COLLECTION_GAP      : num  -17462 -17462 -17462 -17462 -11231 ...
# $ SALES_GAP           : num  -212509 -212509 -212509 -212509 -136679 ...
# $ L_TYPE              : chr  "positive" "positive" "positive" "positive" ...
# $ DDD                 : num  -17462 -34923 -52385 -69847 -81077 ...
# $ last_debit_flag     : chr  NA NA NA NA ...
# $ prev_debit_date     : chr  NA NA NA NA ...
# $ Debit_Intermittency : int  NA NA NA NA NA NA 2 0 NA NA ...
# $ last_credit_flag    : chr  NA NA NA NA ...
# $ prev_credit_date    : chr  NA NA NA NA ...
# $ Credit_Intermittency: int  NA NA NA NA NA NA NA NA NA NA ...


# FORMAT OF THE SUMMARY FILE in "/apps/input_files/MAT/RFL/RFL_Inputs/2025-06-30/L3_AR/AR_SUMMARY_0706.csv"
# > str(AR_SUMMARY)
# 'data.frame':	372 obs. of  16 variables:
#   $ X                : int  1 2 3 4 5 6 7 8 9 10 ...
# $ entity           : chr  "RFLINKS2_US" "RFLINKS2_US" "RFLINKS2_US" "RFLINKS2_US" ...
# $ account_name     : chr  "ARROW INK LLC-NEW" "CALIFORNIA INDUSTRIAL FAB." "CAPITAL EQUIPMENT" "CET COLOR" ...
# $ SAMPLE_SIZE      : int  536 365 245 518 239 508 591 562 486 118 ...
# $ TARGET_DSO       : int  30 30 30 30 30 30 75 60 30 30 ...
# $ NONZERO_DEBIT_CT : int  98 11 1 34 9 30 138 116 5 3 ...
# $ NONZERO_CREDIT_CT: int  41 15 1 38 9 25 97 47 3 3 ...
# $ NONZERO_TXN_CT   : int  139 26 2 72 18 55 235 163 8 6 ...
# $ OV_AMOUNT        : num  0.841 0.977 1 0.979 0.932 ...
# $ OV_INTERMITTENCY : num  0.658 0.867 NA 0.786 0.789 ...
# $ MEAN_TTM_DSO     : num  22.1 39.2 Inf 27.1 48 ...
# $ SD_TTM_DSO       : num  13.6 37.8 NA 17.5 23.2 ...
# $ SKEW_TTM_DSO     : num  0.9584 0.5833 NA 0.2697 0.0457 ...
# $ KURT_TTM_DSO     : num  1.887 -0.761 NA -0.601 -0.562 ...
# $ OV_GM            : num  0.744 0.92 NA 0.877 0.858 ...
# $ CV               : num  0.614 0.965 NA 0.644 0.483 ...



