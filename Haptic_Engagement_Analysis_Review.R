# Haptic Technology Engagement Analysis
# Required input files in the working directory:
#   HapticData.csv
#   EmotionLookupTable.txt
#   NRC-emotion-lexicon-wordlevel-v0.92.txt
#
# Output:
#   Haptic_Engagement_Inferential_Results.csv
#
# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c("dplyr", "rstatix")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Please install the following required packages before running this script: ",
       paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(rstatix)
})

# ============================================================
# 2. LOAD DATA
# ============================================================

data <- read.csv(
  "HapticData.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)



# ============================================================
# 3. CLEAN TEXT
# ============================================================

data$Comment <- iconv(data$Comment, from = "", to = "UTF-8", sub = " ")

data$Comment <- sapply(data$Comment, function(x) {
  tryCatch(gsub("[^\x01-\x7F]", "", x), error = function(e) NA)
})

data <- data[!is.na(data$Comment) & trimws(data$Comment) != "", ]

data <- data %>% distinct(Comment, .keep_all = TRUE)



# ============================================================
# 4. KEEP THE FIVE INDUSTRIES
# ============================================================

target_industries <- c(
  "Commerce",
  "Education",
  "Health services",
  "Mechanical and electrical engineering",
  "Agriculture",
  "Agriculture; plantations; other rural sectors",
  "Agriculture; plantations;other rural sectors"
)

data <- data %>% filter(Industry %in% target_industries)

data$Industry <- ifelse(
  grepl("^Agriculture", data$Industry),
  "Agriculture",
  data$Industry
)

data$Industry <- factor(
  data$Industry,
  levels = c(
    "Commerce",
    "Education",
    "Health services",
    "Mechanical and electrical engineering",
    "Agriculture"
  )
)



# ============================================================
# 5. PREPARE NUMERIC VARIABLES
# ============================================================

data$Likes <- as.numeric(data$Likes)
data$Views <- as.numeric(data$Views)


# ============================================================
# 6. GENERAL KRUSKAL-WALLIS FUNCTION
# ------------------------------------------------------------
# NOTE: The Kruskal-Wallis test statistic is conventionally
#       reported as H, not chi-square.
#
#       Formula for epsilon-squared effect size:
#         ε² = (H − k + 1) / (N − k)
# ============================================================

run_kw <- function(formula, variable_name, data) {

  model_data <- model.frame(formula, data = data, na.action = na.omit)

  N_used <- nrow(model_data)
  group_variable <- model_data[[2]]
  k_used <- length(unique(group_variable))

  test <- kruskal.test(formula, data = data)

  H  <- as.numeric(test$statistic)
  df <- as.numeric(test$parameter)
  p  <- test$p.value

  epsilon2 <- max(0, (H - k_used + 1) / (N_used - k_used))

  data.frame(
    Variable    = variable_name,
    H           = H,
    df          = df,
    N_used      = N_used,
    Groups_used = k_used,
    p_raw       = p,
    epsilon_sq  = epsilon2,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# PART A: BEHAVIOURAL ENGAGEMENT
# ============================================================
# Behavioural engagement is represented by two separate
# indicators: views (involvement) and likes (affinity).
#
# Multiple-testing correction:
#   Holm correction across the 2 behavioural indicators.
#
# This preserves the original operationalisation and avoids
# creating a new composite behavioural-engagement score.
# ============================================================


# ------------------------------------------------------------
# A1. Descriptive statistics
# ------------------------------------------------------------

behavioural_descriptives <- data %>%
  group_by(Industry) %>%
  summarise(
    n             = n(),
    Views_mean    = mean(Views, na.rm = TRUE),
    Views_median  = median(Views, na.rm = TRUE),
    Likes_mean    = mean(Likes, na.rm = TRUE),
    Likes_median  = median(Likes, na.rm = TRUE),
    .groups = "drop"
  )



# ------------------------------------------------------------
# A2. Kruskal-Wallis tests
# ------------------------------------------------------------

kw_views <- run_kw(
  Views ~ Industry,
  "Views",
  data
)

kw_likes <- run_kw(
  Likes ~ Industry,
  "Likes",
  data
)

behavioural_kw <- bind_rows(
  kw_views,
  kw_likes
)


# ------------------------------------------------------------
# A3. Holm correction across the 2 behavioural indicators
# ------------------------------------------------------------

behavioural_kw$p_holm <- p.adjust(
  behavioural_kw$p_raw,
  method = "holm"
)

behavioural_kw$Correction_family <- "Holm (2 behavioural indicators)"

behavioural_kw$Significant <- ifelse(
  behavioural_kw$p_holm < .05,
  "Yes",
  "No"
)

behavioural_kw$Effect <- cut(
  behavioural_kw$epsilon_sq,
  breaks = c(-Inf, .01, .06, .14, Inf),
  labels = c(
    "Negligible",
    "Small",
    "Moderate",
    "Large"
  ),
  right = FALSE
)



# ------------------------------------------------------------
# A4. Dunn post-hoc tests for significant indicators
# ------------------------------------------------------------

significant_behavioural <- behavioural_kw %>%
  filter(p_holm < .05) %>%
  pull(Variable)

if (length(significant_behavioural) > 0) {

  behavioural_dunn <- bind_rows(
    lapply(significant_behavioural, function(var_name) {

      formula <- if (var_name == "Views") {
        Views ~ Industry
      } else {
        Likes ~ Industry
      }

      result <- dunn_test(
        data,
        formula,
        p.adjust.method = "holm"
      )

      result$Variable <- var_name

      result %>%
        select(
          Variable,
          group1,
          group2,
          n1,
          n2,
          statistic,
          p,
          p.adj,
          p.adj.signif
        )
    })
  )


} else {

  behavioural_dunn <- data.frame()

}


# ------------------------------------------------------------
# A5. Final behavioural interpretation
# ------------------------------------------------------------



# ============================================================
# PART B: COGNITIVE ENGAGEMENT
# ============================================================
# Correction family: Holm across 2 indicators
#   (word count + argumentation score)
#
# Word count and argumentation are kept separate — no composite.
# This is more transparent and defensible.
# ============================================================


comment_lower <- tolower(data$Comment)

# Word count
data$word_count <- sapply(
  strsplit(trimws(data$Comment), "\\s+"),
  length
)

# Argumentation markers
data$has_agreement <- grepl(
  "\\b(agree|yes|exactly|true|correct|right|indeed|absolutely)\\b",
  comment_lower
)

data$has_disagreement <- grepl(
  "\\b(disagree|no|wrong|false|incorrect|nope|nah)\\b",
  comment_lower
)

data$has_reason <- grepl(
  "\\b(because|since|therefore|thus|hence|reason|due|as a result)\\b",
  comment_lower
)

data$has_question <- grepl("\\?", data$Comment)

data$argumentation_score <-
  as.numeric(data$has_agreement) +
  as.numeric(data$has_disagreement) +
  as.numeric(data$has_reason) +
  as.numeric(data$has_question)

# Descriptives
cognitive_descriptives <- data %>%
  group_by(Industry) %>%
  summarise(
    n                    = n(),
    Words_mean           = mean(word_count, na.rm = TRUE),
    Words_median         = median(word_count, na.rm = TRUE),
    Argumentation_mean   = mean(argumentation_score, na.rm = TRUE),
    Argumentation_median = median(argumentation_score, na.rm = TRUE),
    .groups = "drop"
  )


# Kruskal-Wallis
kw_words <- run_kw(word_count ~ Industry, "Word count", data)
kw_arg   <- run_kw(argumentation_score ~ Industry, "Argumentation score", data)

cognitive_kw <- bind_rows(kw_words, kw_arg)

# Holm correction across the 2 cognitive indicators
cognitive_kw$p_holm <- p.adjust(cognitive_kw$p_raw, method = "holm")
cognitive_kw$Correction_family <- "Holm (2 indicators)"
cognitive_kw$Significant <- ifelse(cognitive_kw$p_holm < .05, "Yes", "No")
cognitive_kw$Effect <- cut(
  cognitive_kw$epsilon_sq,
  breaks = c(-Inf, .01, .06, .14, Inf),
  labels = c("Negligible", "Small", "Moderate", "Large"),
  right = FALSE
)


# Dunn post-hoc for significant cognitive indicators
significant_cognitive <- cognitive_kw %>% filter(p_holm < .05) %>% pull(Variable)

if (length(significant_cognitive) > 0) {
  cognitive_dunn <- bind_rows(
    lapply(significant_cognitive, function(var_name) {
      formula <- if (var_name == "Word count") word_count ~ Industry
                 else argumentation_score ~ Industry
      result <- dunn_test(data, formula, p.adjust.method = "holm")
      result$Variable <- var_name
      result %>% select(Variable, group1, group2, n1, n2,
                        statistic, p, p.adj, p.adj.signif)
    })
  )
} else {
  cognitive_dunn <- data.frame()
}


# ============================================================
# PART C: EMOTIONAL ENGAGEMENT
# ============================================================
# Correction families:
#   - SentiStrength polarity: single test → no correction
#   - NRC emotions: Holm across 8 emotions
#   - Sentiment category: single chi-square → no correction
# ============================================================


# --- C1. Text cleaning function ---
clean_text <- function(text) {
  text <- as.character(text)
  if (length(text) == 0 || is.na(text)) return("")
  text <- iconv(text, from = "", to = "UTF-8", sub = " ")
  if (is.na(text)) return("")
  text <- tolower(text)
  text <- gsub("https?://[^[:space:]]+", " ", text)
  text <- gsub("<[^>]+>", " ", text)
  text <- gsub("[^a-z0-9[:space:]]", " ", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

# --- C2. Load SentiStrength lexicon ---
sentilines <- readLines("EmotionLookupTable.txt",
                        warn = FALSE, encoding = "UTF-8")
sentilines <- sentilines[trimws(sentilines) != ""]
sentisplit <- strsplit(sentilines, "\t")

sentilookup <- data.frame(
  term  = sapply(sentisplit, function(x) if (length(x) >= 1) x[1] else NA),
  score = sapply(sentisplit, function(x) if (length(x) >= 2) x[2] else NA),
  stringsAsFactors = FALSE
)

sentilookup <- sentilookup[!is.na(sentilookup$term) & !is.na(sentilookup$score), ]
sentilookup$score <- suppressWarnings(as.numeric(sentilookup$score))
sentilookup <- sentilookup[!is.na(sentilookup$score), ]
sentilookup$term <- gsub("\\*", "", sentilookup$term)
sentilookup$term <- tolower(trimws(sentilookup$term))
sentilookup <- sentilookup[!duplicated(sentilookup$term), ]
sentilookup <- sentilookup[sentilookup$term != "", ]


# --- C3. SentiStrength scoring ---
calculate_sentistrength <- function(text) {
  text_clean <- clean_text(text)
  if (text_clean == "") {
    return(data.frame(positive = 0, negative = 0,
                      polarity = NA_real_, sentiment = "Neutral",
                      stringsAsFactors = FALSE))
  }
  words <- unlist(strsplit(text_clean, "\\s+"))
  words <- words[words != ""]
  scores <- sentilookup$score[match(words, sentilookup$term)]
  scores <- scores[!is.na(scores)]
  if (length(scores) == 0) {
    return(data.frame(positive = 0, negative = 0,
                      polarity = NA_real_, sentiment = "Neutral",
                      stringsAsFactors = FALSE))
  }
  positive <- if (any(scores > 0)) max(scores[scores > 0]) else 0
  negative <- if (any(scores < 0)) min(scores[scores < 0]) else 0
  polarity <- positive + negative
  sentiment <- if (polarity > 0) "Positive" else
               if (polarity < 0) "Negative" else "Neutral"
  data.frame(positive = positive, negative = negative,
             polarity = polarity, sentiment = sentiment,
             stringsAsFactors = FALSE)
}

sent_results <- bind_rows(lapply(data$Comment, calculate_sentistrength))
data <- cbind(data, sent_results)


# --- C4. SentiStrength descriptives ---
sentiment_descriptives <- data %>%
  group_by(Industry) %>%
  summarise(
    n               = n(),
    Polarity_n      = sum(!is.na(polarity)),
    Polarity_mean   = mean(polarity, na.rm = TRUE),
    Polarity_median = median(polarity, na.rm = TRUE),
    .groups = "drop"
  )


# --- C5. Kruskal-Wallis for polarity (no correction) ---
kw_polarity <- run_kw(polarity ~ Industry, "SentiStrength polarity", data)
kw_polarity$p_holm <- kw_polarity$p_raw
kw_polarity$Correction_family <- "None (single test)"
kw_polarity$Significant <- ifelse(kw_polarity$p_holm < .05, "Yes", "No")
kw_polarity$Effect <- cut(
  kw_polarity$epsilon_sq,
  breaks = c(-Inf, .01, .06, .14, Inf),
  labels = c("Negligible", "Small", "Moderate", "Large"),
  right = FALSE
)


# --- C6. Chi-square for sentiment category × industry (no correction) ---
sentiment_table <- table(data$Industry, data$sentiment)

pearson_test <- suppressWarnings(chisq.test(sentiment_table))

set.seed(12345)
monte_carlo_test <- chisq.test(sentiment_table,
                               simulate.p.value = TRUE,
                               B = 10000)

chi_square <- as.numeric(pearson_test$statistic)
N_chi <- sum(sentiment_table)
r <- nrow(sentiment_table)
c <- ncol(sentiment_table)
cramers_v <- sqrt(chi_square / (N_chi * min(r - 1, c - 1)))


# --- C7. Load NRC emotion lexicon ---
nrc_lines <- readLines("NRC-emotion-lexicon-wordlevel-v0.92.txt",
                       warn = FALSE, encoding = "UTF-8")
nrc_lines <- nrc_lines[trimws(nrc_lines) != ""]
nrc_split <- strsplit(nrc_lines, "\t")

nrc <- data.frame(
  word        = sapply(nrc_split, function(x) if (length(x) >= 1) x[1] else NA),
  emotion     = sapply(nrc_split, function(x) if (length(x) >= 2) x[2] else NA),
  association = sapply(nrc_split, function(x) if (length(x) >= 3) x[3] else NA),
  stringsAsFactors = FALSE
)

nrc$word        <- tolower(trimws(nrc$word))
nrc$emotion     <- tolower(trimws(nrc$emotion))
nrc$association <- suppressWarnings(as.numeric(trimws(nrc$association)))

nrc <- nrc[!is.na(nrc$word) & !is.na(nrc$emotion) & !is.na(nrc$association), ]
nrc <- nrc[nrc$association == 1, ]

nrc_emotions <- c("anger", "anticipation", "disgust", "fear",
                  "joy", "sadness", "surprise", "trust")

nrc <- nrc[nrc$emotion %in% nrc_emotions, ]


# --- C8. NRC scoring function ---
calculate_nrc <- function(text) {
  text_clean <- clean_text(text)
  words <- unlist(strsplit(text_clean, "\\s+"))
  words <- words[words != ""]
  scores <- setNames(rep(0, length(nrc_emotions)), nrc_emotions)
  if (length(words) > 0) {
    matched <- nrc[nrc$word %in% words, ]
    if (nrow(matched) > 0) {
      counts <- table(matched$emotion)
      scores[names(counts)] <- as.numeric(counts)
    }
  }
  as.data.frame(as.list(scores))
}

nrc_results <- bind_rows(lapply(data$Comment, calculate_nrc))
data <- cbind(data, nrc_results)

# --- C9. NRC descriptives ---
nrc_descriptives <- data %>%
  group_by(Industry) %>%
  summarise(
    across(
      all_of(nrc_emotions),
      list(mean = ~ mean(.x, na.rm = TRUE),
           median = ~ median(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )


# --- C10. Kruskal-Wallis for all 8 NRC emotions ---
nrc_kw <- bind_rows(lapply(nrc_emotions, function(emotion) {
  run_kw(as.formula(paste(emotion, "~ Industry")), emotion, data)
}))

# Holm correction across the 8 NRC emotions
nrc_kw$p_holm <- p.adjust(nrc_kw$p_raw, method = "holm")
nrc_kw$Correction_family <- "Holm (8 emotions)"
nrc_kw$Significant <- ifelse(nrc_kw$p_holm < .05, "Yes", "No")
nrc_kw$Effect <- cut(
  nrc_kw$epsilon_sq,
  breaks = c(-Inf, .01, .06, .14, Inf),
  labels = c("Negligible", "Small", "Moderate", "Large"),
  right = FALSE
)


# --- C11. Dunn post-hoc for significant NRC emotions ---
significant_nrc <- nrc_kw %>% filter(p_holm < .05) %>% pull(Variable)

if (length(significant_nrc) > 0) {
  nrc_dunn <- bind_rows(lapply(significant_nrc, function(emotion) {
    result <- dunn_test(
      data,
      as.formula(paste(emotion, "~ Industry")),
      p.adjust.method = "holm"
    )
    result$Emotion <- emotion
    result %>% select(Emotion, group1, group2, n1, n2,
                      statistic, p, p.adj, p.adj.signif)
  }))
} else {
  nrc_dunn <- data.frame()
}


# ============================================================
# PART D: FINAL SUMMARY TABLE
# ============================================================
# Reporting:
#   - H statistic (not chi-square)
#   - Raw p and Holm-adjusted p
#   - Correction family
#   - Epsilon-squared effect size
# ============================================================

final_results <- bind_rows(

  behavioural_kw %>%
    select(Variable, H, df, N_used, p_raw, p_holm,
           Correction_family, epsilon_sq, Effect, Significant),

  cognitive_kw %>%
    select(Variable, H, df, N_used, p_raw, p_holm,
           Correction_family, epsilon_sq, Effect, Significant),

  kw_polarity %>%
    select(Variable, H, df, N_used, p_raw, p_holm,
           Correction_family, epsilon_sq, Effect, Significant),

  nrc_kw %>%
    select(Variable, H, df, N_used, p_raw, p_holm,
           Correction_family, epsilon_sq, Effect, Significant)
)


sentiment_result <- data.frame(
  Test           = "Sentiment category × Industry",
  Chi_square     = chi_square,
  df             = pearson_test$parameter,
  N              = N_chi,
  p_pearson      = pearson_test$p.value,
  p_monte_carlo  = monte_carlo_test$p.value,
  Cramers_V      = cramers_v
)



# ============================================================
# SAVE THE INFERENTIAL RESULTS
# ============================================================

write.csv(
  final_results,
  file = "Haptic_Engagement_Inferential_Results.csv",
  row.names = FALSE,
  na = ""
)

