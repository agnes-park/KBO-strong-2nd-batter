# =============================================================================
# 강한 2번 타자 이론 검증: 5,040개 라인업 회귀분석 (Model 1 ~ 6)
#
# 입력: data/lineup_5040_expected_runs.csv
#       (마르코프 체인 시뮬레이션으로 계산한 라인업별 9이닝 기대득점)
# 출력: output/model_summary.csv, figures/*.png
#
# 실행: 저장소 최상위 폴더에서  Rscript R/regression_models.R
# =============================================================================

library(dplyr)
library(ggplot2)
library(broom)

df <- read.csv("data/lineup_5040_expected_runs.csv")
stopifnot(nrow(df) == 5040)

# 타자 등급 점수: S = 3, A = 2, B = 1, C = 0
# 각 라인업은 S 1명, A 2명, B 3명, C 3명으로 구성된다.

# -----------------------------------------------------------------------------
# Model 1 ~ 4: 기본 모델 (팀원 작성 코드를 바탕으로 정리)
# -----------------------------------------------------------------------------

# Model 1. S급 타자가 2번이면 기대득점이 높아지는가?
m1 <- lm(game_exp ~ is_super2, data = df)

# Model 2. S급 또는 A급 타자가 2번이면 기대득점이 높아지는가?
m2 <- lm(game_exp ~ is_strong2, data = df)

# Model 3. 1~3번 상위타선과 3~5번 중심타선 중 어디가 더 중요한가?
m3 <- lm(game_exp ~ top123_strength + cleanup345_strength + bottom6789_strength,
         data = df)

# Model 4. 타선 구간별 강도를 통제해도 S급 2번 효과가 남는가?
m4 <- lm(game_exp ~ is_super2 + top123_strength + cleanup345_strength +
           bottom6789_strength, data = df)

# -----------------------------------------------------------------------------
# Model 5 ~ 6: 기본 모델의 한계를 보완한 추가 모델
# -----------------------------------------------------------------------------

# Model 5. S급 타자의 위치 자체가 기대득점을 어떻게 바꾸는가?
# Model 4의 top123_strength는 1~3번의 평균이라, S급이 1번이든 2번이든 3번이든
# 같은 값이 된다. 그래서 S급 타자의 위치(1~9번)를 직접 비교한다. 기준은 1번.
df$S_pos <- factor(df$typeS_position, levels = 1:9)
m5 <- lm(game_exp ~ S_pos, data = df)

# Model 6. 2번 타자의 등급별로 기대득점이 어떻게 다른가?
# Model 1, 2는 2번이 강한지 여부만 봤다. 등급을 나눠 비교한다. 기준은 A급.
df$bat2_type <- factor(df$bat2, levels = c("A", "B", "C", "S"))
m6 <- lm(game_exp ~ bat2_type, data = df)

# -----------------------------------------------------------------------------
# 결과 정리
# -----------------------------------------------------------------------------
models <- list(`Model 1` = m1, `Model 2` = m2, `Model 3` = m3,
               `Model 4` = m4, `Model 5` = m5, `Model 6` = m6)

summary_tbl <- bind_rows(lapply(names(models), function(nm) {
  tidy(models[[nm]]) %>%
    mutate(model = nm, r_squared = summary(models[[nm]])$r.squared)
})) %>%
  select(model, term, estimate, std.error, p.value, r_squared) %>%
  mutate(across(c(estimate, std.error, r_squared), ~ round(.x, 4)))

dir.create("output", showWarnings = FALSE)
write.csv(summary_tbl, "output/model_summary.csv", row.names = FALSE)
print(summary_tbl, n = Inf)

# 핵심 비교: S급 2번 효과가 통제 전후 어떻게 바뀌는가
cat(sprintf("\nS급 2번 효과  Model 1: %+.4f  ->  Model 4: %+.4f\n",
            coef(m1)["is_super2"], coef(m4)["is_super2"]))

# -----------------------------------------------------------------------------
# 그림
# -----------------------------------------------------------------------------
dir.create("figures", showWarnings = FALSE)

# 그림 1. S급 타자 위치별 기대득점 (Model 5)
pos_tbl <- df %>%
  group_by(typeS_position) %>%
  summarise(mean_exp = mean(game_exp), .groups = "drop") %>%
  mutate(diff = mean_exp - mean_exp[typeS_position == 1])

p1 <- ggplot(pos_tbl, aes(factor(typeS_position), diff)) +
  geom_col(fill = ifelse(pos_tbl$typeS_position <= 4, "#2F6B9A", "#C0504D"),
           width = 0.65) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_text(aes(label = sprintf("%+.2f", diff),
                vjust = ifelse(diff >= 0, -0.4, 1.3)), size = 3.6) +
  labs(title = "S급 타자를 몇 번에 두느냐에 따른 9이닝 기대득점 차이",
       subtitle = "1번 기준. 1~4번은 차이가 거의 없고, 5번부터 줄어든다.",
       x = "S급 타자 타순", y = "1번 대비 기대득점 차이 (점)") +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave("figures/model5_s_position.png", p1, width = 8, height = 4.8, dpi = 150)

# 그림 2. 통제 전후 S급 2번 효과 (Model 1 vs Model 4)
eff_tbl <- data.frame(
  model = factor(c("Model 1\n(2번 여부만)", "Model 4\n(타선 강도 통제)"),
                 levels = c("Model 1\n(2번 여부만)", "Model 4\n(타선 강도 통제)")),
  effect = c(coef(m1)["is_super2"], coef(m4)["is_super2"])
)
p2 <- ggplot(eff_tbl, aes(model, effect)) +
  geom_col(fill = c("#9DB4C8", "#2F6B9A"), width = 0.5) +
  geom_text(aes(label = sprintf("%+.3f점", effect)), vjust = -0.4, size = 4) +
  labs(title = "S급 타자를 2번에 둔 효과",
       subtitle = "상위·중심·하위 타선 강도를 통제하면 효과 대부분이 사라진다.",
       x = NULL, y = "9이닝 기대득점 변화 (점)") +
  ylim(0, 0.38) +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave("figures/super2_effect_before_after.png", p2, width = 6.5, height = 4.5, dpi = 150)

cat("\n저장 완료: output/model_summary.csv, figures/*.png\n")
