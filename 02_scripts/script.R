library(palmerpenguins)
library(DHARMa)

penguins

hist(penguins$body_mass_g)
hist(penguins$bill_length_mm)

mod <- glm(body_mass_g ~ bill_length_mm, data = na.omit(penguins))
mod

da <- penguins %>%
  tidyr::drop_na() %>%
  dplyr::mutate(pred = predict(mod))
da

ggplot(da, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_segment(aes(xend = bill_length_mm, yend = mod$fitted.values), color = "lightblue") +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_classic(base_size = 20)

ggplot(da, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_segment(aes(xend = bill_length_mm,
                   yend = mean(body_mass_g)), color = "tomato") +
  geom_point() +
  geom_abline(aes(intercept = mean(body_mass_g), slope = 0), color = "red", size = 1) +
  theme_classic(base_size = 20)

ggplot(da, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_segment(aes(xend = bill_length_mm,
                   yend = mean(body_mass_g)-1000), color = "tomato") +
  geom_point() +
  geom_abline(aes(intercept = 3000, slope = 10), color = "red", size = 1) +
  theme_classic(base_size = 20)

summary(mod)
mod$deviance/mod$df.residual

mod$deviance
mod$null.deviance
sum((na.omit(penguins$body_mass_g) - mean(na.omit(penguins$body_mass_g)))^2)



