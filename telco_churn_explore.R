library(tidyverse)
library(scales)
library(ggcorrplot)

telco_churn <- read_csv("data/telco/telco_churn_sample.csv")
telco_churn |> 
  glimpse()

telco_churn |> 
  summary()

telco_churn <- telco_churn |> 
  mutate(churn = factor(churn, 
                        levels = c(1, 0), 
                        labels = c("Yes", "No")))

telco_churn |> 
  count(churn) |> 
  mutate(churn = factor(churn), 
         pct = n/sum(n)) |> 
  ggplot(aes(x = churn, y = n, fill = churn)) + 
  geom_col(alpha = 0.5) + 
  geom_text(aes(label = percent(pct, accuracy = 0.1)), 
            vjust = -0.25) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  labs(y = "Jumlah Nasabah") + 
  theme_bw() + 
  theme(legend.position = "none")

telco_churn |> 
  ggplot(aes(x = los)) + 
  geom_histogram(color = "grey", fill = "steelblue", alpha = 0.75) + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = los, fill = churn)) + 
  geom_density(color = "grey", alpha = 0.5) + 
  scale_x_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw()


telco_churn |> 
  ggplot(aes(x = los, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(legend.position = "none")

telco_churn |> 
  ggplot(aes(x = los, fill = churn)) + 
  geom_density(alpha = 0.5) + 
  geom_boxplot(alpha = 0.5, width = 0.0001) + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = voice_rev/1000)) + 
  geom_histogram(color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(labels = number_format(big.mark = ",", suffix = "K")) + 
  labs(x = "voice_rev") + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = voice_rev/1000, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_continuous(labels = number_format(big.mark = ",", suffix = "K")) + 
  labs(x = "voice_rev") + 
  theme_bw() + 
  theme(legend.position = "none")

telco_churn |> 
  ggplot(aes(x = voice_rev/1000, fill = churn)) + 
  geom_density(color = "grey", alpha = 0.5) + 
  # geom_boxplot(alpha = 0.5, width = 0.01) + 
  scale_x_continuous(breaks = seq(0, 500, by = 25), 
                     labels = number_format(big.mark = ",", suffix = "K")) + 
  labs(x = "voice_rev") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

telco_churn |> 
  ggplot(aes(x = voice_rev/1000, fill = churn)) + 
  geom_density(alpha = 0.5) + 
  geom_boxplot(alpha = 0.5, width = 0.01) + 
  scale_x_continuous(breaks = seq(0, 500, by = 25), 
                     labels = number_format(big.mark = ",", suffix = "K")) + 
  labs(x = "voice_rev") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

telco_churn |> 
  ggplot(aes(x = voice_trx)) + 
  geom_histogram(color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = voice_trx, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 500, by = 25)) + 
  labs(x = "voice_trx") + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = voice_trx, fill = churn)) + 
  geom_density(color = "grey", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 500, by = 25)) + 
  labs(x = "voice_trx") + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = voice_mou, fill = churn)) + 
  geom_histogram(color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(breaks = seq(0, 3600, by = 300), 
                     labels = number_format(big.mark = ",")) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

telco_churn |> 
  ggplot(aes(x = voice_mou, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 3600, by = 300), 
                     labels = number_format(big.mark = ",")) +
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = voice_mou, fill = churn)) + 
  geom_density(color = "grey", alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 3600, by = 300), 
                     labels = number_format(big.mark = ",")) +
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = voice_dou)) + 
  geom_bar(color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(labels = number_format(big.mark = ",")) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

telco_churn |> 
  ggplot(aes(x = voice_dou, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) +
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = voice_dou, fill = churn)) + 
  geom_bar(color = "grey", position = "dodge", alpha = 0.5) +
  scale_y_continuous(labels = number_format(big.mark = ",")) +
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = sms_rev/1000)) + 
  geom_histogram(color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(breaks = seq(0, 75, by = 5), 
                     labels = number_format(big.mark = ",", suffix = "K")) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) +
  labs(x = "sms_rev") + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank()
  ) 

telco_churn |> 
  ggplot(aes(x = sms_rev/1000, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 75, by = 5), 
                     labels = scales::number_format(big.mark = ",", suffix = "K")) + 
  labs(x = "sms_rev") + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = sms_rev/1000, fill = churn)) + 
  geom_density(color = "grey", alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 75, by = 5), 
                     labels = scales::number_format(big.mark = ",", suffix = "K")) + 
  labs(x = "sms_rev") + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = sms_trx)) + 
  geom_histogram(color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(breaks = seq(0, 1600, by = 100), 
                     labels = number_format(big.mark = ",")) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = sms_trx, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.75) + 
  scale_x_continuous(breaks = seq(0, 1600, by = 100), 
                     labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = sms_trx, fill = churn)) + 
  geom_density(color = "grey", alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 1600, by = 100), 
                     labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = sms_dou)) + 
  geom_bar(color = "grey", fill = "steelblue", alpha = 0.75) + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = sms_dou, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = sms_dou, fill = churn)) + 
  geom_bar(position = "dodge", alpha = 0.5) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = broadband_rev/1000)) + 
  geom_histogram(binwidth = 15, color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(breaks = seq(0, 500, by = 30), 
                     labels = number_format(big.mark = ",", suffix = "K")) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  labs(x = "broadband_rev") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank())

telco_churn |> 
  ggplot(aes(x = broadband_rev/1000, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 500, by = 30), 
                     labels = number_format(big.mark = ",", suffix = "K")) + 
  labs(x = "broadband_rev") + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
    )

telco_churn |> 
  ggplot(aes(x = broadband_rev/1000, fill = churn)) + 
  geom_density(color = "grey", alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 500, by = 30), 
                     labels = number_format(big.mark = ",", suffix = "K")) + 
  labs(x = "broadband_rev") + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = broadband_usg)) + 
  geom_histogram(binwidth = 1000, color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(breaks = seq(0, 10000, by = 1000), 
                     labels = number_format(big.mark = ",")) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor.y = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = broadband_usg, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 10000, by = 1000), 
                     labels = number_format(big.mark = ",")) + 
  # labs(x = "broadband_rev") + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = broadband_usg, fill = churn)) + 
  geom_density(color = "grey", alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 10000, by = 1000), 
                     labels = number_format(big.mark = ",")) + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = broadband_dou)) + 
  geom_bar(color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = broadband_dou, y = churn, fill = churn)) + 
  geom_boxplot(position = "dodge", alpha = 0.5) + 
  scale_x_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = broadband_dou, fill = churn)) + 
  geom_bar(position = "dodge", alpha = 0.5) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = voice_package_rev)) + 
  geom_histogram(binwidth = 5000, color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(breaks = seq(0, 100000, by = 10000), 
                     labels = number_format(big.mark = ",")) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor.y = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = voice_package_rev, y = churn, fill = churn)) + 
  geom_boxplot(position = "dodge", alpha = 0.5) + 
  scale_x_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = voice_package_rev, fill = churn)) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = voice_package_trx)) + 
  geom_histogram(binwidth = 1, color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(#breaks = seq(0, 100000, by = 10000), 
                     labels = number_format(big.mark = ",")) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor.y = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = voice_package_trx, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor.y = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = voice_package_trx, fill = churn)) + 
  geom_bar(position = "dodge", alpha = 0.5) +
  # geom_density(alpha = 0.5) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw()

telco_churn |> 
  ggplot(aes(x = voice_package_dou)) + 
  geom_bar(color = "grey", fill = "steelblue", alpha = 0.75) + 
  scale_x_continuous(#breaks = seq(0, 100000, by = 10000), 
    labels = number_format(big.mark = ",")) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor.y = element_blank()
  )

telco_churn |> 
  ggplot(aes(x = voice_package_dou, y = churn, fill = churn)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw() + 
  theme(
    panel.grid.minor.y = element_blank(), 
    legend.position = "none"
  )

telco_churn |> 
  ggplot(aes(x = voice_package_dou, fill = churn)) + 
  geom_bar(position = "dodge", alpha = 0.5) + 
  scale_y_continuous(labels = number_format(big.mark = ",")) + 
  theme_bw()


# Korelasi ----------------------------------------------------------------

pmat <- telco_churn |> 
  select(-MSISDN, -churn) |> 
  cor_pmat()

telco_churn |> 
  select(-MSISDN, -churn) |> 
  cor() |> 
  ggcorrplot(lab = TRUE, p.mat = pmat, digits = 1, 
             colors = c("firebrick", "white", "darkgreen"))
