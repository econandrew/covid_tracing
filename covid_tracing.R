library(pbapply)
library(ggplot2)

# Proportion of people who get infected
ATTACK_RATE <- 0.10
TRACE_SUCCESS <- 0.20
SECONDARY_TRACE_THRESHOLD <- 2

props <- pbsapply(1:50000, function(m) {
  # Allocate uninfected people to events
  ppl <- data.frame(
    event = c(
      rep(paste0("weddng", 1:2), 100),
      rep(paste0("brunch", 1:80), 10)
    ),
    infected = FALSE,
    traced = NA
  )
  
  # Infect people uniformly
  ppl$infected[sample.int(nrow(ppl), nrow(ppl) * ATTACK_RATE)] <- TRUE
  
  # Primary contact tracing
  ppl$traced[ppl$infected] <- runif(sum(ppl$infected)) < TRACE_SUCCESS
  summary(ppl$traced)
  
  # Secondary contact tracing
  event_trace_counts <- table(ppl$event[ppl$traced])
  events_traced <- names(event_trace_counts)[event_trace_counts >= SECONDARY_TRACE_THRESHOLD]
  ppl$traced[ppl$infected & ppl$event %in% events_traced] <- TRUE
  summary(ppl$traced)
  
  # Summarise results
  ppl$event_type <- substr(ppl$event, 1, 1)
  
  wedding_infections <- sum(ppl$infected & ppl$event_type == "w")
  brunch_infections <- sum(ppl$infected & ppl$event_type == "b")
  p_wedding_infections <- wedding_infections / (brunch_infections + wedding_infections)

  wedding_traces <- sum(ppl$infected & ppl$traced & ppl$event_type == "w")
  brunch_traces <- sum(ppl$infected & ppl$traced & ppl$event_type == "b")
  p_wedding_traces <- wedding_traces / (brunch_traces + wedding_traces)
  
  c(p_wedding_infections, p_wedding_traces)
})

props_df <- setNames(as.data.frame(t(props)), c("Infections", "Traces"))

p <- ggplot(props_df) +
  geom_histogram(aes(x = Infections), fill = "blue", alpha = 0.75, binwidth = 0.05) +
  scale_x_continuous(breaks = 0:5/5, minor_breaks = 0:20/20, limits = c(0, 1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  xlab("Proportion of infections resulting from weddings") +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

png("p_true.png", width = 8, height = 4, units = "in", res = 192)
p
dev.off()

png("p_observed.png", width = 8, height = 4, units = "in", res = 192)
p +
  geom_histogram(aes(x = Traces), fill = "red", alpha = 0.75, binwidth = 0.05) +
  annotate("text", 0.25, 17000, label = "True proportion", color = "blue", alpha = 0.75, hjust = 0) +
  annotate("text", 0.4, 7500, label = "Observed proportion", color = "red", alpha = 0.75, hjust = 0) +
  ggtitle(
    "Contact tracing gives a biased view of the pandemic",
    "Distribution of true versus observed proportion of cases from weddings in a very simple model"
  )
dev.off()
