# How ecx_reference() builds the absolute references, and when it sets NA
#
# Run from the package root. Uses the package's own ecx_reference() and
# find_crossings(), so the plots show what the code does. Needs ggplot2 and
# patchwork, which are not package dependencies.
#
# Under type = "absolute" with ecx_val = p:
#   decreasing reference = control - (control - 0) * p / 100
#   increasing reference = control + (1 - control) * p / 100, NA if control >= 1

devtools::load_all()
library(ggplot2)

ecx_val <- 10
x_vec <- seq(0, 10, length.out = 101)

colour_curve <- "#2a78d6"
colour_reference <- "#eb6834"
colour_na <- "grey60"

# One realisation per row of `curve`. Returns the curves in long form, the two
# references for each draw, and the crossing found in each direction.
draw_data <- function(curve, ecx_val) {
  reference <- ecx_reference(curve, ecx_val, "absolute")
  draws <- seq_len(nrow(curve))

  curves <- data.frame(
    draw = factor(rep(draws, each = length(x_vec))),
    x = rep(x_vec, nrow(curve)),
    y = as.vector(t(curve))
  )

  references <- data.frame(
    draw = factor(rep(draws, 2)),
    direction = rep(c("decreasing", "increasing"), each = nrow(curve)),
    control = rep(curve[, 1], 2),
    reference = c(reference$decreasing, reference$increasing)
  )

  references$crossing <- vapply(
    seq_len(nrow(references)),
    function(i) {
      j <- as.integer(references$draw[i])
      find_crossings(curve[j, ], x_vec, references$reference[i])[[
        references$direction[i]
      ]]
    },
    numeric(1)
  )
  list(curves = curves, references = references)
}

label_reference <- function(references) {
  ifelse(
    is.na(references$reference),
    paste0(references$direction, ": NA (control >= 1)"),
    paste0(
      references$direction,
      ": ",
      format(round(references$reference, 3), nsmall = 3),
      ifelse(is.na(references$crossing), ", no crossing", "")
    )
  )
}

# A. An increasing proportion, control below 1 -------------------------------

y <- 0.2 + 0.6 * (1 - exp(-0.4 * x_vec))
a <- draw_data(rbind(y), ecx_val)

plot_a <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dotted", colour = colour_na) +
  annotate(
    "text",
    x = 10,
    y = 1,
    label = "upper bound 1",
    hjust = 1,
    vjust = -0.5,
    size = 3
  ) +
  geom_hline(
    data = a$references,
    aes(yintercept = reference),
    linetype = "dashed",
    colour = colour_reference
  ) +
  geom_line(
    data = a$curves,
    aes(x, y),
    colour = colour_curve,
    linewidth = 0.8
  ) +
  geom_point(
    data = a$references[!is.na(a$references$crossing), ],
    aes(crossing, reference),
    colour = colour_reference,
    size = 3
  ) +
  annotate(
    "text",
    x = 10,
    y = a$references$reference,
    label = label_reference(a$references),
    hjust = 1,
    vjust = c(1.5, -0.5),
    size = 3
  ) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(
    title = "A. Increasing proportion, control 0.2",
    subtitle = paste0(
      "Increasing reference: 0.2 + (1 - 0.2) x 10% = 0.28, ",
      "a crossing is found.\nDecreasing reference: 0.2 x 90% = 0.18, ",
      "the curve never falls, so NA."
    ),
    x = "concentration",
    y = "response"
  )

# B. A decreasing response on a gaussian scale, control 20 --------------------

y <- 5 + 15 * exp(-0.3 * x_vec)
b <- draw_data(rbind(y), ecx_val)
unguarded <- 20 + (1 - 20) * ecx_val / 100

plot_b <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dotted", colour = colour_na) +
  annotate(
    "text",
    x = 10,
    y = 1,
    label = "upper bound 1",
    hjust = 1,
    vjust = -0.5,
    size = 3
  ) +
  geom_hline(yintercept = unguarded, linetype = "dashed", colour = colour_na) +
  annotate(
    "text",
    x = 10,
    y = unguarded,
    label = paste0(
      "increasing without the guard: 20 + (1 - 20) x 10% = ",
      unguarded,
      ", below the control"
    ),
    hjust = 1,
    vjust = -0.5,
    size = 3,
    colour = "grey40"
  ) +
  geom_hline(
    data = b$references[b$references$direction == "decreasing", ],
    aes(yintercept = reference),
    linetype = "dashed",
    colour = colour_reference
  ) +
  geom_line(
    data = b$curves,
    aes(x, y),
    colour = colour_curve,
    linewidth = 0.8
  ) +
  geom_point(
    data = b$references[!is.na(b$references$crossing), ],
    aes(crossing, reference),
    colour = colour_reference,
    size = 3
  ) +
  annotate(
    "text",
    x = 10,
    y = b$references$reference[1],
    label = label_reference(b$references)[1],
    hjust = 1,
    vjust = 1.5,
    size = 3
  ) +
  coord_cartesian(ylim = c(0, 21)) +
  labs(
    title = "B. Decreasing response, control 20 (e.g. gaussian)",
    subtitle = paste0(
      "The increasing direction is searched on every call (T10). Its ",
      "reference would be 18.1, a\n10% fall towards 1 that sits on top of ",
      "the decreasing reference (18.0), so it is set to NA\nrather than ",
      "erroring (section 3.10 asked for an error)."
    ),
    x = "concentration",
    y = "response"
  )

# C. Draws whose controls straddle 1 ------------------------------------------

controls <- c(0.90, 0.95, 0.98, 1.00, 1.03, 1.08)
curve <- t(vapply(
  controls,
  function(c0) c0 + 0.6 * (1 - exp(-0.4 * x_vec)),
  numeric(length(x_vec))
))
d <- draw_data(curve, ecx_val)
inc <- d$references[d$references$direction == "increasing", ]
d$curves$has_reference <- !is.na(inc$reference[as.integer(d$curves$draw)])

plot_c <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dotted", colour = colour_na) +
  geom_line(
    data = d$curves,
    aes(x, y, group = draw, colour = has_reference),
    linewidth = 0.6
  ) +
  geom_point(
    data = inc[!is.na(inc$crossing), ],
    aes(crossing, reference),
    colour = colour_reference,
    size = 2.5
  ) +
  scale_colour_manual(
    values = c(`TRUE` = colour_curve, `FALSE` = colour_na),
    labels = c(`TRUE` = "control < 1: reference", `FALSE` = "control >= 1: NA"),
    name = NULL
  ) +
  labs(
    title = "C. One increasing curve, six draws, controls 0.90 to 1.08",
    subtitle = paste0(
      sum(!is.na(inc$reference)),
      " draws get an increasing reference and ",
      sum(is.na(inc$reference)),
      " get NA, decided draw by draw.\n",
      "The summary then has a mix of values and NA for a reason unrelated ",
      "to the curve (#60)."
    ),
    x = "concentration",
    y = "response"
  ) +
  theme(legend.position = "bottom")

# D. The decreasing side has no matching guard --------------------------------

y <- -0.13 + 0.3 * (1 - exp(-0.5 * x_vec))
dd <- draw_data(rbind(y), ecx_val)
dec <- dd$references[dd$references$direction == "decreasing", ]

plot_d <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", colour = colour_na) +
  annotate(
    "text",
    x = 10,
    y = 0,
    label = "lower bound 0",
    hjust = 1,
    vjust = 1.5,
    size = 3
  ) +
  geom_hline(
    yintercept = dec$reference,
    linetype = "dashed",
    colour = colour_reference
  ) +
  geom_line(
    data = dd$curves,
    aes(x, y),
    colour = colour_curve,
    linewidth = 0.8
  ) +
  annotate(
    "point",
    x = find_crossings(y, x_vec, dec$reference)$increasing,
    y = dec$reference,
    colour = colour_na,
    size = 3
  ) +
  annotate(
    "text",
    x = 10,
    y = dec$reference,
    label = paste0(
      "decreasing reference: -0.13 x 90% = ",
      round(dec$reference, 3),
      ", above the control"
    ),
    hjust = 1,
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "D. Control below 0 (gaussian), decreasing direction",
    subtitle = paste0(
      "The decreasing reference is not set to NA, but it lies above the ",
      "control. The curve rises\nthrough it, which counts as an increasing ",
      "crossing, so the decreasing ECx is NA anyway."
    ),
    x = "concentration",
    y = "response"
  )

# Combine ----------------------------------------------------------------------

plots <- patchwork::wrap_plots(plot_a, plot_b, plot_c, plot_d, ncol = 2) &
  theme_minimal(base_size = 10) &
  theme(
    plot.subtitle = element_text(size = 8.5, colour = "grey30"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
plots
