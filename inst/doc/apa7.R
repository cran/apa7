## ----setup, include = FALSE---------------------------------------------------
library(apa7)
library(flextable)
library(ftExtra)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(psych)
set_flextable_defaults(theme_fun = theme_apa, 
                       font.family = "Times New Roman", 
                       text.align = "center", 
                       table_align = "left")


## ----setupdisplay-------------------------------------------------------------
library(apa7)
library(flextable)
library(ftExtra)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(psych)
set_flextable_defaults(theme_fun = theme_apa, 
                       font.family = "Times New Roman", 
                       text.align = "center", 
                       table_align = "left")


## ----rawdata------------------------------------------------------------------
d <- tibble(
  Model = paste("Model", c(rep(1,2), rep(2, 3))),
  Predictor = c(
    "Constant", "Socioeconomic status", 
    "Constant", "Socioeconomic status", "Age"),
  b = c(-4.5, 1.23, 
        -5.1, 1.45, -.23),
  beta = c(NA, .24, 
           NA, .31, .031),
  t = c(-18.457, 2.345, 
        -22.457, 2.114, .854),
  df = c(85,85, 
         84, 84, 84),
  p = c(.0001, .0245, 
        .0001, .0341, .544))

d


## ----initialflextable---------------------------------------------------------
set_flextable_defaults(
  theme_fun = theme_apa, 
  font.family = "Times New Roman", 
  text.align = "center",
  table_align = "left")

flextable(d) 


## ----apa_flextable------------------------------------------------------------
apa_flextable(d, row_title_column = Model) 


## ----centeralign--------------------------------------------------------------
apa_flextable(d, 
              row_title_column = Model, 
              row_title_align = "center") 


## ----vmerge-------------------------------------------------------------------
d |> 
  mutate(Model = str_remove(Model, "Model ")) |> 
apa_flextable() |> 
  merge_v() |>
  align(j = "Predictor", part = "all") |>
  align(j = "Model", align = "center") |>
  valign(j = "Model", valign = "middle") |>
  surround(i = 2, border.bottom = flextable::fp_border_default()) |>
  width(width = c(.8, 1.75, rep(.8, 5)))


## ----selectivebold------------------------------------------------------------
apa_flextable(d, row_title_column = Model) |> 
  bold(i = 3, j = 3)


## ----flextablefunctions-------------------------------------------------------
#| echo: false
tibble::tribble(
  ~Target,      ~Function,               ~Purpose,
   "Cell",        "align", "Horizontal alignment",
   "Cell",           "bg",     "Background color",
   "Cell", "line_spacing",         "Line spacing",
   "Cell",      "padding",         "Cell padding",
   "Cell",       "valign",   "Vertical alignment",
   "Cell",       "rotate",          "Rotate text",
   "Cell",     "surround",         "Cell borders",
   "Cell",        "width",         "Column width",
   "Text",         "font",          "Font family",
   "Text",     "fontsize",            "Font size",
   "Text",       "italic",       "Italicize text",
   "Text",         "bold",            "Bold text",
   "Text",        "color",           "Color text",
   "Text",    "highlight",      "Highlight color"
  ) |> 
  arrange(desc(Target), Function) |> 
  mutate(
    Function = paste0("[", tagger(Function, "`"), "](https://davidgohel.github.io/flextable/reference/", Function, ".html)"),
    Target = bold_md(Target)) |> 
  # as_grouped_data() |> 
  apa_flextable(row_title_column = Target, 
                row_title_align = "left",
                table_width = .5) |> 
  align()


## ----autooff------------------------------------------------------------------
apa_flextable(d, 
              row_title_column = Model, 
              auto_format_columns = FALSE)


## ----collumnformat------------------------------------------------------------
cf_predictor <- column_format(
  name = "Predictor", 
  header = "Variable",
  latex = "Variable",
  formatter = stringr::str_to_upper)

cf_predictor


## ----myformats----------------------------------------------------------------
# Make new formatter object with default accuracy of .001
my_formats <- column_formats(accuracy = .001)

# Add Predictor column formatter
my_formats$Predictor <- cf_predictor

# Remove formatter for beta column
my_formats$beta <- NULL

apa_flextable(d, 
              row_title_column = Model, 
              column_formats = my_formats)


## ----formattibble-------------------------------------------------------------
my_formats@get_tibble |> 
  select(-formatter) |>
  dplyr::arrange(name, .locale = "en") |> 
  apa_flextable(markdown_body = F)


## ----intrepid-----------------------------------------------------------------
d |> 
  # Create column spanners
  rename_with(.cols = c(b, beta), 
              \(x) paste0("Coefficients_", x)) |> 
  rename_with(.cols = c(t, df, p), 
              .fn = \(x) paste0("Significance Test_", x)) |>
  # Step 1: Space between column spanners
  add_break_columns(ends_with("beta")) |> 
  # Step 2: Make row titles
  flextable::as_grouped_data("Model") |> 
  mutate(row_title = Model, .before = 1) |>
  fill(Model) |>
  # Step 3: Format data
  apa_format_columns() %>% 
  # Step 4: Convert to flextable
  flextable(col_keys = colnames(
    select(., -Model, -row_title))) |> 
  mk_par(i = ~ !is.na(row_title), 
         value = as_paragraph(row_title)) |>
  merge_h(i = ~ !is.na(row_title)) |>
  # Step 5: Separate headers into column spanners and deckered heads
  flextable::separate_header() |>  
  # Step 6: Make borders between row groups
  surround(
    i = ~ !is.na(row_title),
    border.top = list(
      color = "gray20",
      style = "solid",
      width = 1
    )
  ) |> 
  # Step 7: Style table and convert markdown
  apa_style() |> 
  align(j = 1, i = ~is.na(row_title)) |> 
  align(i = ~!is.na(row_title), align = "center") |> 
  # Step 8: Pretty widths
  pretty_widths() 


## ----diamonds-----------------------------------------------------------------
d_diamonds <- ggplot2::diamonds %>% 
  select(cut, carat, depth, table) %>% 
  arrange(cut) %>% 
  rename_with(str_to_title) %>% 
  pivot_longer(where(is.numeric), names_to = "Variable") %>% 
  summarise(
    M = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    .by = c(Variable, Cut)) %>% 
  pivot_longer(c(M, SD)) %>% 
  unite(Variable, Variable, name) %>%
  pivot_wider(names_from = Variable) 
d_diamonds


## ----flexdiamonds-------------------------------------------------------------
apa_flextable(d_diamonds)


## ----breakdiamonds------------------------------------------------------------
d_diamonds |> 
  add_break_columns(Carat_SD, Depth_SD)


## ----breakflexdiamonds--------------------------------------------------------
d_diamonds |> 
  add_break_columns(ends_with("SD"), 
                    omit_last = TRUE) |> 
  apa_flextable()


## ----columnspanners-----------------------------------------------------------
d |> 
  column_spanner_label("Significance test", c(t,df,p)) |> 
  column_spanner_label("Coefficients", starts_with("b")) |> 
  apa_flextable(row_title_column = Model)


## ----alighchr-----------------------------------------------------------------
tibble(x = align_chr(c(2.431, -0.4, -10, 101))) |> 
  apa_flextable(table_width = .2) |> 
  align(align = "center")


## ----zeroes-------------------------------------------------------------------
tibble(x = align_chr(c(2.431, -0.4, -10, 101),
                     drop0trailing = TRUE,
                     trim_leading_zeros = TRUE)) |>
  apa_flextable(table_width = .2) |>
  align(align = "center")


## ----quotetable---------------------------------------------------------------
d_quote <- tibble(
  Quote = c(
    "Believe those who are seeking the truth. Doubt those who find it.",
    "Resentment is like drinking poison and waiting for the other person to die.",
    "What you read when you don’t have to, determines what you will be when you can’t help it.",
    "Advice is what we ask for when we already know the answer but wish we didn’t.",
    "Do not ask whether a statement is true until you know what it means.",
    "Tact is the art of making a point without making an enemy.",
    "Short cuts make long delays.",
    "The price one pays for pursuing any profession or calling is an intimate knowledge of its ugly side.",
    "There is a stubbornness about me that never can bear to be frightened at the will of others. My courage always rises at every attempt to intimidate me",
    "There is a crack in everything, that’s how the light gets in.",
    "If you choose to dig a rather deep hole, someday you will have no choice but to keep on digging, even with tears.",
    "We long for self-confidence, till we look at the people who have it.",
    "Writing is a way to end up thinking something you couldn’t have started out thinking.",
    "A little inaccuracy sometimes saves tons of explanation.",
    "Each snowflake in an avalanche pleads not guilty.",
    "What I write is smarter than I am. Because I can rewrite it."
  ),
  Attribution = c(
    "Andre Gide",
    "Carrie Fisher",
    "Charles Francis Potter",
    "Erica Jong",
    "Errett Bishop",
    "Howard W. Newton",
    "J.R.R. Tolkien",
    "James Baldwin",
    "Jane Austin",
    "Leonard Cohen",
    "Liyun Chen",
    "Mignon McLaughlin",
    "Peter Elbow",
    "Saki",
    "Stanislaw J. Lec",
    "Susan Sontag"
  )
) |> 
  arrange(nchar(Quote))

d_quote |> 
  mutate(Quote = paste0(seq_along(Quote), 
                        ".\u2007",
                        Quote) |> 
           align_chr(side = "left") |> 
           hanging_indent(width = 55, indent = 7)) |> 
  apa_flextable() |>  
  align(j = "Attribution", part = "all") |> 
  width(width = c(4.5, 2))


## ----numberedlist-------------------------------------------------------------
d_quote |> 
  
    mutate(linechar = purrr::map_int(Quote, \(x) {
    stringr::str_split(x, "\\\\\n") |> 
      purrr::map(str_trim) |> 
      purrr::map(nchar) |> 
      purrr::map_int(max)
  })) |> 
  arrange(linechar) |> 
  select(-linechar) |> 
  add_list_column(Quote) |> 
  apa_flextable() |> 
  align(j = "Attribution", part = "all") |>
  width(width = c(.3, 4.2, 2))


## ----letterlist---------------------------------------------------------------
d_quote |> 
  add_list_column(Quote, type = "A", sep = ") ") |> 
  apa_flextable() |> 
  align(j = "Attribution", part = "all") |>
  width(width = c(.3, 4.2, 2))


## ----addstar------------------------------------------------------------------
d_star <- tibble(
  Predictor = c("Constant", "Socioeconomic status"),
  b = c(.45,.55),
  p = c(.02, .0002)) |> 
  add_star_column(b, p = p) 

d_star

apa_flextable(d_star)


## ----separatestarcolumn-------------------------------------------------------
d_star <- tibble(Predictor = c("Constant", "Socioeconomic status"),
                 b = c("1.10***", "2.32*"),
                 beta = c(NA, .34)) |> 
  separate_star_column(b)

d_star

apa_flextable(d_star)


## ----fullcontrol--------------------------------------------------------------
d |> 
  # # decimal align b and append p-value stars
  mutate(b = paste0(
    align_chr(b), 
    p2stars(p))) |> 
  # deselect t, df, and p
  select(-c(t,df, p)) |> 
  # restructure data
  pivot_wider_name_first(names_from = Model, 
                      values_from = c(b, beta)) |> 
  # convert to flextable
  apa_flextable() |> 
  # add footnotes
  add_footer_lines(
    values = as_paragraph_md(
      c(paste(
        "*Note*. *b* = unstandardized regression coefficient.",
        "&beta; = standardized regression coefficient."),
        apa_p_star_note()))) |>  
  # align footnote
  align(part = "footer", align = "left") |> 
  # Make column widths even
  width(width = c(2.05, 1.1, 1.1, .05, 1.1, 1.1))


## ----apaparameters------------------------------------------------------------
fit <- lm(price ~ carat, data = ggplot2::diamonds) 

fit |> 
  apa_parameters() |> 
  apa_flextable()


## ----apaperformance-----------------------------------------------------------
apa_performance(fit) |> 
  apa_flextable()


## ----metrics------------------------------------------------------------------
apa_performance(fit, metrics = c("R2", "Sigma", "AIC", "BIC")) |>
  apa_flextable()


## ----allmetrics---------------------------------------------------------------
apa_performance(fit, metrics = "all") |> 
  apa_flextable() 


## ----fit3---------------------------------------------------------------------
fit_3 <- list(
  lm(price ~ cut, data = ggplot2::diamonds),
  lm(price ~ cut + table, data = ggplot2::diamonds),
  lm(price ~ cut + table + carat, data = ggplot2::diamonds)
)

fit_3 |> 
  apa_parameters() |>
  apa_flextable(row_title_column = Model, 
                row_title_align = "center")


## ----comparison---------------------------------------------------------------
fit_3 |> 
  apa_performance_comparison() |> 
  apa_flextable()


## ----correlation--------------------------------------------------------------
ggplot2::diamonds |> 
  select(table,  carat, length = x, width = y , depth = z) |> 
  apa_cor() 


## ----chidiamons---------------------------------------------------------------
ggplot2::diamonds |> 
  select(Cut = cut, Color = color ) |> 
  apa_chisq()


## ----ggdiamonds---------------------------------------------------------------
#| fig-width: 8
#| fig-height: 8
library(ggplot2)
ggplot2::diamonds |>
  select(Cut = cut, Color = color) |>
  count(Cut, Color) |>
  mutate(p = scales::percent(n / sum(n), accuracy = .1), .by = Cut) |>
  ggplot(aes(Cut, n, fill = Color)) +
  geom_col(position = position_fill(),
           alpha = .6,
           width = .96) +
  geom_text(
    aes(label = paste0(p, " (", n, ")")),
    position = position_fill(vjust = .5),
    size.unit = "pt",
    size = 14 * .8,
    color = "gray10"
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  scale_y_continuous(
    "Cumulative Proportion",
    expand = expansion(c(0, .025)),
    labels = \(x) scales::percent(x, accuracy = 1)
  ) +
  scale_x_discrete(expand = expansion()) +
  theme(panel.grid.major.x = element_blank())


## ----fa-----------------------------------------------------------------------
# Get variable names
rename_items <- psych::bfi.dictionary |>
  tibble::rownames_to_column("variable") |> 
  mutate(Item = str_remove(Item, "\\.$")) |> 
  select(Item, variable) |> 
  deframe()

# Make data
d <- psych::bfi |> 
  select(-gender:-age) |> 
  rename(any_of(rename_items))

# Analysis
fit <- fa(d, nfactors = 5, fm = "pa", )


# Make table
fit |>
  apa_loadings() |> 
  rename(Extraversion = PA1,
         Neuroticism = PA2,
         Conscientiousness = PA3,
         Openness = PA4,
         Agreeableness = PA5) |> 
  apa_flextable(no_format_columns = Variable) 

