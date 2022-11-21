library(RCurl)
library(XML)
library(parallel)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(geeM)
library(igraph)
library(visNetwork)
library(shiny)
library(DT)
library(writexl)

options(mc.cores = detectCores() - 1)
RNGkind("L'Ecuyer-CMRG")
set.seed(666)

# Declare location of current script
i_am('R/app_shiny.R')

# ------------------------------- REDCap data ------------------------------- #

# API
uri   <- 'https://redcap.unisante.ch/api/'

# Token
token <- here('misc', 'redcap_personal_data.token')
token <- readChar(token, file.info(token)$size - 1)

# Import personal data with the API (RCurl)
pdata <- read.csv(text = postForm(
  uri = uri,
  token = token,
  content = 'record',
  format = 'csv'
))
rm(uri, token)

# ------------------------------- Graph data -------------------------------- #

files <- sapply(list.dirs(here('data-raw', 'matrices_20221110'),
                          recursive = FALSE),
                list.files, full.names = TRUE)
gdata <- mclapply(files, function(f) {
  # Family ID
  fid <- sub('\\.csv$', '', basename(f))
  # Contingency matrices
  cmat <- as.matrix(read.csv(f, row.names = 1, check.names = FALSE))
  cmat[is.na(cmat)] <- ''
  cmat <- trimws(cmat)
  rownames(cmat) <- substr(rownames(cmat), 1, 10)
  colnames(cmat) <- substr(colnames(cmat), 1, 10)
  # Nodes
  nodes <- pdata %>%
    filter(cat_e_participant_type %in% 1:2) %>%
    select(id = cat_ec_participant_sid, indexCase = cat_e_participant_type,
           armExp = cat_s_arm, positive = cat_l_result,
           firstName = cat_dp_prenom_v2, sexM = cat_dp_sexe_v2) %>%
    mutate(participate = 1L) %>%
    right_join(data.frame(id = unique(rownames(cmat))), by = 'id') %>%
    mutate(
      indexCase = if_else(is.na(indexCase), 0L, 2L - indexCase),
      armExp = 2L - armExp,
      positive = 2L - positive,
      sexM = 2L - sexM,
      participate = if_else(is.na(participate), 0L, participate)
    )
  nodes[is.na(nodes$armExp), 'armExp'] <- 
    if (all(is.na(nodes$armExp))) NA else na.omit(nodes$armExp)[1]
  # Edges
  edges <- map_dfr(rownames(cmat), function(x) {
    z <- cmat[x, ][cmat[x, ] != '']
    if (length(z) == 0) {
      data.frame(from = character(0), to = character(0), label = character(0))
    } else {
      data.frame(from = x, to = names(z), label = unname(z))
    }
  })
  # We only keep the edges which go away of the index case (desymmetrization)
  if (nrow(edges) > 0 & sum(nodes$indexCase) == 1) {
    g <- graph_from_edgelist(as.matrix(edges[, c('from', 'to'), drop = FALSE]))
    d <- distances(g)[nodes[nodes$indexCase == 1, 'id'], ]
    edges <- edges[d[edges[, 'from']] < d[edges[, 'to']], , drop = FALSE]
  }
  # Complete missing sex values
  sex <- edges %>%
    select(id = to, sexM.2 = label) %>%
    mutate(sexM.2 = recode(sexM.2, Father = 1L, Mother = 0L, Brother = 1L,
                           Sister = 0L, Son = 1L, Daughter = 0L))
  nodes <- nodes %>%
    left_join(sex, by = 'id') %>%
    mutate(sexM = if_else(is.na(sexM), sexM.2, sexM)) %>%
    select(!sexM.2)
  # Levels (generations)
  if (nrow(edges) > 0 & sum(nodes$indexCase) == 1) {
    ic <- nodes[nodes$indexCase == 1, 'id']
    lvls <- edges %>%
      mutate(lvl = case_when(
        label %in% c('Father', 'Mother') ~ -1L,
        label %in% c('Brother', 'Sister') ~ 0L,
        label %in% c('Son', 'Daughter') ~ 1L,
        TRUE ~ NA_integer_
      )) %>%
    select(!label)
    for (i in unique(lvls$from)) {
      lvls <- add_row(lvls, from = i, to = i, lvl = 0L)
    }
    k <- 0
    while (!all(names(V(g)) %in% lvls[lvls$from == ic, 'to']) & k <= 15) {
      lvls <- left_join(lvls, lvls, by = c('to' = 'from'),
                        suffix = c('', '_update')) %>%
        mutate(
          b = !is.na(to_update),
          to = if_else(b, to_update, to),
          lvl = if_else(b, lvl + lvl_update, lvl)
        ) %>%
        select(from, to, lvl) %>%
        unique()
      k <- k + 1
    }
    lvls <- lvls %>%
      filter(from == ic) %>%
      select(id = to, level = lvl) %>%
      mutate(level = level - min(level) + 1L)
    nodes <- left_join(nodes, lvls, by = 'id') %>%
      mutate(level = if_else(!is.na(level), level, 1L))
  }
  # Not eligible relatives
  # * If one parent of a positive case is positive, then the other parent is
  #   not eligible
  # * Relatives of a negative case, who is not an index case, are not eligible
  nonEligibleParents <- edges %>%
    filter(label %in% c('Father', 'Mother')) %>%
    rename(id = from, parentId = to) %>%
    group_by(id) %>%
    mutate(n_parents = n()) %>%
    left_join(select(nodes, id, positiveParent = positive),
              by = c('parentId' = 'id')) %>%
    mutate(
      anyPositiveParent = any(positiveParent == 1, na.rm = TRUE),
      nonEligibleParent = anyPositiveParent & !(positiveParent %in% 1)
    ) %>%
    ungroup() %>%
    select(id = parentId, nonEligibleParent)
  nonEligibleRelatives <- edges %>%
    rename(id = from, relativeId = to) %>%
    left_join(select(nodes, id, indexCase, positive), by = 'id') %>%
    mutate(nonEligibleRelative = indexCase == 0 & positive == 0) %>%
    select(id = relativeId, nonEligibleRelative) %>%
    full_join(nonEligibleParents, by = 'id') %>%
    mutate(nonEligibleParent = replace_na(nonEligibleParent, FALSE),
           nonEligible = nonEligibleParent | nonEligibleRelative) %>%
    select(id, nonEligible)
  nodes <- nodes %>%
    left_join(nonEligibleRelatives, by = 'id') %>%
    mutate(nonEligible = as.integer(replace_na(nonEligible, FALSE)))
  # Format nodes
  nodes <- nodes %>%
    mutate(
      label = firstName,
      #label = id,
      shape = case_when(sexM == 1L ~ 'square',
                        sexM == 0L ~ 'dot',
                        TRUE ~ 'diamond'),
      color = case_when(positive == 1L ~ 'red',
                        positive == 0L ~ 'green',
                        participate == 1L ~ 'grey',
                        nonEligible == 1L ~ 'brown',
                        TRUE ~ 'black')
    )
  # return results
  list(nodes = nodes, edges = edges, cmat = cmat, fid = fid)
})
names(gdata) <- sapply(gdata, function(z) z$fid)
rm(files)

# -------------------------------- Analyses --------------------------------- #

# Number of relatives, participants, results and positives per family
ndata <- map_dfr(gdata, ~ mutate(.x$nodes, fid = .x$fid)) %>%
  group_by(fid) %>%
  mutate(
    positiveIndexCase = 
      if (sum(indexCase) == 1) positive[indexCase == 1] else NA_integer_,
    armExp = if (length(unique(na.omit(armExp))) == 1) na.omit(armExp)[1] else
      NA_integer_
  ) %>%
  filter(!(nonEligible %in% 1), indexCase == 0) %>%
  summarise(
    positiveIndexCase = positiveIndexCase[1],
    armExp = armExp[1],
    n_relatives = n(),
    n_participants = sum(participate),
    n_results = sum(!is.na(positive)),
    n_positive = sum(positive, na.rm = TRUE)
  )

# Participation and positivity rates (proportions)
tbls <- c(participation_rate.all_IC      = 1,
          participation_rate.negative_IC = 2,
          participation_rate.positive_IC = 3,
          positivity_rate                = 4)
tbls <- mclapply(tbls, function(j) {
  k <- if (j <= 3) 1 else 2
  dta <- if (j == 1) {
    ndata
  } else if (j == 2) {
    filter(ndata, positiveIndexCase == 0)
  } else {
    filter(ndata, positiveIndexCase == 1)
  }
  dta <- filter(dta, !is.na(armExp))
  dta <- filter(dta, if (k == 1) n_relatives > 0 else n_results > 0)
  dta <- map_dfr(1:nrow(dta), function(i) {
    with(dta[i, ], data.frame(
      fid = fid,
      arm = factor(armExp, 0:1, c('Control', 'Experimental')),
      y = if (k == 1) {
        c(rep(0L, n_relatives - n_participants), rep(1L, n_participants))
      } else {
        c(rep(0L, n_results - n_positive), rep(1L, n_positive))
      }
    ))
  })
  tab <- map_dfr(1:2, function(l) {
    fml <- if (l == 1) y ~ 1 else y ~ arm
    fit <- geem(fml, id = fid, family = binomial, data = dta,
                corstr = 'exchangeable')
    smy <- summary(fit)
    # odds ratio of the GEE model
    or <- smy$beta
    s <- qnorm(0.975) * smy$se.robust
    or <- exp(cbind(or = or, or_lwr = or - s, or_upr = or + s))
    or <- cbind(data.frame(coef = smy$coefnames), or,
                data.frame(pval = smy$p))
    # predictions of the gee model
    x <- if (length(fit$beta) == 1) list(1) else list(c(1, 0), c(1, 1))
    s <- qnorm(0.975) * 
      sqrt(sapply(x, function(z) (z %*% fit$var %*% z)[1, 1]))
    p <- sapply(x, function(z) z %*% fit$beta)
    p <- 1 / (1 + exp(-cbind(prop = p, prop_lwr = p - s, prop_upr = p + s)))
    p <- cbind(data.frame(coef = smy$coefnames), p)
    # Table
    w <- data.frame(or = NA, or_lwr = NA, or_upr = NA, pval = NA)
    tab <- cbind(p, rbind(w, or[-1, -1]))
    if (nrow(tab) == 1) {
      tab$coef <- ''
    } else {
      tab$coef <- sub('\\(Intercept\\)', 'Control', tab$coef)
      tab$coef <- sub('^arm', '', tab$coef)
    }
    names(tab)[names(tab) == 'coef'] <- 'arm'
    tab
  })
  Merge <- function(x, y) merge(x, y, by = 'arm')
  n_fam <- aggregate(fid ~ arm, dta, function(z) length(unique(z)))
  names(n_fam)[2] <- 'n_families'
  n0 <- data.frame(arm = '', n_families = length(unique(dta$fid)))
  n_fam <- rbind(n0, n_fam)
  n <- Reduce(Merge, lapply(c('length', 'sum', 'mean'), function(w) {
    z <- aggregate(y ~ arm, dta, get(w))
    names(z)[2] <- w
    z0 <- setNames(data.frame('', get(w)(dta$y)), c('arm', w))
    return(rbind(z0, z))
  }))
  names(n)[names(n) == 'length'] <- c('n_relatives', 'n_results')[k]
  names(n)[names(n) == 'sum'] <- c('n_participants', 'n_positive')[k]
  names(n)[names(n) == 'mean'] <- 'raw_prop'
  Merge(Merge(n_fam, n), tab)
})

# Detection rate
dta <- ndata %>%
  filter(positiveIndexCase == 1) %>%
  mutate(arm = factor(armExp, 0:1, c('Control', 'Experimental'))) %>%
  select(n_positive, arm) %>%
  na.omit()
tab <- map_dfr(1:2, function(l) {
  fml <- if (l == 1) n_positive ~ 1 else n_positive ~ arm
  fit <- glm(fml, family = poisson, data = dta)
  # rate ratios of the Poisson model
  beta <- coef(fit)
  s <- qnorm(0.975) * sqrt(diag(vcov(fit)))
  rr <- exp(cbind(rr = beta, rr_lwr = beta - s, rr_upr = beta + s))
  rr <- cbind(data.frame(coef = names(beta)), rr,
              pval = coef(summary(fit))[, 4])
  # predictions of the Poisson model
  x <- if (length(beta) == 1) list(1) else list(c(1, 0), c(1, 1))
  s <- qnorm(0.975) *
    sqrt(sapply(x, function(z) (z %*% vcov(fit) %*% z)[1, 1]))
  r <- sapply(x, function(z) z %*% beta)
  r <- exp(cbind(rate = r, rate_lwr = r - s, rate_upr = r + s))
  r <- cbind(data.frame(coef = names(beta)), r)
  # Table
  w <- data.frame(rr = NA, rr_lwr = NA, rr_upr = NA, pval = NA)
  tab <- cbind(r, rbind(w, rr[-1, -1]))
  if (nrow(tab) == 1) {
    tab$coef <- ''
  } else {
    tab$coef <- sub('\\(Intercept\\)', 'Control', tab$coef)
    tab$coef <- sub('^arm', '', tab$coef)
  }
  names(tab)[names(tab) == 'coef'] <- 'arm'
  names(tab) <- sub('^rr', 'rate_ratio', names(tab))
  rownames(tab) <- NULL
  tab
})
n <- data.frame(
  arm = c('', 'Control', 'Experimental'),
  n_families = c(nrow(dta), sum(dta$arm == 'Control'),
                 sum(dta$arm == 'Experimental')),
  n_positive = c(sum(dta$n_positive),
                 sum(dta[dta$arm == 'Control', 'n_positive']),
                 sum(dta[dta$arm == 'Experimental', 'n_positive']))
)
tbls$detection_rate <- full_join(n, tab, by = 'arm')
rm(dta, tab, n)

# Formatted tables
ftbls <- lapply(tbls, function(tbl) {
  names(tbl) = recode(names(tbl),
    arm            = 'Arm',
    n_families     = 'Index cases',
    n_relatives    = 'Eligible relatives',
    n_participants = 'Tested relatives',
    n_results      = 'Relatives with test restitution',
    n_positive     = 'Positive relatives',
    raw_prop       = 'Raw proportion'
  )
  if (all(c('prop', 'or') %in% names(tbl))) {
    tbl %>%
      mutate(
        prop = paste0(sprintf("%.2f", prop), ' (', sprintf("%.2f", prop_lwr),
                      ',', sprintf("%.2f", prop_upr), ')'),
        or = if_else(!is.na(or), 
          paste0(sprintf("%.2f", or), ' (', sprintf("%.2f", or_lwr), ',',
                 sprintf("%.2f", or_upr), ')'),
          as.character(NA))
      ) %>%
      select(!c(prop_lwr, prop_upr, or_lwr, or_upr)) %>%
      rename(`Estimated proportion` = prop, `Odds ratio` = or,
             `p-value` = pval)
  } else {
    tbl %>%
      mutate(
        rate = paste0(sprintf("%.2f", rate), ' (', sprintf("%.2f", rate_lwr),
                      ',', sprintf("%.2f", rate_upr), ')'),
        rate_ratio = if_else(!is.na(rate_ratio), 
          paste0(sprintf("%.2f", rate_ratio), ' (',
                 sprintf("%.2f", rate_ratio_lwr), ',',
                 sprintf("%.2f", rate_ratio_upr), ')'),
          as.character(NA))
      ) %>%
      select(!c(rate_lwr, rate_upr, rate_ratio_lwr, rate_ratio_upr)) %>%
      rename(`Rate` = rate, `Rate ratio` = rate_ratio, `p-value` = pval)
  }
})

# -------------------------------- ShinyApp --------------------------------- #

ui <- shinyUI(fluidPage(
  titlePanel('CATCH'),
  tabsetPanel(
    # --- Family Trees
    tabPanel('Plot Family Trees',
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = 'family',
              label   = 'Family',
              choices = setNames(names(gdata), names(gdata))
            ),
            radioButtons(
              inputId = 'node_id_to_label',
              label   = 'Node labels',
              choices = list(`First Name (if known)` = FALSE,
                             `ID`                    = TRUE)
            ),
            div(
              style = "border-style: solid; border-color: black;
                       border-width: thin; text-align:justify",
              HTML("
                Square: Male, Circle: Female
                <br>
                Red: Positive case
                <br>
                Green: Negative case
                <br>
                Grey: Undetermined case
                <br>
                Black: Does not participate
                <br>
                Brown: relative who will not be asked to participate
              "),
            )
          ),
          mainPanel(
            visNetworkOutput('gplot', width = '700px', height = '700px')
          )
        )
      )
    ),
    # --- Participation rate
    tabPanel('Proportion of relatives who participates',
      fluidPage(
        h3('All index cases'),
        tableOutput('tbl_part_all'),
        h3('Positive index cases'),
        tableOutput('tbl_part_pos'),
        h3('Negative index cases'),
        tableOutput('tbl_part_neg'),
        fluidRow(
          downloadButton('dl_tbls', 'Export tables'),
          downloadButton('dl_ftbls', 'Export formatted tables'),
        )
      )
    ),
    # --- Participation rate
    tabPanel('Proportion of positive relatives',
      fluidPage(
        h3('Positive index cases'),
        tableOutput('tbl_pos'),
        fluidRow(
          downloadButton('dl_tbls_2', 'Export tables'),
          downloadButton('dl_ftbls_2', 'Export formatted tables'),
        )
      )
    ),
    # --- Detection rate
    tabPanel('Detection rate',
      fluidPage(
        h3('Positive index cases'),
        tableOutput('tbl_detect'),
        fluidRow(
          downloadButton('dl_tbls_3', 'Export tables'),
          downloadButton('dl_ftbls_3', 'Export formatted tables'),
        )
      )
    ),
    # --- Data
    tabPanel('Data',
      fluidPage(
        dataTableOutput('tbl_data'),
      )
    ),
    # ---
  )
))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input, output) {
  # Node coordinates (not initially defined)
  nodes_coordinates <- reactiveValues()
  # Graph
  g = reactive({
    fid <- input$family
    nodes <- gdata[[fid]]$nodes
    edges <- gdata[[fid]]$edges
    list(graph = graph.data.frame(edges, directed = TRUE, vertices = nodes),
         levels = nodes$level, fid = fid)
  })
  # Graph output
  output$gplot = renderVisNetwork({
    visIgraph(g()$graph, idToLabel = input$node_id_to_label) %>%
      { 
        if (is.null(nodes_coordinates[[g()$fid]])) {
          visIgraphLayout(., layout = 'layout_with_sugiyama',
                          layers = g()$levels)
        } else {
          visIgraphLayout(., layout = "layout.norm",
                          layoutMatrix = nodes_coordinates[[g()$fid]])
        }
      } %>%
      visExport(type = 'jpeg', name = g()$fid, label = 'Eport graph')
  })
  # Get nodes coordinates and put them in a matrix
  observeEvent(input$node_id_to_label, {
    visNetworkProxy('gplot') %>%
      visGetNodes(input = paste0('nodes_', g()$fid))
  })
  observe({
    if (!is.null(input[[paste0('nodes_', g()$fid)]])) {
      nodes_coordinates[[g()$fid]] <- input[[paste0('nodes_', g()$fid)]] %>%
        sapply(function(n) c(x = n$x, y = n$y)) %>%
        t()
    }
  })
  # Output tables
  output$tbl_part_all <- renderTable(ftbls$participation_rate.all_IC)
  output$tbl_part_pos <- renderTable(ftbls$participation_rate.positive_IC)
  output$tbl_part_neg <- renderTable(ftbls$participation_rate.negative_IC)
  output$tbl_pos <- renderTable(ftbls$positivity_rate)
  output$tbl_detect <- renderTable(ftbls$detection_rate)
  #output$tbl_data <- renderDataTable(ndata)
  output$tbl_data <- ndata %>%
    mutate(positiveIndexCase = factor(positiveIndexCase),
           armExp = factor(armExp)) %>%
    renderDT(rownames = FALSE, filter = "top")
  # Download tables
  output$dl_tbls <- downloadHandler(
    filename = function() {
      paste0('catch_tables_', format(Sys.Date(), '%Y%m%d'), '.xlsx')
    },
    content = function(file) {
      write_xlsx(append(tbls, list(data = ndata)), path = file)
    }
  )
  output$dl_ftbls <- downloadHandler(
    filename = function() {
      paste0('catch_formatted_tables_', format(Sys.Date(), '%Y%m%d'), '.xlsx')
    },
    content = function(file) {
      write_xlsx(append(ftbls, list(data = ndata)), path = file)
    }
  )
  output$dl_tbls_2 <- downloadHandler(
    filename = function() {
      paste0('catch_tables_', format(Sys.Date(), '%Y%m%d'), '.xlsx')
    },
    content = function(file) {
      write_xlsx(append(tbls, list(data = ndata)), path = file)
    }
  )
  output$dl_ftbls_2 <- downloadHandler(
    filename = function() {
      paste0('catch_formatted_tables_', format(Sys.Date(), '%Y%m%d'), '.xlsx')
    },
    content = function(file) {
      write_xlsx(append(ftbls, list(data = ndata)), path = file)
    }
  )
  output$dl_tbls_3 <- downloadHandler(
    filename = function() {
      paste0('catch_tables_', format(Sys.Date(), '%Y%m%d'), '.xlsx')
    },
    content = function(file) {
      write_xlsx(append(tbls, list(data = ndata)), path = file)
    }
  )
  output$dl_ftbls_3 <- downloadHandler(
    filename = function() {
      paste0('catch_formatted_tables_', format(Sys.Date(), '%Y%m%d'), '.xlsx')
    },
    content = function(file) {
      write_xlsx(append(ftbls, list(data = ndata)), path = file)
    }
  )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shinyApp(ui, server)

