# Tier 1 tests: the cell table. No graphics device is opened anywhere in this
# file. Everything here is pure arithmetic over data.

# ---------------------------------------------------------------------------
# elide_index() -- pure integer arithmetic
# ---------------------------------------------------------------------------

test_that("elide_index() elides nothing at n <= max_n", {
  for (n in c(0L, 1L, 5L, 19L, 20L)) {
    e <- elide_index(n, 20L)
    expect_equal(e$keep, seq_len(n), info = n)
    expect_true(is.na(e$gap), info = n)
    expect_equal(e$hidden, 0L, info = n)
  }
})

test_that("elide_index() elides the middle the moment n exceeds max_n", {
  e <- elide_index(21L, 20L)

  # 20 drawn lanes: 19 of data plus 1 spent on the "...".
  expect_equal(length(e$keep) + !is.na(e$gap), 20L)
  expect_equal(e$hidden, 2L)
  # Both ends survive -- this is middle elision, not head-only truncation.
  expect_equal(e$keep[1L], 1L)
  expect_equal(e$keep[length(e$keep)], 21L)
  expect_equal(e$keep, c(1:10, 13:21))
  expect_equal(e$gap, 11L)
})

test_that("elide_index() keeps both ends of a huge n", {
  e <- elide_index(1e6, 20L)

  expect_equal(length(e$keep), 19L)
  expect_equal(e$keep, c(1:10, 999992:1000000))
  expect_equal(e$gap, 11L)
  expect_equal(e$hidden, 1e6 - 19L)
  expect_type(e$keep, "integer")
  expect_type(e$hidden, "integer")
})

test_that("elide_index() degrades sanely at max_n = 2 and max_n = 1", {
  e2 <- elide_index(10L, 2L)
  expect_equal(e2$keep, 1L) # one lane of data
  expect_equal(e2$gap, 2L) # one lane of "..."
  expect_equal(e2$hidden, 9L)

  e1 <- elide_index(10L, 1L)
  expect_equal(e1$keep, integer(0)) # the single lane is the "..."
  expect_equal(e1$gap, 1L)
  expect_equal(e1$hidden, 10L)
})

test_that("elide_index() never draws more lanes than max_n, and never loses a value", {
  for (n in c(1L, 2L, 3L, 7L, 20L, 21L, 99L, 1000L)) {
    for (m in c(1L, 2L, 3L, 10L, 20L)) {
      e <- elide_index(n, m)
      lanes <- length(e$keep) + !is.na(e$gap)
      expect_lte(lanes, m)
      expect_equal(e$hidden, n - length(e$keep))
      expect_equal(e$keep, sort(unique(e$keep)))
      if (length(e$keep) > 0L) {
        expect_true(all(e$keep >= 1L & e$keep <= n))
      }
    }
  }
})

test_that("elide_index() rejects nonsense", {
  expect_error(elide_index(NA, 5L), "single non-negative")
  expect_error(elide_index(c(1L, 2L), 5L), "single non-negative")
  expect_error(elide_index(-1L, 5L), "single non-negative")
  expect_error(elide_index(5L, 0L), "at least 1")
})

test_that("drawn_pos() steps over the gap lane", {
  e <- elide_index(30L, 20L)
  p <- drawn_pos(e$keep, e$gap)

  expect_equal(length(p), length(e$keep))
  expect_equal(p, c(1:10, 12:20)) # lane 11 is the "..."
  expect_false(e$gap %in% p)
  expect_equal(max(p), 20L)

  # No gap: drawn position is just position.
  expect_equal(drawn_pos(1:5, NA_integer_), 1:5)
})

test_that("elide_note() pluralises and omits empty halves", {
  expect_equal(elide_note(18L, 12L), "# 18 more rows, 12 more columns")
  expect_equal(elide_note(18L, 0L), "# 18 more rows")
  expect_equal(elide_note(0L, 12L), "# 12 more columns")
  expect_equal(elide_note(1L, 1L), "# 1 more row, 1 more column")
  expect_true(is.na(elide_note(0L, 0L)))
})

test_that("elide_note() counts a VECTOR in elements, never rows or columns", {
  # A vector is 1-D, and this is a TEACHING package: the n x 1 (or 1 x n) grid it is
  # drawn on is an artefact of the drawing, not a fact about the data. It elides in
  # exactly one direction -- whichever one it is laid out along -- so both halves
  # read the same way.
  expect_equal(elide_note(26L, 0L, is_vec = TRUE), "# 26 more elements")
  expect_equal(elide_note(0L, 26L, is_vec = TRUE), "# 26 more elements")
  # Pluralised, in the singular too.
  expect_equal(elide_note(1L, 0L, is_vec = TRUE), "# 1 more element")
  expect_equal(elide_note(0L, 1L, is_vec = TRUE), "# 1 more element")
  expect_true(is.na(elide_note(0L, 0L, is_vec = TRUE)))

  # And a matrix or a data frame still has rows and columns.
  expect_equal(elide_note(18L, 12L, is_vec = FALSE), "# 18 more rows, 12 more columns")
  expect_equal(elide_note(1L, 1L, is_vec = FALSE), "# 1 more row, 1 more column")
})

test_that("a painted vector is told it has ELEMENTS, in both layouts", {
  # The bug, exactly as a student met it: `paint_vector(seq_len(40), layout =
  # "horizontal")` read "# 26 more columns". A vector has no columns.
  expect_equal(
    attr(paint_cells(seq_len(40), layout = "horizontal"), "note"),
    "# 26 more elements"
  )
  expect_equal(
    attr(paint_cells(seq_len(40), layout = "vertical"), "note"),
    "# 21 more elements"
  )
  expect_equal(attr(paint_cells(letters), "note"), "# 7 more elements")

  # Neither word appears in a vector's note, in either layout, whatever it holds.
  for (lay in c("vertical", "horizontal")) {
    for (x in list(seq_len(40), letters, as.character(seq_len(30)))) {
      note <- attr(paint_cells(x, layout = lay), "note")
      expect_false(grepl("row|column", note), info = paste(lay, class(x)))
      expect_match(note, "^# [0-9]+ more elements$")
    }
  }

  # Nothing hidden, nothing said.
  expect_true(is.na(attr(paint_cells(1:5), "note")))

  # ... while a matrix and a data frame keep their rows and their columns.
  expect_equal(
    attr(paint_cells(matrix(1:900, nrow = 30)), "note"),
    "# 11 more rows, 16 more columns"
  )
  expect_equal(attr(paint_cells(iris), "note"), "# 141 more rows")
})

# ---------------------------------------------------------------------------
# BUG 4 -- the fmt_group rule, expressed as data
# ---------------------------------------------------------------------------

test_that("bug 4: a matrix is ONE formatting unit", {
  m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  cells <- paint_cells(m)
  v <- cells[cells$kind == "value", ]

  expect_equal(unique(v$fmt_group), 1L)
  expect_equal(v$fmt_group, rep(1L, 6))
})

test_that("bug 4: a vector is ONE formatting unit, in both layouts", {
  v1 <- paint_cells(c(1, 2, 3), layout = "vertical")
  v2 <- paint_cells(c(1, 2, 3), layout = "horizontal")

  expect_equal(unique(v1$fmt_group[v1$kind == "value"]), 1L)
  expect_equal(unique(v2$fmt_group[v2$kind == "value"]), 1L)
})

test_that("bug 4: a data frame is ONE formatting unit PER COLUMN", {
  df <- data.frame(a = c(1, 2), b = c(3.5, 4.5), c = c("x", "y"))
  cells <- paint_cells(df)
  v <- cells[cells$kind == "value", ]

  expect_equal(sort(unique(v$fmt_group)), seq_len(ncol(df)))
  # Column-major: all of column 1, then column 2, then column 3.
  expect_equal(v$fmt_group, rep(1:3, each = 2))
  # And the group really is the column.
  expect_equal(v$fmt_group, v$j)
})

test_that("bug 4: the SAME DATA formats differently as a matrix and as a data frame", {
  # This is the whole point of fmt_group, and the fixed-vs-scientific flip is
  # where you can see it. The outlier lives in column 2 only.
  m <- matrix(c(1, 2, 3, 1e15), nrow = 2)

  # A matrix is one unit: the outlier flips EVERY cell, column 1 included.
  mv <- paint_cells(m)
  mv <- mv[mv$kind == "value", ]
  expect_equal(
    paste0(mv$sig, mv$insig),
    c("1.00e+00", "2.00e+00", "3.00e+00", "1.00e+15")
  )
  # Hard rule: in scientific mode insig is always "" -- the exponent is
  # significant and a naive split would grey it.
  expect_true(all(mv$insig == ""))

  # A data frame is one unit PER COLUMN: column 1 is untouched.
  dv <- paint_cells(as.data.frame(m))
  dv <- dv[dv$kind == "value", ]
  expect_equal(
    paste0(dv$sig, dv$insig),
    c("1", "2", "3.00e+00", "1.00e+15")
  )
})

test_that("tokens are not decimal-padded: alignment is geometric, via head/tail", {
  # pillar pads with spaces to line the decimal points up in a terminal. We do
  # not: `head`/`tail` are the anchors the layout tier lines up on the same x.
  # Layout must take max(w(head)) and max(w(tail)) PER fmt_group.
  cells <- paint_cells(matrix(c(1, 2, 1234.5678, 2), nrow = 2))
  v <- cells[cells$kind == "value", ]

  expect_equal(paste0(v$sig, v$insig), c("1", "2", "1235.", "2"))
  expect_equal(v$head, c("1", "2", "1235", "2"))
  expect_equal(v$tail, c("", "", ".", ""))
  # The rounding marker is retained, and it is grey.
  expect_equal(v$sig[3], "123")
  expect_equal(v$insig[3], "5.")
})

# ---------------------------------------------------------------------------
# ELIDE FIRST, THEN FORMAT
# ---------------------------------------------------------------------------

test_that("a hidden outlier cannot flip the visible cells to scientific", {
  # 25 x 25 of 1s, with one 1e15 buried at [13, 13]. elide_index(25, 20) keeps
  # rows 1:10 and 17:25, so row 13 and column 13 are both hidden.
  m <- matrix(1, nrow = 25, ncol = 25)
  m[13, 13] <- 1e15

  cells <- paint_cells(m, max_rows = 20L, max_cols = 20L)
  v <- cells[cells$kind == "value", ]
  tok <- paste0(v$sig, v$insig)

  expect_false(13L %in% v$i)
  expect_false(13L %in% v$j)
  # The outlier is not drawn, so it does not get a vote.
  expect_true(all(tok == "1"))
  expect_false(any(grepl("e+", tok, fixed = TRUE)))

  # Show it, and it does get a vote: the WHOLE unit flips. Same data, same code,
  # different visible slice.
  all_cells <- paint_cells(m, show_all = TRUE)
  av <- all_cells[all_cells$kind == "value", ]
  atok <- paste0(av$sig, av$insig)
  expect_true(all(grepl("e+", atok, fixed = TRUE)))
  expect_true("1.00e+15" %in% atok)
  expect_equal(sum(atok == "1.00e+00"), 25 * 25 - 1)
})

test_that("elision keeps the ORIGINAL indices, so the last row is still row 30", {
  m <- matrix(seq_len(30 * 3), nrow = 30)
  cells <- paint_cells(m, show_indices = "row")
  v <- cells[cells$kind == "value", ]

  expect_equal(max(v$i), 30L)
  expect_equal(sort(unique(v$i)), c(1:10, 22:30))
  expect_true("[30, ]" %in% cells$sig[cells$kind == "rowlabel"])
  expect_equal(attr(cells, "hidden_rows"), 11L)
  expect_equal(attr(cells, "note"), "# 11 more rows")
})

test_that("the gap is ordinary cell rows: kind = 'ellipsis', fit = FALSE", {
  m <- matrix(1:900, nrow = 30)
  cells <- paint_cells(m)
  e <- cells[cells$kind == "ellipsis", ]

  expect_gt(nrow(e), 0L)
  expect_true(all(!e$fit))
  expect_true(all(e$sig == "..."))
  expect_true(all(e$insig == ""))
  expect_true(all(is.na(e$i)))
  expect_true(all(is.na(e$j)))
  expect_true(all(is.na(e$fmt_group)))
  expect_true(all(is.na(e$fill)))
  expect_true(all(is.na(e$border)))

  # One lane each way, and the four corners of the matrix all survive.
  expect_equal(length(unique(e$row[e$col == max(cells$col)])), 1L)
  expect_equal(attr(cells, "n_row"), 20L)
  expect_equal(attr(cells, "n_col"), 15L)
  expect_equal(attr(cells, "note"), "# 11 more rows, 16 more columns")

  v <- cells[cells$kind == "value", ]
  corners <- paste(v$i, v$j)
  expect_true(all(c("1 1", "1 30", "30 1", "30 30") %in% corners))
})

test_that("show_all = TRUE skips elision entirely", {
  m <- matrix(1:900, nrow = 30)
  cells <- paint_cells(m, show_all = TRUE)

  expect_equal(sum(cells$kind == "value"), 900L)
  expect_equal(sum(cells$kind == "ellipsis"), 0L)
  expect_true(is.na(attr(cells, "note")))
  expect_equal(attr(cells, "n_row"), 30L)
  expect_equal(attr(cells, "n_col"), 30L)
})

test_that("a data frame elides at 10, not 20", {
  df <- as.data.frame(matrix(1:(15 * 12), nrow = 15))
  cells <- paint_cells(df)

  expect_equal(attr(cells, "hidden_rows"), 6L)
  expect_equal(attr(cells, "hidden_cols"), 3L)
  expect_equal(attr(cells, "note"), "# 6 more rows, 3 more columns")
  # 10 value lanes each way, plus the name row and the type row.
  expect_equal(attr(cells, "n_col"), 10L)
  expect_equal(attr(cells, "n_row"), 12L)
})

# ---------------------------------------------------------------------------
# the table itself
# ---------------------------------------------------------------------------

test_that("paint_cells() returns a BARE data frame with the exact columns", {
  cells <- paint_cells(matrix(1:6, nrow = 3))

  expect_identical(class(cells), "data.frame")
  expect_identical(
    names(cells),
    c(
      "i", "j", "row", "col", "fmt_group", "sig", "insig", "head", "tail",
      "ink", "fill", "border", "lwd", "align", "size_rel", "dy_rel", "fit", "kind"
    )
  )
  expect_type(cells$i, "integer")
  expect_type(cells$j, "integer")
  expect_type(cells$row, "integer")
  expect_type(cells$col, "integer")
  expect_type(cells$fmt_group, "integer")
  expect_type(cells$lwd, "double")
  expect_type(cells$size_rel, "double")
  expect_type(cells$dy_rel, "double")
  expect_type(cells$fit, "logical")
  expect_type(cells$kind, "character")
  expect_true(all(cells$kind %in% c(
    "value", "outline", "rowlabel", "collabel",
    "header", "type", "cellindex", "ellipsis"
  )))
})

test_that("the span contract holds on every row of the table", {
  cases <- list(
    matrix(c(1, 1 / 3, 123456.789, 1e15), nrow = 2),
    matrix(letters[1:6], nrow = 2),
    data.frame(a = 1:3, b = c("x", "yy", "zzz"), stringsAsFactors = FALSE),
    c(TRUE, FALSE, NA)
  )
  for (k in seq_along(cases)) {
    cells <- paint_cells(cases[[k]], show_indices = if (is.data.frame(cases[[k]])) "all" else "none")
    expect_equal(
      paste0(cells$sig, cells$insig),
      paste0(cells$head, cells$tail),
      info = k
    )
    expect_false(any(is.na(cells$sig)), info = k)
  }
})

test_that("the cell table is pure ASCII", {
  cells <- paint_cells(matrix(1:900, nrow = 30), show_indices = "all")
  txt <- unlist(cells[vapply(cells, is.character, logical(1))])
  txt <- txt[!is.na(txt)]

  expect_false(any(grepl("[^\x01-\x7f]", txt)))
  expect_true("..." %in% cells$sig)
})

test_that("highlight resolves to a colour string in `fill`", {
  m <- matrix(1:6, nrow = 3)
  h <- highlight_locations(m, rbind(c(1, 1), c(3, 2)))
  cells <- paint_cells(m, highlight_area = h, highlight_color = "lemonchiffon")
  v <- cells[cells$kind == "value", ]

  expect_equal(v$fill[v$i == 1 & v$j == 1], "lemonchiffon")
  expect_equal(v$fill[v$i == 3 & v$j == 2], "lemonchiffon")
  expect_equal(sum(v$fill == "lemonchiffon"), 2L)
  expect_equal(sum(v$fill == "white"), 4L)

  # NULL, a bare FALSE, and an NA all mean "not highlighted".
  expect_true(all(paint_cells(m)$fill[paint_cells(m)$kind == "value"] == "white"))
  expect_true(all(paint_cells(m, highlight_area = FALSE)$fill[
    paint_cells(m, highlight_area = FALSE)$kind == "value"
  ] == "white"))

  hna <- h
  hna[2, 1] <- NA
  na_cells <- paint_cells(m, highlight_area = hna)
  expect_equal(sum(na_cells$fill == "lemonchiffon", na.rm = TRUE), 2L)
})

test_that("a wrongly shaped highlight_area errors and reports the ACTUAL shape", {
  m <- matrix(1:6, nrow = 3)
  expect_error(
    paint_cells(m, highlight_area = matrix(TRUE, 2, 2)),
    "must be a 3 by 2 logical matrix to match the data, but it is 2 by 2",
    fixed = TRUE
  )
  expect_error(
    paint_cells(m, highlight_area = c(TRUE, FALSE)),
    "but it is a length-2 vector",
    fixed = TRUE
  )
  expect_error(paint_cells(m, highlight_area = 1:6), "must be a logical")
})

test_that("index lanes are cells like any other", {
  m <- matrix(1:6, nrow = 3)
  cells <- paint_cells(m, show_indices = "all")

  expect_equal(
    cells$sig[cells$kind == "rowlabel"],
    c("[1, ]", "[2, ]", "[3, ]")
  )
  expect_equal(cells$sig[cells$kind == "collabel"], c("[, 1]", "[, 2]"))
  expect_true("[3, 2]" %in% cells$sig[cells$kind == "cellindex"])

  # The lanes take drawn space: one row on top, one column on the left.
  expect_equal(attr(cells, "n_row"), 4L)
  expect_equal(attr(cells, "n_col"), 3L)
  expect_equal(min(cells$row[cells$kind == "value"]), 2L)
  expect_equal(min(cells$col[cells$kind == "value"]), 2L)

  # A cell index shares its value's CELL -- the same drawn (row, col) -- and is
  # then nudged off its value by `dy_rel`. Both halves matter: the shared cell is
  # what puts it in the right box, and the nudge is the only thing that stops it
  # being stamped on top of the number.
  ci <- cells[cells$kind == "cellindex", ]
  vv <- cells[cells$kind == "value", ]
  expect_equal(ci$row, vv$row)
  expect_equal(ci$col, vv$col)
  expect_true(all(ci$size_rel < 1))
  expect_true(all(ci$dy_rel < 0))
  expect_equal(unique(ci$dy_rel), cellindex_dy)

  # ... and it is the ONLY kind that is nudged. Everything else is centred.
  expect_true(all(cells$dy_rel[cells$kind != "cellindex"] == 0))
})

test_that("a data frame emits a header row and a type row", {
  df <- data.frame(
    n = c(1.5, 2.5),
    i = 1:2,
    s = c("a", "b"),
    f = factor(c("u", "v")),
    l = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  cells <- paint_cells(df)

  expect_equal(cells$sig[cells$kind == "header"], c("n", "i", "s", "f", "l"))
  expect_equal(
    cells$sig[cells$kind == "type"],
    c("<dbl>", "<int>", "<chr>", "<fct>", "<lgl>")
  )
  # Names on top, types under them, values under those.
  expect_true(all(cells$row[cells$kind == "header"] == 1L))
  expect_true(all(cells$row[cells$kind == "type"] == 2L))
  expect_true(all(cells$row[cells$kind == "value"] >= 3L))
  expect_equal(cells$j[cells$kind == "header"], 1:5)

  # A header is text: a numeric column's header aligns right, not "decimal".
  expect_equal(cells$align[cells$kind == "header"], c("right", "right", "left", "left", "right"))

  # Types can be switched off without disturbing the names.
  no_types <- paint_cells(df, show_types = FALSE)
  expect_equal(sum(no_types$kind == "type"), 0L)
  expect_true(all(no_types$row[no_types$kind == "header"] == 1L))
  expect_true(all(no_types$row[no_types$kind == "value"] >= 2L))

  bare <- paint_cells(df, show_names = FALSE, show_types = FALSE)
  expect_equal(attr(bare, "n_row"), 2L)
  expect_true(all(bare$row[bare$kind == "value"] >= 1L))
})

test_that("a long string truncates with an ASCII ellipsis, and a list column is a placeholder", {
  df <- data.frame(s = "supercalifragilistic", stringsAsFactors = FALSE)
  df$lst <- list(1:3)
  cells <- paint_cells(df)
  v <- cells[cells$kind == "value", ]

  expect_equal(v$sig[v$j == 1], "supercali...")
  expect_equal(nchar(v$sig[v$j == 1]), 12L)
  expect_equal(v$sig[v$j == 2], "<list>")
  expect_equal(v$ink[v$j == 2], "grey50")
  expect_equal(cells$sig[cells$kind == "type"], c("<chr>", "<list>"))
})

test_that("an AsIs list column is still a <list> placeholder", {
  # data.frame(x = I(list(...))) is the ONLY way to build a list column with
  # data.frame(), and it gives the column class "AsIs". paint_format() then
  # dispatches to .default and renders "1, 2, 3". The cell builder strips the
  # wrapper so dispatch lands on .list.
  df <- data.frame(s = "x", lst = I(list(1:3)), stringsAsFactors = FALSE)
  expect_equal(class(df$lst), "AsIs")

  cells <- paint_cells(df)
  v <- cells[cells$kind == "value", ]
  expect_equal(v$sig[v$j == 2], "<list>")
  expect_equal(v$ink[v$j == 2], "grey50")

  # An AsIs atomic vector still reaches its own method.
  a <- paint_cells(I(c(1.5, 2.5)))
  expect_equal(a$sig[a$kind == "value"], c("1.5", "2.5"))
})

test_that("a vector paints as an n x 1 or a 1 x n grid", {
  v <- c(10, 20, 30)

  vert <- paint_cells(v, layout = "vertical", show_indices = "outside")
  vv <- vert[vert$kind == "value", ]
  expect_equal(vv$i, 1:3)
  expect_equal(vv$j, rep(1L, 3))
  expect_equal(vv$row, 1:3)
  expect_equal(vv$col, rep(2L, 3)) # column 1 is the label gutter
  expect_equal(vert$sig[vert$kind == "rowlabel"], c("[1]", "[2]", "[3]"))
  expect_equal(sum(vert$kind == "collabel"), 0L)

  horiz <- paint_cells(v, layout = "horizontal", show_indices = "outside")
  hv <- horiz[horiz$kind == "value", ]
  expect_equal(hv$i, rep(1L, 3))
  expect_equal(hv$j, 1:3)
  expect_equal(hv$row, rep(2L, 3)) # row 1 is the label lane
  expect_equal(hv$col, 1:3)
  expect_equal(horiz$sig[horiz$kind == "collabel"], c("[1]", "[2]", "[3]"))

  # A long horizontal vector elides along the lane it is drawn on -- but the LANE is
  # the drawing's business. What the note tells the student is elements: the grid is
  # 1 x 15 because that is how a 1-D thing was laid across the page, and calling
  # those 15 boxes "columns" is exactly the miseducation this package exists to undo.
  long <- paint_cells(seq_len(60), layout = "horizontal")
  expect_equal(attr(long, "note"), "# 46 more elements")
  expect_equal(attr(long, "n_col"), 15L)
  expect_equal(attr(long, "n_row"), 1L)
})

test_that("character and logical data reach the table as themselves (bug 1, downstream)", {
  cells <- paint_cells(matrix(c("a", "b", NA, "d"), nrow = 2))
  v <- cells[cells$kind == "value", ]

  expect_equal(v$sig, c("a", "b", "NA", "d"))
  expect_equal(v$ink, c("black", "black", "red", "black"))
  expect_false(any(grepl("Unknown", unlist(cells), fixed = TRUE)))

  lg <- paint_cells(c(TRUE, FALSE, NA))
  lv <- lg[lg$kind == "value", ]
  expect_equal(lv$sig, c("TRUE", "FALSE", "NA"))
  expect_equal(lv$align, rep("right", 3))
})

test_that("the outline is one cell, and its box is the value block", {
  m <- matrix(1:900, nrow = 30)
  cells <- paint_cells(m, show_indices = "all")

  o <- cells[cells$kind == "outline", ]
  expect_equal(nrow(o), 1L)
  expect_false(o$fit)
  expect_equal(o$border, "black")
  expect_true(is.na(o$fill))

  box <- outline_box(cells)
  # The label lanes are OUTSIDE the box: they are lane 1 in each direction.
  expect_equal(box$row0, 2L)
  expect_equal(box$col0, 2L)
  expect_equal(box$row1, attr(cells, "n_row"))
  expect_equal(box$col1, attr(cells, "n_col"))
})

test_that("the outline's line weight is DATA, and it is the only heavy stroke", {
  # What makes the outline heavy is a number on the cell table, not a rule inside a
  # renderer. It was a rule inside a renderer -- one renderer -- and the other one
  # never learned it, so ggplot2 drew no outline at all for a whole release.
  for (x in list(matrix(1:9, nrow = 3), iris, letters, matrix(1:900, nrow = 30))) {
    cells <- paint_cells(x)
    o <- cells[cells$kind == "outline", ]

    expect_equal(nrow(o), 1L)
    expect_equal(o$lwd, outline_lwd)
    expect_gt(o$lwd, 1)
    # Every other cell is stroked like an ordinary cell border.
    expect_true(all(cells$lwd[cells$kind != "outline"] == 1))
  }
})

test_that("boxed_cells() draws the heaviest stroke LAST, and names no kind", {
  cells <- paint_cells(matrix(1:9, nrow = 3), show_indices = "all")
  b <- boxed_cells(cells)

  # Only the cells that actually get a rectangle.
  expect_equal(nrow(b), sum(!is.na(cells$fill) | !is.na(cells$border)))
  expect_false(any(is.na(b$fill) & is.na(b$border)))

  # The heavy stroke is last, so the cell borders it shares its edges with cannot
  # paint over it. The rule is `order(lwd)` -- a fact about the data -- and the
  # outline lands there because it is heavy, not because it is called "outline".
  expect_equal(b$kind[[nrow(b)]], "outline")
  expect_equal(b$lwd[[nrow(b)]], max(b$lwd))
  expect_true(all(b$lwd[-nrow(b)] == 1))
  # Stable: everything else keeps its table order.
  rest <- b[-nrow(b), ]
  expect_equal(rest$kind, cells$kind[cells$kind == "value"])
})

test_that("paint_cells() refuses the impossible", {
  expect_error(paint_cells(numeric(0)), "empty data structure")
  expect_error(paint_cells(matrix(nrow = 0, ncol = 3)), "empty data structure")
  expect_error(paint_cells(data.frame()), "empty data structure")
  expect_error(paint_cells(array(1:8, c(2, 2, 2))), "one- and two-dimensional")
  # The hard ceiling holds even under show_all.
  expect_error(
    paint_cells(matrix(0, nrow = 1000, ncol = 101), show_all = TRUE),
    "more than the 100000 cells"
  )
  expect_error(paint_cells(matrix(1:6, 3), show_indices = "banana"), "should be one of")
})

test_that("the numeric edge cases all build a table", {
  cases <- list(
    one = 5,
    zero = c(0, -0, 1),
    xmax = c(.Machine$double.xmax, 0),
    nonfinite = c(NA, NaN, Inf, -Inf),
    all_na = matrix(NA_real_, 2, 2),
    fct = factor(c("a", "b", "a")),
    date = as.Date("2024-01-01") + 1:3,
    cpl = complex(real = 1:2, imaginary = 1:2),
    iris = iris
  )
  for (nm in names(cases)) {
    cells <- paint_cells(cases[[nm]])
    expect_gt(nrow(cells), 0L)
    expect_false(any(is.na(cells$sig)), info = nm)
    expect_equal(
      paste0(cells$sig, cells$insig),
      paste0(cells$head, cells$tail),
      info = nm
    )
  }

  # -0 is the same number as 0 and must not render as "-0".
  z <- paint_cells(c(0, -0, 1))
  expect_equal(z$sig[z$kind == "value"], c("0", "0", "1"))

  # An all-non-finite unit does not flip to scientific and does not crash.
  nf <- paint_cells(c(NA, NaN, Inf, -Inf))
  nf <- nf[nf$kind == "value", ]
  expect_equal(nf$sig, c("NA", "NaN", "Inf", "-Inf"))
  expect_equal(nf$ink, c("red", "blue", "blue", "blue"))

  # iris: 150 rows elide to 10, its 5 columns do not.
  ic <- paint_cells(iris)
  expect_equal(attr(ic, "note"), "# 141 more rows")
  expect_equal(ic$sig[ic$kind == "type"], c(rep("<dbl>", 4), "<fct>"))
})

test_that("max_rows = 1 degrades to a lone '...' instead of crashing", {
  cells <- paint_cells(matrix(1:6, nrow = 3), max_rows = 1L)

  expect_equal(sum(cells$kind == "value"), 0L)
  expect_equal(sum(cells$kind == "ellipsis"), 2L)
  expect_equal(attr(cells, "n_row"), 1L)
  expect_equal(attr(cells, "note"), "# 3 more rows")
})

# ---------------------------------------------------------------------------
# column_widths()
# ---------------------------------------------------------------------------

test_that("column_widths() gives a matrix uniform, square-ish columns", {
  m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  cells <- paint_cells(m)
  w <- column_widths(cells)

  expect_length(w, 2L)
  expect_equal(w, c(1, 1)) # single digits: exactly square
  expect_true(all(w == w[1]))
})

test_that("column_widths() unifies a matrix's columns even when one holds a wide value", {
  # Column 2 has the long token; a matrix is one formatting unit, so BOTH columns
  # must widen or the cells stop being a grid.
  m <- matrix(c(1, 2, 123456.789, 4), nrow = 2)
  cells <- paint_cells(m)
  w <- column_widths(cells)

  expect_equal(length(unique(w)), 1L)
  expect_gt(w[1], 1)
})

test_that("column_widths() lets a data frame's columns differ", {
  df <- data.frame(
    a = c(1, 2),
    supercalifrag = c(3, 4),
    stringsAsFactors = FALSE
  )
  cells <- paint_cells(df)
  w <- column_widths(cells)

  expect_length(w, 2L)
  expect_gt(w[2], w[1])
  # The header is what makes column 2 wide: 13 chars truncated to 12.
  expect_equal(cells$sig[cells$kind == "header"], c("a", "supercali..."))
})

test_that("column_widths() covers every drawn column, gutters and gap lanes included", {
  m <- matrix(1:900, nrow = 30)
  cells <- paint_cells(m, show_indices = "all")
  w <- column_widths(cells)

  expect_length(w, attr(cells, "n_col"))
  expect_true(all(w >= 1))
  expect_true(all(is.finite(w)))

  # Drawn column 1 is the row-label gutter. The "..." lane is the only other
  # column that holds no values -- note the ellipsis ROW crosses every column,
  # so the gap COLUMN is the one with no value cells at all.
  value_cols <- sort(unique(cells$col[cells$kind == "value"]))
  gap_col <- setdiff(seq_len(attr(cells, "n_col")), c(1L, value_cols))
  expect_length(gap_col, 1L)
  expect_equal(length(unique(round(w[value_cols], 10))), 1L)

  # The gap lane is narrower: it only ever holds "...".
  expect_lt(w[gap_col], w[value_cols[1]])
  expect_gte(w[gap_col], 1)
})

# ---------------------------------------------------------------------------
# snapshots of the bare table
# ---------------------------------------------------------------------------

test_that("the cell table snapshots: a numeric matrix with every index lane", {
  m <- matrix(c(1, 1 / 3, 123456.789, 20, -1, NA), nrow = 3)
  expect_snapshot(paint_cells(m, show_indices = "all"))
})

test_that("the cell table snapshots: a data frame with names and types", {
  df <- data.frame(
    n = c(1.5, 22.25),
    s = c("a", "bb"),
    l = c(TRUE, NA),
    stringsAsFactors = FALSE
  )
  expect_snapshot(paint_cells(df))
})

test_that("the cell table snapshots: an elided matrix", {
  m <- matrix(seq_len(30 * 30), nrow = 30)
  cells <- paint_cells(m, max_rows = 4L, max_cols = 4L)
  expect_snapshot(cells)
  expect_snapshot(column_widths(cells))
})

test_that("the cell table snapshots: a highlighted vector", {
  expect_snapshot(
    paint_cells(
      c(3, NA, -1, NaN, Inf),
      highlight_area = highlight_locations(c(3, NA, -1, NaN, Inf), c(2, 4)),
      show_indices = "outside"
    )
  )
})
