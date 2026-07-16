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
      "i", "j", "row", "col", "row_end", "col_end", "fmt_group", "sig", "insig",
      "head", "tail", "ink", "fill", "border", "lwd", "align", "fontface",
      "size_rel", "dy_rel", "fit", "radius", "kind"
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
  expect_type(cells$radius, "double")
  expect_type(cells$kind, "character")
  expect_true(all(cells$kind %in% c(
    "value", "outline", "rowlabel", "collabel",
    "header", "type", "cellindex", "ellipsis", "headerband"
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

test_that("a control character in a dimname never reaches a label lane", {
  # A dimname with a newline would break the row lane exactly as a value does:
  # the renderer would stack it onto a second line. It flows through the same
  # truncate_chr() choke point, so the label lane is clean.
  m <- matrix(1:4, nrow = 2, dimnames = list(c("a\nb", "c"), c("d\te", "f")))
  cells <- paint_cells(m, show_dimnames = "all")
  labels <- cells$sig[cells$kind %in% c("rowlabel", "collabel")]
  # The accessor is built from the SANITISED name, not the raw one: the newline is
  # a space before the quotes go on, so `["a b", ]` is clean end to end.
  expect_true('["a b", ]' %in% labels)
  expect_true('[, "d e"]' %in% labels)
  expect_false(any(grepl("[[:cntrl:]]", cells$sig)))
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

  # A label names the WHOLE column, so it is centred over it -- it does not
  # inherit the values' alignment. Both lanes, every column type.
  expect_equal(cells$align[cells$kind == "header"], rep("center", 5L))
  expect_equal(cells$align[cells$kind == "type"], rep("center", 5L))

  # ... and the VALUES are untouched: the decimal anchoring survives.
  v <- cells[cells$kind == "value", ]
  expect_equal(unique(v$align[v$j == 1L]), "decimal") # dbl
  expect_equal(unique(v$align[v$j == 2L]), "decimal") # int
  expect_equal(unique(v$align[v$j == 3L]), "left") # chr
  expect_equal(unique(v$align[v$j == 4L]), "left") # fct
  expect_equal(unique(v$align[v$j == 5L]), "right") # lgl

  # Types can be switched off without disturbing the names.
  no_types <- paint_cells(df, show_types = FALSE)
  expect_equal(sum(no_types$kind == "type"), 0L)
  expect_true(all(no_types$row[no_types$kind == "header"] == 1L))
  expect_true(all(no_types$row[no_types$kind == "value"] >= 2L))

  bare <- paint_cells(df, show_names = FALSE, show_types = FALSE)
  expect_equal(attr(bare, "n_row"), 2L)
  expect_true(all(bare$row[bare$kind == "value"] >= 1L))
})

test_that("name_align and type_align set their own lane and nothing else", {
  df <- data.frame(
    n = c(1.5, 2.5),
    i = 1:2,
    s = c("a", "b"),
    f = factor(c("u", "v")),
    l = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  # What the values must still be doing, whatever the labels are told.
  value_align <- c("decimal", "decimal", "left", "left", "right")
  values_of <- function(cells) {
    v <- cells[cells$kind == "value", ]
    vapply(1:5, function(jj) unique(v$align[v$j == jj]), character(1))
  }

  for (a in c("left", "center", "right")) {
    # One lane at a time: the OTHER lane must stay at its default.
    only_name <- paint_cells(df, name_align = a)
    expect_equal(only_name$align[only_name$kind == "header"], rep(a, 5L))
    expect_equal(only_name$align[only_name$kind == "type"], rep("center", 5L))
    expect_equal(values_of(only_name), value_align)

    only_type <- paint_cells(df, type_align = a)
    expect_equal(only_type$align[only_type$kind == "type"], rep(a, 5L))
    expect_equal(only_type$align[only_type$kind == "header"], rep("center", 5L))
    expect_equal(values_of(only_type), value_align)
  }

  # The two are genuinely independent: opposite ends at once.
  both <- paint_cells(df, name_align = "right", type_align = "left")
  expect_equal(both$align[both$kind == "header"], rep("right", 5L))
  expect_equal(both$align[both$kind == "type"], rep("left", 5L))
  expect_equal(values_of(both), value_align)
})

test_that("an invalid name_align or type_align is an error", {
  df <- data.frame(n = 1:2)
  expect_error(paint_cells(df, name_align = "middle"))
  expect_error(paint_cells(df, type_align = "middle"))
  # "decimal" is a VALUE alignment. A label is text; it is not on offer.
  expect_error(paint_cells(df, name_align = "decimal"))
  expect_error(paint_cells(df, type_align = "decimal"))
})

test_that("a long string truncates with an ASCII ellipsis, and a list column says what is in it", {
  df <- data.frame(s = "supercalifragilistic", stringsAsFactors = FALSE)
  df$lst <- list(1:3)
  cells <- paint_cells(df)
  v <- cells[cells$kind == "value", ]

  expect_equal(v$sig[v$j == 1], "supercali...")
  expect_equal(nchar(v$sig[v$j == 1]), 12L)
  # PER ELEMENT, not one constant over the column. It used to say `<list>` in every
  # cell of every list column -- the same six characters whatever was in there --
  # which told the reader nothing at all, not even that the entries differed. The
  # TYPE lane still says `<list>`, because the COLUMN genuinely is a list.
  expect_equal(v$sig[v$j == 2], "<int [3]>")
  expect_equal(v$ink[v$j == 2], "grey50")
  expect_equal(cells$sig[cells$kind == "type"], c("<chr>", "<list>"))
})

test_that("an AsIs list column still reaches paint_format.list()", {
  # data.frame(x = I(list(...))) is the ONLY way to build a list column with
  # data.frame(), and it gives the column class "AsIs". paint_format() then
  # dispatches to .default and renders "1, 2, 3". The cell builder strips the
  # wrapper so dispatch lands on .list.
  df <- data.frame(s = "x", lst = I(list(1:3)), stringsAsFactors = FALSE)
  expect_equal(class(df$lst), "AsIs")

  cells <- paint_cells(df)
  v <- cells[cells$kind == "value", ]
  expect_equal(v$sig[v$j == 2], "<int [3]>")
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

  # The outline CARRIES its box, in `row_end`/`col_end`. It used to carry only its
  # top-left, and `paint_resolve()` re-derived the bottom-right with a max() over
  # every value cell in the table -- an arithmetic with one right answer only while
  # there is one block. An array draws several.
  #
  # The label lanes are OUTSIDE the box: they are lane 1 in each direction.
  expect_equal(o$row, 2L)
  expect_equal(o$col, 2L)
  expect_equal(o$row_end, attr(cells, "n_row"))
  expect_equal(o$col_end, attr(cells, "n_col"))
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
  # A 3-D array is DRAWN now (`array_cells()`), so what stays refused is rank ONE:
  # it carries a `dim`, so it is not a vector, and it has no second axis, so it is
  # not a grid. It is the one shape between the two painters.
  expect_error(paint_cells(array(1:3, 3L)), "one- and two-dimensional")
  expect_silent(paint_cells(array(1:8, c(2, 2, 2))))
  # The hard ceiling holds even under show_all.
  expect_error(
    paint_cells(matrix(0, nrow = 1000, ncol = 101), show_all = TRUE),
    "more than the 100000 cells"
  )
  expect_error(paint_cells(matrix(1:6, 3), show_indices = "banana"), "must be one or more of")
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

# ---------------------------------------------------------------------------
# names and dimnames
#
# The picture must never be less informative than `print()`. A named vector, a
# dimnamed matrix and a row-named data frame all carry labels that the cell table
# used to throw away.
# ---------------------------------------------------------------------------

test_that("normalize_names() turns a true NA into a token, at construction", {
  v <- c(1, 2, 3)
  names(v) <- c("a", NA, "")

  n <- normalize_names(names(v))
  expect_equal(n, c("a", "<NA>", ""))
  expect_false(anyNA(n))
  expect_type(n, "character")

  expect_null(normalize_names(NULL))
  expect_equal(normalize_names(factor(c("x", "y"))), c("x", "y"))
})

test_that("check_lanes() validates a lane vector and names the argument", {
  expect_equal(check_lanes(c("row", "column"), c("none", "row", "column", "all"), "show_dimnames"),
               c("row", "column"))
  expect_error(
    check_lanes("banana", c("none", "row", "column", "all"), "show_dimnames"),
    "`show_dimnames` must be one or more of",
    fixed = TRUE
  )
  expect_error(check_lanes(character(0), c("none", "all"), "show_dimnames"), "empty")
  expect_error(check_lanes(NA_character_, c("none", "all"), "show_dimnames"), "must be one or more of")
  expect_error(check_lanes(1L, c("none", "all"), "show_dimnames"), "must be one or more of")

  # The existing argument is now one caller of the general one.
  expect_equal(check_show_indices(c("row", "column")), c("row", "column"))
  expect_error(check_show_indices("banana"), "`show_indices` must be one or more of", fixed = TRUE)
})

test_that("axis_names() reads the accessor, and refuses a data frame's column axis", {
  m <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_equal(axis_names(m, "row"), c("r1", "r2"))
  expect_equal(axis_names(m, "column"), c("c1", "c2", "c3"))
  expect_null(axis_names(matrix(1:6, nrow = 2), "row"))
  expect_null(axis_names(matrix(1:6, nrow = 2), "column"))

  # Half-dimnamed is the daily case (model matrices, table()).
  half <- matrix(1:4, 2, dimnames = list(NULL, c("a", "b")))
  expect_null(axis_names(half, "row"))
  expect_equal(axis_names(half, "column"), c("a", "b"))

  # A data frame's column names are ALREADY drawn, by the header lane. Handing
  # them back here would draw them a second time.
  expect_null(axis_names(head(mtcars, 2), "column"))
  expect_equal(axis_names(head(mtcars, 2), "row"), c("Mazda RX4", "Mazda RX4 Wag"))

  expect_equal(axis_names(c(a = 1, b = 2), "row"), c("a", "b"))
  expect_null(axis_names(c(1, 2), "row"))
})

test_that("axis_names() does not go out of bounds on a 1-D dimnamed array", {
  # `dimnames()` on a 1-D array is a list of length 1, so the hard-coded
  # `[[2L]]` a plain matrix gets away with is out of bounds here. Unreachable
  # through paint_cells() today (both callers are guarded upstream), but the
  # helper is meant to be reused, so it must degrade sanely on its own.
  a <- array(1:3, 3, dimnames = list(c("x", "y", "z")))
  expect_equal(length(dimnames(a)), 1L)
  expect_equal(axis_names(a, "row"), c("x", "y", "z"))
  expect_null(axis_names(a, "column"))
})

test_that("a named vector draws its names, in the lane its layout gives it", {
  v <- c(alpha = 1, beta = 2, gamma = 3)

  # The name lane is the ACCESSOR: `v["alpha"]` is how you reach the cell, so the
  # label reads `["alpha"]` -- the same bracket the positional `[1]` uses, with the
  # name quoted.
  vert <- paint_cells(v)
  expect_equal(vert$sig[vert$kind == "rowlabel"], c('["alpha"]', '["beta"]', '["gamma"]'))
  expect_equal(sum(vert$kind == "collabel"), 0L)
  expect_equal(attr(vert, "n_col"), 2L) # the name gutter

  horiz <- paint_cells(v, layout = "horizontal")
  expect_equal(horiz$sig[horiz$kind == "collabel"], c('["alpha"]', '["beta"]', '["gamma"]'))
  expect_equal(sum(horiz$kind == "rowlabel"), 0L)

  # An unnamed vector falls back to no lane at all, exactly as before.
  expect_equal(sum(paint_cells(c(1, 2, 3))$kind %in% c("rowlabel", "collabel")), 0L)

  # And the names can be switched off.
  off <- paint_cells(v, show_names = FALSE)
  expect_equal(sum(off$kind %in% c("rowlabel", "collabel")), 0L)
})

test_that("a dimnamed matrix draws its dimnames", {
  m <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))

  # A dimname is drawn as the subscript that reaches it: `m["r1", ]` down the
  # gutter, `m[, "c1"]` across the header.
  cells <- paint_cells(m)
  expect_equal(cells$sig[cells$kind == "rowlabel"], c('["r1", ]', '["r2", ]'))
  expect_equal(cells$sig[cells$kind == "collabel"], c('[, "c1"]', '[, "c2"]', '[, "c3"]'))

  # One axis at a time, because dimnames() is a list with one slot per axis.
  only_row <- paint_cells(m, show_dimnames = "row")
  expect_equal(only_row$sig[only_row$kind == "rowlabel"], c('["r1", ]', '["r2", ]'))
  expect_equal(sum(only_row$kind == "collabel"), 0L)

  only_col <- paint_cells(m, show_dimnames = "column")
  expect_equal(only_col$sig[only_col$kind == "collabel"], c('[, "c1"]', '[, "c2"]', '[, "c3"]'))
  expect_equal(sum(only_col$kind == "rowlabel"), 0L)

  none <- paint_cells(m, show_dimnames = "none")
  expect_equal(sum(none$kind %in% c("rowlabel", "collabel")), 0L)

  # An unnamed matrix draws no lane, as before.
  bare <- paint_cells(matrix(1:6, nrow = 2))
  expect_equal(sum(bare$kind %in% c("rowlabel", "collabel")), 0L)

  expect_error(paint_cells(m, show_dimnames = "banana"), "`show_dimnames` must be one or more of",
               fixed = TRUE)
})

test_that("show_indices OVERRIDES names on the axis it names", {
  m <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))

  # The user typed it, so it wins -- on that axis and no other. The axis it does
  # NOT name keeps its dimname, now drawn as the accessor.
  r <- paint_cells(m, show_indices = "row")
  expect_equal(r$sig[r$kind == "rowlabel"], c("[1, ]", "[2, ]"))
  expect_equal(r$sig[r$kind == "collabel"], c('[, "c1"]', '[, "c2"]', '[, "c3"]'))

  cc <- paint_cells(m, show_indices = "column")
  expect_equal(cc$sig[cc$kind == "collabel"], c("[, 1]", "[, 2]", "[, 3]"))
  expect_equal(cc$sig[cc$kind == "rowlabel"], c('["r1", ]', '["r2", ]'))

  a <- paint_cells(m, show_indices = "all")
  expect_equal(a$sig[a$kind == "rowlabel"], c("[1, ]", "[2, ]"))
  expect_equal(a$sig[a$kind == "collabel"], c("[, 1]", "[, 2]", "[, 3]"))

  v <- c(alpha = 1, beta = 2)
  o <- paint_cells(v, show_indices = "outside")
  expect_equal(o$sig[o$kind == "rowlabel"], c("[1]", "[2]"))
})

test_that("an in-cell index COMPOSES with names -- they are different lanes", {
  m <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))

  cells <- paint_cells(m, show_indices = "cell")
  expect_equal(cells$sig[cells$kind == "rowlabel"], c('["r1", ]', '["r2", ]'))
  expect_equal(cells$sig[cells$kind == "collabel"], c('[, "c1"]', '[, "c2"]', '[, "c3"]'))
  # A dimnamed matrix's cell index is the named accessor, one per axis.
  expect_true('["r2", "c3"]' %in% cells$sig[cells$kind == "cellindex"])

  # A VECTOR keeps its in-cell index POSITIONAL: the name lane already shows the
  # full accessor `["alpha"]`, so the in-cell `[1]` adds the complementary
  # position. The two lanes stay different things -- which is the point.
  v <- c(alpha = 1, beta = 2)
  vc <- paint_cells(v, show_indices = "inside")
  expect_equal(vc$sig[vc$kind == "rowlabel"], c('["alpha"]', '["beta"]'))
  expect_equal(vc$sig[vc$kind == "cellindex"], c("[1]", "[2]"))
})

test_that("a half-dimnamed matrix indexes each axis on its own", {
  # The gate is whether the AXIS HAS names, answered per axis: a name where there
  # is one, a number where there is not. `show_dimnames` draws the margins; it does
  # not change what the accessor under the cell says.
  rows_only <- matrix(1:6, 2, dimnames = list(c("r1", "r2"), NULL))
  rc <- paint_cells(rows_only, show_indices = "cell")
  expect_equal(rc$sig[rc$kind == "rowlabel"], c('["r1", ]', '["r2", ]'))
  expect_equal(sum(rc$kind == "collabel"), 0L) # no column names to draw
  expect_true('["r1", 2]' %in% rc$sig[rc$kind == "cellindex"])
  expect_true('["r2", 3]' %in% rc$sig[rc$kind == "cellindex"])

  cols_only <- matrix(1:6, 2, dimnames = list(NULL, c("alpha", "beta", "gamma")))
  cc <- paint_cells(cols_only, show_indices = "cell")
  expect_equal(cc$sig[cc$kind == "collabel"], c('[, "alpha"]', '[, "beta"]', '[, "gamma"]'))
  expect_equal(sum(cc$kind == "rowlabel"), 0L)
  expect_true('[1, "alpha"]' %in% cc$sig[cc$kind == "cellindex"])
  expect_true('[2, "gamma"]' %in% cc$sig[cc$kind == "cellindex"])

  # The cell index names the axis even when its margin is hidden -- the accessor is
  # the accessor whether or not the gutter that echoes it is drawn.
  hidden <- paint_cells(
    matrix(1:6, 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))),
    show_indices = "cell", show_dimnames = "none"
  )
  expect_equal(sum(hidden$kind %in% c("rowlabel", "collabel")), 0L)
  expect_true('["r1", "c1"]' %in% hidden$sig[hidden$kind == "cellindex"])
})

# THE CONTRACT: a cell index is THE EXPRESSION YOU TYPE TO REACH THE CELL, and it
# must return THE VALUE IN THAT CELL. A named subscript only reaches the cell when
# the name is drawn in FULL -- a TRUNCATED name is a wrong subscript
# (`mtcars["Mazda...", 1]` is `NA`, not `21`), so a too-long name falls back to the
# position, which is always exact. This helper evaluates every cell index against
# the object and demands it returns the value at the cell's own `(i, j)` -- the
# value the picture drew there, since the value cell was formatted from that
# position. Rank-2 only: `(i, j)` fully locates a matrix or data frame cell.
expect_cell_index_hits_value <- function(x, ..., label = NULL) {
  cells <- paint_cells(x, ...)
  ci <- cells[cells$kind == "cellindex", , drop = FALSE]
  expect_gt(nrow(ci), 0L)
  env <- new.env(parent = baseenv())
  assign("x", x, envir = env)
  for (r in seq_len(nrow(ci))) {
    lab <- ci$sig[[r]]
    got <- tryCatch(
      eval(parse(text = paste0("x", lab)), envir = env),
      error = function(e) structure(conditionMessage(e), class = "paintr_label_error")
    )
    expect_false(
      inherits(got, "paintr_label_error"),
      info = sprintf("%s: `x%s` ERRORED: %s", label, lab, as.character(got))
    )
    want <- x[ci$i[[r]], ci$j[[r]]]
    expect_equal(
      unname(got), unname(want),
      info = sprintf("%s: `x%s` did not return the cell's value", label, lab)
    )
  }
}

test_that("EVERY matrix / data frame cell index runs AND returns the cell's value", {
  # The flagship regression: `mtcars`'s row names are longer than the 8-char name
  # budget, so a named row subscript would truncate to `["Mazda...", 1]` and return
  # `NA`. The row axis must fall back to the position.
  expect_cell_index_hits_value(head(mtcars, 4), show_indices = "cell", label = "mtcars cell")
  expect_cell_index_hits_value(head(mtcars, 4), show_indices = "all", label = "mtcars all")

  # A matrix with long dimnames on BOTH axes: a named subscript would error
  # ("subscript out of bounds"); both axes fall back to the position.
  long_mat <- matrix(1:6, 2, 3, dimnames = list(
    c("Sepal.Length", "Sepal.Width"),
    c("Column.Alpha", "Column.Beta", "Column.Gamma")
  ))
  expect_cell_index_hits_value(long_mat, show_indices = "cell", label = "long matrix cell")
  expect_cell_index_hits_value(long_mat, show_indices = "all", label = "long matrix all")

  # Short names still index BY NAME, and the named accessor returns the cell.
  short_mat <- matrix(1:6, 2, 3, dimnames = list(c("r1", "r2"), c("alpha", "beta", "gamma")))
  expect_cell_index_hits_value(short_mat, show_indices = "cell", label = "short matrix cell")

  # Mixed: a long axis and a short axis in the SAME structure -- one positional,
  # one named -- each still exact.
  mix_mat <- matrix(1:6, 2, 3, dimnames = list(c("Sepal.Length", "Sepal.Width"), c("a", "b", "c")))
  expect_cell_index_hits_value(mix_mat, show_indices = "cell", label = "mixed matrix cell")

  # A data frame with SHORT row names keeps the named row subscript and still hits.
  short_df <- data.frame(a = 1:2, b = 3:4, row.names = c("x", "y"))
  expect_cell_index_hits_value(short_df, show_indices = "cell", label = "short df cell")

  # A factor column, whose cell value is a level, not a number.
  expect_cell_index_hits_value(head(iris, 3), show_indices = "all", label = "iris factor cell")
})

test_that("the cell index falls back per axis: named only where the name is drawn IN FULL", {
  # Short row + short col: both named.
  both <- matrix(1:6, 2, 3, dimnames = list(c("r1", "r2"), c("alpha", "beta", "gamma")))
  bc <- paint_cells(both, show_indices = "cell")
  expect_true('["r1", "alpha"]' %in% bc$sig[bc$kind == "cellindex"])

  # Long row + short col: the ROW goes positional, the column stays named.
  long_row <- matrix(1:6, 2, 3, dimnames = list(c("Sepal.Length", "Sepal.Width"), c("a", "b", "c")))
  lr <- paint_cells(long_row, show_indices = "cell")
  expect_true('[1, "a"]' %in% lr$sig[lr$kind == "cellindex"])
  expect_true('[2, "c"]' %in% lr$sig[lr$kind == "cellindex"])

  # Short row + long col: the COLUMN goes positional, the row stays named.
  long_col <- matrix(1:6, 2, 3, dimnames = list(c("r1", "r2"), c("Column.Alpha", "Column.Beta", "Column.Gamma")))
  lc <- paint_cells(long_col, show_indices = "cell")
  expect_true('["r1", 1]' %in% lc$sig[lc$kind == "cellindex"])
  expect_true('["r2", 3]' %in% lc$sig[lc$kind == "cellindex"])

  # Long row + long col: both positional.
  both_long <- matrix(1:6, 2, 3, dimnames = list(
    c("Sepal.Length", "Sepal.Width"),
    c("Column.Alpha", "Column.Beta", "Column.Gamma")
  ))
  bl <- paint_cells(both_long, show_indices = "cell")
  expect_true("[1, 1]" %in% bl$sig[bl$kind == "cellindex"])
  expect_true("[2, 3]" %in% bl$sig[bl$kind == "cellindex"])

  # The decision is PER NAME, not per axis wholesale: a name exactly at the 8-char
  # budget is kept, one character over it is dropped -- in the SAME axis.
  edge <- matrix(1:2, 2, 1, dimnames = list(c("12345678", "123456789"), "c"))
  ec <- paint_cells(edge, show_indices = "cell")$sig[paint_cells(edge, show_indices = "cell")$kind == "cellindex"]
  expect_true('["12345678", "c"]' %in% ec)
  expect_true('[2, "c"]' %in% ec)
})

test_that("the MARGIN keeps the (truncated) NAME while the cell index goes positional", {
  # The two lanes are DIFFERENT jobs. The row gutter is a caption naming the row, and
  # it may show a truncated name (`["Mazda RX4...", ]`). The cell index is the strict
  # accessor and must not (`[1, 1]`). This asserts they intentionally differ.
  cells <- paint_cells(head(mtcars, 4), show_indices = "cell")
  rowlab <- cells$sig[cells$kind == "rowlabel"]
  cellix <- cells$sig[cells$kind == "cellindex"]

  # The margin still carries the truncated name.
  expect_true('["Mazda RX4...", ]' %in% rowlab)
  # The cell index for that same first row went positional.
  expect_true("[1, 1]" %in% cellix)
  # And no cell index carries the truncated name that only the margin may show.
  expect_false(any(grepl("Mazda", cellix)))
})

test_that("a cell index skips a name that is a true NA and indexes by position", {
  # `["<NA>", ]` in the margin is a caption; `m["<NA>", ]` is not the row (the row's
  # name is a real NA, not the string "<NA>"), so the cell index must fall back to
  # the position for that axis.
  m <- matrix(1:4, 2, dimnames = list(c(NA, "r2"), c("c1", "c2")))
  cells <- paint_cells(m, show_indices = "cell")
  ci <- cells$sig[cells$kind == "cellindex"]
  # Row 1 (NA name) indexes positionally; row 2 ("r2") stays named.
  expect_true("[1, \"c1\"]" %in% ci)
  expect_true('["r2", "c1"]' %in% ci)
  expect_false(any(grepl("<NA>", ci)))
  # And every one of them runs and returns the cell's value.
  expect_cell_index_hits_value(m, show_indices = "cell", label = "NA-named matrix")
})

test_that("a named label truncates the NAME, then wraps -- no overflow past the cap", {
  # `max_name_chars` caps the visible NAME; the brackets and quotes go on AFTER, so
  # the wrapped token is a little wider but the name budget is what stays bounded.
  m <- matrix(1:4, 2, dimnames = list(NULL, c("Sepal.Length", "x")))
  cc <- paint_cells(m, max_name_chars = 8L)$sig[paint_cells(m, max_name_chars = 8L)$kind == "collabel"]
  expect_equal(cc, c('[, "Sepal..."]', '[, "x"]'))
  inside <- sub('^\\[, "(.*)"\\]$', "\\1", cc)
  expect_true(all(nchar(inside) <= 8L))

  # The gutter is free and keeps the full `max_chars`, so the same long name is cut
  # less tightly there.
  g <- matrix(1:4, 2, dimnames = list(c("Sepal.Length", "x"), NULL))
  rl <- paint_cells(g)$sig[paint_cells(g)$kind == "rowlabel"]
  expect_equal(rl, c('["Sepal.Length", ]', '["x", ]'))
})

test_that("a name that is a true NA becomes a token and never reaches sig as NA", {
  v <- c(1, 2, 3)
  names(v) <- c("a", NA, "")
  cells <- paint_cells(v)

  # The `<NA>` token is wrapped like any other name -- it is the sanitised text the
  # lane already used, now inside the accessor brackets.
  expect_equal(cells$sig[cells$kind == "rowlabel"], c('["a"]', '["<NA>"]', '[""]'))
  expect_false(anyNA(cells$sig))
  expect_false(anyNA(cells$insig))
  # The shared predicate must be able to answer without a warning or a surprise.
  expect_false(anyNA(inked_cells(cells)$sig))

  m <- matrix(1:4, 2, dimnames = list(c(NA, "r2"), c("c1", NA)))
  mc <- paint_cells(m)
  expect_equal(mc$sig[mc$kind == "rowlabel"], c('["<NA>", ]', '["r2", ]'))
  expect_equal(mc$sig[mc$kind == "collabel"], c('[, "c1"]', '[, "<NA>"]'))
  expect_false(anyNA(mc$sig))
})

test_that("a data frame does not draw its column names twice", {
  df <- head(iris, 3)
  cells <- paint_cells(df)

  expect_equal(sum(cells$kind == "collabel"), 0L)
  expect_equal(cells$sig[cells$kind == "header"], names(df))

  # And it does not grow one when it is asked for its dimnames either: a data
  # frame has no `show_dimnames`, and the header lane is the only lane.
  expect_false(any(cells$sig[cells$kind != "header"] %in% names(df)))
})

test_that("a data frame grows a row-name gutter only when the names are real", {
  m <- paint_cells(head(mtcars, 3))
  # The gutter is free -- it holds no value cell -- so the NAME keeps the full
  # `max_chars`, and "Mazda RX4 Wag" is one character over it. The accessor wraps
  # the truncated name: `mtcars["Mazda RX4", ]`.
  expect_equal(
    m$sig[m$kind == "rowlabel"],
    c('["Mazda RX4", ]', '["Mazda RX4...", ]', '["Datsun 710", ]')
  )

  # 1, 2, 3 is noise pretending to be data. `print()` draws it; a picture must not.
  i <- paint_cells(head(iris, 3))
  expect_equal(sum(i$kind == "rowlabel"), 0L)
  expect_equal(sum(paint_cells(iris)$kind == "rowlabel"), 0L)

  # Off by request, on by request.
  off <- paint_cells(head(mtcars, 3), show_rownames = FALSE)
  expect_equal(sum(off$kind == "rowlabel"), 0L)
  on <- paint_cells(head(iris, 3), show_rownames = TRUE)
  expect_equal(on$sig[on$kind == "rowlabel"], c('["1", ]', '["2", ]', '["3", ]'))

  # And an index lane still wins over them.
  idx <- paint_cells(head(mtcars, 2), show_indices = "row")
  expect_equal(idx$sig[idx$kind == "rowlabel"], c("[1, ]", "[2, ]"))
})

test_that("has_row_names() is not fooled by ordinals wearing a different type", {
  # `identical()` compares type as well as value: character row names reading
  # "1", "2", "3" -- exactly what `as.data.frame(as.matrix(...))` produces --
  # are not `identical()` to the integer `seq_len(n)`, and used to slip past
  # the guard and draw the ordinal-noise gutter it exists to suppress.
  d <- as.data.frame(as.matrix(head(iris, 3)))
  expect_true(is.character(row.names(d)))
  expect_equal(row.names(d), c("1", "2", "3"))
  expect_false(has_row_names(d))
  expect_equal(sum(paint_cells(d)$kind == "rowlabel"), 0L)

  # Every previously verified case still holds.
  expect_true(has_row_names(mtcars))
  expect_false(has_row_names(iris))
  expect_false(has_row_names(head(iris, 5)))
  expect_true(has_row_names(iris[c(50, 100), ]))

  df <- data.frame(x = 1:3)
  rownames(df) <- c("alpha", "beta", "gamma")
  expect_true(has_row_names(df))
})

test_that("names truncate at the cap on the shared lane and at max_chars on the free one", {
  m <- matrix(
    1:4, 2,
    dimnames = list(c("a_very_long_row_name", "r2"), c("a_very_long_col_name", "c2"))
  )
  cells <- paint_cells(m)

  # A column name shares a formatting unit with the values it sits over, so it
  # widens EVERY cell of the matrix. The NAME is capped at `max_name_chars` and the
  # accessor brackets go on after, so the visible name -- inside the quotes -- is
  # what stays within the budget.
  expect_equal(cells$sig[cells$kind == "collabel"][1L], '[, "a_ver..."]')
  col_names <- sub('^\\[, "(.*)"\\]$', "\\1", cells$sig[cells$kind == "collabel"])
  expect_true(all(nchar(col_names) <= 8L))
  # A row name is in a gutter that holds no value, so it costs the values nothing
  # and it keeps the full `max_chars`.
  expect_equal(cells$sig[cells$kind == "rowlabel"][1L], '["a_very_lo...", ]')

  wider <- paint_cells(m, max_name_chars = 12L)
  expect_equal(wider$sig[wider$kind == "collabel"][1L], '[, "a_very_lo..."]')

  # A data frame's header lane is one formatting unit per column, so it is free
  # too: unchanged at `max_chars`.
  df <- data.frame(a_very_long_col_name = 1)
  h <- paint_cells(df)
  expect_equal(h$sig[h$kind == "header"], "a_very_lo...")
})

test_that("the row-name gutter sizes alone: it does not widen the value columns", {
  short <- matrix(1:4, 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  long <- matrix(1:4, 2, dimnames = list(c("an_enormous_row_name", "r2"), c("c1", "c2")))

  w_short <- column_widths(paint_cells(short))
  w_long <- column_widths(paint_cells(long))

  # The gutter grows. The value columns do not.
  expect_gt(w_long[[1L]], w_short[[1L]])
  expect_equal(w_long[-1L], w_short[-1L])
})

test_that("ELISION IS UNCHANGED BY THE PRESENCE OF NAMES", {
  bare <- matrix(seq_len(12 * 3), nrow = 3)
  named <- bare
  dimnames(named) <- list(paste0("row", 1:3), paste0("column_", 1:12))

  cb <- paint_cells(bare)
  cn <- paint_cells(named)

  drawn_cols <- function(x) sort(unique(x$j[x$kind == "value"]))
  drawn_rows <- function(x) sort(unique(x$i[x$kind == "value"]))

  # The same 12-column matrix draws the same columns either way.
  expect_equal(drawn_cols(cn), drawn_cols(cb))
  expect_equal(drawn_rows(cn), drawn_rows(cb))
  expect_equal(attr(cn, "hidden_cols"), attr(cb, "hidden_cols"))
  expect_equal(attr(cn, "note"), attr(cb, "note"))

  # And at the elision threshold, where it would actually bite.
  wide <- matrix(seq_len(20 * 3), nrow = 3)
  wide_named <- wide
  dimnames(wide_named) <- list(paste0("row", 1:3), paste0("column_", 1:20))
  expect_equal(drawn_cols(paint_cells(wide_named)), drawn_cols(paint_cells(wide)))
  expect_equal(attr(paint_cells(wide_named), "hidden_cols"), 6L)
  expect_equal(attr(paint_cells(wide), "hidden_cols"), 6L)
})

test_that("the elision gap runs through the name lanes too", {
  m <- matrix(seq_len(30 * 30), nrow = 30)
  dimnames(m) <- list(paste0("r", 1:30), paste0("c", 1:30))
  cells <- paint_cells(m, max_rows = 4L, max_cols = 4L)

  gap <- cells[cells$kind == "ellipsis", ]
  # The gutter shows the gap, exactly as a tibble does.
  expect_true(any(gap$col == 1L))
  expect_equal(sum(cells$kind == "rowlabel"), 3L)
  expect_equal(sum(cells$kind == "collabel"), 3L)
})

test_that("rounded_rect_xy() traces a closed polygon inside its bounds, capped", {
  # r <= 0 is the plain rectangle, four corners, so a caller need not branch.
  sq <- rounded_rect_xy(0, 0, 4, 2, 0)
  expect_equal(sq$x, c(0, 4, 4, 0))
  expect_equal(sq$y, c(0, 0, 2, 2))

  # r > 0: every vertex sits within the rectangle, and the corners are inset by r.
  p <- rounded_rect_xy(0, 0, 4, 2, 0.5, n = 8L)
  expect_true(all(p$x >= 0 - 1e-9 & p$x <= 4 + 1e-9))
  expect_true(all(p$y >= 0 - 1e-9 & p$y <= 2 + 1e-9))
  # No vertex reaches a true corner: the nearest is r away along each edge.
  expect_false(any(abs(p$x - 0) < 1e-9 & abs(p$y - 0) < 1e-9))
  expect_gt(length(p$x), 4L)

  # r is capped to half the shorter side, so a small box cannot invert.
  cap <- rounded_rect_xy(0, 0, 2, 1, 10, n = 8L)
  expect_true(all(cap$x >= -1e-9 & cap$x <= 2 + 1e-9))
  expect_true(all(cap$y >= -1e-9 & cap$y <= 1 + 1e-9))
})
