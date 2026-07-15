# The array painters, and the one promise they exist to keep.

# ---------------------------------------------------------------------------
# THE ACCESSOR TEST: every index the picture draws is an expression that RUNS
# ---------------------------------------------------------------------------
#
# This is the package's one distinguishing promise and the array is where it is
# easiest to break. On a 3-D array `a[1, ]` and `a[2, 3]` are ERRORS, not shorthand,
# so a painter that reused the matrix's labels would teach students to type
# expressions that fail -- in the `show_indices = "all"` call an instructor reaches
# for first.
#
# So: take every label the cell table draws, paste it onto the object's name,
# EVALUATE it, and demand that it neither errors nor comes back empty.

# `pdf(NULL)` is the test device: full strwidth()/par() support, no file I/O, and no
# Rplots.pdf left in the working directory for R CMD check to complain about. It is
# the same helper `test-dispatch.R` uses, and for the same reason.
with_null_pdf <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

# The subscript lanes, filtered to the labels that are ACCESSORS -- the ones that
# start with `[`. That takes the positional margins (`[1, , 3]`), the named matrix
# margins (`["r1", ]`), and every cell index, positional or named
# (`["1st", "Male", "Child", "No"]`). It skips an array's bare margin name (`1st`,
# which is not something you subscript with unquoted) and a slice title
# (`, , Child, No`, which is a title, not a subscript) -- neither is claimed to be
# an accessor, so neither is evaluated.
index_labels <- function(cells) {
  lanes <- cells[cells$kind %in% c("rowlabel", "collabel", "cellindex"), , drop = FALSE]
  s <- lanes$sig
  s[startsWith(s, "[")]
}

expect_labels_run <- function(a, ..., label = NULL) {
  cells <- paint_cells(a, ...)
  labs <- index_labels(cells)
  expect_gt(length(labs), 0L)
  env <- new.env(parent = baseenv())
  assign("a", a, envir = env)
  for (s in labs) {
    expr <- paste0("a", s)
    got <- tryCatch(
      eval(parse(text = expr), envir = env),
      error = function(e) structure(conditionMessage(e), class = "paintr_label_error")
    )
    expect_false(
      inherits(got, "paintr_label_error"),
      info = paste0(label, ": `", expr, "` ERRORED: ", as.character(got))
    )
    expect_gt(length(got), 0L)
  }
  invisible(labs)
}

test_that("every index label RUNS, for a 2-D, 3-D and 4-D array", {
  a2 <- array(1:6, c(2, 3))
  a3 <- array(1:24, c(2, 3, 4))
  a4 <- array(1:48, c(2, 3, 2, 4))

  # The lanes hold indices only when no name has won them, so ask for the indices
  # explicitly -- which is exactly the call the promise is about.
  expect_labels_run(a2, show_indices = "all", label = "2-D")
  expect_labels_run(a3, show_indices = "all", label = "3-D")
  expect_labels_run(a4, show_indices = "all", label = "4-D")

  # And with dimnames present, where an index lane must still WIN the axis it names.
  expect_labels_run(Titanic, show_indices = "all", label = "Titanic")
  expect_labels_run(HairEyeColor, show_indices = "all", label = "HairEyeColor")
  expect_labels_run(UCBAdmissions, show_indices = "all", label = "UCBAdmissions")

  # And with the names explicitly off, which is the other way to get an index lane.
  expect_labels_run(Titanic, show_indices = "all", show_dimnames = "none", label = "no dimnames")
})

test_that("every NAMED and MIXED index label RUNS too, per axis", {
  # The named margins and the named cell index are accessors as much as the
  # positional ones. `show_indices = "cell"` draws the cell index and composes with
  # the default name lanes, so this battery evaluates `["r1", , 3]`, `[, "c2", 3]`
  # and `["r1", "c2", 3]` -- names where an axis has them, numbers where it does
  # not -- and demands each one runs.
  fully <- array(1:24, c(2, 3, 4), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"), NULL))
  rows_only <- array(1:24, c(2, 3, 4), dimnames = list(c("r1", "r2"), NULL, NULL))
  cols_only <- array(1:24, c(2, 3, 4), dimnames = list(NULL, c("c1", "c2", "c3"), NULL))
  slab_named <- array(1:24, c(2, 3, 4), dimnames = list(c("r1", "r2"), NULL, c("s1", "s2", "s3", "s4")))

  expect_labels_run(fully, show_indices = "cell", label = "rows+cols named")
  expect_labels_run(rows_only, show_indices = "cell", label = "rows only")
  expect_labels_run(cols_only, show_indices = "cell", label = "cols only")
  expect_labels_run(slab_named, show_indices = "cell", label = "rows+slab named")

  # The fully dimnamed contingency tables, at their default named display.
  expect_labels_run(Titanic, show_indices = "cell", label = "Titanic named")
  expect_labels_run(HairEyeColor, show_indices = "cell", label = "HairEyeColor named")
  expect_labels_run(UCBAdmissions, show_indices = "cell", label = "UCBAdmissions named")

  # The mix is drawn correctly: a named axis uses its name, an unnamed one its
  # number, in the SAME label.
  ci <- unique(paint_cells(fully, show_indices = "cell")$sig[
    paint_cells(fully, show_indices = "cell")$kind == "cellindex"
  ])
  expect_true('["r1", "c2", 3]' %in% ci)
  rc <- unique(paint_cells(rows_only, show_indices = "cell")$sig[
    paint_cells(rows_only, show_indices = "cell")$kind == "cellindex"
  ])
  expect_true('["r1", 2, 3]' %in% rc)
})

# THE CONTRACT, WITH LONG DIMNAMES: a cell index must return the value drawn in the
# cell. A named subscript reaches the cell only when the name is drawn IN FULL, so a
# dimname longer than the 8-char budget would truncate to a wrong subscript that
# errors or misses. The too-long axis must fall back to its POSITION. This strips
# the dimnames to get a guaranteed-positional cell index for the SAME shape and grid
# (`show_dimnames = "none"` draws no name margins on either twin, so the two grids
# line up cell for cell), then demands the NAMED label returns the same value the
# POSITIONAL label does from the same object.
expect_cell_index_hits_value_nd <- function(x, show_indices = "cell", label = NULL) {
  bare <- x
  dimnames(bare) <- NULL
  named <- paint_cells(x, show_indices = show_indices, show_dimnames = "none")
  pos <- paint_cells(bare, show_indices = show_indices, show_dimnames = "none")
  named <- named[named$kind == "cellindex", , drop = FALSE]
  pos <- pos[pos$kind == "cellindex", , drop = FALSE]
  expect_gt(nrow(named), 0L)
  pos_sig <- pos$sig
  names(pos_sig) <- paste(pos$row, pos$col)
  env <- new.env(parent = baseenv())
  assign("x", x, envir = env)
  for (r in seq_len(nrow(named))) {
    lab <- named$sig[[r]]
    plab <- pos_sig[[paste(named$row[[r]], named$col[[r]])]]
    got <- tryCatch(
      eval(parse(text = paste0("x", lab)), envir = env),
      error = function(e) structure(conditionMessage(e), class = "paintr_label_error")
    )
    expect_false(
      inherits(got, "paintr_label_error"),
      info = sprintf("%s: `x%s` ERRORED: %s", label, lab, as.character(got))
    )
    want <- eval(parse(text = paste0("x", plab)), envir = env)
    expect_equal(
      unname(got), unname(want),
      info = sprintf("%s: `x%s` != positional `x%s`", label, lab, plab)
    )
  }
}

test_that("EVERY array cell index runs AND returns the cell's value, with LONG dimnames", {
  long <- array(1:24, c(2, 3, 4), dimnames = list(
    c("Sepal.Length", "Sepal.Width"),
    c("Column.Alpha", "Column.Beta", "Column.Gamma"),
    c("SlabOne.Long", "SlabTwo.Long", "SlabThree", "SlabFour")
  ))
  expect_cell_index_hits_value_nd(long, show_indices = "cell", label = "long array cell")
  expect_cell_index_hits_value_nd(long, show_indices = "all", label = "long array all")

  # A short slab name ("SlabFour", exactly 8) stays named; the long ones go
  # positional -- per axis, in the same array.
  ci <- unique(paint_cells(long, show_indices = "cell")$sig[
    paint_cells(long, show_indices = "cell")$kind == "cellindex"
  ])
  expect_true("[1, 1, 1]" %in% ci)
  expect_true('[1, 1, "SlabFour"]' %in% ci)

  # Short dimnames still index BY NAME and still return the cell.
  short <- array(1:24, c(2, 3, 4), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"), c("s1", "s2", "s3", "s4")))
  expect_cell_index_hits_value_nd(short, show_indices = "cell", label = "short array cell")
})

test_that("the index labels carry the array's full subscript ARITY", {
  a3 <- array(1:24, c(2, 3, 4))
  cells <- paint_cells(a3, show_indices = "all")

  rl <- unique(cells$sig[cells$kind == "rowlabel"])
  cl <- unique(cells$sig[cells$kind == "collabel"])
  ci <- unique(cells$sig[cells$kind == "cellindex"])

  # A 3-D array takes THREE subscripts. `a[1, ]` is an error; `a[1, , 3]` is the row.
  expect_true(all(grepl("^\\[[0-9]+, , [0-9]+\\]$", rl)))
  expect_true(all(grepl("^\\[, [0-9]+, [0-9]+\\]$", cl)))
  expect_true(all(grepl("^\\[[0-9]+, [0-9]+, [0-9]+\\]$", ci)))

  # The slice subscript is FILLED IN FROM THE BLOCK: the gutter beside block 3 names
  # block 3's row, not all four blocks' rows at once.
  expect_true("[1, , 1]" %in% rl)
  expect_true("[1, , 4]" %in% rl)
  expect_true("[2, 3, 4]" %in% ci)

  # Rank 4 takes four.
  a4 <- array(1:48, c(2, 3, 2, 4))
  c4 <- paint_cells(a4, show_indices = "all")
  expect_true(all(grepl(
    "^\\[[0-9]+, [0-9]+, [0-9]+, [0-9]+\\]$",
    c4$sig[c4$kind == "cellindex"]
  )))
})

test_that("a matrix's labels are UNCHANGED -- rank 2 adds no subscripts", {
  m <- matrix(1:6, nrow = 2)
  cells <- paint_cells(m, show_indices = "all")
  expect_true("[1, ]" %in% cells$sig[cells$kind == "rowlabel"])
  expect_true("[, 2]" %in% cells$sig[cells$kind == "collabel"])
  expect_true("[2, 3]" %in% cells$sig[cells$kind == "cellindex"])
})

# ---------------------------------------------------------------------------
# rank 2 is the degenerate case, and it is served by NOT WRITING IT
# ---------------------------------------------------------------------------

test_that("paint_array() draws a matrix, and draws it as paint_matrix() does", {
  m <- matrix(c(1, 0.333, 123456.7, 20, -1, NA), nrow = 3)

  # `is.array(matrix)` is TRUE, so the painter must accept it...
  expect_true(is.array(m))
  expect_true(is_paint_array(m))

  # ...and the two painters must produce the SAME cell table, because they run the
  # same code. This is the whole of the "k = 1 is the free degenerate case" claim,
  # and it is checked rather than asserted.
  #
  # THE MATRICES BELOW ARE BIGGER THAN THE ARRAY'S CAPS ON PURPOSE. A 3x2 cannot see
  # this class of bug: it is under every threshold either painter has, so the two
  # agree no matter WHOSE caps they used, and a suite calibrated on it will pass
  # while `paint_array()` quietly elides a matrix that `paint_matrix()` draws whole.
  # `paint_array()` used to hard-code `max_rows = 10, max_cols = 8` in its formals,
  # and a rank-2 array carried the ARRAY's caps into the MATRIX builder: a 12x10 drew
  # 63 of its 120 values and announced "# 3 more rows, 3 more columns" where
  # `paint_matrix()` drew all 120 and said nothing. The formals are `NULL` now, so
  # the builder picks the cap that suits the RANK, and 12x10 and 30x30 are here to
  # keep it that way.
  with_null_pdf({
    battery <- list(
      "3x2" = m,
      "12x10, past the array's 10x8 and inside the matrix's 20x15" = matrix(1:120, 12),
      "30x30, past BOTH -- it elides, but it must elide the SAME WAY" =
        matrix(1:900, 30),
      "12x10 dimnamed -- the name lanes must agree too" = matrix(
        1:120, 12,
        dimnames = list(paste0("r", seq_len(12)), paste0("c", seq_len(10)))
      ),
      "dimnamed 2x2" = matrix(
        c(21, 6, 22.8, 4), nrow = 2, byrow = TRUE,
        dimnames = list(c("Mazda", "Datsun"), c("mpg", "cyl"))
      )
    )
    for (nm in names(battery)) {
      mm <- battery[[nm]]
      # `graph_title` deparses `substitute(data)` and the two painters are handed
      # different expressions, so it is pinned -- everything else must match on its
      # own.
      a <- paint_array(mm, graph_title = "T")
      b <- paint_matrix(mm, graph_title = "T")
      expect_identical(a$cells, b$cells, info = nm)
      expect_identical(a$fontsize, b$fontsize, info = nm)
      expect_identical(a$note, b$note, info = nm)

      # The point of the caps bug, stated as the reader would see it: every value the
      # matrix painter draws, the array painter draws too.
      expect_identical(
        sum(a$cells$kind == "value"), sum(b$cells$kind == "value"),
        info = nm
      )
    }

    # A 12x10 is drawn WHOLE by both -- 120 values and no note. This is the
    # regression itself, spelled out rather than inferred from an identity.
    a <- paint_array(matrix(1:120, 12), graph_title = "T")
    expect_identical(sum(a$cells$kind == "value"), 120L)
    # `painter_prep()` hands the renderer `NULL` when nothing was elided.
    expect_null(a$note)
    expect_true(is.na(attr(paint_cells(matrix(1:120, 12)), "note")))

    # An explicit cap still reaches a rank-2 array: `NULL` is a default, not a
    # refusal.
    a10 <- paint_array(matrix(1:120, 12), max_rows = 10L, max_cols = 8L,
                       graph_title = "T")
    m10 <- paint_matrix(matrix(1:120, 12), max_rows = 10L, max_cols = 8L,
                        graph_title = "T")
    expect_identical(a10$cells, m10$cells)
    expect_lt(sum(a10$cells$kind == "value"), 120L)

    # ...and a rank-3 array keeps the ARRAY's tighter caps, because it IS several
    # blocks wide. `NULL` means "the cap that suits the rank", not "no cap".
    big <- array(1:2400, c(12, 10, 2))
    c3 <- paint_cells(big)
    expect_identical(attr(c3, "hidden_rows"), 3L)
    expect_identical(attr(c3, "hidden_cols"), 3L)

    # A dimnamed matrix, with an index lane on it.
    mn <- battery[["dimnamed 2x2"]]
    expect_identical(
      paint_array(mn, show_indices = "cell", graph_title = "T")$cells,
      paint_matrix(mn, show_indices = "cell", graph_title = "T")$cells
    )

    # A 2-D table is an array and is drawn.
    tb <- table(c("a", "b", "a"), c("x", "x", "y"))
    expect_silent(paint_array(tb))
  })
})

# ---------------------------------------------------------------------------
# THE SYNC GUARD: a block IS a matrix, and nothing else may be true of it
# ---------------------------------------------------------------------------
#
# `array_cells()` is a SECOND cell builder. Rank 2 never enters it, so
# `paint_array(m) == paint_matrix(m)` is true BY CONSTRUCTION and the test above
# holds it there -- but the block builder still RESTATES the lane contract, and a
# restatement is a thing that drifts. `lane_ink()`, `lane_size()`, `gap_cells()`,
# `elide_axis()` and `grid_lanes()` were lifted so that there is only one statement
# left to drift; this test is what makes the lift LOAD-BEARING, by failing if
# someone re-inlines a constant into one builder and not the other.
#
# The claim under test is the package's own teaching claim: A BLOCK IS A MATRIX. So
# take a 3-D array's block, take `paint_matrix()` of the very same 2-D slice, and
# demand that the lanes are `identical()` -- ink, size_rel, align, fit, and the text
# of every lane whose text is not fixed by RANK.
#
# ONE thing IS fixed by rank, and it parts here: a subscript lane's TEXT. A
# standalone matrix's row accessor is `["r1", ]` -- two subscripts, the whole of
# it. A block's is not: the block is a slice of a higher-rank array, its slab
# subscripts live in the title above it, and so its margin names the axis BARE
# (`r1`) while its cell index carries the full arity (`["r1", "c1", 1]`). That the
# text differs by rank is itself the contract -- pinned explicitly below -- so the
# structural comparison holds every OTHER attribute of every lane, the subscript
# lanes' STYLING included, and sets their text aside.
test_that("a 3-D array's BLOCK lanes are IDENTICAL to paint_matrix()'s on that slice", {
  # The block's lanes and the matrix's, stripped of the two things that legitimately
  # differ: WHERE they sit, and a subscript lane's TEXT (fixed by rank -- see above).
  # A block is offset inside the grid of blocks and carries a title row the matrix
  # has no need of, so `row`/`col` differ. Everything else -- the ink, the size, the
  # alignment, the fit, the fill, the border, the formatting unit, and the text of
  # every value and gap -- is the contract, and the contract must not.
  subscript_lanes <- c("rowlabel", "collabel", "cellindex")
  lanes_of <- function(cells) {
    x <- cells[cells$kind != "slicelabel" & cells$kind != "outline", , drop = FALSE]
    # Rank fixes a subscript lane's text; the structural contract is everything else.
    # `head` mirrors `sig` at construction, so it is set aside with it.
    is_sub <- x$kind %in% subscript_lanes
    x$sig[is_sub] <- ""
    x$head[is_sub] <- ""
    x <- x[order(x$kind, x$i, x$j, x$sig), c(
      "kind", "sig", "insig", "head", "tail", "ink", "fill", "border", "align",
      "size_rel", "dy_rel", "fit", "fmt_group"
    ), drop = FALSE]
    rownames(x) <- NULL
    x
  }

  # A RANK-3 ARRAY WITH ONE SLICE draws exactly one block, and that block is a
  # picture of the same 2-D data `paint_matrix()` is handed. Rank 3, so it goes
  # through `array_cells()`; one slice, so there is one block to compare.
  cases <- list(
    "dimnamed, both name lanes" = array(
      1:8, c(4, 2, 1),
      dimnames = list(c("1st", "2nd", "3rd", "Crew"), c("Male", "Female"), "Child")
    ),
    "bare, no name lane at all" = array(1:6, c(2, 3, 1)),
    "row names only, half-named" = array(
      1:6, c(2, 3, 1),
      dimnames = list(c("a", "b"), NULL, NULL)
    ),
    "an ELIDING block -- the gap cells must match too" = array(1:240, c(24, 10, 1))
  )
  for (nm in names(cases)) {
    a <- cases[[nm]]
    slice <- a[, , 1L]
    # The array's caps, applied to the matrix, so the two draw the same SHAPE. What
    # is pinned here is the visual contract; the elision POLICY is pinned by the
    # identity test above.
    expect_identical(
      lanes_of(array_cells(a)),
      lanes_of(paint_cells(slice, max_rows = 10L, max_cols = 8L)),
      info = nm
    )
  }

  # The text that lanes_of() sets aside, pinned explicitly so the divergence is
  # intentional: a standalone matrix draws the 2-D accessor, an array block names
  # its axis bare, and the block's full accessor lives in the cell index.
  a <- cases[["dimnamed, both name lanes"]]
  blk <- array_cells(a)
  mat <- paint_cells(a[, , 1L])
  expect_equal(blk$sig[blk$kind == "rowlabel"], c("1st", "2nd", "3rd", "Crew"))
  expect_equal(mat$sig[mat$kind == "rowlabel"], c('["1st", ]', '["2nd", ]', '["3rd", ]', '["Crew", ]'))
  expect_true('["1st", "Male", "Child"]' %in% array_cells(a, show_indices = "cell")$sig)

  # And on a REAL multi-block array, every block draws the same LANE contract as the
  # matrix does -- the tuple set has to match exactly, so re-tuning `grey40` in one
  # builder and not the other fails here.
  #
  # The VALUE cells are deliberately not in this set, and the reason is a feature
  # rather than a dodge: a value's `ink` and `align` come out of `paint_format()`,
  # whose unit for an array is THE WHOLE ARRAY and for a matrix is the matrix (see
  # "the formatting unit is the WHOLE array", below). `HairEyeColor` formats to
  # `decimal` across both slices and its first slice alone formats to `left`, and
  # that difference is the documented behaviour, not drift. What `array_cells()`
  # RESTATES about a value -- `size_rel`, `fit`, `border`, `fmt_group` -- is pinned
  # by the one-slice cases above, where the two formatting units coincide.
  lane_contract <- function(cells) {
    x <- cells[cells$kind %in% c("collabel", "rowlabel", "ellipsis"), , drop = FALSE]
    x <- unique(x[, c("kind", "ink", "align", "size_rel", "dy_rel", "fit")])
    x <- x[order(x$kind, x$ink, x$align, x$size_rel), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  multi <- list(
    "named lanes, 2 blocks" = HairEyeColor,
    "index lanes, 4 blocks" = array(1:24, c(2, 3, 4)),
    "eliding on rows, cols AND slices" = array(1:2400, c(24, 20, 5))
  )
  for (nm in names(multi)) {
    a <- multi[[nm]]
    for (si in c("none", "all")) {
      expect_identical(
        lane_contract(array_cells(a, show_indices = si)),
        lane_contract(
          paint_cells(a[, , 1L], show_indices = si, max_rows = 10L, max_cols = 8L)
        ),
        info = paste(nm, si)
      )
    }
  }
})

test_that("the lane contract has exactly ONE definition", {
  # If someone re-inlines `if (named) "black" else "grey40"` into one builder and
  # re-tunes only the other, the test above catches it. These pin the helpers
  # themselves, so the failure names the constant rather than a diff of 200 rows.
  expect_identical(lane_ink(TRUE), "black")
  expect_identical(lane_ink(FALSE), "grey40")
  expect_identical(lane_size(TRUE), 0.9)
  expect_identical(lane_size(FALSE), 0.8)

  g <- gap_cells(1L, 2L, "...")
  expect_identical(g$ink, "grey50")
  expect_identical(g$size_rel, 1)
  expect_identical(g$fit, FALSE)
  expect_identical(g$sig, "...")

  # `elide_axis()` is `show_all`'s one definition, on every axis of every builder.
  expect_identical(
    elide_axis(30L, 10L, show_all = TRUE),
    list(keep = seq_len(30L), gap = NA_integer_, hidden = 0L)
  )
  expect_identical(elide_axis(30L, 10L, show_all = FALSE), elide_index(30L, 10L))

  # `grid_lanes()` is PR 1's precedence rule's one definition: an index lane the
  # caller asks for WINS the axis it names.
  mn <- matrix(1:4, 2, dimnames = list(c("a", "b"), c("x", "y")))
  l <- grid_lanes(mn, "all", idx_row = TRUE, idx_col = FALSE)
  expect_null(l$nm_row)
  expect_identical(l$nm_col, c("x", "y"))
  expect_true(l$lane_row)
  expect_true(l$lane_col)
  expect_true(l$slice_named)

  l <- grid_lanes(mn, "none", idx_row = FALSE, idx_col = FALSE)
  expect_null(l$nm_row)
  expect_null(l$nm_col)
  expect_false(l$lane_row)
  expect_false(l$lane_col)
  expect_false(l$slice_named)
})

# ---------------------------------------------------------------------------
# the slice title is a lane, and it is capped like one
# ---------------------------------------------------------------------------

test_that("max_name_chars caps the SLICE TITLE, the tightest lane in the picture", {
  # Uncapped, a hostile dimname came out of the FONT rather than the layout -- the
  # span rule bounds the title's width, so it never overflowed and never warned, it
  # just quietly took `Titanic` from 15.96pt to 7.87pt. It is the tightest lane
  # there is: its budget is the width of ONE BLOCK.
  a <- Titanic
  dimnames(a)[[3L]] <- c(strrep("A", 24), strrep("B", 24))
  cells <- paint_cells(a)
  titles <- unique(cells$sig[cells$kind == "slicelabel"])

  # `truncate_chr()` caps the name's TOTAL width at `max_name_chars`, ellipsis
  # included -- which is exactly what it does to a column name, and the point is
  # that the slice title is now capped by the SAME rule, not a parallel one.
  expect_identical(
    sort(titles),
    sort(c(
      ", , AAAAA..., No", ", , BBBBB..., No",
      ", , AAAAA..., Yes", ", , BBBBB..., Yes"
    ))
  )
  expect_false(any(grepl(strrep("A", 9), titles, fixed = TRUE)))
  expect_true(all(nchar(titles) <= nchar(", , ") + 8L + 2L + 8L))

  # The knob is a knob.
  wide <- paint_cells(a, max_name_chars = 24L)
  expect_true(any(grepl(strrep("A", 24), wide$sig[wide$kind == "slicelabel"])))

  # AND THE CANONICAL ARRAYS ARE UNTOUCHED -- every slice dimname they carry is
  # inside 8, so the cap costs the package's own examples exactly nothing.
  expect_identical(
    unique(paint_cells(Titanic)$sig[paint_cells(Titanic)$kind == "slicelabel"]),
    c(", , Child, No", ", , Adult, No", ", , Child, Yes", ", , Adult, Yes")
  )
  hec <- paint_cells(HairEyeColor)
  expect_identical(
    unique(hec$sig[hec$kind == "slicelabel"]), c(", , Male", ", , Female")
  )
})

test_that("the slice title is the SUBSCRIPT, not print()'s full subscript line", {
  # `print(Titanic)` writes `, , Age = Child, Survived = No`, because its dimnames
  # are themselves NAMED. The picture writes `, , Child, No` -- and that is a
  # decision, not an oversight: the title is fitted against the block it spans, and
  # the long form costs Titanic 48% of its font (15.7 -> 8.2pt at 7x5in) while
  # costing a rank-3 table nothing at all. The accessor still runs, which is the
  # promise that matters: `Titanic[, , 1, 1]` IS the block it titles.
  expect_false(is.null(names(dimnames(Titanic))))
  cells <- paint_cells(Titanic)
  titles <- cells$sig[cells$kind == "slicelabel"]
  expect_true(all(startsWith(titles, ", , ")))
  expect_false(any(grepl("=", titles, fixed = TRUE)))
  expect_true(", , Child, No" %in% titles)

  # And the block that title names is the one `[, , 1, 1]` returns.
  expect_identical(dim(Titanic[, , 1L, 1L]), c(4L, 2L))
})

test_that("a 2-D array draws NO slice title", {
  cells <- paint_cells(matrix(1:6, nrow = 2))
  expect_equal(sum(cells$kind == "slicelabel"), 0L)

  # ...and a 3-D one draws exactly one per block.
  c3 <- paint_cells(array(1:24, c(2, 3, 4)))
  expect_equal(sum(c3$kind == "slicelabel"), 4L)
})

test_that("rank 1 is refused, and rank 5 is drawn", {
  expect_error(paint_array(array(1:3)), "array")
  expect_error(paint_cells(array(1:3)), "one- and two-dimensional")
  expect_silent(paint_cells(array(1:32, c(2, 2, 2, 2, 2))))
})

test_that("a list carrying a dim is not an array", {
  l <- list(1, 2, 3, 4)
  dim(l) <- c(2, 2)
  expect_false(is_paint_array(l))
  expect_error(paint_array(l), "array")
})

# ---------------------------------------------------------------------------
# the slice titles
# ---------------------------------------------------------------------------

test_that("the slice title is the block's subscript, as print() lays it out", {
  # NOT "what print() writes", which was a claim this package could not keep:
  # `Titanic`'s dimnames are NAMED, so `print()` writes `, , Age = Child, Survived
  # = No`. See the test below for what that costs and why the picture declines it.
  cells <- paint_cells(Titanic)
  titles <- cells$sig[cells$kind == "slicelabel"]
  expect_true(", , Child, No" %in% titles)
  expect_true(", , Adult, Yes" %in% titles)
  expect_equal(length(titles), 4L)

  # Names off -> the index form, still a title, still identifying the block.
  bare <- paint_cells(Titanic, show_dimnames = "none")
  bt <- bare$sig[bare$kind == "slicelabel"]
  expect_true(", , 1, 1" %in% bt)
  expect_true(", , 2, 2" %in% bt)

  # An undimnamed 3-D array titles by index.
  a3 <- paint_cells(array(1:24, c(2, 3, 4)))
  expect_equal(
    sort(a3$sig[a3$kind == "slicelabel"]),
    c(", , 1", ", , 2", ", , 3", ", , 4")
  )
})

test_that("the slice title SPANS its block and demands no column width", {
  cells <- paint_cells(Titanic)
  sl <- cells[cells$kind == "slicelabel", , drop = FALSE]
  expect_true(all(sl$col_end > sl$col))

  # A spanning cell demands nothing of any single column: it is fitted against the
  # span instead. Widening the title must not widen a single value cell.
  wide <- Titanic
  dimnames(wide)[[3L]] <- c("AAAAAAAAAAAAAAAAAAAA", "BBBBBBBBBBBBBBBBBBBB")
  expect_equal(column_widths(paint_cells(Titanic)), column_widths(paint_cells(wide)))
})

test_that("span_widths() is BIT-IDENTICAL to col_w[col] for a cell in one column", {
  # THE HOT PATH. `fit_fontsize()` and `span_dx()` ask `span_widths()` for the width
  # of EVERY cell of EVERY picture the package draws, so the single-column answer has
  # to be the number it has always been -- not merely equal to it.
  #
  # Writing the function as the one-liner `edge[col_end + 1] - edge[col]` is correct
  # in exact arithmetic and WRONG IN FLOATING POINT: `cumsum()` accumulates, so
  # `(a + b + c) - (a + b)` is not `identical()` to `c`. The ~1e-16 residue travels
  # into `avail_w`, through the `min()` in `fit_fontsize()`, and out into a font size
  # whose last bits have moved -- which it measurably did, for a 30x20 matrix, for
  # `iris`, and for a ragged list, before the single-column case was lifted out.
  #
  # `expect_equal()` would NOT catch this. It has a tolerance. `expect_identical()`
  # is the whole point.
  # `show_indices`'s vocabulary differs by structure -- a vector's is
  # none/inside/outside -- so each case carries its own.
  battery <- list(
    list(matrix(1:6, nrow = 3), "none"),
    list(matrix(c(1, 0.333, 123456.7, 20, -1, NA), nrow = 3), "all"),
    list(matrix(1:600, nrow = 30), "none"),
    list(matrix(letters[1:6], nrow = 2), "cell"),
    list(1:5, "outside"),
    list(c(alpha = 1, beta = 2, gamma = 3), "inside"),
    list(head(mtcars, 6), "all"),
    list(head(iris, 5), "none"),
    list(list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA)), "cell"),
    list(Titanic, "none"),
    list(Titanic, "all"),
    list(array(1:24, c(2, 3, 4)), "all")
  )
  for (case in battery) {
    cells <- paint_cells(case[[1L]], show_indices = case[[2L]])
    col_w <- column_widths(cells)
    one <- cells$col_end == cells$col
    expect_true(any(one))
    expect_identical(
      span_widths(cells, col_w)[one],
      col_w[cells$col][one],
      info = paste(class(case[[1L]])[1L], case[[2L]])
    )
  }
})

test_that("a 3-D array's slabs lay out in ONE line", {
  # ceiling(sqrt(4)) would wrap four slabs into a 2x2 grid and make them
  # PIXEL-IDENTICAL to a genuine 4-D facet -- a layout artefact wearing a
  # dimension's clothes. One line.
  cells <- paint_cells(array(1:24, c(2, 3, 4)))
  sl <- cells[cells$kind == "slicelabel", , drop = FALSE]
  expect_equal(length(unique(sl$row)), 1L)
  expect_equal(length(unique(sl$col)), 4L)

  # A 4-D array's two directions ARE two axes: across is dim 3, down is dim 4.
  c4 <- paint_cells(Titanic)
  s4 <- c4[c4$kind == "slicelabel", , drop = FALSE]
  expect_equal(length(unique(s4$row)), 2L)
  expect_equal(length(unique(s4$col)), 2L)
})

# ---------------------------------------------------------------------------
# the formatting unit is the WHOLE array
# ---------------------------------------------------------------------------

test_that("a 1e15 in slice 3 flips the WHOLE array to scientific", {
  a <- array(1, c(2, 2, 3))
  flat <- paint_cells(a)
  expect_false(any(grepl("e", flat$sig[flat$kind == "value"], fixed = TRUE)))

  a[1, 1, 3] <- 1e15
  sci <- paint_cells(a)
  v <- sci$sig[sci$kind == "value"]
  # EVERY value, in every block, not just slice 3's.
  expect_true(all(grepl("e", v, fixed = TRUE)))
  expect_equal(length(unique(sci$fmt_group[sci$kind == "value"])), 1L)
  expect_equal(unique(sci$fmt_group[sci$kind == "value"]), 1L)
})

test_that("elision runs BEFORE formatting: a hidden 1e15 cannot flip the visible cells", {
  a <- array(1, c(2, 2, 30))
  a[1, 1, 15] <- 1e15  # a slice the default max_slices will not draw
  cells <- paint_cells(a)
  expect_false(any(grepl("e", cells$sig[cells$kind == "value"], fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# elision, on the slice axis too
# ---------------------------------------------------------------------------

test_that("the slice axis elides, and the gap is DRAWN", {
  cells <- paint_cells(UCBAdmissions)  # 2 x 2 x 6, max_slices = 4
  expect_equal(sum(cells$kind == "slicelabel"), 3L)
  expect_equal(attr(cells, "hidden_slices"), 3L)
  expect_match(attr(cells, "note"), "3 more slices")
  # The gap is always drawn.
  expect_gt(sum(cells$kind == "ellipsis"), 0L)
})

test_that("elision is device-free and depends on SHAPE, never on content", {
  a <- array(1:24, c(2, 3, 4))
  b <- a
  b[] <- 1e9
  ca <- paint_cells(a)
  cb <- paint_cells(b)
  expect_equal(attr(ca, "n_row"), attr(cb, "n_row"))
  expect_equal(attr(ca, "n_col"), attr(cb, "n_col"))
  expect_equal(attr(ca, "hidden_slices"), attr(cb, "hidden_slices"))

  # And dimnames do not change WHICH cells are drawn.
  dimnames(b) <- list(c("r1", "r2"), c("c1", "c2", "c3"), c("s1", "s2", "s3", "s4"))
  expect_equal(
    sum(paint_cells(a)$kind == "value"),
    sum(paint_cells(b)$kind == "value")
  )
})

test_that("show_all draws every slice", {
  cells <- paint_cells(UCBAdmissions, show_all = TRUE)
  expect_equal(sum(cells$kind == "slicelabel"), 6L)
  expect_equal(attr(cells, "hidden_slices"), 0L)
  expect_true(is.na(attr(cells, "note")))
})

test_that("every block gets its OWN outline", {
  cells <- paint_cells(array(1:24, c(2, 3, 4)))
  o <- cells[cells$kind == "outline", , drop = FALSE]
  expect_equal(nrow(o), 4L)
  # Four boxes, not one box around all four: no two outlines share a column, and
  # none of them spans the whole picture.
  expect_equal(length(unique(o$col)), 4L)
  expect_true(all(o$col_end < attr(cells, "n_col")[[1L]] | o$col == max(o$col)))
  expect_true(all(o$col_end >= o$col))
  expect_true(all(o$row_end > o$row))
})

# ---------------------------------------------------------------------------
# dimnames, on ALL axes
# ---------------------------------------------------------------------------

test_that("show_dimnames draws the row and column names of a contingency table", {
  # An array's MARGINS name their axis bare, as `print()` does: the slice title
  # above the block carries the slab subscripts, and the cell index below spells
  # the full accessor -- so the gutter does not repeat either. (See array_cells().)
  cells <- paint_cells(Titanic)
  expect_true("1st" %in% cells$sig[cells$kind == "rowlabel"])
  expect_true("Crew" %in% cells$sig[cells$kind == "rowlabel"])
  expect_true("Male" %in% cells$sig[cells$kind == "collabel"])
  expect_true("Female" %in% cells$sig[cells$kind == "collabel"])

  # An index lane WINS the axis it names -- PR 1's precedence rule, unchanged.
  idx <- paint_cells(Titanic, show_indices = "row")
  expect_false("1st" %in% idx$sig[idx$kind == "rowlabel"])
  expect_true(any(startsWith(idx$sig[idx$kind == "rowlabel"], "[")))
  # ...and the column axis keeps its names.
  expect_true("Male" %in% idx$sig[idx$kind == "collabel"])

  # show_dimnames = "none" draws no name lane at all.
  none <- paint_cells(Titanic, show_dimnames = "none")
  expect_equal(sum(none$kind == "rowlabel"), 0L)
  expect_equal(sum(none$kind == "collabel"), 0L)

  # A single axis can be asked for on its own.
  rows <- paint_cells(Titanic, show_dimnames = "row")
  expect_gt(sum(rows$kind == "rowlabel"), 0L)
  expect_equal(sum(rows$kind == "collabel"), 0L)
})

test_that("`slice` governs the block titles alone", {
  cells <- paint_cells(Titanic, show_dimnames = "slice")
  expect_true(", , Child, No" %in% cells$sig[cells$kind == "slicelabel"])
  expect_equal(sum(cells$kind == "rowlabel"), 0L)

  rc <- paint_cells(Titanic, show_dimnames = c("row", "column"))
  expect_true(", , 1, 1" %in% rc$sig[rc$kind == "slicelabel"])
  expect_true("1st" %in% rc$sig[rc$kind == "rowlabel"])
})

test_that("an unknown show_dimnames value is an error", {
  expect_error(paint_array(Titanic, show_dimnames = "slices"), "show_dimnames")
  expect_error(paint_matrix(matrix(1:4, 2), show_dimnames = "slice"), "show_dimnames")
})

# ---------------------------------------------------------------------------
# highlighting: if a painter can draw it, highlight_data() can mask it
# ---------------------------------------------------------------------------

test_that("highlight_data() masks an array of any rank", {
  m <- highlight_data(Titanic, rows = 1)
  expect_equal(dim(m), dim(Titanic))
  expect_type(m, "logical")
  expect_true(all(m[1, , , ]))
  expect_false(any(m[2, , , ]))

  # Selection BY NAME works -- the names are read off the data. The MASK itself
  # carries no dimnames, which is exactly the contract `highlight_data.matrix()` has
  # always had, so it is indexed by position.
  byname <- highlight_data(Titanic, rows = "Crew", columns = "Female")
  expect_null(dimnames(byname))
  expect_true(all(byname[4, , , ]))   # Crew
  expect_true(all(byname[, 2, , ]))   # Female
  expect_false(byname[1, 1, 1, 1])    # 1st / Male

  # A 3-D array.
  a <- array(1:24, c(2, 3, 4))
  expect_true(all(highlight_columns(a, 2)[, 2, ]))
  expect_equal(sum(highlight_rows(a, 1)), 12L)

  # A full coordinate reaches one cell.
  loc <- highlight_locations(a, rbind(c(2, 3, 4), c(1, 1, 1)))
  expect_true(loc[2, 3, 4])
  expect_true(loc[1, 1, 1])
  expect_equal(sum(loc), 2L)

  # A short coordinate is an error, not a wrong mask.
  expect_error(highlight_locations(a, rbind(c(2, 3))), "one coordinate per dimension")
})

test_that("Titanic is maskable because Titanic is paintable", {
  with_null_pdf(expect_silent(
    paint_array(Titanic, highlight_area = highlight_data(Titanic, rows = 1))
  ))
  cells <- paint_cells(Titanic, highlight_area = highlight_data(Titanic, rows = 1))
  v <- cells[cells$kind == "value", , drop = FALSE]
  expect_true(all(v$fill[v$i == 1L] == "lemonchiffon"))
  expect_true(all(v$fill[v$i != 1L] == "white"))
})

test_that("a rank-1 array is refused by highlight_data(), as no painter draws it", {
  expect_error(highlight_data(array(1:3), rows = 1), "dimension")
})

test_that("a wrongly shaped mask is an error that reports the real shape", {
  expect_error(
    paint_cells(array(1:24, c(2, 3, 4)), highlight_area = matrix(TRUE, 2, 3)),
    "2 by 3 by 4"
  )
  # A length-one logical is recycled, which preserves highlight_area = FALSE.
  expect_silent(paint_cells(array(1:24, c(2, 3, 4)), highlight_area = FALSE))
})

# ---------------------------------------------------------------------------
# the painters
# ---------------------------------------------------------------------------

test_that("paint_array() draws the canonical tables and returns a resolved table", {
  with_null_pdf(
    for (a in list(Titanic, HairEyeColor, UCBAdmissions, array(1:24, c(2, 3, 4)))) {
      r <- paint_array(a)
      expect_type(r, "list")
      expect_true(all(c("cells", "fontsize", "floored", "u") %in% names(r)))
      expect_false(r$floored)
    }
  )
})

test_that("the DEFAULT canonical arrays never warn", {
  # min_pt is a WARNING THRESHOLD, and a warning must always mean the caller raised
  # a cap. `max_slices = 4` is calibrated for exactly this.
  grDevices::pdf(NULL, width = 7, height = 5)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(paint_array(Titanic))
  expect_silent(paint_array(HairEyeColor))
  expect_silent(paint_array(UCBAdmissions))
  expect_silent(paint_array(array(1:24, c(2, 3, 4))))
})

test_that("the subtitle reports the array's full SHAPE, not two of its axes", {
  expect_equal(
    array_subtitle(Titanic),
    "Dimensions: 4 x 2 x 2 x 2 | Data Type: table"
  )
  # nrow()/ncol() are DEFINED for an array, so dims_subtitle() would not error --
  # it would lie.
  expect_equal(dims_subtitle(Titanic), "Dimensions: 4 rows x 2 columns | Data Type: table")

  # RANK TWO IS A MATRIX AND SAYS SO. `paint_array(m)` must draw the picture
  # `paint_matrix(m)` draws, chrome included -- so the rank test lives in ONE place
  # and `paint_size()` reserves the chrome the painter really draws.
  m <- matrix(1:6, nrow = 2)
  expect_equal(array_subtitle(m), dims_subtitle(m))
  expect_match(array_subtitle(m), "2 rows x 3 columns")

  with_null_pdf({
    r <- paint_array(Titanic)
    expect_match(r$graph_subtitle, "4 x 2 x 2 x 2")
    expect_equal(r$graph_title, "Data Object: Titanic")

    # NA opts out.
    expect_null(paint_array(Titanic, graph_subtitle = NA)$graph_subtitle)
  })
})

test_that("paint_size() answers for an array", {
  s <- paint_size(Titanic)
  expect_named(s, c("width", "height"))
  expect_true(all(s > 0))
  # And its subtitle is the array's, not the "4 rows x 2 columns" lie.
  expect_gt(paint_size(array(1:24, c(2, 3, 4)))[["width"]], 0)
})

test_that("gpaint_array() returns a ggplot", {
  skip_if_not_installed("ggplot2")
  p <- gpaint_array(Titanic)
  expect_s3_class(p, "ggplot")
  # `print()` with no device open would write an Rplots.pdf, which R CMD check finds.
  with_null_pdf(expect_silent(print(p)))
})

test_that("an empty or oversized array is refused", {
  expect_error(paint_cells(array(integer(0), c(0, 2, 2))), "empty")
  expect_error(
    paint_cells(array(1L, c(100, 100, 100)), show_all = TRUE),
    "100000 cells"
  )
})
