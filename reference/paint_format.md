# Format a vector for painting

Turns the values of one formatting unit into the spans a painter draws.
A formatting unit is a whole matrix or vector, or one column of a data
frame; `paint_format()` is called once per unit, which is exactly why
the same value looks identical in every cell of a matrix.

## Usage

``` r
paint_format(
  x,
  sigfig = 3L,
  max_chars = 12L,
  max_dec_width = 13L,
  subtle_digits = c("insignificant", "rounded", "none"),
  ellipsis = "...",
  ...
)

# S3 method for class 'double'
paint_format(
  x,
  sigfig = 3L,
  max_chars = 12L,
  max_dec_width = 13L,
  subtle_digits = c("insignificant", "rounded", "none"),
  ellipsis = "...",
  ...
)

# S3 method for class 'integer'
paint_format(
  x,
  sigfig = 3L,
  max_chars = 12L,
  max_dec_width = 13L,
  subtle_digits = c("insignificant", "rounded", "none"),
  ellipsis = "...",
  ...
)

# S3 method for class 'character'
paint_format(
  x,
  sigfig = 3L,
  max_chars = 12L,
  max_dec_width = 13L,
  subtle_digits = c("insignificant", "rounded", "none"),
  ellipsis = "...",
  ...
)

# S3 method for class 'logical'
paint_format(
  x,
  sigfig = 3L,
  max_chars = 12L,
  max_dec_width = 13L,
  subtle_digits = c("insignificant", "rounded", "none"),
  ellipsis = "...",
  ...
)

# S3 method for class 'factor'
paint_format(
  x,
  sigfig = 3L,
  max_chars = 12L,
  max_dec_width = 13L,
  subtle_digits = c("insignificant", "rounded", "none"),
  ellipsis = "...",
  ...
)

# S3 method for class 'list'
paint_format(
  x,
  sigfig = 3L,
  max_chars = 12L,
  max_dec_width = 13L,
  subtle_digits = c("insignificant", "rounded", "none"),
  ellipsis = "...",
  ...
)

# Default S3 method
paint_format(
  x,
  sigfig = 3L,
  max_chars = 12L,
  max_dec_width = 13L,
  subtle_digits = c("insignificant", "rounded", "none"),
  ellipsis = "...",
  ...
)
```

## Arguments

- x:

  A vector.

- sigfig:

  Significant digits to show in black. Must be in `1:15`.

- max_chars:

  Strings longer than this are truncated with `ellipsis`.

- max_dec_width:

  Widest fixed-notation token tolerated before the unit flips to
  scientific notation.

- subtle_digits:

  Which digits are returned in the grey `insig` span. `"insignificant"`
  greys everything past the `sigfig`-th significant digit, exact or not
  (so `100000` is black `100` plus grey `000`). `"rounded"` greys only
  values whose token actually lost precision (so `100000` is entirely
  black, but `123456.789` still greys `457.`). `"none"` returns an empty
  `insig` for every value.

- ellipsis:

  The truncation marker. Keep it ASCII unless you know your device can
  encode the alternative.

- ...:

  Passed to methods.

## Value

A data frame with one row per element of `x` and columns:

- `sig`:

  the black span

- `insig`:

  the grey span, `""` when nothing is insignificant

- `head`:

  the token up to the decimal point

- `tail`:

  the decimal point and everything after it

- `ink`:

  `"black"` for a finite value, `"blue"` for `Inf`/`NaN`, `"red"` for
  `NA`

- `align`:

  `"decimal"`, `"left"` or `"right"`

with attributes `tag` (the type tag) and `sci` (did the unit flip to
scientific notation?).

## Details

Numbers are shown at `sigfig` significant digits. The digits past the
`sigfig`-th are not discarded – they are returned separately in `insig`,
so the renderer can draw them in grey. `paste0(sig, insig)` is always
exactly the token that gets drawn.

The whole unit renders in fixed notation, or the whole unit renders in
scientific notation. The decision is made on **width**, not magnitude:
if the widest fixed token would exceed `max_dec_width` characters, the
unit flips. In scientific mode `insig` is always `""`, because every
digit of a scientific token, exponent included, is significant.
