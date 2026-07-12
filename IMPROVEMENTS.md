# IMPROVEMENTS.md

*Analysis date: 2026-07-11*

ultra-ml is an experimental Haskell (literate Haskell, cabal-managed) library and
executable for machine learning that minimises ultrametric loss functions —
p-adic distance and polynomial-degree metrics — with a toy decision-tree trainer
over the "Zorgette" example dataset. The project is at proof-of-concept stage:
the last commit ("At least it compiles into an executable now") got `Main.lhs`
building, but the working tree is dirty, there are no tests, and a 1.5 GB
TinyStories tarball is sitting untracked in the repo root.

## Bugs & Fixes

- **Widespread partial functions.** `src/RegionSlices.lhs` alone has 8 `error`
  calls (null regions, sanity checks, `bruteForceFinder` on empty list, "inside
  multiple regions"), plus more in `Padic.lhs` (division by zero, mixed-prime
  comparison) and `Datasets.lhs` (`selectListElements` length mismatch). Replace
  with `Either`/`Maybe` returns or smart constructors so a bad training input
  produces a recoverable result instead of a runtime crash.
- **`Main.lhs` is a stub.** It prints "Starting", trains on the hard-coded
  `zorgette` dataset, and prints "Ending" — the trained tree (`dectree`) is
  bound but never inspected or shown. Print/score the result, and add CLI
  arguments for choosing dataset and metric.
- **Dead/dangling modules.** `src/Field.lhs` and `src/RegionSelection.lhs`
  exist but are not in the cabal `exposed-modules` list — either wire them in
  or delete them. `RegionSelection.lhs` is only 20 lines and looks unfinished.

## Housekeeping (do this first)

- **Dirty working tree.** `README.md` is modified but uncommitted; `LICENSE`,
  `Setup.hs` are untracked despite `license-file: LICENSE` being declared in
  the cabal file — commit them.
- **1.5 GB `TinyStories_all_data.tar.gz` in the repo root.** Do NOT commit it.
  Add it (plus `dist-newstyle/`, `*.swp`, `*~`) to a `.gitignore` — the repo
  currently has none. There is also a stray `.ultra-ml.cabal.swp` vim swap file.
- **Editor backup litter.** Eight `*.lhs~` files in `src/` should be deleted
  and ignored.
- **Nothing in the code actually uses TinyStories yet** — the README note is
  the only reference. Either add the ingestion code (the cabal file already
  depends on `sqlite-simple`, presumably for this) or drop the dependency and
  the tarball.

## Improvements

- **Pin the build.** `cabal-version: >=1.10` with unbounded `base` and
  `sqlite-simple` deps means builds will bit-rot. Add version bounds, a
  `tested-with:` GHC version, and consider a `cabal.project` freeze file.
- **`sqlite-simple` is declared but apparently unused** in the library modules;
  remove it or use it for dataset storage as intended.
- **Type signatures.** The `sweep.yaml` rule ("If a Haskell function doesn't
  have a type signature, add one") suggests some are missing — enable
  `-Wall -Wmissing-signatures` in `ghc-options` and fix warnings. Sweep.dev
  itself is a defunct service; `sweep.yaml` can probably be deleted.
- **`train_decision_tree` takes the same calculator twice** in `Main.lhs`
  (`padic_distance_calculator` passed for both arguments) — if that's the split
  metric vs. loss metric, demonstrate a mixed case (p-adic split, polynomial
  loss) since that combination is the whole point of the project.

## Testing

- **No test suite at all.** Add a `test-suite` stanza (tasty/hspec +
  QuickCheck). p-adic valuation/distance and ultrametric axioms
  (`d(x,z) <= max(d(x,y), d(y,z))`) are ideal QuickCheck properties;
  `RegionSlices` sanity checks should become property tests instead of
  runtime `error`s.
- Add a GitHub Actions workflow (`haskell-actions/setup`) to build and test on
  push — `sweep.yaml` says `gha_enabled: True` but there is no `.github/`.

## Documentation

- README is two lines plus a personal note. Add: what an ultrametric loss is
  and why (the elevator pitch), how to build (`cabal build`), how to run the
  demo, and a module map (what Zorgette, Padic, Polynomial, RegionSlices each
  do). Fill in the cabal `synopsis`/`description`/`license` fields.
- Commit messages are fun but uninformative ("Cute. Working nicely.") — a
  CHANGELOG.md exists; keep it updated instead.

## Quick Wins

1. `git add LICENSE Setup.hs README.md`, commit; add `.gitignore` covering
   `dist-newstyle/`, `*.tar.gz`, `*~`, `.*.swp`; delete the `~` and `.swp` files.
2. Add `ghc-options: -Wall` to both stanzas and fix the warnings.
3. Make `main` print the trained decision tree so running the executable shows
   something useful.
4. Register or remove `Field.lhs` / `RegionSelection.lhs` in the cabal file.

(Note: this is a Haskell project, so the usual "migrate requirements.txt to uv"
recommendation does not apply here.)
