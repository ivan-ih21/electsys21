# electsys21

> Voting Methods for Ranked, Rated and Approval Ballots

`electsys21` is an R package that determines election winners using a range of
voting methods. The functions accept several ballot formats — rankings,
cardinal utilities (scores) and approvals — with automatic detection of the
input type, configurable tie-breaking and tidy summaries of the results.

## Installation

You can install the development version from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("ivan-ih21/electsys21")
```

## Quick start

```r
library(electsys21)

# Generate 20 ranking ballots over 4 candidates
ballots <- gen_ranks(n_voters = 20, n_candidates = 4, seed = 1)

# Run a First-Past-The-Post election
fptp(ballots)
```

Every method returns a result object; print it or call `summary()` on it for a
formatted results table.

## Voting methods

| Function               | Method                          |
|------------------------|---------------------------------|
| `fptp()`               | First-Past-The-Post             |
| `tworound()`           | Two-Round System                |
| `irv()`                | Instant-Runoff Voting           |
| `borda()`              | Borda Count                     |
| `condorcet()`          | Condorcet Method                |
| `approval()`           | Approval Voting                 |
| `majority_judgement()` | Majority Judgment               |
| `d21()`                | D21 Method                      |
| `d21_minus()`          | D21 Method with minus votes     |

## Ballot formats

The voting functions accept a matrix (or data frame) with one row per voter and
one column per candidate. Three input types are recognised and detected
automatically (or set explicitly via `type`):

- **rank** — integer ranks, `1` = most preferred;
- **utility** — cardinal scores, higher = more preferred;
- **approval** — a logical or `{0, 1}` matrix.

`majority_judgement()` additionally accepts a **score** type, where each entry
is treated directly as an integer grade.

The helpers `gen_ranks()`, `gen_utilities()` and `gen_approvals()` generate
random ballots for testing and demonstration.

## License

MIT © Ivan Iakimov
