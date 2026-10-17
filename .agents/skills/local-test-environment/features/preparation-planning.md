# Preparation planning

## Sub-features

- Inspect ordered build prerequisites and their freshness.
- Distinguish ordinary preparation from explicit source-only preparation.
- Refuse an unknown package without preparing another one.

## How to get to it (user POV)

Run `node scripts/contrib.mjs prepare --package midgard-node --plan` from the
checkout. The command prints a plan and exits; it does not build prerequisites.

## Driving it with contrib

After the [control doctor](README.md), choose a unique evidence directory and
run the command above with `--output <evidence-directory>/plan.json`.
Check exit 0, `schema: midgard-contrib-plan/v1`, `package: midgard-node`,
`sourceOnly: false`, and the declared prerequisite order/freshness. Compare
those prerequisites with the package recipe at the tested ref. [review]

Capture build-output/stamp state before and after; a plan must leave it
unchanged. Run the same command with a nonexistent package and confirm a
nonzero exit with a package diagnostic. Preserve both outputs. [review]

## Gotchas

A stale or missing prerequisite is useful plan output, not a failed build.
`--output` writes the requested evidence JSON. `--execute` performs preparation
and is a different feature. Planning success says nothing about suite results.
