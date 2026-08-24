#!/usr/bin/env node

/**
 * Pins the #628 §5.5.3 transition-trace and §5.3 descriptor-derivation ExUnits
 * frontier against a fresh `aiken check` measurement at the GOAL_SPEC §3.3
 * basis — and pins the *marginal* validator cost, not just the whole-test
 * reading, because on this lane the difference between the two is the finding.
 *
 * **Why this exists.** The #628 review recorded that several transition-trace
 * tests read above the 13.2M memory basis and left the crossing for descriptor
 * derivation as an *interpolated* point between two golden vectors of different
 * shape. Neither claim was falsifiable: an interpolation across a shape change
 * is not a measurement, and an Aiken `test` reading charges fixture
 * construction — MPF roots, canonical CBOR, tx-id hashing, descriptor
 * derivation for the fixtures themselves — to the row, so a whole-test reading
 * over the basis does not by itself say the validator is over the basis. Under
 * the #519/#523 gate-that-cannot-fail doctrine, a figure quoted in a sign-off
 * summary has to be re-checkable.
 *
 * So every measured row on this lane comes in a pair. `..._derives` /
 * `deposit_arm_*` / `l2_no_op_arm` enter the code under measurement;
 * `..._fixture_only` builds the identical fixtures through the identical
 * builder and stops short of the entrypoint. The ledger's `marginal` block
 * names each pair, and this file recomputes the difference from the fresh
 * readings. The reader never has to take a subtraction on trust, and a baseline
 * that quietly stopped doing the fixture work shows up as a marginal figure
 * that moved.
 *
 * Aiken tests cannot observe their own execution units, so the pin lives one
 * level up, taken through `run-focused-check.mjs` (so a row for a test that did
 * not run, or one that did not pass, cannot be published).
 *
 * **Why it is not `exec-ledger-within-basis-v1.mjs`.** That module is the one
 * way a *within-basis* ledger is judged and admits only `within` rows by
 * construction. This lane publishes rows on both sides of the basis on purpose:
 * the whole-test readings that exceed it are the evidence the crossing is real,
 * and bending the shared module to carry them would weaken the lanes that
 * depend on its strictness. This file therefore reuses the two halves that must
 * not be retyped — `measureModule`, the one way a reading is taken, and
 * `GOAL_SPEC_EXECUTION_BASIS_V1`, the one place §3.3 lives — and makes its own
 * judgement, following the #633 evidence gate on branch `wave/lane-l` (pending
 * the owner's #633 ruling, so not yet in this tree), which established this
 * two-sided shape for exactly this situation.
 *
 * **What is checked.**
 *
 *   1. The ledger's declared basis is `GOAL_SPEC_EXECUTION_BASIS_V1`, taken
 *      from the shared module and never from the ledger being judged. A basis
 *      read out of the hand-edited file whose verdicts it decides is not a
 *      basis, and raising it is how a crossing quietly stops existing.
 *   2. Every row's raw reading matches the ledger to the unit.
 *   3. Every row sits on the side of the basis the ledger says it does, and
 *      movement fails in BOTH directions: a `within` row that stops fitting is
 *      a regression, and an `exceeds` row that starts fitting invalidates a
 *      published crossing.
 *   4. Every `exceeds` row declares its exception: `infeasibility` prose saying
 *      what cannot be done at that width and `ruling` naming who owns the
 *      resolution. Without that, "exceeds" becomes an acceptable steady state.
 *   5. Every row declares `kind` (`arm` for a row that enters the code under
 *      measurement, `baseline` for its fixture-only twin) and a non-negative
 *      integer `sizeBytes` naming the swept width. `sizeBytes` is the axis every
 *      crossing below is expressed on, and it is pinned in the measured modules
 *      themselves — the descriptor rows assert their exact output length and
 *      fail rather than be silently relabelled.
 *   6. Every measured module declares neutralisation selectors and they all ran,
 *      in the same invocation as the rows they guard. A cost row over an arm
 *      that accepts everything measures nothing.
 *   7. Every `marginal` entry is recomputed here in exact integer arithmetic
 *      from the fresh readings of its `arm` row and its `baseline` row. The
 *      difference must be positive — a baseline at or above its arm means the
 *      twin is not the twin — and the `netBasisFit` it is recorded under must
 *      agree with the arithmetic in both directions. A marginal figure that
 *      exceeds carries the same `infeasibility` + `ruling` obligation a raw row
 *      does. This is the block that answers whether an over-basis whole-test
 *      reading is an over-basis validator.
 *   8. Every `crossings` entry is recomputed from the fresh readings: the row it
 *      names as the last fit really measured `within`, the row it names as the
 *      first miss really measured `exceeds`, the miss is wider than the fit, no
 *      third row of the same `kind` in the same module lies between them (so the
 *      published bracket is genuinely ADJACENT and cannot be widened by deleting
 *      an inconvenient rung), and the recorded bracket width is the difference.
 *      A crossing whose two rows land on the same side of the basis fails rather
 *      than being re-pinned.
 *   9. Every `derived` entry — an interpolated point, the only kind of figure on
 *      this lane that is not read off a live test — is recomputed here in exact
 *      integer arithmetic from two measured rows that bracket it, must carry
 *      `derivation` prose and an explicit `label` marking it as not measured,
 *      and may never be published where a measured crossing exists for the same
 *      dimension. It is a convenience for the reader, never the headline.
 *  10. The run measured at least one row, recomputed at least one marginal
 *      figure and at least one crossing. A frontier pin that measures nothing,
 *      or that publishes no crossing, passes for free — the "gate that cannot
 *      fail" shape of issues #519 and #523.
 *
 * Usage, from `onchain/aiken/`:
 *
 *   MIDGARD_AIKEN_BIN=<fork> MIDGARD_AIKEN_ENV=testnet node scripts/verify-transition-trace-descriptor-exec-ledger-v1.mjs
 *   MIDGARD_AIKEN_BIN=<fork> MIDGARD_AIKEN_ENV=testnet node scripts/verify-transition-trace-descriptor-exec-ledger-v1.mjs --update
 *
 * `MIDGARD_AIKEN_ENV=testnet` is not optional. One pinned row here reads a
 * deposit's inclusion window, and `env/default.ak` and `env/testnet.ak` differ
 * in `shift_duration`, so the two environments are two programs and their
 * readings are not comparable. The ledger records the environment it was taken
 * under alongside the compiler.
 *
 * `--update` rewrites the raw rows, the recomputed marginal integers, the
 * recomputed crossing widths and the recomputed derived integers, and is the
 * only way to record a legitimate re-take. It never rewrites `basisFit`,
 * `netBasisFit`, `infeasibility`, `ruling`, `kind`, `sizeBytes`, `derivation`,
 * `label` or any `claim` prose: those are judgements, and a re-take must not be
 * able to launder one into the ledger. It is not a bypass — a selector that did
 * not run, a row that did not run, a missing exception declaration, a fresh
 * reading that contradicts a `basisFit`, a non-adjacent crossing or a
 * non-positive marginal figure all fail in update mode too, and the ledger is
 * not rewritten when they do.
 */

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { measureModule } from "./exec-ledger-measure-v1.mjs";
import { GOAL_SPEC_EXECUTION_BASIS_V1 } from "./exec-ledger-within-basis-v1.mjs";

const lane = "transition-trace / descriptor #628 ExUnits frontier";

/**
 * The swept width at which a two-point linear interpolation of `mem` against
 * `sizeBytes` first passes `basis`.
 *
 * BigInt throughout: the cpu readings run to 7e9 and the intermediate product
 * higher still, and a derived integer that depends on float rounding is one two
 * readers can disagree about. Solving `memLow + (s - sizeLow) * dm / ds <=
 * basis` for the largest integer `s`, with `dm > 0` and `ds > 0`, gives
 * `s = sizeLow + floor((basis - memLow) * ds / dm)`; the answer published is
 * the first `s` past it.
 *
 * Interpolation only, never extrapolation: the point must land inside the two
 * rows it is drawn between, which is what stops a derived figure from becoming
 * a projection off the end of the curve wearing an interpolation's label.
 *
 * @param {{sizeBytes: number, mem: number}} low
 * @param {{sizeBytes: number, mem: number}} high
 * @param {number} basis
 * @returns {{largestWithinBasisBytes: number, basisExhaustedAtBytes: number}}
 */
const interpolateBasisExhaustion = (low, high, basis) => {
  const ds = BigInt(high.sizeBytes - low.sizeBytes);
  const dm = BigInt(high.mem - low.mem);
  if (ds <= 0n || dm <= 0n) {
    throw new RangeError(
      "a two-point cost interpolation needs the higher row to carry both more bytes and more memory",
    );
  }
  if (BigInt(low.mem) > BigInt(basis)) {
    throw new RangeError(
      "a two-point cost interpolation needs its lower row to be within the basis",
    );
  }
  if (BigInt(high.mem) <= BigInt(basis)) {
    throw new RangeError(
      "a two-point cost interpolation needs its higher row to be over the basis, " +
        "otherwise the crossing it names lies outside the pair it is drawn between",
    );
  }
  const largest =
    BigInt(low.sizeBytes) + (BigInt(basis) - BigInt(low.mem)) * ds / dm;
  return {
    largestWithinBasisBytes: Number(largest),
    basisExhaustedAtBytes: Number(largest + 1n),
  };
};

/**
 * @param {object} options
 * @param {string} options.ledgerPath
 * @param {{memoryUnits: number, cpuUnits: number}} options.declaredBasis
 * @param {boolean} options.update
 * @param {(module: string, tests: string[]) => Map<string, {mem: number, cpu: number}>} [options.measure]
 * @param {{write: (chunk: string) => unknown}} [options.stdout]
 * @param {{write: (chunk: string) => unknown}} [options.stderr]
 * @returns {number} The process exit code the caller should exit with.
 */
export const checkTransitionTraceDescriptorExecLedger = ({
  ledgerPath,
  declaredBasis,
  update,
  measure = measureModule,
  stdout = process.stdout,
  stderr = process.stderr,
}) => {
  const ledger = JSON.parse(readFileSync(ledgerPath, "utf8"));

  // A **drift** is a measured number that no longer matches the recorded one —
  // what `--update` exists to absorb. A **structural** failure is anything else:
  // a selector or row that did not run, a basis that is not the one this lane is
  // judged at, a `basisFit` the measurement contradicts, an `exceeds` row that
  // declares no exception, a crossing that is not adjacent, a marginal figure
  // that is not positive. None of those is a number to re-pin.
  const failures = [];
  const drifts = [];
  const fail = (message) => failures.push(message);
  const drifted = (message) => drifts.push(message);

  const { memoryUnits, cpuUnits } = ledger.basis ?? {};
  if (
    memoryUnits !== declaredBasis.memoryUnits ||
    cpuUnits !== declaredBasis.cpuUnits
  ) {
    fail(
      `ledger basis mem=${String(memoryUnits)} cpu=${String(cpuUnits)} is not the ` +
        `basis this lane is judged at, mem=${String(declaredBasis.memoryUnits)} ` +
        `cpu=${String(declaredBasis.cpuUnits)}`,
    );
  }

  const modules = Array.isArray(ledger.modules) ? ledger.modules : null;
  if (modules === null) {
    fail("the ledger declares no `modules` array");
  }

  let rowCount = 0;
  // Fresh readings, keyed by row name across every module, so the blocks below
  // can pair rows regardless of which invocation measured them.
  const measured = new Map();

  for (const entry of modules ?? []) {
    // Every field is named before it is used: a malformed entry must be refused
    // by name rather than render as `undefined:` in every message below.
    const module =
      typeof entry.module === "string" ? entry.module : "<unnamed ledger entry>";
    const rows =
      typeof entry.rows === "object" &&
      entry.rows !== null &&
      !Array.isArray(entry.rows)
        ? entry.rows
        : null;
    if (rows === null) {
      fail(`${module}: ledger entry declares no \`rows\` object`);
      continue;
    }
    const neutralisation = Array.isArray(entry.neutralisation)
      ? entry.neutralisation
      : [];
    // An empty list would disarm check 6 silently, so it is refused.
    if (neutralisation.length === 0) {
      fail(`${module}: ledger entry declares no neutralisation selectors`);
      continue;
    }
    const readings = measure(module, [...Object.keys(rows), ...neutralisation]);
    for (const name of neutralisation) {
      if (!readings.has(name)) {
        fail(`${module}: neutralisation selector '${name}' did not run`);
      }
    }
    for (const [name, expected] of Object.entries(rows)) {
      const actual = readings.get(name);
      if (actual === undefined) {
        fail(`${module}: row '${name}' did not run`);
        continue;
      }
      rowCount += 1;

      if (expected.kind !== "arm" && expected.kind !== "baseline") {
        fail(
          `${module}: '${name}' has unexpected kind '${String(expected.kind)}' — ` +
            "every row on this lane is an 'arm' reading or its 'baseline' twin, " +
            "because the marginal cost is a difference between the two",
        );
        continue;
      }
      if (!Number.isInteger(expected.sizeBytes) || expected.sizeBytes < 0) {
        fail(
          `${module}: '${name}' declares no non-negative integer \`sizeBytes\` — ` +
            "the axis every crossing in this ledger is expressed on",
        );
        continue;
      }
      if (measured.has(name)) {
        fail(`row name '${name}' appears in more than one module`);
        continue;
      }
      measured.set(name, {
        module,
        kind: expected.kind,
        sizeBytes: expected.sizeBytes,
        mem: actual.mem,
        cpu: actual.cpu,
      });

      // Judged against the *fresh* reading in both modes and before any write: a
      // re-take that moves a row across the basis is a structural change to a
      // published crossing, not a number to re-pin.
      const within =
        actual.mem <= declaredBasis.memoryUnits &&
        actual.cpu <= declaredBasis.cpuUnits;
      if (expected.basisFit !== "within" && expected.basisFit !== "exceeds") {
        fail(
          `${module}: '${name}' has unexpected basisFit ` +
            `'${String(expected.basisFit)}' — every row is recorded 'within' or 'exceeds' ` +
            "so that movement across the basis fails in either direction",
        );
        continue;
      }
      if (expected.basisFit === "within" && !within) {
        fail(
          `${module}: '${name}' is recorded 'within' but measured ` +
            `mem=${String(actual.mem)} cpu=${String(actual.cpu)} exceeds the ` +
            `basis mem=${String(declaredBasis.memoryUnits)} cpu=${String(declaredBasis.cpuUnits)}`,
        );
        continue;
      }
      if (expected.basisFit === "exceeds" && within) {
        fail(
          `${module}: '${name}' is recorded 'exceeds' but measured ` +
            `mem=${String(actual.mem)} cpu=${String(actual.cpu)}, which is WITHIN the ` +
            "basis — a recorded crossing that moved is not drift. Re-read the " +
            "ledger's note and #628 before re-pinning anything.",
        );
        continue;
      }
      // An `exceeds` row with nowhere to point is how "exceeds" becomes a
      // steady state.
      if (expected.basisFit === "exceeds") {
        if (
          typeof expected.infeasibility !== "string" ||
          expected.infeasibility.trim() === ""
        ) {
          fail(
            `${module}: '${name}' exceeds the basis and declares no \`infeasibility\``,
          );
          continue;
        }
        if (
          typeof expected.ruling !== "string" ||
          expected.ruling.trim() === ""
        ) {
          fail(
            `${module}: '${name}' exceeds the basis and declares no \`ruling\` ` +
              "cross-reference naming who owns the resolution",
          );
          continue;
        }
      }

      if (update) {
        rows[name] = { ...expected, mem: actual.mem, cpu: actual.cpu };
        continue;
      }
      if (actual.mem !== expected.mem || actual.cpu !== expected.cpu) {
        drifted(
          `${module}: '${name}' drifted — ledger mem=${String(expected.mem)} cpu=${String(expected.cpu)}, ` +
            `measured mem=${String(actual.mem)} cpu=${String(actual.cpu)}`,
        );
      }
    }
  }

  // The marginal block: the reason this lane has baselines at all. A whole-test
  // reading is an upper bound on the validator's cost, never the cost itself;
  // subtracting the fixture-only twin is what licenses an arm-cost claim, and
  // recomputing the subtraction here is what stops the claim being prose.
  const marginal = Array.isArray(ledger.marginal) ? ledger.marginal : null;
  if (marginal === null) {
    fail("the ledger declares no `marginal` array");
  }
  let marginalCount = 0;
  for (const claim of marginal ?? []) {
    const label =
      typeof claim.claim === "string" ? claim.claim : "<unnamed marginal claim>";
    const arm = measured.get(claim.arm);
    const baseline = measured.get(claim.baseline);
    if (arm === undefined || baseline === undefined) {
      fail(
        `${label}: names rows '${String(claim.arm)}' and '${String(claim.baseline)}', ` +
          "and at least one is not a measured row here",
      );
      continue;
    }
    if (arm.kind !== "arm" || baseline.kind !== "baseline") {
      fail(
        `${label}: pairs kind '${arm.kind}' with kind '${baseline.kind}' — a marginal ` +
          "figure is an 'arm' row minus its 'baseline' twin, in that order",
      );
      continue;
    }
    if (arm.sizeBytes !== baseline.sizeBytes) {
      fail(
        `${label}: '${claim.arm}' is at ${String(arm.sizeBytes)} bytes but ` +
          `'${claim.baseline}' is at ${String(baseline.sizeBytes)} — a baseline that ` +
          "builds a different fixture is not a baseline",
      );
      continue;
    }
    const marginalMem = arm.mem - baseline.mem;
    const marginalCpu = arm.cpu - baseline.cpu;
    if (marginalMem <= 0 || marginalCpu <= 0) {
      fail(
        `${label}: marginal mem=${String(marginalMem)} cpu=${String(marginalCpu)} is not ` +
          "positive — the baseline is at or above its arm, so the twin is not " +
          "building the fixtures the arm builds",
      );
      continue;
    }
    marginalCount += 1;

    const netWithin =
      marginalMem <= declaredBasis.memoryUnits &&
      marginalCpu <= declaredBasis.cpuUnits;
    if (claim.netBasisFit !== "within" && claim.netBasisFit !== "exceeds") {
      fail(
        `${label}: unexpected netBasisFit '${String(claim.netBasisFit)}'`,
      );
      continue;
    }
    if (claim.netBasisFit === "within" && !netWithin) {
      fail(
        `${label}: recorded net-of-baseline 'within' but the difference ` +
          `mem=${String(marginalMem)} cpu=${String(marginalCpu)} exceeds the basis`,
      );
      continue;
    }
    if (claim.netBasisFit === "exceeds" && netWithin) {
      fail(
        `${label}: recorded net-of-baseline 'exceeds' but the difference ` +
          `mem=${String(marginalMem)} cpu=${String(marginalCpu)} is WITHIN the basis — ` +
          "this is the figure the sign-off summary quotes, so it fails rather " +
          "than being re-pinned.",
      );
      continue;
    }
    if (claim.netBasisFit === "exceeds") {
      if (
        typeof claim.infeasibility !== "string" ||
        claim.infeasibility.trim() === ""
      ) {
        fail(`${label}: net-of-baseline exceeds and declares no \`infeasibility\``);
        continue;
      }
      if (typeof claim.ruling !== "string" || claim.ruling.trim() === "") {
        fail(
          `${label}: net-of-baseline exceeds and declares no \`ruling\` ` +
            "cross-reference naming who owns the resolution",
        );
        continue;
      }
    }

    if (update) {
      claim.marginalMem = marginalMem;
      claim.marginalCpu = marginalCpu;
      continue;
    }
    if (claim.marginalMem !== marginalMem || claim.marginalCpu !== marginalCpu) {
      drifted(
        `${label}: recorded marginalMem=${String(claim.marginalMem)} ` +
          `marginalCpu=${String(claim.marginalCpu)}, recomputed from the fresh readings ` +
          `of '${claim.arm}' and '${claim.baseline}' as ${String(marginalMem)} / ${String(marginalCpu)}`,
      );
    }
  }

  // The crossings: the headline this lane exists to publish, and the one thing
  // in it that is a MEASUREMENT rather than a fit. Each names the widest rung
  // still inside the basis and the narrowest one outside it, and the check below
  // is what makes "adjacent" mean adjacent: deleting the rung that would have
  // tightened the bracket cannot widen a published one, because a third row of
  // the same kind lying between the two fails here.
  const crossings = Array.isArray(ledger.crossings) ? ledger.crossings : null;
  if (crossings === null) {
    fail("the ledger declares no `crossings` array");
  }
  let crossingCount = 0;
  const crossedDimensions = new Set();
  for (const crossing of crossings ?? []) {
    const label =
      typeof crossing.claim === "string" ? crossing.claim : "<unnamed crossing>";
    const lastFit = measured.get(crossing.withinRow);
    const firstMiss = measured.get(crossing.exceedsRow);
    if (lastFit === undefined || firstMiss === undefined) {
      fail(
        `${label}: names rows '${String(crossing.withinRow)}' and ` +
          `'${String(crossing.exceedsRow)}', and at least one is not a measured row here`,
      );
      continue;
    }
    if (lastFit.module !== firstMiss.module || lastFit.kind !== firstMiss.kind) {
      fail(
        `${label}: brackets a row in '${lastFit.module}' (${lastFit.kind}) against one in ` +
          `'${firstMiss.module}' (${firstMiss.kind}) — a crossing is read along one ` +
          "sweep, not across two",
      );
      continue;
    }
    const fitWithin =
      lastFit.mem <= declaredBasis.memoryUnits &&
      lastFit.cpu <= declaredBasis.cpuUnits;
    const missWithin =
      firstMiss.mem <= declaredBasis.memoryUnits &&
      firstMiss.cpu <= declaredBasis.cpuUnits;
    if (!fitWithin || missWithin) {
      fail(
        `${label}: '${crossing.withinRow}' measured mem=${String(lastFit.mem)} and ` +
          `'${crossing.exceedsRow}' measured mem=${String(firstMiss.mem)} — the pair no ` +
          "longer straddles the basis, so the crossing it publishes does not exist",
      );
      continue;
    }
    if (firstMiss.sizeBytes <= lastFit.sizeBytes) {
      fail(
        `${label}: the first row over the basis is at ${String(firstMiss.sizeBytes)} bytes, ` +
          `not wider than the last row within it at ${String(lastFit.sizeBytes)}`,
      );
      continue;
    }
    const between = [...measured.entries()].filter(
      ([, row]) =>
        row.module === lastFit.module &&
        row.kind === lastFit.kind &&
        row.sizeBytes > lastFit.sizeBytes &&
        row.sizeBytes < firstMiss.sizeBytes,
    );
    if (between.length > 0) {
      fail(
        `${label}: rows ${between.map(([name]) => `'${name}'`).join(", ")} lie between the ` +
          "bracket's ends, so the bracket is not adjacent and the interval it " +
          "publishes is wider than the evidence supports",
      );
      continue;
    }
    if (
      typeof crossing.dimension !== "string" ||
      crossing.dimension.trim() === ""
    ) {
      fail(`${label}: declares no \`dimension\` naming what was swept`);
      continue;
    }
    crossedDimensions.add(crossing.dimension);
    crossingCount += 1;

    const bracketWidthBytes = firstMiss.sizeBytes - lastFit.sizeBytes;
    if (update) {
      crossing.withinSizeBytes = lastFit.sizeBytes;
      crossing.exceedsSizeBytes = firstMiss.sizeBytes;
      crossing.bracketWidthBytes = bracketWidthBytes;
      continue;
    }
    if (
      crossing.withinSizeBytes !== lastFit.sizeBytes ||
      crossing.exceedsSizeBytes !== firstMiss.sizeBytes ||
      crossing.bracketWidthBytes !== bracketWidthBytes
    ) {
      drifted(
        `${label}: recorded bracket [${String(crossing.withinSizeBytes)}, ` +
          `${String(crossing.exceedsSizeBytes)}] width ${String(crossing.bracketWidthBytes)}, ` +
          `recomputed from the ledger's rows as [${String(lastFit.sizeBytes)}, ` +
          `${String(firstMiss.sizeBytes)}] width ${String(bracketWidthBytes)}`,
      );
    }
  }

  // Derived (interpolated) points. Kept only as a reader's convenience, held to
  // the arithmetic, and forbidden wherever a measured crossing already covers
  // the same dimension — which is the discipline #628's interpolated ~1,680-byte
  // figure failed.
  let derivedCount = 0;
  const derived = Array.isArray(ledger.derived) ? ledger.derived : [];
  for (const claim of derived) {
    const label =
      typeof claim.claim === "string" ? claim.claim : "<unnamed derivation>";
    const low = measured.get(claim.from);
    const high = measured.get(claim.to);
    if (low === undefined || high === undefined) {
      fail(
        `${label}: names rows '${String(claim.from)}' and '${String(claim.to)}', ` +
          "and at least one is not a measured row here",
      );
      continue;
    }
    if (typeof claim.label !== "string" || !claim.label.includes("NOT MEASURED")) {
      fail(
        `${label}: an interpolated point must carry a \`label\` saying in so many ` +
          "words that it is NOT MEASURED, or a reader will quote it as a reading",
      );
      continue;
    }
    if (typeof claim.derivation !== "string" || claim.derivation.trim() === "") {
      fail(
        `${label}: declares no \`derivation\` prose recording how the figure was ` +
          "obtained and how to reproduce the readings it is drawn from",
      );
      continue;
    }
    if (
      typeof claim.dimension === "string" &&
      crossedDimensions.has(claim.dimension)
    ) {
      fail(
        `${label}: publishes an interpolated point for dimension ` +
          `'${claim.dimension}', which already has a MEASURED crossing — the ` +
          "interpolation may not stand where a measurement exists",
      );
      continue;
    }
    let recomputed;
    try {
      recomputed = interpolateBasisExhaustion(low, high, declaredBasis.memoryUnits);
    } catch (error) {
      fail(`${label}: ${error.message}`);
      continue;
    }
    derivedCount += 1;
    if (update) {
      claim.largestWithinBasisBytes = recomputed.largestWithinBasisBytes;
      claim.basisExhaustedAtBytes = recomputed.basisExhaustedAtBytes;
      continue;
    }
    if (
      claim.largestWithinBasisBytes !== recomputed.largestWithinBasisBytes ||
      claim.basisExhaustedAtBytes !== recomputed.basisExhaustedAtBytes
    ) {
      drifted(
        `${label}: recorded largestWithinBasisBytes=${String(claim.largestWithinBasisBytes)} ` +
          `basisExhaustedAtBytes=${String(claim.basisExhaustedAtBytes)}, recomputed from the ` +
          `fresh readings of '${claim.from}' and '${claim.to}' as ` +
          `${String(recomputed.largestWithinBasisBytes)} / ${String(recomputed.basisExhaustedAtBytes)}`,
      );
    }
  }

  // Three ways to reach here having judged nothing: an empty `modules` array,
  // one whose every entry carries an empty `rows` object, and a ledger with no
  // marginal or crossing blocks at all. All of them would otherwise reach the
  // success write reporting zeroes.
  if (rowCount === 0) {
    fail(
      "the ledger produced no measured rows at all — an execution pin that " +
        "measures nothing passes for free",
    );
  }
  if (marginalCount === 0) {
    fail(
      "the ledger recomputed no marginal figure at all — every whole-test " +
        "reading would then be quoted as a validator cost, which is the #628 " +
        "review's own criticism of the ungated evidence",
    );
  }
  if (crossingCount === 0) {
    fail(
      "the ledger recomputed no measured crossing at all — the frontier this " +
        "lane publishes would then rest on interpolation",
    );
  }

  if (failures.length > 0) {
    for (const failure of failures) {
      stderr.write(`${failure}\n`);
    }
    stderr.write(
      `\n${lane} execution ledger: ${String(failures.length)} structural failure(s). ` +
        (update
          ? "The ledger was NOT rewritten. `--update` absorbs measurement drift and nothing else."
          : "These are not re-takeable numbers; resolve them in the source or in the ledger.") +
        "\n",
    );
    return 1;
  }

  if (update) {
    writeFileSync(ledgerPath, `${JSON.stringify(ledger, null, 2)}\n`);
    stdout.write(
      `${JSON.stringify({ status: "updated", ledger: ledgerPath }, null, 2)}\n`,
    );
    return 0;
  }

  if (drifts.length > 0) {
    for (const drift of drifts) {
      stderr.write(`${drift}\n`);
    }
    stderr.write(
      `\n${lane} execution ledger: ${String(drifts.length)} drift(s). ` +
        "If the re-take is legitimate, re-run with --update.\n",
    );
    return 1;
  }

  stdout.write(
    `${JSON.stringify(
      {
        status: "pass",
        rows: rowCount,
        marginal: marginalCount,
        crossings: crossingCount,
        derived: derivedCount,
        basis: {
          memoryUnits: declaredBasis.memoryUnits,
          cpuUnits: declaredBasis.cpuUnits,
        },
      },
      null,
      2,
    )}\n`,
  );
  return 0;
};

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

// Guarded so the self-test can import the checker without the module body
// running a live measurement as a side effect of the import.
if (process.argv[1] === fileURLToPath(import.meta.url)) {
  // Set rather than `process.exit(code)`: the checker is synchronous and has
  // nothing pending, so letting the process end on its own cannot truncate the
  // report it just wrote.
  process.exitCode = checkTransitionTraceDescriptorExecLedger({
    ledgerPath: resolve(
      scriptDirectory,
      "transition-trace-descriptor-exec-ledger-v1.json",
    ),
    declaredBasis: GOAL_SPEC_EXECUTION_BASIS_V1,
    update: process.argv.includes("--update"),
  });
}
