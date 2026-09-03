import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

/**
 * Pins the #633 native-script scan fault-proof step ExUnits against a COMMITTED
 * artifact, the way `onchain/aiken/scripts/verify-native-script-scan-exec-ledger-v1.mjs`
 * pins the aiken-side one-shot predicate.
 *
 * **Why this exists.** The emulator benchmark it serves
 * (`tests/native-script-scan-fault-proof-exunits-emulator.test.ts`) used to
 * spend ~858 seconds measuring the live staged scan and then assert only that
 * every reading was positive, writing an artifact solely when
 * `MIDGARD_SCAN_BENCH_OUT` was set. Under the default `pnpm test` that is 14
 * minutes of measurement discarded and a gate that cannot fail: any cost, at any
 * multiple of the §3.3 basis, at any transaction size, passed. Moving the
 * benchmark into an opt-in evidence lane makes it cheaper to run the default
 * suite, and that must not mean measuring less — so the readings now gate
 * against a pinned ledger instead of against zero.
 *
 * **Why exact equality is the right comparison here.** The aiken-side ledger
 * compares `aiken check` readings to the unit, and so does this one, for the
 * same reason: the numbers are deterministic. The lucid-evolution `Emulator`
 * evaluates the applied validator locally (`complete({ localUPLCEval: true })`)
 * against a fixed `PROTOCOL_PARAMETERS_DEFAULT` cost model, over a transaction
 * built from fixed keys, fixed out-refs and a fixed blueprint. There is no
 * network, no wall clock and no randomness in the measured path, so a moved
 * reading means the blueprint, the trace builder, the transaction shape or the
 * cost model moved — every one of which is a thing this lane exists to notice.
 * A tolerance band would convert exactly those signals into noise.
 *
 * **What is a drift and what is structural.** A **drift** is a raw reading that
 * no longer matches the ledger — `mem`, `cpu`, `fee`, `completeSignedBytes` —
 * plus the two integers recomputed from them. That is what
 * `MIDGARD_SCAN_BENCH_UPDATE=1` exists to absorb. Everything else is
 * **structural** and is refused in both modes: a curve point or row that
 * appeared or vanished (which is how an env filter would otherwise re-pin a
 * truncated ledger), a declared basis that is not the one this lane is judged
 * at, a `stage`/`nativeStage`/`stepIndex`/`stackDepth`/`proofStepCounts`/
 * payload size that moved (the trace shape changed — not a number to re-pin), a
 * `basisFit`/`l1Fit` the fresh reading contradicts, or an exceeding row that
 * declares no exception. None of those is a re-takeable number.
 *
 * **One deliberate divergence from the aiken verifier.** That one refuses to
 * write when a structural failure is found. Here the readings cost ~858 seconds
 * of emulator time, so a BOOTSTRAP write (no committed artifact yet) writes the
 * measured file first and *then* fails red naming the rows whose
 * `infeasibility`/`ruling` prose a human still owes. The numbers are never lost;
 * the lane is still red until the judgements are written by hand. Re-takes over
 * an existing ledger keep the aiken behaviour exactly: nothing is written when a
 * structural failure is found.
 */

/** The §3.3 per-transaction execution basis the #633 measurements are read against. */
export const SCAN_BENCH_EXECUTION_BASIS = {
  memoryUnits: 13_200_000n,
  cpuUnits: 8_000_000_000n,
  source:
    "GOAL_SPEC §3.3 — 20% margin off the supported L1 floor " +
    "MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits of 16,500,000 mem. " +
    "The same basis onchain/aiken/scripts/exec-ledger-within-basis-v1.mjs declares " +
    "as GOAL_SPEC_EXECUTION_BASIS_V1; never read out of the ledger it judges.",
} as const;

export const SCAN_BENCH_LEDGER_PATH = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../../evidence/native-script-scan-fault-proof-exunits-v1.json",
);

/** The env vars that narrow what the benchmark measures. */
export const SCAN_BENCH_FILTER_ENV = [
  "MIDGARD_SCAN_BENCH_SHAPES",
  "MIDGARD_SCAN_BENCH_NODES",
  "MIDGARD_SCAN_BENCH_STEPS",
  "MIDGARD_SCAN_BENCH_MAXFIT",
] as const;

export type ScanBenchProofStepCounts = {
  readonly total: number;
  readonly nativeToken: number;
  readonly nativeFrame: number;
  readonly nativeFinalize: number;
};

/** One fresh reading, as the benchmark produces it. */
export type ScanBenchStepReading = {
  readonly label: string;
  readonly stepIndex: number;
  readonly stage: number;
  readonly nativeStage: number | null;
  readonly stackDepth: number | null;
  readonly mem: bigint;
  readonly cpu: bigint;
  readonly fee: bigint;
  readonly completeSignedBytes: number;
};

/** One fresh curve point, as the benchmark produces it. */
export type ScanBenchCurveReading = {
  readonly shape: string;
  readonly nodes: number;
  readonly payloadBytes: number;
  readonly spentOutputBytes: number;
  readonly proofStepCounts: ScanBenchProofStepCounts;
  readonly measurements: readonly ScanBenchStepReading[];
};

type LedgerRow = {
  stepIndex: number;
  stage: number;
  nativeStage: number | null;
  stackDepth: number | null;
  mem: string;
  cpu: string;
  fee: string;
  completeSignedBytes: number;
  l1ByteMargin: number;
  memBasisShareBasisPoints: number;
  basisFit: string;
  l1Fit: string;
  infeasibility?: string | null;
  ruling?: string | null;
};

type LedgerCurvePoint = {
  shape: string;
  nodes: number;
  payloadBytes: number;
  spentOutputBytes: number;
  proofStepCounts: ScanBenchProofStepCounts;
  rows: Record<string, LedgerRow>;
};

export type ScanBenchLedger = {
  measurement: string;
  issue: string;
  spec: string;
  note: string;
  basis: {
    memoryUnits: number;
    cpuUnits: number;
    maxL1ProofTxBytes: number;
    maxOutputCanonicalCborBytes: number;
    source: string;
  };
  curvePoints: LedgerCurvePoint[];
};

const curveKey = (shape: string, nodes: number): string =>
  `${shape}:${nodes.toString()}`;

/**
 * `mem` as a share of the memory basis, in exact-integer basis points. Recorded
 * so the "×basis" figure this lane is quoted as is recomputable by a reader
 * rather than taken on trust, and so the memory basis participates in the
 * ledger rather than only in a console table.
 */
const memBasisShareBasisPoints = (mem: bigint, basisMemory: bigint): number =>
  Number((mem * 10_000n) / basisMemory);

export const readScanBenchLedger = (
  ledgerPath: string = SCAN_BENCH_LEDGER_PATH,
): ScanBenchLedger | null => {
  if (!existsSync(ledgerPath)) return null;
  return JSON.parse(readFileSync(ledgerPath, "utf8")) as ScanBenchLedger;
};

export const writeScanBenchLedger = (
  ledgerPath: string,
  ledger: ScanBenchLedger,
): void => {
  mkdirSync(dirname(ledgerPath), { recursive: true });
  writeFileSync(ledgerPath, `${JSON.stringify(ledger, null, 2)}\n`);
};

export type ScanBenchLedgerBasis = {
  readonly memoryUnits: bigint;
  readonly cpuUnits: bigint;
  readonly maxL1ProofTxBytes: number;
  readonly maxOutputCanonicalCborBytes: number;
  readonly source: string;
};

export type ScanBenchLedgerVerdict = {
  /** Not re-takeable: resolve these in the source or in the ledger. */
  readonly failures: readonly string[];
  /** Re-takeable readings: what `MIDGARD_SCAN_BENCH_UPDATE=1` absorbs. */
  readonly drifts: readonly string[];
  /** Rows compared (or written). Zero is itself a failure. */
  readonly rowCount: number;
  /** The ledger to write, in update/bootstrap mode; `null` otherwise. */
  readonly updated: ScanBenchLedger | null;
  /** True when there was no committed artifact and one was synthesised. */
  readonly bootstrapped: boolean;
};

const rowFromReading = (
  reading: ScanBenchStepReading,
  basis: ScanBenchLedgerBasis,
  judgements: Pick<LedgerRow, "basisFit" | "l1Fit"> &
    Partial<Pick<LedgerRow, "infeasibility" | "ruling">>,
): LedgerRow => ({
  stepIndex: reading.stepIndex,
  stage: reading.stage,
  nativeStage: reading.nativeStage,
  stackDepth: reading.stackDepth,
  mem: reading.mem.toString(),
  cpu: reading.cpu.toString(),
  fee: reading.fee.toString(),
  completeSignedBytes: reading.completeSignedBytes,
  l1ByteMargin: basis.maxL1ProofTxBytes - reading.completeSignedBytes,
  memBasisShareBasisPoints: memBasisShareBasisPoints(
    reading.mem,
    basis.memoryUnits,
  ),
  basisFit: judgements.basisFit,
  l1Fit: judgements.l1Fit,
  ...(judgements.infeasibility === undefined
    ? {}
    : { infeasibility: judgements.infeasibility }),
  ...(judgements.ruling === undefined ? {} : { ruling: judgements.ruling }),
});

/**
 * Judge fresh readings against the committed ledger.
 *
 * `ledger === null` means no committed artifact: in `update` mode one is
 * synthesised from the readings (see the bootstrap note in the module comment),
 * and in compare mode that is a single, explicit failure telling the reader
 * which lane to run.
 */
export const checkScanBenchLedger = ({
  ledger,
  readings,
  basis,
  update,
  filtersInEffect = [],
}: {
  readonly ledger: ScanBenchLedger | null;
  readonly readings: readonly ScanBenchCurveReading[];
  readonly basis: ScanBenchLedgerBasis;
  readonly update: boolean;
  readonly filtersInEffect?: readonly string[];
}): ScanBenchLedgerVerdict => {
  const failures: string[] = [];
  const drifts: string[] = [];
  let rowCount = 0;

  const withinBasis = (mem: bigint, cpu: bigint): boolean =>
    mem <= basis.memoryUnits && cpu <= basis.cpuUnits;
  const withinL1 = (bytes: number): boolean => bytes <= basis.maxL1ProofTxBytes;

  if (ledger === null) {
    if (!update) {
      return {
        failures: [
          "no committed artifact at " +
            `${SCAN_BENCH_LEDGER_PATH} — run the update lane ` +
            "(`pnpm --dir demo/midgard-validation run test:evidence:update`, i.e. " +
            "MIDGARD_VALIDATION_EVIDENCE=1 MIDGARD_SCAN_BENCH_UPDATE=1) to record " +
            "the readings, write the `infeasibility`/`ruling` prose for every row " +
            "that exceeds the basis or the L1 cap, and commit the artifact. Until " +
            "then this benchmark measures without pinning, which is the " +
            "gate-that-cannot-fail shape it was moved into the evidence lane to " +
            "close.",
        ],
        drifts: [],
        rowCount: 0,
        updated: null,
        bootstrapped: false,
      };
    }
    // Bootstrap must not be able to pin a truncated ledger: a narrowed run
    // would record whatever it happened to measure as the whole of what this
    // lane measures, and every later compare would then pass on a subset.
    if (filtersInEffect.length > 0) {
      return {
        failures: [
          "refusing to bootstrap the artifact with benchmark filters in " +
            `effect (${filtersInEffect.join(", ")}) — the first committed ` +
            "ledger defines what this lane measures, and a narrowed run would " +
            "pin a subset as the whole of it. Re-run the update lane with none " +
            "of these set.",
        ],
        drifts: [],
        rowCount: 0,
        updated: null,
        bootstrapped: false,
      };
    }

    const owed: string[] = [];
    const curvePoints: LedgerCurvePoint[] = readings.map((curve) => {
      const rows: Record<string, LedgerRow> = {};
      for (const reading of curve.measurements) {
        const basisFit = withinBasis(reading.mem, reading.cpu)
          ? "within"
          : "exceeds";
        const l1Fit = withinL1(reading.completeSignedBytes)
          ? "within"
          : "exceeds";
        if (basisFit === "exceeds" || l1Fit === "exceeds") {
          owed.push(
            `${curveKey(curve.shape, curve.nodes)} '${reading.label}' ` +
              `(basisFit=${basisFit}, l1Fit=${l1Fit})`,
          );
        }
        rows[reading.label] = rowFromReading(reading, basis, {
          basisFit,
          l1Fit,
          ...(basisFit === "exceeds" || l1Fit === "exceeds"
            ? { infeasibility: null, ruling: null }
            : {}),
        });
        rowCount += 1;
      }
      return {
        shape: curve.shape,
        nodes: curve.nodes,
        payloadBytes: curve.payloadBytes,
        spentOutputBytes: curve.spentOutputBytes,
        proofStepCounts: curve.proofStepCounts,
        rows,
      };
    });

    if (rowCount === 0) {
      // Nothing to protect and nothing to record: an empty ledger would be a
      // pin that measures nothing, and committing one is worse than having none.
      return {
        failures: [
          "the benchmark produced no measured rows at all — an execution pin " +
            "that measures nothing passes for free, so no artifact was written",
        ],
        drifts: [],
        rowCount: 0,
        updated: null,
        bootstrapped: false,
      };
    }
    if (owed.length > 0) {
      failures.push(
        `${owed.length.toString()} bootstrapped row(s) exceed the basis or the ` +
          "L1 cap and carry no `infeasibility`/`ruling` prose. The measured " +
          "numbers HAVE been written so the ~858s run is not lost, but this lane " +
          "stays red until a human writes, for each row, what cannot be done and " +
          `who owns the resolution: ${owed.join("; ")}`,
      );
    }

    return {
      failures,
      drifts,
      rowCount,
      updated: {
        measurement:
          "native-script-scan-fault-proof-step-exunits-emulator (#633 R3 baseline)",
        issue:
          "#633 — the LIVE one-token-per-transaction staged native-script scan, " +
          "measured as the fault-proof transaction that advances it. These rows " +
          "are the baseline the direction-(d) staged-proof design and any scan " +
          "optimisation are judged against.",
        spec:
          "GOAL_SPEC §3.3 for the execution basis; docs/spec/midgard-tx.md §5.5 " +
          "for the reference-script field and §6.2 for output canonicity; the " +
          "L1 transaction cap is MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes.",
        note:
          "Emulator ExUnits are DETERMINISTIC, so every reading is pinned to the " +
          "unit rather than to a tolerance band: the lucid-evolution Emulator " +
          "evaluates the applied validator locally against a fixed cost model, " +
          "over a transaction built from fixed keys, fixed out-refs and a fixed " +
          "blueprint. A moved reading means the blueprint, the trace builder, the " +
          "transaction shape or the cost model moved. Structural fields " +
          "(stage/nativeStage/stepIndex/stackDepth/proofStepCounts/payload sizes) " +
          "and judgements (basisFit/l1Fit/infeasibility/ruling) are never " +
          "rewritten by the update lane.",
        basis: {
          memoryUnits: Number(basis.memoryUnits),
          cpuUnits: Number(basis.cpuUnits),
          maxL1ProofTxBytes: basis.maxL1ProofTxBytes,
          maxOutputCanonicalCborBytes: basis.maxOutputCanonicalCborBytes,
          source: basis.source,
        },
        curvePoints,
      },
      bootstrapped: true,
    };
  }

  // --- Declared basis. Read from the caller's constants, never from the file
  // whose verdicts it decides: raising it inside the ledger is exactly how an
  // over-basis row would quietly become a within-basis one. ---
  const declared = ledger.basis ?? {
    memoryUnits: undefined,
    cpuUnits: undefined,
    maxL1ProofTxBytes: undefined,
    maxOutputCanonicalCborBytes: undefined,
  };
  if (
    declared.memoryUnits !== Number(basis.memoryUnits) ||
    declared.cpuUnits !== Number(basis.cpuUnits) ||
    declared.maxL1ProofTxBytes !== basis.maxL1ProofTxBytes ||
    declared.maxOutputCanonicalCborBytes !== basis.maxOutputCanonicalCborBytes
  ) {
    failures.push(
      `ledger basis mem=${String(declared.memoryUnits)} cpu=${String(declared.cpuUnits)} ` +
        `l1Bytes=${String(declared.maxL1ProofTxBytes)} ` +
        `outputCborBytes=${String(declared.maxOutputCanonicalCborBytes)} is not the ` +
        `basis this lane is judged at, mem=${basis.memoryUnits.toString()} ` +
        `cpu=${basis.cpuUnits.toString()} l1Bytes=${basis.maxL1ProofTxBytes.toString()} ` +
        `outputCborBytes=${basis.maxOutputCanonicalCborBytes.toString()}`,
    );
  }

  const ledgerCurves = Array.isArray(ledger.curvePoints)
    ? ledger.curvePoints
    : [];
  if (ledgerCurves.length === 0) {
    failures.push("the ledger declares no `curvePoints` array");
  }

  // --- Curve-point set equality. A vanished point is how a narrowed run would
  // otherwise re-pin a subset; an added one is a point nobody judged. ---
  const freshByKey = new Map(
    readings.map((curve) => [curveKey(curve.shape, curve.nodes), curve]),
  );
  const ledgerKeys = new Set(
    ledgerCurves.map((curve) => curveKey(curve.shape, curve.nodes)),
  );
  for (const key of freshByKey.keys()) {
    if (!ledgerKeys.has(key)) {
      failures.push(
        `curve point '${key}' was measured but is not in the ledger — a new ` +
          "point is a change to what this lane measures, not a reading to re-pin",
      );
    }
  }

  for (const curve of ledgerCurves) {
    const key = curveKey(curve.shape, curve.nodes);
    const fresh = freshByKey.get(key);
    if (fresh === undefined) {
      failures.push(
        `curve point '${key}' is in the ledger but was not measured — a ledger ` +
          "row that did not run cannot be judged (benchmark filters narrow the " +
          "run; they do not narrow the ledger)",
      );
      continue;
    }
    const structural: [string, unknown, unknown][] = [
      ["payloadBytes", curve.payloadBytes, fresh.payloadBytes],
      ["spentOutputBytes", curve.spentOutputBytes, fresh.spentOutputBytes],
      [
        "proofStepCounts.total",
        curve.proofStepCounts?.total,
        fresh.proofStepCounts.total,
      ],
      [
        "proofStepCounts.nativeToken",
        curve.proofStepCounts?.nativeToken,
        fresh.proofStepCounts.nativeToken,
      ],
      [
        "proofStepCounts.nativeFrame",
        curve.proofStepCounts?.nativeFrame,
        fresh.proofStepCounts.nativeFrame,
      ],
      [
        "proofStepCounts.nativeFinalize",
        curve.proofStepCounts?.nativeFinalize,
        fresh.proofStepCounts.nativeFinalize,
      ],
    ];
    for (const [name, recorded, measured] of structural) {
      if (recorded !== measured) {
        failures.push(
          `${key}: ${name} is ${String(recorded)} in the ledger but ` +
            `${String(measured)} was measured — the proof trace this lane pins ` +
            "changed shape; that is not a reading to re-pin",
        );
      }
    }

    const rows = curve.rows;
    if (typeof rows !== "object" || rows === null || Array.isArray(rows)) {
      failures.push(`${key}: curve point declares no \`rows\` object`);
      continue;
    }
    const freshRows = new Map(
      fresh.measurements.map((reading) => [reading.label, reading]),
    );
    for (const label of freshRows.keys()) {
      if (!(label in rows)) {
        failures.push(
          `${key}: row '${label}' was measured but is not in the ledger`,
        );
      }
    }

    for (const [label, recorded] of Object.entries(rows)) {
      const actual = freshRows.get(label);
      if (actual === undefined) {
        failures.push(`${key}: row '${label}' did not run`);
        continue;
      }
      rowCount += 1;

      const rowStructural: [string, unknown, unknown][] = [
        ["stepIndex", recorded.stepIndex, actual.stepIndex],
        ["stage", recorded.stage, actual.stage],
        ["nativeStage", recorded.nativeStage, actual.nativeStage],
        ["stackDepth", recorded.stackDepth, actual.stackDepth],
      ];
      let structurallyMoved = false;
      for (const [name, expected, measured] of rowStructural) {
        if (expected !== measured) {
          structurallyMoved = true;
          failures.push(
            `${key}: '${label}' ${name} is ${String(expected)} in the ledger ` +
              `but ${String(measured)} was measured — the step this row names ` +
              "is not the step that ran",
          );
        }
      }
      if (structurallyMoved) continue;

      // Judged against the FRESH reading, in both modes and before any write.
      const within = withinBasis(actual.mem, actual.cpu);
      const l1 = withinL1(actual.completeSignedBytes);
      if (recorded.basisFit !== "within" && recorded.basisFit !== "exceeds") {
        failures.push(
          `${key}: '${label}' has unexpected basisFit ` +
            `'${String(recorded.basisFit)}' — every row is recorded 'within' or ` +
            "'exceeds' so that movement across the basis fails in either direction",
        );
        continue;
      }
      if (recorded.l1Fit !== "within" && recorded.l1Fit !== "exceeds") {
        failures.push(
          `${key}: '${label}' has unexpected l1Fit '${String(recorded.l1Fit)}'`,
        );
        continue;
      }
      if (recorded.basisFit === "within" && !within) {
        failures.push(
          `${key}: '${label}' is recorded 'within' the §3.3 basis but measured ` +
            `mem=${actual.mem.toString()} cpu=${actual.cpu.toString()}, over ` +
            `mem=${basis.memoryUnits.toString()} cpu=${basis.cpuUnits.toString()} ` +
            "— a step that stopped fitting is a regression, not drift",
        );
        continue;
      }
      if (recorded.basisFit === "exceeds" && within) {
        failures.push(
          `${key}: '${label}' is recorded 'exceeds' but measured ` +
            `mem=${actual.mem.toString()} cpu=${actual.cpu.toString()}, which is ` +
            "WITHIN the basis — a recorded infeasibility that became feasible is " +
            "not drift; re-read the ledger's note and #633",
        );
        continue;
      }
      if (recorded.l1Fit === "within" && !l1) {
        failures.push(
          `${key}: '${label}' is recorded within the L1 cap but measured ` +
            `${actual.completeSignedBytes.toString()} signed bytes, over ` +
            `${basis.maxL1ProofTxBytes.toString()} — the proof transaction stopped fitting L1`,
        );
        continue;
      }
      if (recorded.l1Fit === "exceeds" && l1) {
        failures.push(
          `${key}: '${label}' is recorded over the L1 cap but measured ` +
            `${actual.completeSignedBytes.toString()} signed bytes, which FITS — a ` +
            "recorded infeasibility that became feasible is not drift",
        );
        continue;
      }
      // An exceeding row with nowhere to point is how "exceeds" becomes an
      // acceptable steady state.
      if (recorded.basisFit === "exceeds" || recorded.l1Fit === "exceeds") {
        if (
          typeof recorded.infeasibility !== "string" ||
          recorded.infeasibility.trim() === ""
        ) {
          failures.push(
            `${key}: '${label}' exceeds the basis or the L1 cap and declares no ` +
              "`infeasibility` prose saying what cannot be done",
          );
          continue;
        }
        if (
          typeof recorded.ruling !== "string" ||
          recorded.ruling.trim() === ""
        ) {
          failures.push(
            `${key}: '${label}' exceeds the basis or the L1 cap and declares no ` +
              "`ruling` cross-reference naming who owns the resolution",
          );
          continue;
        }
      }

      if (update) {
        rows[label] = {
          ...recorded,
          ...rowFromReading(actual, basis, {
            basisFit: recorded.basisFit,
            l1Fit: recorded.l1Fit,
          }),
          ...(recorded.infeasibility === undefined
            ? {}
            : { infeasibility: recorded.infeasibility }),
          ...(recorded.ruling === undefined ? {} : { ruling: recorded.ruling }),
        };
        continue;
      }

      const expectedMargin =
        basis.maxL1ProofTxBytes - actual.completeSignedBytes;
      const expectedShare = memBasisShareBasisPoints(
        actual.mem,
        basis.memoryUnits,
      );
      const raw: [string, string, string][] = [
        ["mem", String(recorded.mem), actual.mem.toString()],
        ["cpu", String(recorded.cpu), actual.cpu.toString()],
        ["fee", String(recorded.fee), actual.fee.toString()],
        [
          "completeSignedBytes",
          String(recorded.completeSignedBytes),
          actual.completeSignedBytes.toString(),
        ],
        [
          "l1ByteMargin",
          String(recorded.l1ByteMargin),
          expectedMargin.toString(),
        ],
        [
          "memBasisShareBasisPoints",
          String(recorded.memBasisShareBasisPoints),
          expectedShare.toString(),
        ],
      ];
      for (const [name, expected, measured] of raw) {
        if (expected !== measured) {
          drifts.push(
            `${key}: '${label}' ${name} drifted — ledger ${expected}, measured ${measured}`,
          );
        }
      }
    }
  }

  if (rowCount === 0) {
    failures.push(
      "the ledger produced no compared rows at all — an execution pin that " +
        "measures nothing passes for free",
    );
  }

  return {
    failures,
    drifts,
    rowCount,
    updated: update ? ledger : null,
    bootstrapped: false,
  };
};

/** The benchmark filter env vars that are actually set, for the bootstrap guard. */
export const scanBenchFiltersInEffect = (
  environment: NodeJS.ProcessEnv = process.env,
): readonly string[] =>
  SCAN_BENCH_FILTER_ENV.filter(
    (name) => environment[name] !== undefined && environment[name] !== "",
  );
