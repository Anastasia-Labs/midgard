import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { createRequire } from "node:module";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Metric } from "effect";

import { fullScanCounter as confirmedLedgerFullScanCounter } from "../database/confirmedLedger.js";
import {
  buildNativeRootProbe,
  deleteMpfStore,
  MidgardMpf,
  setMpfScratchBuild,
  type TransitionTraceSourceEvent,
} from "../mpf/index.js";
import { ProductionNativeMpfOwnerService } from "../services/mpf-native-owner/index.js";
import {
  buildCanonicalFixtureEntries,
  canonicalOutrefCborFromLabel,
  decodeCanonicalProbeRow,
  ledgerFixtureMpfEntries,
} from "./mpf-engine-probe-corpus.js";
import {
  decodeArchitectureGCorpusFunding,
  decodeArchitectureGFixtureCreation,
  validateArchitectureGRootProbeResult,
} from "./utils/mpf-commit-candidate-artifacts.js";
import {
  closeMpfRootWorkers,
  configureMpfRootWorkers,
  prewarmMpfRootWorkers,
} from "./utils/mpf-root-pool.js";

const probePath = resolve(fileURLToPath(import.meta.url));
const probeSha256 = createHash("sha256")
  .update(readFileSync(probePath))
  .digest("hex");

const transactionCount = Math.max(
  1,
  Number.parseInt(process.env.MPF_ENGINE_PROBE_TXS ?? "1000", 10),
);
const initialUtxoCount = Math.max(
  transactionCount,
  Number.parseInt(
    process.env.MPF_ENGINE_PROBE_INITIAL_UTXOS ?? transactionCount.toString(),
    10,
  ),
);
const require = createRequire(import.meta.url);
const levelFixturePath = process.env.MPF_ENGINE_PROBE_LEVEL_DB?.trim() ?? "";
const reuseLevelFixture =
  process.env.MPF_ENGINE_PROBE_REUSE_LEVEL_DB === "true";
const createLevelFixture =
  process.env.MPF_ENGINE_PROBE_CREATE_LEVEL_FIXTURE === "true";
const formatErrorChain = (error: unknown): string => {
  const rendered: string[] = [];
  const seen = new Set<unknown>();
  let current: unknown = error;
  while (current !== undefined && current !== null && !seen.has(current)) {
    seen.add(current);
    if (current instanceof Error) {
      rendered.push(current.stack ?? `${current.name}: ${current.message}`);
      current = current.cause;
      continue;
    }
    rendered.push(JSON.stringify(current) ?? "undefined");
    break;
  }
  return rendered.join("\nCaused by: ");
};
const key = (index: number): Buffer => {
  const value = Buffer.alloc(32);
  value.writeUInt32BE(index, 28);
  return value;
};
// A reuse process must model production's root-only Level hydration. Keeping a
// million-entry JS fixture resident would make the measured block pay for
// harness GC even though production does not materialize confirmed_ledger.
const initialEntriesToMaterialize =
  (reuseLevelFixture && levelFixturePath.length > 0) ||
  (createLevelFixture &&
    (process.env.MPF_ENGINE_PROBE_CORPUS_SLICE_PATH?.trim().length ?? 0) > 0)
    ? transactionCount
    : initialUtxoCount;
const initial = Array.from(
  { length: initialEntriesToMaterialize },
  (_, index) => ({
    key: key(index),
    value: Buffer.alloc(64, index % 251),
  }),
);
const h32 = (index: number): string => {
  const value = Buffer.alloc(32);
  value.writeUInt32BE(index, 28);
  return value.toString("hex");
};
const buildProbeSourceEvents = (
  transactionIds: readonly string[],
): readonly TransitionTraceSourceEvent[] => {
  if (transactionIds.length !== transactionCount) {
    throw new Error(
      `Probe transaction-id count mismatch: expected=${transactionCount.toString()},actual=${transactionIds.length.toString()}`,
    );
  }
  return initial.slice(0, transactionCount).map((entry, index) => ({
    phase: "L2Transaction",
    eventKey: {
      L2TransactionEventKey: { tx_id: transactionIds[index]! },
    } as SDK.EventKey,
    ledgerOps: [
      { type: "delete", key: entry.key },
      {
        type: "insert",
        // Keep the exact operation stream identical for the 100k/300k/1m
        // growth comparison while staying outside every seeded fixture.
        key: key(2_000_000 + index),
        value: Buffer.alloc(64, (index + 1) % 251),
      },
    ],
  }));
};
const sourceEvents = buildProbeSourceEvents(
  Array.from({ length: transactionCount }, (_, index) => h32(index)),
);
const transactionOps = Array.from({ length: transactionCount }, (_, index) => ({
  type: "insert" as const,
  key: key(index),
  // processMpfs treats the transaction commitment as opaque bytes after its
  // canonical-CBOR decoder has produced encodeTransactionRootValue(...).
  value: Buffer.concat([Buffer.from("MIDGARD-ARCH-G-PROBE-TX-V1"), key(index)]),
}));
const workloadSha256 = (
  events: readonly TransitionTraceSourceEvent[],
  operations: readonly {
    readonly type: "insert";
    readonly key: Buffer;
    readonly value: Buffer;
  }[],
): string => {
  const state = createHash("sha256");
  for (const event of events) {
    state.update(event.phase);
    for (const op of event.ledgerOps) {
      state.update(op.type).update(op.key);
      if (op.type === "insert") state.update(op.value);
    }
  }
  for (const op of operations) {
    state.update(op.type).update(op.key).update(op.value);
  }
  return state.digest("hex");
};

const loadCanonicalTransactionSlice = async (): Promise<
  | {
      readonly path: string;
      readonly sha256: string;
      readonly corpusSha256: string;
      readonly transactionOps: readonly {
        readonly type: "insert";
        readonly key: Buffer;
        readonly value: Buffer;
      }[];
      readonly transactionIds: readonly string[];
      readonly sourceEvents: readonly TransitionTraceSourceEvent[];
      readonly fundingRoots: readonly {
        readonly walletId: string;
        readonly outref: string;
      }[];
    }
  | undefined
> => {
  const path = process.env.MPF_ENGINE_PROBE_CORPUS_SLICE_PATH?.trim() ?? "";
  if (path.length === 0) return undefined;
  const expectedSha256 =
    process.env.MPF_ENGINE_PROBE_CORPUS_SLICE_SHA256?.trim() ?? "";
  const corpusSha256 = process.env.MPF_ENGINE_PROBE_CORPUS_SHA256?.trim() ?? "";
  if (!/^[0-9a-f]{64}$/.test(expectedSha256)) {
    throw new Error(
      "Canonical corpus slice requires MPF_ENGINE_PROBE_CORPUS_SLICE_SHA256",
    );
  }
  if (!/^[0-9a-f]{64}$/u.test(corpusSha256)) {
    throw new Error(
      "Canonical corpus slice requires MPF_ENGINE_PROBE_CORPUS_SHA256",
    );
  }
  const bytes = await readFile(path);
  const actualSha256 = createHash("sha256").update(bytes).digest("hex");
  if (actualSha256 !== expectedSha256) {
    throw new Error(
      `Canonical corpus slice SHA-256 mismatch: expected=${expectedSha256},actual=${actualSha256}`,
    );
  }
  const rows = bytes
    .toString("utf8")
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line, index) => {
      const parsed = JSON.parse(line) as Record<string, unknown>;
      const walletId = parsed.senderWalletId;
      if (
        typeof walletId !== "string" ||
        walletId.trim().length === 0 ||
        walletId.length > 4096
      ) {
        throw new Error(
          `Canonical corpus slice row ${(index + 1).toString()} has an invalid senderWalletId`,
        );
      }
      return {
        decoded: decodeCanonicalProbeRow(parsed, index),
        walletId,
      };
    });
  if (rows.length !== transactionCount) {
    throw new Error(
      `Canonical corpus slice row count mismatch: expected=${transactionCount.toString()},actual=${rows.length.toString()}`,
    );
  }
  if (new Set(rows.map((row) => row.decoded.txHash)).size !== rows.length) {
    throw new Error(
      "Canonical corpus slice contains duplicate transaction hashes",
    );
  }
  return {
    path,
    sha256: actualSha256,
    corpusSha256,
    transactionOps: rows.map((row) => row.decoded.transactionOp),
    transactionIds: rows.map((row) => row.decoded.txHash),
    sourceEvents: rows.map((row) => row.decoded.sourceEvent),
    fundingRoots: rows
      .filter((row) => row.decoded.parentTxHash === null)
      .map((row) => ({
        walletId: row.walletId,
        outref: row.decoded.selectedInputOutref,
      })),
  };
};

const loadCanonicalFundingMap = async (
  canonicalSlice: Awaited<ReturnType<typeof loadCanonicalTransactionSlice>>,
): Promise<
  | {
      readonly path: string;
      readonly sha256: string;
      readonly entries: ReadonlyMap<string, Buffer>;
    }
  | undefined
> => {
  if (canonicalSlice === undefined) return undefined;
  const path = process.env.MPF_ENGINE_PROBE_CORPUS_FUNDING_PATH?.trim() ?? "";
  const expectedSha256 =
    process.env.MPF_ENGINE_PROBE_CORPUS_FUNDING_SHA256?.trim() ?? "";
  if (path.length === 0 || !/^[0-9a-f]{64}$/u.test(expectedSha256)) {
    throw new Error(
      "Canonical corpus slice requires a SHA-bound canonical funding map",
    );
  }
  const bytes = await readFile(path);
  const actualSha256 = createHash("sha256").update(bytes).digest("hex");
  if (actualSha256 !== expectedSha256) {
    throw new Error(
      `Canonical corpus funding map SHA-256 mismatch: expected=${expectedSha256},actual=${actualSha256}`,
    );
  }
  const parsed = decodeArchitectureGCorpusFunding({
    value: JSON.parse(bytes.toString("utf8")) as unknown,
    expectedCorpusSha256: canonicalSlice.corpusSha256,
    expectedSliceSha256: canonicalSlice.sha256,
    expectedFundingRoots: canonicalSlice.fundingRoots,
  });
  const entries = new Map<string, Buffer>();
  for (const [index, value] of parsed.entries.entries()) {
    const outref = value.outref;
    const outputCbor = value.outputCbor;
    canonicalOutrefCborFromLabel(outref);
    const output = Buffer.from(outputCbor, "hex");
    if (entries.has(outref)) {
      throw new Error(
        `Canonical corpus funding map entry ${index.toString()} is invalid or duplicated`,
      );
    }
    entries.set(outref, output);
  }
  for (const { outref } of canonicalSlice.fundingRoots) {
    if (!entries.has(outref)) {
      throw new Error(`Canonical corpus funding map is missing root ${outref}`);
    }
  }
  if (entries.size !== canonicalSlice.fundingRoots.length) {
    throw new Error(
      "Canonical corpus funding map contains roots outside the selected prefix",
    );
  }
  return { path, sha256: actualSha256, entries };
};

const createProbeLedgerFixture = (
  name: string,
  entries: readonly {
    readonly key: Buffer;
    readonly value: Buffer;
  }[],
): Effect.Effect<MidgardMpf, unknown> =>
  Effect.gen(function* () {
    yield* deleteMpfStore(levelFixturePath, `${name}-fixture`);
    return yield* MidgardMpf.createLevelFromListForBenchmark(
      name,
      levelFixturePath,
      entries,
      { mode: "overlay" },
    );
  });

void Effect.runPromise(
  Effect.gen(function* () {
    const blake2b = require("blake2b") as {
      readonly ready: (callback: () => void) => void;
    };
    yield* Effect.promise(
      () => new Promise<void>((resolve) => blake2b.ready(resolve)),
    );
    setMpfScratchBuild("fromlist");
    configureMpfRootWorkers({
      enabled: process.env.MPF_ENGINE_PROBE_PARALLEL_ROOTS !== "false",
      workers: 2,
      minEntries: 1,
      timeoutMs: 120_000,
    });
    yield* Effect.promise(() => prewarmMpfRootWorkers());
    const canonicalSlice = yield* Effect.promise(() =>
      loadCanonicalTransactionSlice(),
    );
    const canonicalFunding = yield* Effect.promise(() =>
      loadCanonicalFundingMap(canonicalSlice),
    );
    if (createLevelFixture) {
      if (levelFixturePath.length === 0 || reuseLevelFixture) {
        return yield* Effect.fail(
          new Error(
            "Fixture creation requires MPF_ENGINE_PROBE_LEVEL_DB and forbids MPF_ENGINE_PROBE_REUSE_LEVEL_DB=true",
          ),
        );
      }
      const fixtureStartedAt = performance.now();
      const fixtureEntries =
        canonicalFunding === undefined
          ? initial
          : buildCanonicalFixtureEntries(
              canonicalFunding.entries,
              initialUtxoCount,
            );
      // Canonical entries are real outputs, stored under their production
      // ledger descriptors; the synthetic filler values are not outputs.
      const fixture = yield* createProbeLedgerFixture(
        "mpf-engine-probe-fixture",
        canonicalFunding === undefined
          ? fixtureEntries
          : ledgerFixtureMpfEntries(fixtureEntries),
      );
      const marker = yield* fixture.rootHex();
      const diagnostics = yield* fixture.diagnostics();
      yield* fixture.close();
      closeMpfRootWorkers();
      const utxoPayloadAggregate = {
        entryCount: fixtureEntries.length,
        encodedTupleBytes: fixtureEntries.reduce(
          (total, entry) =>
            total +
            SDK.daPayloadEntryEncodedSize([
              entry.key.toString("hex"),
              entry.value.toString("hex"),
            ]),
          0,
        ),
      };
      return decodeArchitectureGFixtureCreation({
        value: {
          fixtureCreated: true,
          fixturePath: levelFixturePath,
          initialUtxoCount,
          marker,
          durationMs: performance.now() - fixtureStartedAt,
          diagnostics,
          utxoPayloadAggregate,
          canonicalFunding:
            canonicalFunding === undefined
              ? null
              : {
                  path: canonicalFunding.path,
                  sha256: canonicalFunding.sha256,
                  entryCount: canonicalFunding.entries.size,
                },
        },
        expectedFixturePath: levelFixturePath,
        expectedMarker: marker,
        expectedUtxos: initialUtxoCount,
        expectedAggregate: utxoPayloadAggregate,
        expectedFundingMapSha256: canonicalFunding?.sha256 ?? null,
      });
    }
    if (levelFixturePath.length === 0 || !reuseLevelFixture) {
      return yield* Effect.fail(
        new Error(
          "Architecture G probe requires MPF_ENGINE_PROBE_REUSE_LEVEL_DB=true and a marker-matched Level fixture",
        ),
      );
    }
    const architectureTransactionOps =
      canonicalSlice?.transactionOps ?? transactionOps;
    const architectureSourceEvents =
      canonicalSlice?.sourceEvents ?? sourceEvents;
    const binaryPath =
      process.env.MPF_NATIVE_OWNER_BINARY_PATH?.trim() ||
      resolve(
        process.cwd(),
        "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
      );
    const binarySha256 =
      process.env.MPF_NATIVE_OWNER_BINARY_SHA256?.trim() ||
      createHash("sha256")
        .update(yield* Effect.promise(() => readFile(binaryPath)))
        .digest("hex");
    const startupStartedAt = performance.now();
    const processStatus = yield* Effect.promise(() =>
      readFile("/proc/self/status", "utf8"),
    );
    const cpuAffinity =
      processStatus.match(/^Cpus_allowed_list:\s*(.+)$/m)?.[1]?.trim() ??
      "unknown";
    const owner = yield* Effect.promise(() =>
      ProductionNativeMpfOwnerService.create({
        levelPath: levelFixturePath,
        binaryPath,
        binarySha256,
        sidecarPath:
          process.env.MPF_NATIVE_OWNER_SIDECAR_PATH?.trim() ||
          `${levelFixturePath}.architecture-g-probe.sidecar`,
      }),
    );
    const startupMs = performance.now() - startupStartedAt;
    const before = yield* Effect.promise(() => owner.diagnostics());
    const handle = yield* Effect.promise(() => owner.fork(before.durableRoot));
    const nativeMpf = {
      client: owner,
      handle,
      ownerBinarySha256: binarySha256,
    };
    yield* Effect.promise(
      () =>
        new Promise<void>((resolve) =>
          setImmediate(() => {
            (globalThis as { gc?: () => void }).gc?.();
            resolve();
          }),
        ),
    );
    const confirmedLedgerScansBefore = yield* Metric.value(
      confirmedLedgerFullScanCounter,
    );
    const result = yield* buildNativeRootProbe({
      nativeMpf,
      sourceEvents: architectureSourceEvents,
      transactionOps: architectureTransactionOps,
    });
    const confirmedLedgerScansAfter = yield* Metric.value(
      confirmedLedgerFullScanCounter,
    );
    yield* Effect.promise(() => owner.discard(handle));
    const after = yield* Effect.promise(() => owner.diagnostics());
    yield* Effect.promise(() => owner.close());
    closeMpfRootWorkers();
    const artifact = {
      engine: "architecture_g",
      transactionCount,
      initialUtxoCount,
      workloadSha256: workloadSha256(
        architectureSourceEvents,
        architectureTransactionOps,
      ),
      canonicalCorpusSlice:
        canonicalSlice === undefined
          ? null
          : {
              path: canonicalSlice.path,
              sha256: canonicalSlice.sha256,
              rowCount: canonicalSlice.transactionOps.length,
            },
      canonicalFunding:
        canonicalFunding === undefined
          ? null
          : {
              path: canonicalFunding.path,
              sha256: canonicalFunding.sha256,
              entryCount: canonicalFunding.entries.size,
            },
      levelBackedInitialView: true,
      reusedLevelFixture: true,
      ledgerOpCount: architectureSourceEvents.reduce(
        (total, event) => total + event.ledgerOps.length,
        0,
      ),
      startupMs,
      durationMs: result.durationMs,
      buildPlusCaptureMs: result.durationMs,
      phaseMs: result.phaseMs,
      utxoRoot: result.utxoRoot,
      rawTxRoot: result.rawTxRoot,
      txRoot: result.txRoot,
      transitionTraceRoot: result.transitionTraceRoot,
      eventToStepRoot: result.eventToStepRoot,
      depositsRoot: result.depositsRoot,
      withdrawalsRoot: result.withdrawalsRoot,
      forcedTransactionsRoot: result.forcedTransactionsRoot,
      transitionRoots: result.transitionRoots,
      nativePhaseMs: result.transitionTraceBuild.nativePhaseMs,
      pathHydration: result.transitionTraceBuild.pathHydration,
      confirmedLedgerFullScans:
        confirmedLedgerScansAfter.count - confirmedLedgerScansBefore.count,
      binarySha256,
      cpuAffinity,
      ownerBefore: before,
      ownerAfter: after,
      probePath,
      probeSha256,
    };
    return validateArchitectureGRootProbeResult({
      value: artifact,
      expectedTransactionCount: transactionCount,
      expectedInitialUtxoCount: initialUtxoCount,
      expectedProbePath: probePath,
      expectedProbeSha256: probeSha256,
    });
  }),
).then(
  (result) =>
    process.stdout.write(
      `${JSON.stringify({ ...result, probePath, probeSha256 })}\n`,
    ),
  (error: unknown) => {
    process.stderr.write(
      `${formatErrorChain(error)}\n${inspect(error, { depth: 12, getters: true })}\n`,
    );
    process.exitCode = 1;
  },
);
