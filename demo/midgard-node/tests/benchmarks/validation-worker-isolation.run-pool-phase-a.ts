import { resolve } from "node:path";
import { performance } from "node:perf_hooks";
import { pathToFileURL } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  deserializePhaseACandidate,
  type PhaseAResult,
  type PhaseBResultWithPatch,
  type QueuedTx,
} from "@al-ft/midgard-validation";

import {
  ledgerEntry,
  makeNativeTx,
  makeOutput,
  makeQueued,
  outRefFromByte,
} from "../../../midgard-validation/tests/validation-fixtures.js";
import { MempoolLedgerDB } from "../../src/database/index.js";
import { FixedValidationWorkerPool } from "../../src/services/validation-pool.js";
import { packPhaseAJob } from "../../src/workers/utils/validation-pool.js";

export const quick = process.env.BENCH_QUICK === "1";

export const batchSize = Number(
  process.env.BENCH_PHASE2_BATCH_SIZE ?? (quick ? 512 : 4_096),
);

export const poolSize = Number(process.env.BENCH_PHASE2_POOL_SIZE ?? 6);

export const chunkSize = Number(process.env.BENCH_PHASE2_CHUNK_SIZE ?? 64);

export const durationMs = Number(
  process.env.BENCH_PHASE2_DURATION_MS ?? (quick ? 5_000 : 300_000),
);

export const assertGate = process.env.BENCH_ASSERT_PHASE2 === "1";

export const assertLeakSoak = process.env.BENCH_ASSERT_PHASE2_LEAK_SOAK === "1";

export const targetTps = Number(process.env.BENCH_PHASE2_TARGET_TPS ?? 2_500);

export const steadyStateWarmupMs = Number(
  process.env.BENCH_PHASE2_STEADY_STATE_WARMUP_MS ??
    (assertLeakSoak ? 300_000 : 0),
);

export const expectedNodeImage =
  process.env.BENCH_PHASE2_NODE_IMAGE ?? "node:22.22.2";

export const expectedNodeImageId = process.env.BENCH_PHASE2_NODE_IMAGE_ID ?? "";

export const runDatabaseDiagnostic =
  process.env.BENCH_PHASE2_DATABASE_DIAGNOSTIC === "1";

export const benchmarkDatabaseNamePattern =
  /^midgard_phase2_bench_[a-z0-9_]+$/u;

export const workerEntry = pathToFileURL(resolve("dist/validation.js"));

export const outputPath = resolve(
  process.env.BENCH_PHASE2_OUTPUT_PATH ??
    "tests/benchmarks/output/validation-worker-isolation.json",
);

export const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase2_worker_isolation",
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
} as const;

export const percentile = (samples: readonly number[], p: number): number => {
  const sorted = [...samples].sort((left, right) => left - right);
  return (
    sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * p))] ?? 0
  );
};

export const normalizePhaseB = (result: PhaseBResultWithPatch) => ({
  acceptedTxIds: result.accepted.map((candidate) =>
    candidate.ledgerTx.txId.toString("hex"),
  ),
  rejected: result.rejected.map((rejection) => ({
    txId: rejection.txId.toString("hex"),
    code: rejection.code,
    detail: rejection.detail,
  })),
  statePatch: result.statePatch,
});

export const buildCorpus = (): {
  readonly queued: readonly QueuedTx[];
  readonly preState: Map<string, Buffer>;
  readonly preStateRows: readonly MempoolLedgerDB.EntryNoTimeStamp[];
} => {
  const queued: QueuedTx[] = [];
  const preState = new Map<string, Buffer>();
  const preStateRows: MempoolLedgerDB.EntryNoTimeStamp[] = [];
  for (let index = 0; index < batchSize; index += 1) {
    const spent = outRefFromByte(
      (index % 250) + 1,
      BigInt(Math.floor(index / 250)),
    );
    const output = makeOutput(10n);
    const fixture = makeNativeTx({ spendInputs: [spent], outputs: [output] });
    queued.push(makeQueued(fixture.txId, fixture.txCbor, BigInt(index)));
    const entry = ledgerEntry(spent, output);
    preState.set(entry.outref.toString("hex"), entry.output);
    preStateRows.push({
      ...entry,
      [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
    });
  }
  return { queued, preState, preStateRows };
};

export const runPoolPhaseA = async (
  pool: FixedValidationWorkerPool,
  queued: readonly QueuedTx[],
): Promise<{
  result: PhaseAResult;
  serializeMs: number;
  deserializeMs: number;
}> => {
  let serializeMs = 0;
  const requests = [];
  for (let offset = 0; offset < queued.length; offset += chunkSize) {
    const startedAt = performance.now();
    requests.push(
      packPhaseAJob(
        pool.allocateJobId(),
        queued.slice(offset, offset + chunkSize),
      ),
    );
    serializeMs += performance.now() - startedAt;
  }
  const responses = await Promise.all(
    requests.map((request) => pool.submit(request)),
  );
  const deserializeStartedAt = performance.now();
  const accepted = [];
  const rejected = [];
  for (const response of responses) {
    if (response.kind !== "phase_a")
      throw new Error(`unexpected ${response.kind}`);
    for (const item of response.results) {
      if (item.ok) accepted.push(deserializePhaseACandidate(item.candidate));
      else
        rejected.push({
          txId: Buffer.from(item.txId),
          code: item.code,
          detail: item.detail,
        });
    }
  }
  return {
    result: { accepted, rejected },
    serializeMs,
    deserializeMs: performance.now() - deserializeStartedAt,
  };
};
