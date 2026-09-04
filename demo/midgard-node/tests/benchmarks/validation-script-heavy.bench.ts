import { createHash } from "node:crypto";
import fs from "node:fs";
import { availableParallelism, cpus, hostname } from "node:os";
import { resolve } from "node:path";
import { monitorEventLoopDelay, performance } from "node:perf_hooks";
import { pathToFileURL } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  deserializePhaseACandidate,
  MidgardRedeemerTag,
  type PhaseAResult,
  type PhaseAValidatedTx,
  type PhaseBResultWithPatch,
  type QueuedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  hashScriptWitness,
  makeNativeTx,
  makeProtectedScriptOutput,
  makeQueued,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "../../../midgard-validation/tests/validation-fixtures.js";
import { FixedValidationWorkerPool } from "../../src/services/validation-pool.js";
import {
  copyToTransferable,
  packPhaseAJob,
} from "../../src/workers/utils/validation-pool.js";
import {
  readPhase2ContainerIdentity,
  readPhase2CpuTopology,
} from "./phase2-cpu-topology.js";

const quick = process.env.BENCH_QUICK === "1";
const assertProductionGate = process.env.BENCH_ASSERT_PHASE2_SCRIPT === "1";
const assertChunk128CandidateGate =
  process.env.BENCH_ASSERT_PHASE2_SCRIPT_CHUNK128 === "1";
if (assertProductionGate && assertChunk128CandidateGate) {
  throw new Error(
    "BENCH_ASSERT_PHASE2_SCRIPT and BENCH_ASSERT_PHASE2_SCRIPT_CHUNK128 are mutually exclusive",
  );
}
const assertGate = assertProductionGate || assertChunk128CandidateGate;
const batchSize = Number(
  process.env.BENCH_PHASE2_SCRIPT_BATCH_SIZE ?? (quick ? 32 : 256),
);
const poolSize = Number(process.env.BENCH_PHASE2_POOL_SIZE ?? 6);
const chunkSize = Number(process.env.BENCH_PHASE2_CHUNK_SIZE ?? 64);
const durationMs = Number(
  process.env.BENCH_PHASE2_DURATION_MS ?? (quick ? 5_000 : 300_000),
);
const expectedNodeImage = process.env.BENCH_PHASE2_NODE_IMAGE ?? "node:22.22.2";
const expectedNodeImageId = process.env.BENCH_PHASE2_NODE_IMAGE_ID ?? "";
const outputPath = resolve(
  process.env.BENCH_PHASE2_OUTPUT_PATH ??
    "tests/benchmarks/output/validation-script-heavy.json",
);
const workerEntry = pathToFileURL(resolve("dist/validation.js"));
const chunkAbExperimentId =
  process.env.BENCH_PHASE2_CHUNK_AB_EXPERIMENT_ID ?? "";
const corpusPath = resolve(process.env.BENCH_PHASE2_CORPUS_PATH ?? "");
const corpusManifestPath = resolve(
  process.env.BENCH_PHASE2_CORPUS_MANIFEST_PATH ?? "",
);
const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase2_script_heavy",
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
} as const;

const blueprint = JSON.parse(
  fs.readFileSync(resolve("blueprints/always-succeeds/plutus.json"), "utf8"),
) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};
const scriptBytes = Buffer.from(
  blueprint.validators.find(
    (validator) => validator.title === "midgard.deposit_spend.else",
  )?.compiledCode ?? "",
  "hex",
);

const requirePositiveSafeInteger = (value: number, label: string): void => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${label} must be a positive safe integer, got ${value}`);
  }
};

const readChunkAbCorpusBinding = (): {
  readonly corpusPath: string;
  readonly corpusManifestPath: string;
  readonly corpusSha256: string;
  readonly corpusRowCount: number;
} => {
  if (!/^cab_\d{8}t\d{6}z$/u.test(chunkAbExperimentId)) {
    throw new Error(
      "BENCH_PHASE2_CHUNK_AB_EXPERIMENT_ID must be cab_YYYYMMDDtHHMMSSz",
    );
  }
  if (
    process.env.BENCH_PHASE2_CORPUS_PATH === undefined ||
    process.env.BENCH_PHASE2_CORPUS_MANIFEST_PATH === undefined
  ) {
    throw new Error(
      "chunk-128 candidate gate requires BENCH_PHASE2_CORPUS_PATH and BENCH_PHASE2_CORPUS_MANIFEST_PATH",
    );
  }
  const manifest = JSON.parse(fs.readFileSync(corpusManifestPath, "utf8")) as {
    readonly files?: {
      readonly corpus?: {
        readonly sha256?: unknown;
        readonly rowCount?: unknown;
      };
    };
  };
  const corpusSha256 = manifest.files?.corpus?.sha256;
  const corpusRowCount = manifest.files?.corpus?.rowCount;
  if (
    typeof corpusSha256 !== "string" ||
    !/^[0-9a-f]{64}$/u.test(corpusSha256)
  ) {
    throw new Error("corpus manifest must contain an exact lowercase SHA-256");
  }
  if (!Number.isSafeInteger(corpusRowCount) || Number(corpusRowCount) < 1) {
    throw new Error("corpus manifest rowCount must be a positive safe integer");
  }
  const corpusBytes = fs.readFileSync(corpusPath);
  const actualSha256 = createHash("sha256").update(corpusBytes).digest("hex");
  if (actualSha256 !== corpusSha256) {
    throw new Error(
      `corpus SHA-256 mismatch: expected ${corpusSha256}, got ${actualSha256}`,
    );
  }
  const newlineCount = corpusBytes.reduce(
    (count, byte) => count + (byte === 0x0a ? 1 : 0),
    0,
  );
  const actualRowCount =
    corpusBytes.length === 0
      ? 0
      : newlineCount + (corpusBytes[corpusBytes.length - 1] === 0x0a ? 0 : 1);
  if (actualRowCount !== corpusRowCount) {
    throw new Error(
      `corpus row count mismatch: expected ${String(corpusRowCount)}, got ${actualRowCount.toString()}`,
    );
  }
  return {
    corpusPath,
    corpusManifestPath,
    corpusSha256,
    corpusRowCount: Number(corpusRowCount),
  };
};

const normalizePhaseB = (result: PhaseBResultWithPatch) => ({
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

const buildCorpus = (): {
  readonly queued: readonly QueuedTx[];
  readonly preState: Map<string, Buffer>;
} => {
  if (scriptBytes.length === 0) {
    throw new Error("missing always-succeeds Plutus spend fixture");
  }
  const script = plutusV3ScriptWitness(scriptBytes);
  const scriptHash = hashScriptWitness(script);
  const queued: QueuedTx[] = [];
  const preState = new Map<string, Buffer>();
  for (let index = 0; index < batchSize; index += 1) {
    const spent = outRefFromByte(
      (index % 250) + 1,
      BigInt(Math.floor(index / 250)),
    );
    const fixture = makeNativeTx({
      spendInputs: [spent],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    queued.push(makeQueued(fixture.txId, fixture.txCbor, BigInt(index)));
    preState.set(
      spent.toString("hex"),
      makeProtectedScriptOutput(scriptHash, 10n),
    );
  }
  return { queued, preState };
};

const runPoolPhaseA = async (
  pool: FixedValidationWorkerPool,
  queued: readonly QueuedTx[],
): Promise<PhaseAResult> => {
  const responses = await Promise.all(
    Array.from({ length: Math.ceil(queued.length / chunkSize) }, (_, chunk) =>
      pool.submit(
        packPhaseAJob(
          pool.allocateJobId(),
          queued.slice(chunk * chunkSize, chunk * chunkSize + chunkSize),
        ),
      ),
    ),
  );
  const accepted: PhaseAValidatedTx[] = [];
  const rejected: PhaseAResult["rejected"][number][] = [];
  for (const response of responses) {
    if (response.kind !== "phase_a") {
      throw new Error(`expected phase_a response, got ${response.kind}`);
    }
    for (const result of response.results) {
      if (result.ok)
        accepted.push(deserializePhaseACandidate(result.candidate));
      else {
        rejected.push({
          txId: Buffer.from(result.txId),
          code: result.code,
          detail: result.detail,
        });
      }
    }
  }
  return { accepted, rejected };
};

const runPoolPhaseB = (
  pool: FixedValidationWorkerPool,
  accepted: readonly PhaseAValidatedTx[],
  preState: Map<string, Buffer>,
): Promise<PhaseBResultWithPatch> =>
  Effect.runPromise(
    runPhaseBValidationWithPatch(accepted, preState, {
      nowCardanoSlotNo: 0n,
      bucketConcurrency: poolSize,
      evaluateScript: (bytes, contextCbor) =>
        Effect.tryPromise(() =>
          pool.submit({
            kind: "uplc",
            jobId: pool.allocateJobId(),
            scriptBytes: copyToTransferable(bytes),
            contextCbor: copyToTransferable(contextCbor),
          }),
        ).pipe(
          Effect.flatMap((response) => {
            if (response.kind !== "uplc") {
              return Effect.fail(
                new Error(`expected uplc response, got ${response.kind}`),
              );
            }
            return Effect.succeed(
              response.result.ok
                ? {
                    kind: "accepted" as const,
                    budget: {
                      cpu: response.result.cpu,
                      memory: response.result.memory,
                    },
                  }
                : {
                    kind: "script_invalid" as const,
                    detail: response.result.detail,
                  },
            );
          }),
        ),
    }),
  );

describe("Phase 2 script-heavy UPLC worker benchmark", () => {
  it(
    "keeps every Plutus spend off the coordinator event loop",
    async () => {
      requirePositiveSafeInteger(batchSize, "BENCH_PHASE2_SCRIPT_BATCH_SIZE");
      requirePositiveSafeInteger(poolSize, "BENCH_PHASE2_POOL_SIZE");
      requirePositiveSafeInteger(chunkSize, "BENCH_PHASE2_CHUNK_SIZE");
      requirePositiveSafeInteger(durationMs, "BENCH_PHASE2_DURATION_MS");
      expect(fs.existsSync(workerEntry)).toBe(true);
      const cpuTopology = await readPhase2CpuTopology();
      const pinnedEightCore = cpuTopology.pinnedEightCore && poolSize === 6;
      const containerIdentity = assertGate
        ? await readPhase2ContainerIdentity(
            expectedNodeImage,
            cpuTopology.logicalCpuIds,
          )
        : undefined;
      const containerIdentityProved =
        containerIdentity?.proved === true &&
        expectedNodeImage === "node:22.22.2" &&
        /^sha256:[0-9a-f]{64}$/u.test(expectedNodeImageId) &&
        containerIdentity.imageId === expectedNodeImageId &&
        process.version === "v22.22.2";
      const assertedChunkSize = assertChunk128CandidateGate ? 128 : 64;
      if (
        assertGate &&
        (!pinnedEightCore ||
          batchSize !== 256 ||
          chunkSize !== assertedChunkSize ||
          durationMs < 300_000 ||
          !containerIdentityProved)
      ) {
        throw new Error(
          `${assertChunk128CandidateGate ? "BENCH_ASSERT_PHASE2_SCRIPT_CHUNK128" : "BENCH_ASSERT_PHASE2_SCRIPT"} requires a proved node:22.22.2 AutoRemove container whose immutable image ID exactly matches BENCH_PHASE2_NODE_IMAGE_ID, eight pinned physical cores, six workers, batch size 256, chunk size ${assertedChunkSize.toString()}, and at least 300000ms`,
        );
      }
      const chunkAbCorpusBinding = assertChunk128CandidateGate
        ? readChunkAbCorpusBinding()
        : undefined;
      const { queued, preState } = buildCorpus();
      const inlinePhaseA = await Effect.runPromise(
        runPhaseAValidation(queued, phaseAConfig),
      );
      expect(inlinePhaseA.rejected).toHaveLength(0);
      const inlinePhaseB = await Effect.runPromise(
        runPhaseBValidationWithPatch(inlinePhaseA.accepted, preState, {
          nowCardanoSlotNo: 0n,
          bucketConcurrency: 1,
        }),
      );
      expect(inlinePhaseB.rejected).toHaveLength(0);

      const pool = new FixedValidationWorkerPool(
        poolSize,
        poolSize * 4,
        30_000,
        workerEntry,
        { config: phaseAConfig, signatureVerifier: "node" },
      );
      const lag = monitorEventLoopDelay({ resolution: 1 });
      let accepted = 0;
      let rejected = 0;
      let batches = 0;
      let durationMsObserved = 0;
      try {
        await pool.start();
        const warmPhaseA = await runPoolPhaseA(pool, queued);
        const warmPhaseB = await runPoolPhaseB(
          pool,
          warmPhaseA.accepted,
          preState,
        );
        expect(warmPhaseA.rejected).toStrictEqual(inlinePhaseA.rejected);
        expect(normalizePhaseB(warmPhaseB)).toStrictEqual(
          normalizePhaseB(inlinePhaseB),
        );

        lag.enable();
        const startedAt = performance.now();
        do {
          const phaseA = await runPoolPhaseA(pool, queued);
          const phaseB = await runPoolPhaseB(pool, phaseA.accepted, preState);
          expect(phaseA.rejected).toStrictEqual(inlinePhaseA.rejected);
          expect(normalizePhaseB(phaseB)).toStrictEqual(
            normalizePhaseB(inlinePhaseB),
          );
          accepted += phaseB.accepted.length;
          rejected += phaseA.rejected.length + phaseB.rejected.length;
          batches += 1;
          durationMsObserved = performance.now() - startedAt;
        } while (durationMsObserved < durationMs);
      } finally {
        lag.disable();
        await pool.close();
      }

      const eventLoopDelayP99Ms = lag.percentile(99) / 1_000_000;
      const report = {
        generatedAtIso: new Date().toISOString(),
        host: hostname(),
        cpuModel: cpus()[0]?.model ?? "unknown",
        availableParallelism: availableParallelism(),
        nodeVersion: process.version,
        expectedNodeImage,
        expectedNodeImageId,
        nodeImage: containerIdentity?.image ?? expectedNodeImage,
        nodeImageId: containerIdentity?.imageId,
        containerIdentity,
        containerIdentityProved,
        affinityLogicalCpuIds: cpuTopology.logicalCpuIds,
        affinityPhysicalCoreIds: cpuTopology.physicalCoreIds,
        pinnedEightCore,
        batchSize,
        poolSize,
        chunkSize,
        signatureVerifier: "node",
        gateMode: assertChunk128CandidateGate
          ? "chunk128_candidate"
          : "production_default_chunk64",
        everyTransactionHasPlutusSpend: true,
        everyTransactionIsPlutusV3: true,
        uplcInWorkers: true,
        verdictMatchesInline: true,
        statePatchMatchesInline: true,
        chunkAbExperimentId:
          chunkAbCorpusBinding === undefined ? undefined : chunkAbExperimentId,
        ...chunkAbCorpusBinding,
        durationMsRequested: durationMs,
        durationMsObserved,
        accepted,
        rejected,
        batches,
        eventLoopDelayP99Ms,
        gateAsserted:
          assertGate &&
          containerIdentityProved &&
          pinnedEightCore &&
          batchSize === 256 &&
          chunkSize === assertedChunkSize &&
          durationMsObserved >= 300_000 &&
          accepted > 0 &&
          batches > 0 &&
          rejected === 0,
      };
      fs.mkdirSync(resolve(outputPath, ".."), { recursive: true });
      fs.writeFileSync(outputPath, `${JSON.stringify(report, null, 2)}\n`);
      console.log(JSON.stringify(report));

      expect(rejected).toBe(0);
      if (assertGate) {
        expect(durationMsObserved).toBeGreaterThanOrEqual(300_000);
        expect(eventLoopDelayP99Ms).toBeLessThan(50);
        expect(report.gateAsserted).toBe(true);
      }
    },
    Math.max(420_000, durationMs + 120_000),
  );
});
