import fs from "node:fs";
import { availableParallelism, cpus, hostname } from "node:os";
import { resolve } from "node:path";
import { monitorEventLoopDelay, performance } from "node:perf_hooks";
import { pathToFileURL } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { MidgardRedeemerTag } from "@al-ft/midgard-validation/midgard-redeemers";
import { runPhaseAValidation } from "@al-ft/midgard-validation/phase-a";
import {
  type PhaseBResultWithPatch,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation/phase-b";
import type {
  PhaseAResult,
  PhaseAValidatedTx,
  QueuedTx,
} from "@al-ft/midgard-validation/types";
import { deserializePhaseACandidate } from "@al-ft/midgard-validation/wire";
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
import { FixedValidationWorkerPool } from "../../src/services/validation-pool.fixed-validation-worker-pool.js";
import { packPhaseAJob } from "../../src/workers/utils/validation-pool.js";
import {
  readPhase2ContainerIdentity,
  readPhase2CpuTopology,
} from "./phase2-cpu-topology.js";

const quick = process.env.BENCH_QUICK === "1";
const assertProductionGate = process.env.BENCH_ASSERT_PHASE2_SCRIPT === "1";
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
  accepted: readonly PhaseAValidatedTx[],
  preState: Map<string, Buffer>,
): Promise<PhaseBResultWithPatch> =>
  Effect.runPromise(
    runPhaseBValidationWithPatch(accepted, preState, {
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      nowCardanoSlotNo: 0n,
      bucketConcurrency: poolSize,
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
      const containerIdentity = assertProductionGate
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
      if (
        assertProductionGate &&
        (!pinnedEightCore ||
          batchSize !== 256 ||
          chunkSize !== 64 ||
          durationMs < 300_000 ||
          !containerIdentityProved)
      ) {
        throw new Error(
          `BENCH_ASSERT_PHASE2_SCRIPT requires a proved node:22.22.2 AutoRemove container whose immutable image ID exactly matches BENCH_PHASE2_NODE_IMAGE_ID, eight pinned physical cores, six workers, batch size 256, chunk size 64, and at least 300000ms`,
        );
      }
      const { queued, preState } = buildCorpus();
      const inlinePhaseA = await Effect.runPromise(
        runPhaseAValidation(queued, phaseAConfig),
      );
      expect(inlinePhaseA.rejected).toHaveLength(0);
      const inlinePhaseB = await Effect.runPromise(
        runPhaseBValidationWithPatch(inlinePhaseA.accepted, preState, {
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
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
        { config: phaseAConfig },
      );
      const lag = monitorEventLoopDelay({ resolution: 1 });
      let accepted = 0;
      let rejected = 0;
      let batches = 0;
      let durationMsObserved = 0;
      try {
        await pool.start();
        const warmPhaseA = await runPoolPhaseA(pool, queued);
        const warmPhaseB = await runPoolPhaseB(warmPhaseA.accepted, preState);
        expect(warmPhaseA.rejected).toStrictEqual(inlinePhaseA.rejected);
        expect(normalizePhaseB(warmPhaseB)).toStrictEqual(
          normalizePhaseB(inlinePhaseB),
        );

        lag.enable();
        const startedAt = performance.now();
        do {
          const phaseA = await runPoolPhaseA(pool, queued);
          const phaseB = await runPoolPhaseB(phaseA.accepted, preState);
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
        everyTransactionHasPlutusSpend: true,
        everyTransactionIsPlutusV3: true,
        verdictMatchesInline: true,
        statePatchMatchesInline: true,
        durationMsRequested: durationMs,
        durationMsObserved,
        accepted,
        rejected,
        batches,
        eventLoopDelayP99Ms,
        gateAsserted:
          assertProductionGate &&
          containerIdentityProved &&
          pinnedEightCore &&
          batchSize === 256 &&
          chunkSize === 64 &&
          durationMsObserved >= 300_000 &&
          accepted > 0 &&
          batches > 0 &&
          rejected === 0,
      };
      fs.mkdirSync(resolve(outputPath, ".."), { recursive: true });
      fs.writeFileSync(outputPath, `${JSON.stringify(report, null, 2)}\n`);
      console.log(JSON.stringify(report));

      expect(rejected).toBe(0);
      if (assertProductionGate) {
        expect(durationMsObserved).toBeGreaterThanOrEqual(300_000);
        expect(eventLoopDelayP99Ms).toBeLessThan(50);
        expect(report.gateAsserted).toBe(true);
      }
    },
    Math.max(420_000, durationMs + 120_000),
  );
});
