import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";

import { fullScanCounter as confirmedLedgerFullScanCounter } from "@/database/confirmedLedger.js";
import * as MpfEngineStateDB from "@/database/mpfEngineState.js";
import { NodeConfig } from "@/services/config.js";
import { ProductionNativeMpfOwnerService } from "@/services/mpf-native-owner/index.js";
import {
  provideCommitBlockWorkerServices,
  runCommitBlockHeaderCandidateBuildProgram,
} from "@/workers/commit-block-header.js";
import type { WorkerInput } from "@/workers/utils/commit-block-header.js";

type CandidateProbeInput = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-input-v1";
  readonly levelPath: string;
  readonly binaryPath: string;
  readonly binarySha256: string;
  readonly sidecarPath: string;
  readonly expectedTransactionCount: number;
  readonly corpusSha256: string;
  readonly corpusSliceSha256: string;
  readonly fundingMapSha256: string;
  readonly fixtureCreationPath: string;
  readonly fixtureCreationSha256: string;
  readonly fixtureInitialUtxoCount: number;
  readonly baseUtxoPayloadAggregate: {
    readonly entryCount: number;
    readonly encodedTupleBytes: number;
  };
  readonly workerInput: Omit<WorkerInput, "nativeMpf">;
};

const inputPath =
  process.env.MPF_COMMIT_CANDIDATE_INPUT?.trim() ??
  process.argv[2]?.trim() ??
  "";
const probePath = resolve(fileURLToPath(import.meta.url));
const probeSha256 = createHash("sha256")
  .update(readFileSync(probePath))
  .digest("hex");

const requireHash = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${field} must be a canonical SHA-256 hex digest`);
  }
  return value;
};

const loadInput = async (): Promise<{
  readonly input: CandidateProbeInput;
  readonly resolvedInputPath: string;
  readonly inputSha256: string;
}> => {
  if (inputPath.length === 0) {
    throw new Error(
      "Set MPF_COMMIT_CANDIDATE_INPUT or pass the candidate input path",
    );
  }
  const resolvedInputPath = resolve(inputPath);
  const inputBytes = await readFile(resolvedInputPath);
  const parsed = JSON.parse(inputBytes.toString("utf8")) as CandidateProbeInput;
  if (
    parsed.schemaVersion !==
      "midgard-architecture-g-commit-candidate-input-v1" ||
    !Number.isSafeInteger(parsed.expectedTransactionCount) ||
    parsed.expectedTransactionCount <= 0 ||
    !Number.isSafeInteger(parsed.baseUtxoPayloadAggregate?.entryCount) ||
    parsed.baseUtxoPayloadAggregate.entryCount <= 0 ||
    !Number.isSafeInteger(parsed.baseUtxoPayloadAggregate?.encodedTupleBytes) ||
    parsed.baseUtxoPayloadAggregate.encodedTupleBytes <= 0 ||
    !Number.isSafeInteger(parsed.fixtureInitialUtxoCount) ||
    parsed.fixtureInitialUtxoCount <= 0 ||
    parsed.workerInput?.data?.speculativeBuild === undefined
  ) {
    throw new Error("Invalid Architecture G commit-candidate probe input");
  }
  requireHash(parsed.binarySha256, "binarySha256");
  requireHash(parsed.corpusSha256, "corpusSha256");
  requireHash(parsed.corpusSliceSha256, "corpusSliceSha256");
  requireHash(parsed.fundingMapSha256, "fundingMapSha256");
  requireHash(parsed.fixtureCreationSha256, "fixtureCreationSha256");
  const fixtureCreationBytes = await readFile(parsed.fixtureCreationPath);
  const actualFixtureCreationSha256 = createHash("sha256")
    .update(fixtureCreationBytes)
    .digest("hex");
  if (actualFixtureCreationSha256 !== parsed.fixtureCreationSha256) {
    throw new Error("Fixture creation evidence SHA-256 mismatch");
  }
  const fixtureCreation = JSON.parse(fixtureCreationBytes.toString("utf8")) as {
    readonly fixtureCreated?: unknown;
    readonly fixturePath?: unknown;
    readonly marker?: unknown;
    readonly initialUtxoCount?: unknown;
    readonly utxoPayloadAggregate?: {
      readonly entryCount?: unknown;
      readonly encodedTupleBytes?: unknown;
    };
  };
  if (
    fixtureCreation.fixtureCreated !== true ||
    resolve(String(fixtureCreation.fixturePath ?? "")) !==
      resolve(parsed.levelPath) ||
    fixtureCreation.marker !==
      parsed.workerInput.data.speculativeBuild.base.utxosRoot ||
    fixtureCreation.initialUtxoCount !== parsed.fixtureInitialUtxoCount ||
    parsed.fixtureInitialUtxoCount !==
      parsed.baseUtxoPayloadAggregate.entryCount ||
    fixtureCreation.utxoPayloadAggregate?.entryCount !==
      parsed.baseUtxoPayloadAggregate.entryCount ||
    fixtureCreation.utxoPayloadAggregate?.encodedTupleBytes !==
      parsed.baseUtxoPayloadAggregate.encodedTupleBytes
  ) {
    throw new Error(
      "Fixture creation evidence does not bind the candidate path, root, cardinality, and payload aggregate",
    );
  }
  return {
    input: parsed,
    resolvedInputPath,
    inputSha256: createHash("sha256").update(inputBytes).digest("hex"),
  };
};

void (async () => {
  const { input, resolvedInputPath, inputSha256 } = await loadInput();
  const owner = await ProductionNativeMpfOwnerService.create({
    levelPath: input.levelPath,
    binaryPath: input.binaryPath,
    binarySha256: input.binarySha256,
    sidecarPath: input.sidecarPath,
  });
  try {
    const before = await owner.diagnostics();
    const processStatus = await readFile("/proc/self/status", "utf8");
    const cpuAffinity =
      processStatus.match(/^Cpus_allowed_list:\s*(.+)$/mu)?.[1]?.trim() ??
      "unknown";
    if (
      input.workerInput.data.speculativeBuild?.base.utxosRoot !==
      before.durableRoot
    ) {
      throw new Error(
        `Commit-candidate input base root ${String(input.workerInput.data.speculativeBuild?.base.utxosRoot)} does not match owner durable root ${before.durableRoot}`,
      );
    }
    const port = owner.createWorkerPort();
    let providerBoundaryAttempts = 0;
    const rejectProviderBoundary = () =>
      Effect.sync(() => {
        providerBoundaryAttempts += 1;
        throw new Error(
          "Commit-candidate probe crossed the provider/signing boundary",
        );
      });
    const program = Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const nodeConfig = yield* NodeConfig;
      if (
        nodeConfig.MPF_ENGINE !== "architecture_g" ||
        nodeConfig.MPF_NATIVE_OWNER_BINARY_SHA256 !== input.binarySha256 ||
        nodeConfig.MPF_SCRATCH_BUILD !== "fromlist" ||
        nodeConfig.MPF_PAYLOAD_ROOT_CHECK !== "off" ||
        !nodeConfig.MPF_PARALLEL_ROOTS ||
        nodeConfig.COMMIT_BUILD_COST_MODEL !== "ewma" ||
        nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE <
          input.expectedTransactionCount ||
        nodeConfig.COMMIT_MAX_L2_TX_COUNT < input.expectedTransactionCount ||
        nodeConfig.COMMIT_MAX_LEDGER_OP_COUNT <
          input.expectedTransactionCount * 3 ||
        nodeConfig.COMMIT_MAX_TRANSITION_STEP_COUNT <
          input.expectedTransactionCount
      ) {
        return yield* Effect.fail(
          new Error(
            "Commit-candidate probe requires recorded Architecture G candidate settings (fromlist, payload off, parallel roots, EWMA, and sufficient page/planner caps)",
          ),
        );
      }
      yield* MpfEngineStateDB.stampLedgerPayloadAggregate({
        rootHex: before.durableRoot,
        aggregate: input.baseUtxoPayloadAggregate,
      });
      const journalBefore = yield* sql<{
        readonly count: string;
      }>`SELECT COUNT(*)::text AS count FROM pending_block_finalizations`;
      const scansBefore = yield* Metric.value(confirmedLedgerFullScanCounter);
      const startedAt = performance.now();
      const candidate = yield* runCommitBlockHeaderCandidateBuildProgram(
        {
          ...input.workerInput,
          nativeMpf: {
            port,
            durableRoot: before.durableRoot,
            ownerBinarySha256: input.binarySha256,
          },
        },
        rejectProviderBoundary,
      );
      const durationMs = performance.now() - startedAt;
      const scansAfter = yield* Metric.value(confirmedLedgerFullScanCounter);
      const journalAfter = yield* sql<{
        readonly count: string;
      }>`SELECT COUNT(*)::text AS count FROM pending_block_finalizations`;
      return {
        candidate,
        durationMs,
        confirmedLedgerFullScans: scansAfter.count - scansBefore.count,
        journalRowsBefore: Number(journalBefore[0]?.count ?? "-1"),
        journalRowsAfter: Number(journalAfter[0]?.count ?? "-1"),
        candidateConfig: {
          mpfEngine: nodeConfig.MPF_ENGINE,
          scratchBuild: nodeConfig.MPF_SCRATCH_BUILD,
          payloadRootCheck: nodeConfig.MPF_PAYLOAD_ROOT_CHECK,
          parallelRoots: nodeConfig.MPF_PARALLEL_ROOTS,
          costModel: nodeConfig.COMMIT_BUILD_COST_MODEL,
          mempoolRetrievePageSize: nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE,
          maxL2TxCount: nodeConfig.COMMIT_MAX_L2_TX_COUNT,
          maxLedgerOpCount: nodeConfig.COMMIT_MAX_LEDGER_OP_COUNT,
          maxTransitionStepCount: nodeConfig.COMMIT_MAX_TRANSITION_STEP_COUNT,
        },
      };
    });
    const measured = await Effect.runPromise(
      provideCommitBlockWorkerServices(program),
    );
    const after = await owner.diagnostics();
    if (
      measured.candidate.expectedL2TransactionCount !==
      input.expectedTransactionCount
    ) {
      throw new Error(
        `Commit-candidate selected ${measured.candidate.expectedL2TransactionCount.toString()} transactions, expected ${input.expectedTransactionCount.toString()}`,
      );
    }
    if (measured.journalRowsAfter !== measured.journalRowsBefore) {
      throw new Error("Commit-candidate build-only probe mutated the journal");
    }
    process.stdout.write(
      `${JSON.stringify({
        schemaVersion: "midgard-architecture-g-commit-candidate-probe-v1",
        probePath,
        probeSha256,
        inputPath: resolvedInputPath,
        inputSha256,
        expectedTransactionCount: input.expectedTransactionCount,
        corpusSha256: input.corpusSha256,
        corpusSliceSha256: input.corpusSliceSha256,
        fundingMapSha256: input.fundingMapSha256,
        fixtureCreationSha256: input.fixtureCreationSha256,
        fixtureInitialUtxoCount: input.fixtureInitialUtxoCount,
        baseUtxoPayloadAggregate: input.baseUtxoPayloadAggregate,
        binarySha256: input.binarySha256,
        cpuAffinity,
        durationMs: measured.durationMs,
        confirmedLedgerFullScans: measured.confirmedLedgerFullScans,
        journalRowsBefore: measured.journalRowsBefore,
        journalRowsAfter: measured.journalRowsAfter,
        candidateConfig: measured.candidateConfig,
        providerBoundaryAttempts,
        submissionAttempts: providerBoundaryAttempts,
        candidate: measured.candidate,
        ownerBefore: before,
        ownerAfter: after,
      })}\n`,
    );
  } finally {
    await owner.close();
  }
})().catch((error: unknown) => {
  process.stderr.write(
    `${error instanceof Error ? (error.stack ?? error.message) : inspect(error, { depth: 12 })}\n`,
  );
  process.exitCode = 1;
});
