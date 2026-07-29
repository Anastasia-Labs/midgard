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
import { fetchLocalOgmiosShelleyGenesisSlotConfig } from "@/local-ledger-slot.js";
import { NodeConfig } from "@/services/config.js";
import { ProductionNativeMpfOwnerService } from "@/services/mpf-native-owner/index.js";
import {
  provideCommitBlockWorkerServices,
  runCommitBlockHeaderCandidateBuildProgram,
} from "@/workers/commit-block-header.js";
import {
  assertArchitectureGCandidateSlotRuntimeIdentityV1,
  decodeArchitectureGCommitCandidateInputV1,
  decodeArchitectureGFixtureCreationV1,
} from "@/workers/utils/mpf-commit-candidate-artifacts.js";

const inputPath =
  process.env.MPF_COMMIT_CANDIDATE_INPUT?.trim() ??
  process.argv[2]?.trim() ??
  "";
const probePath = resolve(fileURLToPath(import.meta.url));
const probeSha256 = createHash("sha256")
  .update(readFileSync(probePath))
  .digest("hex");

const loadInput = async (): Promise<{
  readonly input: ReturnType<typeof decodeArchitectureGCommitCandidateInputV1>;
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
  const parsed = decodeArchitectureGCommitCandidateInputV1(
    JSON.parse(inputBytes.toString("utf8")) as unknown,
  );
  const fixtureCreationBytes = await readFile(parsed.fixtureCreationPath);
  const actualFixtureCreationSha256 = createHash("sha256")
    .update(fixtureCreationBytes)
    .digest("hex");
  if (actualFixtureCreationSha256 !== parsed.fixtureCreationSha256) {
    throw new Error("Fixture creation evidence SHA-256 mismatch");
  }
  decodeArchitectureGFixtureCreationV1({
    value: JSON.parse(fixtureCreationBytes.toString("utf8")) as unknown,
    expectedFixturePath: parsed.levelPath,
    expectedMarker: parsed.workerInput.data.speculativeBuild.base.utxosRoot,
    expectedUtxos: parsed.fixtureInitialUtxoCount,
    expectedAggregate: parsed.baseUtxoPayloadAggregate,
    expectedFundingMapSha256: parsed.fundingMapSha256,
  });
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
      const customGenesis =
        nodeConfig.NETWORK === "Custom"
          ? yield* fetchLocalOgmiosShelleyGenesisSlotConfig({
              ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
              timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
            })
          : undefined;
      yield* Effect.try({
        try: () =>
          assertArchitectureGCandidateSlotRuntimeIdentityV1({
            input,
            runtimeNetwork: nodeConfig.NETWORK,
            ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
            customGenesis,
          }),
        catch: (cause) =>
          cause instanceof Error
            ? cause
            : new Error(
                "Failed to validate Architecture G slot runtime identity",
                { cause },
              ),
      });
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
