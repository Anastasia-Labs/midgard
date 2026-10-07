import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";

import { fullScanCounter as confirmedLedgerFullScanCounter } from "../database/confirmedLedger.js";
import {
  CommitBuildCalibrationDB,
  MempoolDB,
  MempoolTxDeltasDB,
  ProcessedMempoolDB,
} from "../database/index.js";
import * as Tx from "../database/utils/tx.js";
import { fetchLocalOgmiosShelleyGenesisSlotConfig } from "../local-ledger-slot.js";
import { NodeConfig } from "../services/config.js";
import { type Lucid } from "../services/index.js";
import { ProductionNativeMpfOwnerService } from "../services/mpf-native-owner/index.js";
import { batchProgram, breakDownTx } from "../utils.js";
import {
  type CommitLucidFactory,
  provideCommitBlockWorkerServices,
  runCommitBlockHeaderWorkerProgram,
} from "./commit-block-header.js";
import {
  assertArchitectureGCandidateSlotRuntimeIdentity,
  decodeArchitectureGCommitCandidateInput,
  decodeArchitectureGFixtureCreation,
  validateArchitectureGCommitCandidateProbeResult,
} from "./utils/mpf-commit-candidate-artifacts.js";

const inputPath =
  process.env.MPF_COMMIT_CANDIDATE_INPUT?.trim() ??
  process.argv[2]?.trim() ??
  "";
const probePath = resolve(fileURLToPath(import.meta.url));
const probeSha256 = createHash("sha256")
  .update(readFileSync(probePath))
  .digest("hex");

const loadInput = async (): Promise<{
  readonly input: ReturnType<typeof decodeArchitectureGCommitCandidateInput>;
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
  const parsed = decodeArchitectureGCommitCandidateInput(
    JSON.parse(inputBytes.toString("utf8")) as unknown,
  );
  const fixtureCreationBytes = await readFile(parsed.fixtureCreationPath);
  const actualFixtureCreationSha256 = createHash("sha256")
    .update(fixtureCreationBytes)
    .digest("hex");
  if (actualFixtureCreationSha256 !== parsed.fixtureCreationSha256) {
    throw new Error("Fixture creation evidence SHA-256 mismatch");
  }
  decodeArchitectureGFixtureCreation({
    value: JSON.parse(fixtureCreationBytes.toString("utf8")) as unknown,
    expectedFixturePath: parsed.levelPath,
    expectedMarker: parsed.baseUtxosRoot,
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
    if (input.baseUtxosRoot !== before.durableRoot) {
      throw new Error(
        `Commit-candidate input base root ${input.baseUtxosRoot} does not match owner durable root ${before.durableRoot}`,
      );
    }
    const port = owner.createWorkerPort();
    // The build runs the production default path, which reads the deposit,
    // withdrawal, and tx-order barriers (and tx-order CEK program material)
    // through `api.utxosAt`. Those reads see an empty chain; any other
    // provider or wallet access is a boundary crossing and fails the run.
    let providerReads = 0;
    let providerBoundaryAttempts = 0;
    const crossBoundary = (property: PropertyKey): never => {
      providerBoundaryAttempts += 1;
      throw new Error(
        `Commit-candidate probe crossed the provider/signing boundary (${String(property)})`,
      );
    };
    const stubApi = new Proxy(
      {},
      {
        get: (_target, property) =>
          property === "utxosAt"
            ? async () => {
                providerReads += 1;
                return [];
              }
            : crossBoundary(property),
      },
    );
    const stubLucid = new Proxy(
      {},
      {
        get: (_target, property) =>
          property === "api" ? stubApi : crossBoundary(property),
      },
    ) as unknown as Lucid;
    const stubLucidFactory: CommitLucidFactory = () =>
      Effect.succeed(stubLucid);
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
          assertArchitectureGCandidateSlotRuntimeIdentity({
            input,
            runtimeNetwork: nodeConfig.NETWORK,
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
      // The deferral this build ends in moves the selected transactions to
      // processed_mempool and updates the build calibration. Each gate run
      // starts from the same seeded state, so both are restored afterwards.
      const processedBefore = yield* ProcessedMempoolDB.retrieve;
      if (processedBefore.length !== 0) {
        return yield* Effect.fail(
          new Error(
            "Commit-candidate probe requires an empty processed_mempool",
          ),
        );
      }
      const seededMempool = yield* Tx.retrieveAllEntries(MempoolDB.tableName);
      if (seededMempool.length !== input.expectedTransactionCount) {
        return yield* Effect.fail(
          new Error(
            `Commit-candidate probe requires ${input.expectedTransactionCount.toString()} seeded mempool transactions, found ${seededMempool.length.toString()}`,
          ),
        );
      }
      const calibrationBefore = yield* CommitBuildCalibrationDB.retrieve;
      const journalBefore = yield* sql<{
        readonly count: string;
      }>`SELECT COUNT(*)::text AS count FROM pending_block_finalizations`;
      const scansBefore = yield* Metric.value(confirmedLedgerFullScanCounter);
      const startedAt = performance.now();
      const output = yield* runCommitBlockHeaderWorkerProgram(
        {
          ...input.workerInput,
          nativeMpf: {
            port,
            durableRoot: before.durableRoot,
            ownerBinarySha256: input.binarySha256,
          },
        },
        undefined,
        stubLucidFactory,
      );
      const durationMs = performance.now() - startedAt;
      const scansAfter = yield* Metric.value(confirmedLedgerFullScanCounter);
      const journalAfter = yield* sql<{
        readonly count: string;
      }>`SELECT COUNT(*)::text AS count FROM pending_block_finalizations`;
      if (
        output.type !== "SkippedSubmissionOutput" ||
        output.candidate === undefined
      ) {
        return yield* Effect.fail(
          new Error(
            `Commit-candidate build did not defer a built block: ${JSON.stringify(output)}`,
          ),
        );
      }
      // The deferred candidate carries no deposit, forced-transaction or
      // withdrawal roots; they stay empty only while the fixture holds none
      // of those events, which the stub provider's empty reads cannot add.
      const userEventRows = yield* sql<{
        readonly deposits: string;
        readonly forcedTransactions: string;
        readonly withdrawals: string;
      }>`SELECT
        (SELECT COUNT(*) FROM deposits_utxos)::text AS deposits,
        (SELECT COUNT(*) FROM forced_transaction_utxos)::text AS "forcedTransactions",
        (SELECT COUNT(*) FROM withdrawal_utxos)::text AS withdrawals`;
      yield* ProcessedMempoolDB.clear;
      yield* batchProgram(
        1_000,
        seededMempool.length,
        "candidate-restore-mempool",
        (start, end) =>
          Tx.insertEntries(
            MempoolDB.tableName,
            seededMempool.slice(start, end),
          ),
        1,
      );
      const restoredTxs = yield* Effect.forEach(
        seededMempool,
        (entry) => breakDownTx(entry[Tx.Columns.TX]),
        { concurrency: 8 },
      );
      yield* batchProgram(
        1_000,
        restoredTxs.length,
        "candidate-restore-deltas",
        (start, end) =>
          MempoolTxDeltasDB.upsertMany(
            restoredTxs.slice(start, end).map(MempoolDB.toTxDelta),
          ),
        1,
      );
      yield* CommitBuildCalibrationDB.update(calibrationBefore.msPerTxEwma);
      const restoredCounts = yield* sql<{
        readonly mempool: string;
        readonly deltas: string;
        readonly processed: string;
      }>`SELECT
        (SELECT COUNT(*) FROM mempool)::text AS mempool,
        (SELECT COUNT(*) FROM mempool_tx_deltas)::text AS deltas,
        (SELECT COUNT(*) FROM processed_mempool)::text AS processed`;
      if (
        Number(restoredCounts[0]?.mempool ?? "-1") !== seededMempool.length ||
        Number(restoredCounts[0]?.deltas ?? "-1") !== seededMempool.length ||
        Number(restoredCounts[0]?.processed ?? "-1") !== 0
      ) {
        return yield* Effect.fail(
          new Error(
            `Commit-candidate probe failed to restore the seeded mempool: ${JSON.stringify(restoredCounts[0])}`,
          ),
        );
      }
      return {
        candidate: {
          endTimeMs: output.candidate.endTimeMs,
          l2TransactionCount: output.mempoolTxsCount,
          roots: output.candidate.roots,
        },
        durationMs,
        confirmedLedgerFullScans: scansAfter.count - scansBefore.count,
        userEventRows: {
          deposits: Number(userEventRows[0]?.deposits ?? "-1"),
          forcedTransactions: Number(
            userEventRows[0]?.forcedTransactions ?? "-1",
          ),
          withdrawals: Number(userEventRows[0]?.withdrawals ?? "-1"),
        },
        journalRowsBefore: Number(journalBefore[0]?.count ?? "-1"),
        journalRowsAfter: Number(journalAfter[0]?.count ?? "-1"),
        candidateConfig: {
          // Evidence label: the engine that built the candidate.
          mpfEngine: "architecture_g",
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
      measured.candidate.l2TransactionCount !== input.expectedTransactionCount
    ) {
      throw new Error(
        `Commit-candidate selected ${measured.candidate.l2TransactionCount.toString()} transactions, expected ${input.expectedTransactionCount.toString()}`,
      );
    }
    if (measured.journalRowsAfter !== measured.journalRowsBefore) {
      throw new Error("Commit-candidate build-only probe mutated the journal");
    }
    const artifact = validateArchitectureGCommitCandidateProbeResult({
      value: {
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
        userEventRows: measured.userEventRows,
        journalRowsBefore: measured.journalRowsBefore,
        journalRowsAfter: measured.journalRowsAfter,
        candidateConfig: measured.candidateConfig,
        providerReads,
        providerBoundaryAttempts,
        submissionAttempts: providerBoundaryAttempts,
        candidate: measured.candidate,
        ownerBefore: before,
        ownerAfter: after,
      },
      expectedInput: input,
      expectedInputPath: resolvedInputPath,
      expectedInputSha256: inputSha256,
      expectedProbePath: probePath,
      expectedProbeSha256: probeSha256,
      expectedCpuAffinity: cpuAffinity,
    });
    process.stdout.write(`${JSON.stringify(artifact)}\n`);
  } finally {
    await owner.close();
  }
})().catch((error: unknown) => {
  process.stderr.write(
    `${error instanceof Error ? (error.stack ?? error.message) : inspect(error, { depth: 12 })}\n`,
  );
  process.exitCode = 1;
});
