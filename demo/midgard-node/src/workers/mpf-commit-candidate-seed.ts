import { readFile } from "node:fs/promises";
import { inspect } from "node:util";

import { decodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { Effect } from "effect";

import { decodeNodeUtxo } from "../commands/command-utils.js";
import {
  CommitBuildCalibrationDB,
  ConfirmedLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  MigrationRunner,
  ProcessedMempoolDB,
} from "../database/index.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";
import { Database, NodeConfig } from "../services/index.js";
import { sha256Hex } from "../sha256.js";
import { batchProgram, breakDownTx } from "../utils.js";
import {
  buildCanonicalFixtureEntries,
  canonicalOutrefCborFromLabel,
  decodeCanonicalProbeRow,
} from "./mpf-engine-probe-corpus.js";
import {
  decodeArchitectureGCommitCandidateSeedInput,
  decodeArchitectureGCorpusFunding,
  toJsonSafeCount,
  validateArchitectureGCommitCandidateSeedResult,
} from "./utils/mpf-commit-candidate-artifacts.js";

const inputPath =
  process.env.MPF_COMMIT_CANDIDATE_SEED_INPUT?.trim() ??
  process.argv[2]?.trim() ??
  "";
const batchSize = 1_000;

const loadInput = async (): Promise<{
  readonly input: ReturnType<
    typeof decodeArchitectureGCommitCandidateSeedInput
  >;
  readonly rows: readonly { readonly txHash: string; readonly cbor: Buffer }[];
  readonly funding: readonly MempoolLedgerDB.EntryNoTimeStamp[];
  readonly confirmedLedger: readonly Ledger.Entry[];
}> => {
  if (inputPath.length === 0) throw new Error("Missing candidate seed input");
  const input = decodeArchitectureGCommitCandidateSeedInput(
    JSON.parse(await readFile(inputPath, "utf8")),
  );
  const corpusBytes = await readFile(input.corpusSlicePath);
  const fundingBytes = await readFile(input.fundingMapPath);
  if (sha256Hex(corpusBytes) !== input.corpusSliceSha256) {
    throw new Error("Candidate seed corpus slice SHA-256 mismatch");
  }
  if (sha256Hex(fundingBytes) !== input.fundingMapSha256) {
    throw new Error("Candidate seed funding map SHA-256 mismatch");
  }
  const rows = corpusBytes
    .toString("utf8")
    .split(/\r?\n/u)
    .filter((line) => line.trim().length > 0)
    .map((line, index) => {
      const row = decodeCanonicalProbeRow(
        JSON.parse(line) as Record<string, unknown>,
        index,
      );
      return { txHash: row.txHash, cbor: row.cbor };
    });
  if (rows.length !== input.expectedTransactionCount) {
    throw new Error(
      `Candidate seed expected ${input.expectedTransactionCount.toString()} rows, got ${rows.length.toString()}`,
    );
  }
  const fundingMap = decodeArchitectureGCorpusFunding({
    value: JSON.parse(fundingBytes.toString("utf8")),
    expectedCorpusSha256: input.phase1FormalBinding.corpus.corpusSha256,
    expectedSliceSha256: input.corpusSliceSha256,
  });
  const funding = fundingMap.entries.map((entry) => {
    const label = String(entry.outref ?? "").toLowerCase();
    const encodedOutref = canonicalOutrefCborFromLabel(label);
    const decoded = decodeNodeUtxo({
      outref: encodedOutref.toString("hex"),
      outputCbor: String(entry.outputCbor ?? "").toLowerCase(),
    });
    return {
      [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(label.slice(0, 64), "hex"),
      [MempoolLedgerDB.Columns.OUTREF]: encodedOutref,
      [MempoolLedgerDB.Columns.OUTPUT]: decoded.outputCbor,
      [MempoolLedgerDB.Columns.ADDRESS]: decoded.address,
      [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
    } satisfies MempoolLedgerDB.EntryNoTimeStamp;
  });
  // confirmed_ledger is the commit base the default build path hydrates: the
  // exact entries the Level fixture was created from, so its root equals the
  // fixture marker the native owner serves.
  const addressByOutput = new Map<string, string>();
  const confirmedLedger = buildCanonicalFixtureEntries(
    new Map(
      fundingMap.entries.map((entry) => [
        String(entry.outref ?? "").toLowerCase(),
        Buffer.from(String(entry.outputCbor ?? "").toLowerCase(), "hex"),
      ]),
    ),
    input.fixtureInitialUtxoCount,
  ).map(({ key, value }) => {
    const outputHex = value.toString("hex");
    let address = addressByOutput.get(outputHex);
    if (address === undefined) {
      address = decodeNodeUtxo({
        outref: key.toString("hex"),
        outputCbor: outputHex,
      }).address;
      addressByOutput.set(outputHex, address);
    }
    return {
      [Ledger.Columns.TX_ID]: Buffer.from(
        decodeMidgardSpendInputItem(key).txId,
      ),
      [Ledger.Columns.OUTREF]: key,
      [Ledger.Columns.OUTPUT]: value,
      [Ledger.Columns.ADDRESS]: address,
    };
  });
  return { input, rows, funding, confirmedLedger };
};

void (async () => {
  const databaseName = process.env.POSTGRES_DB ?? "";
  if (!/^midgard_phase3_arch_g_[a-z0-9_]+$/u.test(databaseName)) {
    throw new Error(
      `Refusing candidate seed for non-benchmark POSTGRES_DB=${JSON.stringify(databaseName)}`,
    );
  }
  const { input, rows, funding, confirmedLedger } = await loadInput();
  const firstTimestamp = new Date(input.firstTimestampIso);
  if (!Number.isFinite(firstTimestamp.getTime())) {
    throw new Error("Candidate seed firstTimestampIso is invalid");
  }
  const processed = await Effect.runPromise(
    Effect.forEach(
      rows,
      ({ txHash, cbor }) =>
        breakDownTx(cbor).pipe(
          Effect.tap((tx) =>
            tx.txId.toString("hex") === txHash
              ? Effect.void
              : Effect.fail(
                  new Error(
                    `Candidate seed transaction id mismatch expected=${txHash},actual=${tx.txId.toString("hex")}`,
                  ),
                ),
          ),
        ),
      { concurrency: 8 },
    ),
  );
  const program = Effect.gen(function* () {
    yield* MigrationRunner.migrate({
      appVersion: "phase3-architecture-g-candidate-seed",
      actor: "phase3-architecture-g-candidate-seed",
    });
    yield* MempoolDB.clear;
    yield* MempoolLedgerDB.clear;
    yield* MempoolTxDeltasDB.clear;
    yield* ProcessedMempoolDB.clear;
    yield* ConfirmedLedgerDB.clear;
    yield* CommitBuildCalibrationDB.update(0.05);
    yield* batchProgram(
      batchSize,
      confirmedLedger.length,
      "candidate-confirmed-ledger",
      (start, end) =>
        ConfirmedLedgerDB.insertMultiple(confirmedLedger.slice(start, end)),
      1,
    );
    yield* batchProgram(
      batchSize,
      funding.length,
      "candidate-funding",
      (start, end) => MempoolLedgerDB.insert(funding.slice(start, end)),
      1,
    );
    yield* batchProgram(
      batchSize,
      processed.length,
      "candidate-mempool",
      (start, end) =>
        Tx.insertEntries(
          MempoolDB.tableName,
          processed.slice(start, end).map((tx, offset) => ({
            [Tx.Columns.TX_ID]: tx.txId,
            [Tx.Columns.TX]: tx.txCbor,
            [Tx.Columns.TIMESTAMPTZ]: new Date(
              firstTimestamp.getTime() + start + offset,
            ),
          })),
        ),
      1,
    );
    yield* batchProgram(
      batchSize,
      processed.length,
      "candidate-deltas",
      (start, end) =>
        MempoolTxDeltasDB.upsertMany(
          processed.slice(start, end).map(MempoolDB.toTxDelta),
        ),
      1,
    );
    const net = MempoolDB.compactLedgerEffects(processed);
    yield* batchProgram(
      batchSize,
      net.produced.length,
      "candidate-terminal-ledger",
      (start, end) => MempoolLedgerDB.insert(net.produced.slice(start, end)),
      1,
    );
    yield* batchProgram(
      batchSize,
      net.spent.length,
      "candidate-funding-spend",
      (start, end) =>
        MempoolLedgerDB.clearUTxOs(net.spent.slice(start, end)).pipe(
          Effect.asVoid,
        ),
      1,
    );
    const mempoolTxCount = toJsonSafeCount(
      yield* MempoolDB.retrieveTxCount,
      "Candidate seed mempool transaction count",
    );
    return {
      mempoolTxCount,
      fundingCount: funding.length,
      terminalLedgerCount: net.produced.length,
      deltaCount: processed.length,
      confirmedLedgerCount: confirmedLedger.length,
    };
  }).pipe(
    Effect.provide(Database.workerLayer),
    Effect.provide(NodeConfig.layer),
  );
  const result = await Effect.runPromise(program);
  const artifact = validateArchitectureGCommitCandidateSeedResult({
    value: {
      schemaVersion: "midgard-architecture-g-commit-candidate-seed-result-v1",
      databaseName,
      corpusSliceSha256: input.corpusSliceSha256,
      ...result,
    },
    expectedDatabaseName: databaseName,
    expectedCorpusSliceSha256: input.corpusSliceSha256,
    expectedTransactionCount: input.expectedTransactionCount,
    expectedConfirmedLedgerCount: input.fixtureInitialUtxoCount,
  });
  process.stdout.write(`${JSON.stringify(artifact)}\n`);
})().catch((error: unknown) => {
  process.stderr.write(
    `${error instanceof Error ? (error.stack ?? error.message) : inspect(error, { depth: 12 })}\n`,
  );
  process.exitCode = 1;
});
