import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { inspect } from "node:util";

import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { decodeNodeUtxo } from "@/commands/command-utils.js";
import {
  CommitBuildCalibrationDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  MigrationRunner,
} from "@/database/index.js";
import * as Tx from "@/database/utils/tx.js";
import { Database, NodeConfig } from "@/services/index.js";
import { batchProgram, breakDownTx } from "@/utils.js";

type SeedInput = {
  readonly schemaVersion: "midgard-architecture-g-commit-candidate-seed-v1";
  readonly corpusSlicePath: string;
  readonly corpusSliceSha256: string;
  readonly fundingMapPath: string;
  readonly fundingMapSha256: string;
  readonly expectedTransactionCount: number;
  readonly firstTimestampIso: string;
};

const inputPath =
  process.env.MPF_COMMIT_CANDIDATE_SEED_INPUT?.trim() ??
  process.argv[2]?.trim() ??
  "";
const batchSize = 1_000;

const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const outrefCbor = (label: string): Buffer => {
  const match = /^([0-9a-f]{64})#(0|[1-9]\d*)$/u.exec(label.toLowerCase());
  if (match === null) throw new Error(`Invalid funding outref ${label}`);
  return Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(match[1]!),
      BigInt(match[2]!),
    ).to_cbor_bytes(),
  );
};

const loadInput = async (): Promise<{
  readonly input: SeedInput;
  readonly rows: readonly { readonly txHash: string; readonly cbor: Buffer }[];
  readonly funding: readonly MempoolLedgerDB.EntryNoTimeStamp[];
}> => {
  if (inputPath.length === 0) throw new Error("Missing candidate seed input");
  const input = JSON.parse(await readFile(inputPath, "utf8")) as SeedInput;
  if (
    input.schemaVersion !== "midgard-architecture-g-commit-candidate-seed-v1" ||
    !Number.isSafeInteger(input.expectedTransactionCount) ||
    input.expectedTransactionCount <= 0
  ) {
    throw new Error("Invalid candidate seed input");
  }
  const corpusBytes = await readFile(input.corpusSlicePath);
  const fundingBytes = await readFile(input.fundingMapPath);
  if (sha256(corpusBytes) !== input.corpusSliceSha256) {
    throw new Error("Candidate seed corpus slice SHA-256 mismatch");
  }
  if (sha256(fundingBytes) !== input.fundingMapSha256) {
    throw new Error("Candidate seed funding map SHA-256 mismatch");
  }
  const rows = corpusBytes
    .toString("utf8")
    .split(/\r?\n/u)
    .filter((line) => line.trim().length > 0)
    .map((line) => {
      const row = JSON.parse(line) as {
        readonly txHash?: unknown;
        readonly canonicalCborHex?: unknown;
      };
      const txHash = String(row.txHash ?? "").toLowerCase();
      const cborHex = String(row.canonicalCborHex ?? "").toLowerCase();
      const cbor = Buffer.from(cborHex, "hex");
      if (
        !/^[0-9a-f]{64}$/u.test(txHash) ||
        cborHex.length === 0 ||
        cborHex.length % 2 !== 0 ||
        cbor.toString("hex") !== cborHex
      ) {
        throw new Error("Candidate seed corpus row is invalid");
      }
      return { txHash, cbor };
    });
  if (rows.length !== input.expectedTransactionCount) {
    throw new Error(
      `Candidate seed expected ${input.expectedTransactionCount.toString()} rows, got ${rows.length.toString()}`,
    );
  }
  const fundingMap = JSON.parse(fundingBytes.toString("utf8")) as {
    readonly schemaVersion?: unknown;
    readonly entries?: readonly {
      readonly outref?: unknown;
      readonly outputCbor?: unknown;
    }[];
  };
  if (
    fundingMap.schemaVersion !== "midgard-architecture-g-corpus-funding-v1" ||
    !Array.isArray(fundingMap.entries)
  ) {
    throw new Error("Candidate seed funding map schema is invalid");
  }
  const funding = fundingMap.entries.map((entry) => {
    const label = String(entry.outref ?? "").toLowerCase();
    const encodedOutref = outrefCbor(label);
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
  return { input, rows, funding };
};

void (async () => {
  const databaseName = process.env.POSTGRES_DB ?? "";
  if (!/^midgard_phase3_arch_g_[a-z0-9_]+$/u.test(databaseName)) {
    throw new Error(
      `Refusing candidate seed for non-benchmark POSTGRES_DB=${JSON.stringify(databaseName)}`,
    );
  }
  const { input, rows, funding } = await loadInput();
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
    yield* CommitBuildCalibrationDB.update(0.05);
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
    return {
      mempoolTxCount: yield* MempoolDB.retrieveTxCount,
      fundingCount: funding.length,
      terminalLedgerCount: net.produced.length,
      deltaCount: processed.length,
    };
  }).pipe(
    Effect.provide(Database.workerLayer),
    Effect.provide(NodeConfig.layer),
  );
  const result = await Effect.runPromise(program);
  process.stdout.write(
    `${JSON.stringify({
      schemaVersion: "midgard-architecture-g-commit-candidate-seed-result-v1",
      databaseName,
      corpusSliceSha256: input.corpusSliceSha256,
      ...result,
    })}\n`,
  );
})().catch((error: unknown) => {
  process.stderr.write(
    `${error instanceof Error ? (error.stack ?? error.message) : inspect(error, { depth: 12 })}\n`,
  );
  process.exitCode = 1;
});
