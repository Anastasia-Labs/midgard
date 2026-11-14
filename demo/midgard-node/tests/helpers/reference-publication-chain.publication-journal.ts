import { open } from "node:fs/promises";
import { dirname } from "node:path";

import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";

import { acquirePublicationJournalLock } from "./publication-journal-lock.js";

export type PublicationOutcome =
  | "prepared"
  | "submitted"
  | "confirmed"
  | "rejected"
  | "unknown";

export type OutRef = Pick<UTxO, "txHash" | "outputIndex">;

export type PublicationTransaction = Readonly<{
  hash: string;
  signedCbor: string;
  signedBytes: number;
  dependencies: readonly string[];
  inputs: readonly OutRef[];
  outputs: readonly { cbor: string; utxo: UTxO }[];
  roles: readonly { role: string; outputIndex: number }[];
  fundingOutputIndex: number;
  expiresAtSlot: number;
  preparedAt: number;
}>;

export type PublicationRecord = {
  transaction: PublicationTransaction;
  outcome: PublicationOutcome;
  updatedAt: number;
  reason?: string;
};

export type PublicationSchedule = Readonly<{
  maxUnconfirmedTransactions: number;
  maxUnconfirmedBytes: number;
  /** Measured construction, signing and submission rate, excluding block waits. */
  measuredTransactionsPerSecond: number;
  measuredBytesPerSecond: number;
  confirmationAllowanceMs: number;
}>;

export const DEFAULT_PUBLICATION_SCHEDULE: PublicationSchedule = {
  maxUnconfirmedTransactions: 8,
  maxUnconfirmedBytes: 100_000,
  measuredTransactionsPerSecond: 0.2,
  measuredBytesPerSecond: 2_000,
  confirmationAllowanceMs: 180_000,
};

export const publicationAuthorityLifetime = (
  roleCount: number,
  schedule: PublicationSchedule,
): number => {
  if (
    !Number.isSafeInteger(roleCount) ||
    roleCount < 1 ||
    !Number.isSafeInteger(schedule.maxUnconfirmedTransactions) ||
    schedule.maxUnconfirmedTransactions < 1 ||
    !Number.isSafeInteger(schedule.maxUnconfirmedBytes) ||
    schedule.maxUnconfirmedBytes < 16_384 ||
    !Number.isFinite(schedule.measuredTransactionsPerSecond) ||
    schedule.measuredTransactionsPerSecond <= 0 ||
    !Number.isFinite(schedule.measuredBytesPerSecond) ||
    schedule.measuredBytesPerSecond <= 0 ||
    !Number.isSafeInteger(schedule.confirmationAllowanceMs) ||
    schedule.confirmationAllowanceMs < 1 ||
    schedule.confirmationAllowanceMs > 15 * 60_000
  )
    throw new Error("Invalid bounded publication schedule");
  // One signed transaction per role is a conservative planning bound. Packing
  // uses actual signed sizes; the allowance covers a final slow block, not an
  // arbitrary multi-day authority. Include preparation AND chain service time.
  return (
    Math.ceil(
      1_000 *
        (roleCount / schedule.measuredTransactionsPerSecond +
          (roleCount * 16_384) / schedule.measuredBytesPerSecond),
    ) + schedule.confirmationAllowanceMs
  );
};

export const key = ({ txHash, outputIndex }: OutRef) =>
  `${txHash}#${outputIndex}`;

const plain = (utxo: UTxO) =>
  utxo.scriptRef == null &&
  utxo.datum == null &&
  utxo.datumHash == null &&
  Object.keys(utxo.assets).every((unit) => unit === "lovelace") &&
  (utxo.assets.lovelace ?? 0n) > 0n;

const stringify = (value: unknown) =>
  JSON.stringify(value, (_, item) =>
    typeof item === "bigint" ? { bigint: item.toString() } : item,
  );

const parse = (value: string) =>
  JSON.parse(value, (_, item) =>
    item !== null &&
    typeof item === "object" &&
    Object.keys(item).length === 1 &&
    typeof item.bigint === "string"
      ? BigInt(item.bigint)
      : item,
  );

export const publicationTransaction = (
  signedCbor: string,
  roles: PublicationTransaction["roles"],
  knownHashes: ReadonlySet<string>,
  now: number,
): PublicationTransaction => {
  const body = CML.Transaction.from_cbor_hex(signedCbor).body();
  const hash = CML.hash_transaction(body).to_hex();
  const inputs = Array.from({ length: body.inputs().len() }, (_, i) => {
    const input = body.inputs().get(i);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  });
  const outputs = Array.from(
    { length: body.outputs().len() },
    (_, outputIndex) => {
      const native = body.outputs().get(outputIndex);
      const output = coreToTxOutput(native);
      return {
        cbor: native.to_cbor_hex(),
        utxo: {
          txHash: hash,
          outputIndex,
          address: output.address,
          assets: output.assets,
          datum: output.datum ?? undefined,
          datumHash: output.datumHash ?? undefined,
          scriptRef: output.scriptRef ?? undefined,
        },
      };
    },
  );
  const funding = outputs
    .filter(({ utxo }) => plain(utxo))
    .sort((a, b) =>
      (a.utxo.assets.lovelace ?? 0n) > (b.utxo.assets.lovelace ?? 0n) ? -1 : 1,
    )[0];
  const ttl = body.ttl();
  if (funding === undefined || ttl === undefined || roles.length === 0)
    throw new Error(
      "Publication must designate funding, expiry and reference roles",
    );
  return {
    hash,
    signedCbor,
    signedBytes: signedCbor.length / 2,
    dependencies: [
      ...new Set(
        inputs.map(({ txHash }) => txHash).filter((id) => knownHashes.has(id)),
      ),
    ],
    inputs,
    outputs,
    roles,
    fundingOutputIndex: funding.utxo.outputIndex,
    expiresAtSlot: Number(ttl),
    preparedAt: now,
  };
};

/** Append-only signed records. An fsync completes before any network submit. */
export class PublicationJournal {
  readonly records = new Map<string, PublicationRecord>();
  private constructor(
    private readonly file: Awaited<ReturnType<typeof open>>,
    private readonly releaseLock: () => Promise<void>,
  ) {}

  static async open(path: string): Promise<PublicationJournal> {
    const releaseLock = await acquirePublicationJournalLock(`${path}.lock`);
    let file: Awaited<ReturnType<typeof open>>;
    try {
      file = await open(path, "a+", 0o600);
      await file.sync();
      const directory = await open(dirname(path), "r");
      try {
        await directory.sync();
      } finally {
        await directory.close();
      }
    } catch (cause) {
      await releaseLock();
      throw cause;
    }
    const journal = new PublicationJournal(file, releaseLock);
    try {
      const content = await file.readFile("utf8");
      const completeEnd = content.lastIndexOf("\n") + 1;
      if (completeEnd !== content.length) {
        // A partial write cannot have preceded a submission: prepare fsync is
        // awaited. Preserve the interrupted bytes before dropping only the tail.
        const tail = await open(
          `${path}.interrupted-${Date.now()}`,
          "wx",
          0o600,
        );
        await tail.writeFile(content.slice(completeEnd));
        await tail.sync();
        await tail.close();
        await file.truncate(Buffer.byteLength(content.slice(0, completeEnd)));
        await file.sync();
      }
      for (const line of content
        .slice(0, completeEnd)
        .split("\n")
        .filter(Boolean)) {
        const event = parse(line);
        if (event.kind === "prepared") {
          const tx: PublicationTransaction = event.transaction;
          const derived = publicationTransaction(
            tx.signedCbor,
            tx.roles,
            new Set(journal.records.keys()),
            tx.preparedAt,
          );
          if (
            stringify(derived) !== stringify(tx) ||
            journal.records.has(tx.hash)
          )
            throw new Error(
              "Publication journal signed transaction metadata differs",
            );
          journal.records.set(tx.hash, {
            transaction: tx,
            outcome: "prepared",
            updatedAt: tx.preparedAt,
          });
        } else if (
          event.kind === "outcome" &&
          journal.records.has(event.hash)
        ) {
          Object.assign(journal.records.get(event.hash)!, {
            outcome: event.outcome,
            updatedAt: event.at,
            reason: event.reason,
          });
        } else throw new Error("Invalid publication journal event");
      }
      return journal;
    } catch (cause) {
      await journal.close();
      throw cause;
    }
  }

  private async append(event: unknown) {
    await this.file.writeFile(`${stringify(event)}\n`);
    await this.file.sync();
  }
  async prepare(transaction: PublicationTransaction) {
    const derived = publicationTransaction(
      transaction.signedCbor,
      transaction.roles,
      new Set(this.records.keys()),
      transaction.preparedAt,
    );
    if (stringify(derived) !== stringify(transaction))
      throw new Error(
        "Prepared publication metadata differs from actual signed bytes",
      );
    if (this.records.has(transaction.hash))
      throw new Error("Transaction already journaled");
    await this.append({ kind: "prepared", transaction });
    this.records.set(transaction.hash, {
      transaction,
      outcome: "prepared",
      updatedAt: transaction.preparedAt,
    });
  }
  async outcome(hash: string, outcome: PublicationOutcome, reason?: string) {
    const record = this.records.get(hash);
    if (record === undefined)
      throw new Error("Outcome without signed transaction record");
    const at = Date.now();
    await this.append({ kind: "outcome", hash, outcome, at, reason });
    Object.assign(record, { outcome, updatedAt: at, reason });
  }
  async close() {
    try {
      await this.file.close();
    } finally {
      await this.releaseLock();
    }
  }
}

export type PublicationObservation = Readonly<{
  /** Slot of a canonical node tip for which the indexer has caught up. */
  slot: number;
  confirmed: boolean;
  /** A canonical spend of a root funding input invalidates this transaction. */
  conflictingInputs?: readonly OutRef[];
}>;

export type PublicationChainBackend = Readonly<{
  observe: (
    transaction: PublicationTransaction,
  ) => Promise<PublicationObservation>;
  submit: (transaction: PublicationTransaction) => Promise<string>;
  wait: () => Promise<void>;
}>;
