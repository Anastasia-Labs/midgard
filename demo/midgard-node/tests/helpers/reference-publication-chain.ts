import { open } from "node:fs/promises";
import { dirname } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect, Schema } from "effect";

import { acquirePublicationJournalLock } from "./publication-journal-lock.js";

export type PublicationOutcome =
  | "prepared"
  | "submitted"
  | "confirmed"
  | "rejected"
  | "unknown";
type OutRef = Pick<UTxO, "txHash" | "outputIndex">;
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

const key = ({ txHash, outputIndex }: OutRef) => `${txHash}#${outputIndex}`;
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

/** Bounds apply to every durable transaction which could still enter the chain. */
export class ReferencePublicationChain {
  readonly metrics = {
    transactionCount: 0,
    peakUnconfirmedTransactions: 0,
    peakUnconfirmedBytes: 0,
    backpressureCount: 0,
    resubmissions: 0,
    submissionDurationMs: 0,
  };
  constructor(
    readonly journal: PublicationJournal,
    private readonly backend: PublicationChainBackend,
    private readonly schedule: PublicationSchedule,
  ) {
    publicationAuthorityLifetime(1, schedule);
  }

  pending() {
    return [...this.journal.records.values()].filter(
      ({ outcome }) => outcome !== "confirmed" && outcome !== "rejected",
    );
  }
  private measure() {
    const pending = this.pending();
    this.metrics.peakUnconfirmedTransactions = Math.max(
      this.metrics.peakUnconfirmedTransactions,
      pending.length,
    );
    this.metrics.peakUnconfirmedBytes = Math.max(
      this.metrics.peakUnconfirmedBytes,
      pending.reduce((n, { transaction }) => n + transaction.signedBytes, 0),
    );
  }
  async reconcile(includeConfirmed = false) {
    for (const record of this.journal.records.values()) {
      if (
        record.outcome === "rejected" ||
        (record.outcome === "confirmed" && !includeConfirmed)
      )
        continue;
      const tx = record.transaction;
      const observation = await this.backend.observe(tx);
      if (observation.confirmed) {
        if (record.outcome !== "confirmed")
          await this.journal.outcome(tx.hash, "confirmed");
      } else if (
        observation.slot >= tx.expiresAtSlot ||
        observation.conflictingInputs?.length ||
        tx.dependencies.some(
          (id) => this.journal.records.get(id)?.outcome === "rejected",
        )
      ) {
        await this.journal.outcome(
          tx.hash,
          "rejected",
          observation.slot >= tx.expiresAtSlot
            ? "expired without canonical publication"
            : "funding input or predecessor invalidated",
        );
      } else if (record.outcome === "confirmed") {
        await this.journal.outcome(
          tx.hash,
          "unknown",
          "previously confirmed publication rolled back",
        );
      }
    }
  }
  private async submit(record: PublicationRecord) {
    if (
      record.transaction.dependencies.some(
        (id) =>
          !["submitted", "confirmed"].includes(
            this.journal.records.get(id)?.outcome ?? "missing",
          ),
      )
    )
      throw new Error(
        "Publication predecessor outcome must be resolved before child submission",
      );
    // Unknown is durable before I/O, including process death after node acceptance.
    await this.journal.outcome(
      record.transaction.hash,
      "unknown",
      "submission in progress",
    );
    let submittedHash: string;
    const submissionStarted = Date.now();
    try {
      submittedHash = await this.backend.submit(record.transaction);
    } catch (cause) {
      await this.journal.outcome(
        record.transaction.hash,
        "unknown",
        String(cause),
      );
      return;
    } finally {
      this.metrics.submissionDurationMs += Date.now() - submissionStarted;
    }
    if (submittedHash !== record.transaction.hash) {
      await this.journal.outcome(
        record.transaction.hash,
        "unknown",
        "provider returned a different transaction hash",
      );
      throw new Error("Publication submission hash mismatch");
    }
    await this.journal.outcome(record.transaction.hash, "submitted");
  }
  async resume() {
    await this.reconcile(true);
    if (
      this.pending().length > this.schedule.maxUnconfirmedTransactions ||
      this.pending().reduce(
        (bytes, { transaction }) => bytes + transaction.signedBytes,
        0,
      ) > this.schedule.maxUnconfirmedBytes
    )
      throw new Error(
        "Recorded unresolved chain exceeds the configured bounds; reconcile with its original schedule before resubmission",
      );
    // Replay in parent order using identical bytes, never replacing an ambiguous
    // transaction. All descendants remain journaled even if a parent expired.
    for (const record of this.pending()) {
      if (
        record.transaction.dependencies.some(
          (id) => this.journal.records.get(id)?.outcome === "rejected",
        )
      )
        continue;
      this.metrics.resubmissions += 1;
      await this.submit(record);
      if (record.outcome === "unknown")
        await this.waitForOutcome(record.transaction.hash);
    }
    await this.reconcile();
    this.measure();
  }
  async enqueue(transaction: PublicationTransaction) {
    if (transaction.signedBytes > this.schedule.maxUnconfirmedBytes)
      throw new Error("Signed publication exceeds pending byte bound");
    const deadline = Date.now() + this.schedule.confirmationAllowanceMs;
    while (
      this.pending().length >= this.schedule.maxUnconfirmedTransactions ||
      this.pending().reduce((n, { transaction: tx }) => n + tx.signedBytes, 0) +
        transaction.signedBytes >
        this.schedule.maxUnconfirmedBytes
    ) {
      this.metrics.backpressureCount += 1;
      await this.reconcile();
      if (Date.now() > deadline)
        throw new Error(
          "Publication backpressure confirmation deadline exceeded; resume the journal",
        );
      if (
        this.pending().length >= this.schedule.maxUnconfirmedTransactions ||
        this.pending().reduce(
          (n, { transaction: tx }) => n + tx.signedBytes,
          0,
        ) +
          transaction.signedBytes >
          this.schedule.maxUnconfirmedBytes
      )
        await this.backend.wait();
    }
    if (
      transaction.dependencies.some(
        (hash) => this.journal.records.get(hash)?.outcome === "rejected",
      )
    )
      throw new Error(
        "Publication predecessor rejected before child admission; reconcile before replacing the chain",
      );
    for (const {
      transaction: prior,
      outcome,
    } of this.journal.records.values()) {
      if (outcome === "rejected") continue;
      if (
        prior.roles.some(({ role }) =>
          transaction.roles.some((entry) => entry.role === role),
        )
      )
        throw new Error(
          "Publication role already assigned to a live transaction",
        );
      if (
        prior.inputs.some((input) =>
          transaction.inputs.some((other) => key(input) === key(other)),
        )
      )
        throw new Error("Publication funding input already reserved");
    }
    await this.journal.prepare(transaction);
    this.metrics.transactionCount += 1;
    this.measure();
    await this.submit(this.journal.records.get(transaction.hash)!);
    if (this.journal.records.get(transaction.hash)!.outcome === "unknown")
      await this.drain();
    if (this.journal.records.get(transaction.hash)!.outcome === "rejected")
      throw new Error(
        "Publication was rejected; signed record retained and descendants must be reconciled",
      );
  }
  private async waitForOutcome(hash: string) {
    const deadline = Date.now() + this.schedule.confirmationAllowanceMs;
    while (
      ["prepared", "submitted", "unknown"].includes(
        this.journal.records.get(hash)!.outcome,
      )
    ) {
      await this.reconcile();
      if (
        !["prepared", "submitted", "unknown"].includes(
          this.journal.records.get(hash)!.outcome,
        )
      )
        return;
      if (Date.now() > deadline)
        throw new Error(
          "Ambiguous publication outcome remains journaled; resume before extending its descendants",
        );
      await this.backend.wait();
    }
  }
  async drain() {
    const deadline = Date.now() + this.schedule.confirmationAllowanceMs;
    while (this.pending().length > 0) {
      await this.reconcile();
      if (this.pending().length === 0) break;
      if (Date.now() > deadline)
        throw new Error(
          "Unresolved publication outcomes remain durably journaled; resume to reconcile",
        );
      await this.backend.wait();
    }
  }
}

const canonicalSlotSchema = Schema.Number.pipe(
  Schema.filter((slot) => Number.isSafeInteger(slot) && slot >= 0),
);
const blockHashSchema = Schema.String.pipe(Schema.pattern(/^[0-9a-f]{64}$/u));
const canonicalTipSchema = Schema.Struct({
  error: Schema.optional(Schema.Null),
  result: Schema.Struct({ slot: canonicalSlotSchema, id: blockHashSchema }),
});
const indexerCheckpointsSchema = Schema.Array(
  Schema.Struct({
    slot_no: canonicalSlotSchema,
    header_hash: blockHashSchema,
  }),
);

/** The barrier prevents Kupo lag from being mistaken for an expired transaction. */
export const synchronizePublicationIndexer = async (
  ogmiosUrl: string,
  kupoUrl: string,
): Promise<number> => {
  const response = await fetch(ogmiosUrl, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryLedgerState/tip",
      params: {},
      id: "publication-tip",
    }),
  });
  const body: unknown = await response.json();
  if (!response.ok || !Schema.is(canonicalTipSchema)(body))
    throw new Error("Cannot establish canonical publication tip");
  const { slot, id: blockHash } = body.result;
  const deadline = Date.now() + 60_000;
  while (true) {
    const checkpointResponse = await fetch(`${kupoUrl}/checkpoints`);
    const checkpoints: unknown = await checkpointResponse.json();
    if (
      !checkpointResponse.ok ||
      !Schema.is(indexerCheckpointsSchema)(checkpoints)
    )
      throw new Error("Cannot read publication indexer checkpoints");
    if (
      checkpoints.some(
        (checkpoint) =>
          checkpoint.slot_no === slot && checkpoint.header_hash === blockHash,
      )
    )
      return slot;
    if (Date.now() >= deadline)
      throw new Error("Publication indexer has not reached canonical node tip");
    await pause(500);
  }
};

export const publishReferenceChain = async (
  params: Readonly<{
    lucid: LucidEvolution;
    targets: readonly SDK.ReferenceScriptTarget[];
    authPolicy: SDK.ReferenceScriptAuthPolicy;
    journalPath: string;
    maxTargetsPerBatch: number;
    schedule: PublicationSchedule;
    publicationLimit: (role: string) => number;
    synchronize: () => Promise<number>;
    wait: () => Promise<void>;
    now: () => number;
    priorPublications?: readonly {
      role: string;
      signedCbor: string;
      outRef: OutRef;
    }[];
  }>,
) => {
  const { lucid, authPolicy, targets } = params;
  const walletAddress = await lucid.wallet().address();
  const journal = await PublicationJournal.open(params.journalPath);
  const startedAt = params.now();
  const startedAtWall = Date.now();
  let constructionDurationMs = 0;
  const backend: PublicationChainBackend = {
    submit: (tx) => lucid.config().provider!.submitTx(tx.signedCbor),
    wait: params.wait,
    observe: async (tx) => {
      const slot = await params.synchronize();
      const outputs = await lucid.utxosByOutRef(
        tx.roles.map(({ outputIndex }) => ({ txHash: tx.hash, outputIndex })),
      );
      const confirmed = tx.roles.every(({ role, outputIndex }) => {
        const output = outputs.find((utxo) => utxo.outputIndex === outputIndex);
        const expected = targets.find((target) => target.name === role);
        return (
          expected !== undefined &&
          output?.scriptRef != null &&
          validatorToScriptHash(output.scriptRef) ===
            validatorToScriptHash(expected.script) &&
          output.assets[
            SDK.referenceScriptAuthUnit(authPolicy.policyId, role)
          ] === 1n
        );
      });
      const transactionStatus = confirmed
        ? undefined
        : await lucid.transactionStatus(tx.hash);
      const rootInputs = tx.inputs.filter(
        ({ txHash }) =>
          !journal.records.has(txHash) ||
          journal.records.get(txHash)!.outcome === "confirmed",
      );
      const visibleInputs =
        confirmed || rootInputs.length === 0
          ? []
          : await lucid.utxosByOutRef(rootInputs);
      const conflictingInputs =
        confirmed || transactionStatus?.status !== "not_found"
          ? []
          : rootInputs.filter(
              (input) =>
                !visibleInputs.some((visible) => key(visible) === key(input)),
            );
      return { slot, confirmed, conflictingInputs };
    },
  };
  const scheduler = new ReferencePublicationChain(
    journal,
    backend,
    params.schedule,
  );
  try {
    if (journal.records.size === 0 && params.priorPublications?.length) {
      const imported = new Map<
        string,
        { signedCbor: string; roles: { role: string; outputIndex: number }[] }
      >();
      for (const receipt of params.priorPublications) {
        const entry = imported.get(receipt.outRef.txHash) ?? {
          signedCbor: receipt.signedCbor,
          roles: [],
        };
        if (
          entry.signedCbor !== receipt.signedCbor ||
          entry.roles.some(({ role }) => role === receipt.role)
        )
          throw new Error(
            "Retained publication receipts disagree or duplicate a role",
          );
        entry.roles.push({
          role: receipt.role,
          outputIndex: receipt.outRef.outputIndex,
        });
        imported.set(receipt.outRef.txHash, entry);
      }
      for (const [hash, entry] of imported) {
        const tx = publicationTransaction(
          entry.signedCbor,
          entry.roles,
          new Set(journal.records.keys()),
          params.now(),
        );
        if (tx.hash !== hash || !(await backend.observe(tx)).confirmed)
          throw new Error(
            "Retained publication must have matching signed bytes and canonical references before adoption",
          );
        await journal.prepare(tx);
        await journal.outcome(
          hash,
          "confirmed",
          "adopted retained canonical signed receipt",
        );
      }
    }
    // Journal identity is authenticated against the current policy and scripts,
    // including before replaying any recorded signed bytes.
    for (const { transaction: tx } of journal.records.values()) {
      for (const { role, outputIndex } of tx.roles) {
        const output = tx.outputs[outputIndex]?.utxo;
        const target = targets.find((candidate) => candidate.name === role);
        if (
          target === undefined ||
          output?.address !== walletAddress ||
          output.scriptRef == null ||
          validatorToScriptHash(output.scriptRef) !==
            validatorToScriptHash(target.script) ||
          output.assets[
            SDK.referenceScriptAuthUnit(authPolicy.policyId, role)
          ] !== 1n
        )
          throw new Error(
            "Publication journal differs from deployment identity",
          );
      }
    }
    await scheduler.resume();
    if (
      [...journal.records.values()].some(
        ({ outcome }) => outcome === "rejected",
      )
    )
      throw new Error(
        "Publication parent and descendants rejected and reconciled; replacements require proven mutually exclusive funding, or a new authority after expiry",
      );
    const assigned = new Set(
      [...journal.records.values()]
        .filter(({ outcome }) => outcome !== "rejected")
        .flatMap(({ transaction }) =>
          transaction.roles.map(({ role }) => role),
        ),
    );
    const confirmedWallet = await lucid.wallet().getUtxos();
    for (const target of targets) {
      if (
        !assigned.has(target.name) &&
        confirmedWallet.some(
          (utxo) =>
            (utxo.assets[
              SDK.referenceScriptAuthUnit(authPolicy.policyId, target.name)
            ] ?? 0n) !== 0n,
        )
      )
        throw new Error(
          `Reference ${target.name} exists without a signed journal record`,
        );
    }
    let remaining = targets.filter(({ name }) => !assigned.has(name));
    let funding: readonly UTxO[];
    if (
      remaining.length > 0 &&
      (await params.synchronize()) >= authPolicy.expiresAtSlot
    )
      throw new Error(
        "Publication authority expired with missing roles; invalid descendants are resolved and this identity cannot mint replacements",
      );
    const tail = [...journal.records.values()]
      .filter(({ outcome }) => outcome !== "rejected")
      .at(-1)?.transaction;
    const tailFunding = tail?.outputs[tail.fundingOutputIndex]?.utxo;
    const usableTail =
      tailFunding !== undefined &&
      (journal.records.get(tailFunding.txHash)!.outcome !== "confirmed" ||
        (await lucid.utxosByOutRef([tailFunding])).length === 1);
    if (tail !== undefined && usableTail)
      funding = [tail.outputs[tail.fundingOutputIndex]!.utxo];
    else {
      funding = SDK.selectReferenceScriptFundingUtxos(
        confirmedWallet,
        SDK.referenceScriptPublicationFundingTarget(targets.length) +
          BigInt(targets.length) * 10_000_000n,
      );
      if (funding.length === 0)
        throw new Error(
          "Insufficient plain confirmed funding for planned reference publication workload",
        );
    }
    while (remaining.length > 0) {
      const constructionStarted = Date.now();
      let count = 0;
      let estimatedBytes = 1024;
      for (const candidate of remaining.slice(0, params.maxTargetsPerBatch)) {
        const candidateBytes = candidate.script.script.length / 2 + 512;
        if (count > 0 && estimatedBytes + candidateBytes > 15_000) break;
        count += 1;
        estimatedBytes += candidateBytes;
      }
      let transaction: PublicationTransaction | undefined;
      while (count > 0) {
        const batch = remaining.slice(0, count);
        try {
          const { tx, layout } = await Effect.runPromise(
            SDK.completeReferenceScriptPublicationTxProgram({
              lucid,
              selectedFundingInputs: funding,
              walletAddress,
              referenceScriptsAddress: walletAddress,
              missingTargets: batch,
              authPolicy,
            }),
          );
          // Seed-wallet signing also resolves input owners. Give it the same
          // exact predecessor context used by completion, then release the view.
          lucid.overrideUTxOs([...funding]);
          const signed = await tx.sign
            .withWallet()
            .complete()
            .finally(() => lucid.clearUTxOOverride());
          if (
            batch.some(
              ({ name }) =>
                signed.toCBOR().length / 2 > params.publicationLimit(name),
            )
          ) {
            if (count === 1)
              throw new Error(
                `${batch[0]!.name} exceeds signed publication limit`,
              );
            count -= 1;
            continue;
          }
          transaction = publicationTransaction(
            signed.toCBOR(),
            batch.map(({ name }) => {
              const output = layout.localReferenceOutputs.get(name);
              if (output === undefined)
                throw new Error(`Missing publication output ${name}`);
              return { role: name, outputIndex: output.outputIndex };
            }),
            new Set(journal.records.keys()),
            params.now(),
          );
          break;
        } catch (cause) {
          // Lucid enforces maxTxSize before signing. Only a size error justifies
          // retrying a smaller batch; evaluation/funding failures remain fatal.
          if (
            count > 1 &&
            /maximum transaction size|max.*tx.*size|transaction.*too.*large|maximum.*size|MaxTxSize|MaxTransactionSize/i.test(
              String(cause),
            )
          ) {
            count -= 1;
            continue;
          }
          throw cause;
        }
      }
      if (transaction === undefined)
        throw new Error("Could not construct signed publication batch");
      constructionDurationMs += Date.now() - constructionStarted;
      await scheduler.enqueue(transaction);
      funding = [transaction.outputs[transaction.fundingOutputIndex]!.utxo];
      remaining = remaining.slice(count);
    }
    await scheduler.drain();
    if (scheduler.pending().length !== 0)
      throw new Error(
        "Reference publication ended with unresolved transactions",
      );
    return {
      transactions: [...journal.records.values()]
        .filter(({ outcome }) => outcome === "confirmed")
        .map(({ transaction }) => transaction),
      metrics: {
        ...scheduler.metrics,
        transactionCount: [...journal.records.values()].filter(
          ({ outcome }) => outcome === "confirmed",
        ).length,
        rejectedTransactions: [...journal.records.values()].filter(
          ({ outcome }) => outcome === "rejected",
        ).length,
        durationMs: Date.now() - startedAtWall,
        chainDurationMs: params.now() - startedAt,
        constructionDurationMs,
      },
    };
  } finally {
    await journal.close();
  }
};
