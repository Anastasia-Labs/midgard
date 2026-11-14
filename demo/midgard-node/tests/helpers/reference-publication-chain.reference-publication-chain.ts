import {
  key,
  publicationAuthorityLifetime,
  type PublicationChainBackend,
  PublicationJournal,
  type PublicationRecord,
  type PublicationSchedule,
  type PublicationTransaction,
} from "./reference-publication-chain.publication-journal.js";

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
