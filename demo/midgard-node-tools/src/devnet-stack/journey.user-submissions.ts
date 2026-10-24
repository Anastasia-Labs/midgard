import { join } from "node:path";

import type { UserRole } from "./identities.js";
import { describeError, FatalJourneyError } from "./journey-cli.js";
import { JourneyRuntime } from "./journey-runtime.js";
import {
  type DepositRecord,
  type Json,
  jsonValue,
  toValue,
  type TransferRecord,
  type Value,
  type WithdrawalRecord,
} from "./journey-values.js";

/**
 * What a submission will do, journaled before its first CLI call. A rerun
 * resumes the same submission id with exactly these parameters.
 */
type DepositIntent = {
  readonly submissionId: string;
  readonly user: UserRole;
  readonly value: Record<string, string>;
};
/**
 * `excludeOutRefs` is fixed when the intent is first journaled (absent on
 * intents journaled before it existed, which excluded nothing): the CLI
 * journals it as part of the submission id's intent.
 */
type TransferIntent = {
  readonly submissionId: string;
  readonly from: UserRole;
  readonly to: UserRole;
  readonly value: Record<string, string>;
  readonly excludeOutRefs?: readonly string[];
};

/** `submissionId` and `user` are absent on intents journaled before they were recorded. */
export type WithdrawalIntent = {
  readonly submissionId?: string;
  readonly user?: UserRole;
  readonly l2OutRef: string;
  readonly l1Address: string;
};

export const requireText = (
  value: unknown,
  what: string,
  transcript: string,
) => {
  if (typeof value === "string" && value !== "") return value;
  throw new FatalJourneyError(
    `${what} is missing from the command result; transcript ${transcript}`,
  );
};

/**
 * The deposit and L2-transfer half of the journey: every submission journals
 * its intent (a stable submission id and its parameters) before the first CLI
 * call and its result after, so a failed or interrupted submission is retried
 * under the same id.
 */
export class JourneyUserSubmissions extends JourneyRuntime {
  // Deposits ----------------------------------------------------------------

  async deposit(
    id: string,
    user: UserRole,
    value: Value,
  ): Promise<DepositRecord> {
    const key = `deposit:${id}`;
    const existing = this.journal.get<DepositRecord | DepositIntent>(key);
    if (existing !== undefined && "txHash" in existing) return existing;
    const intent: DepositIntent = existing ?? {
      submissionId: this.submissionId(id),
      user,
      value: jsonValue(value),
    };
    if (existing === undefined) this.journal.set(key, intent);
    else
      this.requireSameIntent(`deposit ${id}`, existing, {
        user,
        value: jsonValue(value),
      });
    const amount = toValue(intent.value);
    const { result, transcript } = await this.submit(
      `deposit ${id}`,
      intent.submissionId,
      intent.user,
      [
        "submit-deposit",
        "--submission-id",
        intent.submissionId,
        "--wallet-seed-phrase-env",
        "USER_SEED_PHRASE",
        "--l2-address",
        this.address(intent.user),
        "--lovelace",
        (amount.lovelace ?? 0n).toString(),
        ...this.assetSpecs(amount),
      ],
      `deposit-${id}`,
    );
    const record: DepositRecord = {
      user: intent.user,
      value: intent.value,
      txHash: requireText(result.txHash, `deposit ${id} txHash`, transcript),
      eventId: requireText(
        (result.metadata as Json | undefined)?.depositEventId,
        `deposit ${id} metadata.depositEventId`,
        transcript,
      ),
    };
    this.journal.set(key, record);
    this.log(`deposit ${id} submitted ${record.txHash}`);
    return record;
  }

  deposits() {
    return this.journal
      .withPrefix<DepositRecord | DepositIntent>("deposit:")
      .filter((record): record is DepositRecord => "txHash" in record);
  }

  async waitCredited(
    record: DepositRecord,
    timeoutMs = this.deadlines.depositCreditedMs,
  ) {
    return this.until(
      `deposit ${record.eventId} to be credited`,
      timeoutMs,
      async () => {
        const { status, body } = await this.http(
          `/deposit-status?eventId=${record.eventId}`,
        );
        if (status !== 200) return undefined;
        return (body.status === "projected" || body.status === "consumed") &&
          typeof body.projectedHeaderHash === "string"
          ? body
          : undefined;
      },
    );
  }

  // L2 transfers --------------------------------------------------------------

  /**
   * The CLI's journal of signed transfers, kept inside the run: its default
   * is machine-wide, and submission ids repeat across runs.
   */
  get transferJournalDir() {
    return join(this.runKey, "l2-transfer-submissions");
  }

  private transferArgs(intent: TransferIntent) {
    const amount = toValue(intent.value);
    return [
      "submit-l2-transfer",
      "--submission-id",
      intent.submissionId,
      "--submission-journal-dir",
      this.transferJournalDir,
      "--wallet-seed-phrase-env",
      "USER_SEED_PHRASE",
      "--l2-address",
      this.address(intent.to),
      "--lovelace",
      (amount.lovelace ?? 0n).toString(),
      "--endpoint",
      this.nodeUrl,
      ...(intent.excludeOutRefs ?? []).flatMap((outRef) => [
        "--exclude-out-ref",
        outRef,
      ]),
      ...this.assetSpecs(amount),
    ];
  }

  async transfer(
    id: string,
    from: UserRole,
    to: UserRole,
    value: Value,
  ): Promise<TransferRecord> {
    const key = `transfer:${id}`;
    const existing = this.journal.get<
      TransferRecord | TransferIntent | { intent: true }
    >(key);
    if (existing !== undefined && "txId" in existing) return existing;
    // Written by a journey whose transfers had no submission id: that
    // submission cannot be resumed, and a new one could pay twice.
    if (existing !== undefined && !("submissionId" in existing))
      throw new FatalJourneyError(
        `transfer ${id} was started without a submission id and its outcome was not recorded; see ${this.logDir}`,
      );
    const intent: TransferIntent = existing ?? {
      submissionId: this.submissionId(id),
      from,
      to,
      value: jsonValue(value),
      excludeOutRefs: this.claimedOutRefs(from),
    };
    if (existing === undefined) this.journal.set(key, intent);
    else
      this.requireSameIntent(`transfer ${id}`, existing, {
        from,
        to,
        value: jsonValue(value),
      });
    const { result, transcript } = await this.submit(
      `transfer ${id}`,
      intent.submissionId,
      intent.from,
      this.transferArgs(intent),
      `transfer-${id}`,
    );
    const txId = requireText(result.txId, `transfer ${id} txId`, transcript);
    // A resumed submission reports what the node decided; a rejected
    // transaction never commits, and resubmitting its bytes cannot help.
    if (result.status === "rejected")
      throw new FatalJourneyError(
        `transfer ${id} (L2 tx ${txId}, submission id ${intent.submissionId}) was rejected: ${await this.rejectionReason(txId)}; transcript ${transcript}`,
      );
    const record: TransferRecord = {
      from: intent.from,
      to: intent.to,
      value: intent.value,
      txId,
      selectedInputs: (result.selectedInputs ?? []) as string[],
      submissionId: intent.submissionId,
      ...(intent.excludeOutRefs === undefined
        ? {}
        : { excludeOutRefs: intent.excludeOutRefs }),
    };
    this.journal.set(key, record);
    this.log(
      `transfer ${id} ${record.from}->${record.to} submitted ${record.txId}`,
    );
    return record;
  }

  /**
   * The outputs of `user` this run's withdrawals named. Admission refuses to
   * spend them while the withdrawal is pending, so a transfer excludes them.
   */
  private claimedOutRefs(user: UserRole) {
    const claimed = this.journal
      .withPrefix<WithdrawalRecord | WithdrawalIntent>("withdrawal:")
      .filter((record) => record.user === undefined || record.user === user)
      .map((record) => record.l2OutRef);
    return [...new Set(claimed)].sort();
  }

  /** The node's reason for rejecting `txId`, as far as /tx-status tells. */
  private async rejectionReason(txId: string) {
    try {
      const { body } = await this.http(`/tx-status?tx_hash=${txId}`);
      return body.status === "rejected"
        ? [body.reasonCode, body.reasonDetail]
            .filter((part) => typeof part === "string" && part !== "")
            .join(": ")
        : `the node now reports ${JSON.stringify(body)}`;
    } catch (error) {
      return `no reason: /tx-status unreadable (${describeError(error)})`;
    }
  }

  transfers() {
    return this.journal
      .withPrefix<
        TransferRecord | TransferIntent | { intent: true }
      >("transfer:")
      .filter((record): record is TransferRecord => "txId" in record);
  }

  async waitTx(
    txId: string,
    target: "accepted" | "committed",
    timeoutMs = this.txDeadline(target),
  ) {
    return this.waitTxStatus(txId, target, timeoutMs, undefined);
  }

  /**
   * waitTx for a journaled transfer. While the node does not know the
   * transaction it is resubmitted under its submission id, which sends the
   * journaled signed bytes again and never builds a new transaction.
   */
  async waitTransfer(
    record: TransferRecord,
    target: "accepted" | "committed",
    timeoutMs = this.txDeadline(target),
  ) {
    const { submissionId } = record;
    if (submissionId === undefined)
      return this.waitTx(record.txId, target, timeoutMs);
    return this.waitTxStatus(record.txId, target, timeoutMs, async () => {
      this.log(
        `L2 tx ${record.txId} is unknown to the node; resubmitting ${submissionId}`,
      );
      const { result } = await this.attempt(
        record.from,
        this.transferArgs({ ...record, submissionId }),
        "transfer-resubmit",
        submissionId,
      );
      if (result.txId !== record.txId)
        throw new FatalJourneyError(
          `resubmitting ${submissionId} returned tx ${String(result.txId)}, not the journaled ${record.txId}`,
        );
    });
  }

  private txDeadline(target: "accepted" | "committed") {
    return target === "accepted"
      ? this.deadlines.txAcceptedMs
      : this.deadlines.txCommittedMs;
  }

  private async waitTxStatus(
    txId: string,
    target: "accepted" | "committed",
    timeoutMs: number,
    resubmit: (() => Promise<void>) | undefined,
  ) {
    const order = [
      "queued",
      "validating",
      "accepted",
      "pending_commit",
      "awaiting_local_recovery",
      "committed",
    ];
    let lastSubmitted = Date.now();
    return this.until(`L2 tx ${txId} to be ${target}`, timeoutMs, async () => {
      const { body } = await this.http(`/tx-status?tx_hash=${txId}`);
      if (body.status === "rejected")
        throw new FatalJourneyError(
          `L2 tx ${txId} was rejected: ${JSON.stringify(body)}`,
        );
      if (
        body.status === "not_found" &&
        resubmit !== undefined &&
        Date.now() - lastSubmitted >= this.resubmitAfterMs
      ) {
        lastSubmitted = Date.now();
        await resubmit();
        return undefined;
      }
      return order.indexOf(String(body.status)) >= order.indexOf(target)
        ? body
        : undefined;
    });
  }
}
