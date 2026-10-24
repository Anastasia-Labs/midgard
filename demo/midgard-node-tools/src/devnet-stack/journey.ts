import type { UserRole } from "./identities.js";
import {
  JourneyUserSubmissions,
  requireText,
  type WithdrawalIntent,
} from "./journey.user-submissions.js";
import { FatalJourneyError, JourneyDeadlineError } from "./journey-cli.js";
import { type SettlementWatch } from "./journey-runtime.js";
import {
  addValues,
  type Json,
  jsonValue,
  type L2Utxo,
  negate,
  sameValue,
  toValue,
  type Value,
  type WithdrawalRecord,
} from "./journey-values.js";

export * from "./journey-cli.js";
export * from "./journey-runtime.js";
export { runScenario } from "./journey-scenario.js";
export {
  addValues,
  type DepositRecord,
  drained,
  drainResidue,
  expectedHoldings,
  type L2Utxo,
  negate,
  sameValue,
  toValue,
  type TransferRecord,
  type Value,
  type WithdrawalRecord,
} from "./journey-values.js";

/**
 * Drives user activity through the supported user interfaces only: the
 * node's `submit-deposit`, `submit-l2-transfer`, `submit-withdrawal` and read
 * commands, and its public HTTP API. Every submission journals its intent (a
 * stable submission id and its parameters) before the first CLI call and its
 * result after; a failed or interrupted submission is retried under the same
 * id, which the CLI resumes instead of building a second transaction.
 */
export class Journey extends JourneyUserSubmissions {
  // Withdrawals ---------------------------------------------------------------

  async l2Utxos(user: UserRole): Promise<L2Utxo[]> {
    const result = await this.until(
      `the L2 UTxOs of ${user}`,
      this.deadlines.readMs,
      () =>
        this.cli(
          undefined,
          ["utxos", "--address", this.address(user)],
          `utxos-${user}`,
        ),
    );
    return ((result.utxos ?? []) as Json[]).map((utxo) => ({
      outRef: `${String(utxo.txHash)}#${String(utxo.outputIndex)}`,
      value: toValue(utxo.assets),
    }));
  }

  async l2Totals(user: UserRole) {
    return addValues(...(await this.l2Utxos(user)).map((utxo) => utxo.value));
  }

  async withdraw(
    id: string,
    user: UserRole,
    payoutIndex: number,
    choose: (utxos: readonly L2Utxo[]) => string | undefined,
  ): Promise<WithdrawalRecord> {
    const key = `withdrawal:${id}`;
    const existing = this.journal.get<WithdrawalRecord | WithdrawalIntent>(key);
    if (existing !== undefined && "withdrawalEventId" in existing)
      return existing;
    let intent = existing;
    if (intent === undefined) {
      // Never an output another withdrawal of this run claimed: it stays in
      // the ledger view until that withdrawal is projected.
      const claimed = new Set(
        this.journal
          .withPrefix<WithdrawalRecord | WithdrawalIntent>("withdrawal:")
          .map((record) => record.l2OutRef),
      );
      const l2OutRef = choose(
        (await this.l2Utxos(user)).filter((utxo) => !claimed.has(utxo.outRef)),
      );
      if (l2OutRef === undefined)
        throw new FatalJourneyError(
          `withdrawal ${id}: ${user} has no suitable L2 UTxO`,
        );
      intent = {
        submissionId: this.submissionId(id),
        user,
        l2OutRef,
        l1Address: this.payoutAddress(user, payoutIndex),
      };
      // Fixed before submission, so a resumed run withdraws the same output
      // to the same address under the same submission id.
      this.journal.set(key, intent);
    } else this.requireSameIntent(`withdrawal ${id}`, intent, { user });
    const submissionId = intent.submissionId ?? this.submissionId(id);
    const { result, transcript } = await this.submit(
      `withdrawal ${id}`,
      submissionId,
      user,
      [
        "submit-withdrawal",
        "--submission-id",
        submissionId,
        "--wallet-seed-phrase-env",
        "USER_SEED_PHRASE",
        "--l2-out-ref",
        intent.l2OutRef,
        "--l1-address",
        intent.l1Address,
        "--endpoint",
        this.nodeUrl,
      ],
      `withdrawal-${id}`,
    );
    const record: WithdrawalRecord = {
      user,
      l2OutRef: intent.l2OutRef,
      l1Address: intent.l1Address,
      txHash: requireText(result.txHash, `withdrawal ${id} txHash`, transcript),
      withdrawalEventId: requireText(
        result.withdrawalEventId,
        `withdrawal ${id} withdrawalEventId`,
        transcript,
      ),
      l2Value: jsonValue(toValue(result.l2Value)),
    };
    this.journal.set(key, record);
    this.log(
      `withdrawal ${id} submitted ${record.txHash} for ${record.l2OutRef}`,
    );
    return record;
  }

  withdrawals() {
    return this.journal
      .withPrefix<WithdrawalRecord | WithdrawalIntent>("withdrawal:")
      .filter(
        (record): record is WithdrawalRecord => "withdrawalEventId" in record,
      );
  }

  /** Unspent value at an L1 address as Kupo sees it; throws while Kupo is unavailable. */
  async l1Value(address: string): Promise<Value> {
    const { status, body } = await this.http(
      `/matches/${address}?unspent`,
      `http://127.0.0.1:${this.context.run.kupoPort}`,
    );
    if (status !== 200 || !Array.isArray(body))
      throw new Error(
        `Kupo answered ${status}: ${JSON.stringify(body).slice(0, 300)}`,
      );
    const matches = body as {
      value: {
        coins: number | string;
        assets?: Record<string, number | string>;
      };
    }[];
    return addValues(
      ...matches.map((match) =>
        addValues(
          { lovelace: BigInt(String(match.value.coins)) },
          Object.fromEntries(
            Object.entries(match.value.assets ?? {}).map(([unit, amount]) => [
              unit.replace(".", ""),
              BigInt(String(amount)),
            ]),
          ),
        ),
      ),
    );
  }

  async waitPaidOut(
    record: WithdrawalRecord,
    timeoutMs = this.deadlines.payoutMs,
  ) {
    const watch: SettlementWatch = {};
    await this.until(
      `payout of withdrawal ${record.withdrawalEventId}`,
      timeoutMs,
      async () => {
        const status = await this.cli(
          undefined,
          ["payout-status", "--withdrawal-event-id", record.withdrawalEventId],
          "payout-status",
        );
        if (status.phase === "concluded") return status;
        // Thrown as "not yet", so the wait's reports and deadline name the
        // settlement worker's error (e.g. a payout no reserve UTxO can fund)
        // and its health; this withdrawal's job failing without a break, or a
        // crash-looping worker, ends the wait early.
        const failing = await this.failingSettlementJob(
          "withdrawal",
          record.withdrawalEventId,
        );
        const health = await this.settlementHealth(
          watch,
          record.withdrawalEventId,
          failing,
        );
        throw new Error(
          `${failing ?? `payout in phase ${String(status.phase)}`}; settlement ${health}`,
        );
      },
    );
    // Kupo may still be catching up with the concluding transaction, so less
    // than expected is "not yet"; more than expected is wrong at once.
    const expected = toValue(record.l2Value);
    const mismatch = (received: Value) =>
      `withdrawal ${record.withdrawalEventId} paid ${JSON.stringify(jsonValue(received))} to ${record.l1Address}, expected exactly ${JSON.stringify(record.l2Value)}`;
    let received: Value = {};
    try {
      await this.until(
        `the payout of ${record.withdrawalEventId} to show at ${record.l1Address}`,
        this.deadlines.payoutVisibleMs,
        async () => {
          received = await this.l1Value(record.l1Address);
          if (sameValue(received, expected)) return received;
          if (
            Object.values(addValues(received, negate(expected))).some(
              (amount) => amount > 0n,
            )
          )
            throw new FatalJourneyError(mismatch(received));
          return undefined;
        },
      );
    } catch (error) {
      if (!(error instanceof JourneyDeadlineError)) throw error;
      throw new FatalJourneyError(`${mismatch(received)} (${error.message})`);
    }
    this.log(`withdrawal ${record.withdrawalEventId} paid out exactly`);
  }

  // Drain -----------------------------------------------------------------------

  async waitReady(timeoutMs = this.deadlines.readyMs) {
    return this.until("the node to be ready", timeoutMs, async () => {
      const { status, body } = await this.http("/readyz");
      return status === 200 ? body : undefined;
    });
  }
}
