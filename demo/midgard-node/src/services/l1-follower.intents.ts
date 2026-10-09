/**
 * The node follower's intent stage (plan §8.3 S6, I1): after each driver
 * run at the node's tip it seeds the node's own wallets (once, and again
 * after a rewind below a seed), then reconciles every journaled intent from
 * the facts. A live intent the node's mempool lacks gets its exact journaled
 * bytes again, at most once per tip; a dead one is never sent; a live one
 * whose family predicate fails is abandoned.
 *
 * - The tracked set covers the journal's invariant (§8.2): the node's own
 *   wallets, the reference-script addresses, and the protocol validators'
 *   payment credentials, with the hub-oracle policy for the protocol-init tx.
 * - Family predicates (§8.4) are reads over projections
 *   (`l1-follower.intent-predicates.ts`), passed in as `wanted`.
 * - A failed pass, or an intent whose mempool read, predicate or submission
 *   failed, is a named transient `/readyz` hold, retried on the follower's
 *   backoff. The process stays up.
 * - A live intent whose resend the ledger refuses at consecutive tips is a
 *   named refusal hold for its family (`INTENT_RESUBMIT_REJECTED`); it is
 *   still resent each tip until its facts make it dead, or until the ledger
 *   refused it at `RESUBMIT_REJECTIONS_TO_ABANDON` distinct tips at or past
 *   its lower validity bound: then it is abandoned (`ledger_rejected`), the
 *   hold clears, and the family builds again from current facts.
 * - While the store replays a tracked-set reset (its record's `replaying`
 *   flag, set until the cursor first reaches the node tip), its facts are a
 *   prefix of the chain: S6 takes no decision from them. The pass is
 *   skipped under the follower's `tracked_set_changed` reason, and a
 *   predicate read during a pass that a reset overtook waits under it (no
 *   resend, no abandon); the next pass after the replay decides as usual.
 */
import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  createIntentReconciler,
  createWalletSeeder,
  type FactStore,
  FOLLOWER_TRACKED_SET_CHANGED,
  type IntentState,
  type ReconcileReport,
  type TrackedSet,
  WALLET_SEED_PENDING,
} from "@al-ft/midgard-l1-follower";

import {
  type DriverHold,
  failureHold,
  notRetried,
} from "../l1-events/driver.js";

/** An S6 pass failed as a whole (the store read); the next trigger retries. */
export const INTENT_RECONCILE_FAILED = "intent_reconcile_failed";
/** An intent's mempool read, predicate or submission failed this pass. */
export const INTENT_RECONCILE_TRANSIENT = "intent_reconcile_transient";
/**
 * The node's ledger refused S6's resend of a live intent at
 * `RESUBMIT_REJECTIONS_TO_HOLD` tips in a row (I1-R3 refusal hold, one per
 * family). It clears without operator action once the intent is no longer
 * live (it landed, another landed transaction spent one of its inputs, its
 * upper validity bound passed, a dependency died, it was abandoned or
 * pruned), or once a later resend is accepted or the mempool holds it.
 */
export const INTENT_RESUBMIT_REJECTED = "intent_resubmit_rejected";
/** Rejections at distinct tips (S6 sends at most once per tip) before a hold. */
export const RESUBMIT_REJECTIONS_TO_HOLD = 2;
/**
 * Rejections at distinct tips, each at or past the intent's lower validity
 * bound, before S6 abandons it (`ledger_rejected`, I1C-R1). Whichever of
 * the abandoned bytes and their replacement lands wins.
 */
export const RESUBMIT_REJECTIONS_TO_ABANDON = 3;

/**
 * A family predicate that cannot decide at this view yet (a later view
 * will): S6 keeps the intent live and does not send it this pass, and the
 * stage raises `reason` as the intent's `/readyz` hold instead of
 * `INTENT_RECONCILE_TRANSIENT`.
 */
export class IntentPredicateWait extends Error {
  constructor(
    readonly reason: string,
    detail: string,
  ) {
    super(detail);
    this.name = "IntentPredicateWait";
  }
}

/**
 * The node follower's protocol tracked set (§8.2's invariant plus protocol
 * init). The seeded wallets are not in it: they go to the store as
 * `FactStoreOptions.wallets`, tracked by address but never recorded.
 */
export const nodeIntentTrackedSet = (input: {
  readonly protocolPaymentCredentials: readonly string[];
  readonly hubOraclePolicyId: string;
}): TrackedSet => ({
  addresses: new Set(),
  paymentCredentials: new Set(input.protocolPaymentCredentials),
  policies: new Set([input.hubOraclePolicyId]),
});

export type NodeIntentStage = Readonly<{
  /** Seeds owed wallets, then one S6 pass; returns the holds it leaves. */
  run(): Promise<readonly DriverHold[]>;
  holds(): readonly DriverHold[];
  /** The last pass's report, for status. */
  lastReport(): ReconcileReport | null;
  close(): void;
}>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * The holds a pass leaves: one per transiently failed intent, under the
 * reason of the predicate wait it threw (`waits`, by tx hash hex), if any.
 */
export const reconcileHolds = (
  report: ReconcileReport,
  waits: ReadonlyMap<string, IntentPredicateWait> = new Map(),
): DriverHold[] =>
  report.intents.flatMap((entry) =>
    entry.action === "wait_transient"
      ? [
          {
            reason:
              waits.get(entry.intent.txHash.toString("hex"))?.reason ??
              INTENT_RECONCILE_TRANSIENT,
            detail: `${entry.intent.family} ${entry.intent.workflowKey} (${entry.intent.txHash.toString("hex")}): ${entry.error ?? "unknown"}`,
          },
        ]
      : [],
  );

type Rejected = Readonly<{
  family: string;
  workflowKey: string;
  txHash: Buffer;
  /** Rejections in a row, one per tip. */
  times: number;
  detail: string;
}>;

/** The longest rejection detail a hold names (the ledger's error, hex). */
const REJECTION_DETAIL_CHARS = 512;

/**
 * S6's resend rejections, by intent: a rejection counts, an accepted resend
 * or a mempool hit resets, and an intent abandoned this pass or no longer
 * live (or no longer journaled) leaves. Holds: one per family, for its intents rejected at
 * `RESUBMIT_REJECTIONS_TO_HOLD` tips in a row.
 */
export const createResubmitRejections = () => {
  const rejected = new Map<string, Rejected>();
  return {
    observe(report: ReconcileReport): void {
      for (const entry of report.intents) {
        const key = entry.intent.txHash.toString("hex");
        if (entry.action === "abandon") rejected.delete(key);
        else if (entry.rejection !== undefined) {
          const before = rejected.get(key);
          rejected.set(key, {
            family: entry.intent.family,
            workflowKey: entry.intent.workflowKey,
            txHash: entry.intent.txHash,
            times: (before?.times ?? 0) + 1,
            detail: entry.rejection,
          });
        } else if (
          entry.action === "wait_in_mempool" ||
          entry.action === "resubmit"
        )
          rejected.delete(key);
      }
      for (const [key, { txHash }] of [...rejected])
        if (report.entry(txHash)?.status.kind !== "live") rejected.delete(key);
    },
    holds(): DriverHold[] {
      const byFamily = new Map<string, Rejected[]>();
      for (const entry of rejected.values())
        if (entry.times >= RESUBMIT_REJECTIONS_TO_HOLD)
          byFamily.set(entry.family, [
            ...(byFamily.get(entry.family) ?? []),
            entry,
          ]);
      return (
        [...byFamily]
          .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
          // The node refuses them at every tip: a timer retry at the same tip
          // cannot lift that, the next tip runs S6 again.
          .map(([family, entries]) =>
            notRetried({
              reason: INTENT_RESUBMIT_REJECTED,
              detail: `${family}: ${entries
                .map(
                  (entry) =>
                    `${entry.workflowKey} (${entry.txHash.toString("hex")}) refused at ${entry.times.toString()} tips: ${entry.detail.slice(0, REJECTION_DETAIL_CHARS)}`,
                )
                .join("; ")}`,
            }),
          )
      );
    },
  };
};

/** S6 decides nothing while the store replays a tracked-set reset. */
const REPLAYING_DETAIL =
  "the store is replaying a tracked-set reset; intents are not resent or abandoned until the replay reaches the node tip";

/** An `IntentPredicateWait` under `tracked_set_changed` while the store replays. */
const assertNotReplaying = async (store: FactStore): Promise<void> => {
  if ((await store.trackedSetRecord())?.replaying === true)
    throw new IntentPredicateWait(
      FOLLOWER_TRACKED_SET_CHANGED,
      REPLAYING_DETAIL,
    );
};

export const createNodeIntentStage = (input: {
  readonly store: FactStore;
  readonly transport: Pick<
    L1NodeTransport,
    "hasTx" | "submit" | "withLedgerState"
  >;
  readonly securityParameter: number;
  readonly seededAddresses: readonly Buffer[];
  readonly wanted: (state: IntentState) => Promise<boolean>;
  readonly log: (line: string) => void;
}): NodeIntentStage => {
  const { store, transport } = input;
  const seeder = createWalletSeeder({
    store,
    ledger: transport,
    wallets: input.seededAddresses,
  });
  /** This pass's predicate waits, by tx hash hex. */
  const waits = new Map<string, IntentPredicateWait>();
  const reconciler = createIntentReconciler({
    dialect: store.dialect,
    transaction: (mode, run) => store.transaction(mode, run),
    securityParameter: input.securityParameter,
    abandonAfterRejections: RESUBMIT_REJECTIONS_TO_ABANDON,
    inMempool: (intent) => transport.hasTx(intent.txHash.toString("hex")),
    wanted: async (state) => {
      try {
        // A reset that overtook this pass: its view is the replay's.
        await assertNotReplaying(store);
        return await input.wanted(state);
      } catch (error) {
        if (error instanceof IntentPredicateWait)
          waits.set(state.intent.txHash.toString("hex"), error);
        throw error;
      }
    },
    submit: async (intent) => {
      const result = await transport.submit(new Uint8Array(intent.txCbor));
      if (result.accepted) return { kind: "accepted" };
      return {
        kind: "rejected",
        detail: Buffer.from(result.rejection).toString("hex"),
      };
    },
  });
  const rejections = createResubmitRejections();
  let holds: readonly DriverHold[] = [];
  let last: ReconcileReport | null = null;
  const run = async (): Promise<readonly DriverHold[]> => {
    const left: DriverHold[] = [];
    if (!seeder.ready()) {
      let failure: { error: unknown } | undefined;
      const seeded = await seeder.step().catch((error: unknown) => {
        failure = { error };
        return {
          kind: "pending",
          reason: "error",
          detail: message(error),
        } as const;
      });
      if (seeded.kind === "pending") {
        const detail = `${seeded.reason}: ${seeded.detail}`;
        left.push(
          failure === undefined
            ? { reason: WALLET_SEED_PENDING, detail }
            : failureHold(WALLET_SEED_PENDING, detail, failure.error),
        );
      }
    }
    try {
      waits.clear();
      if ((await store.trackedSetRecord())?.replaying === true)
        left.push({
          reason: FOLLOWER_TRACKED_SET_CHANGED,
          detail: REPLAYING_DETAIL,
        });
      else {
        last = await reconciler.reconcile();
        for (const entry of last.intents)
          if (entry.action === "resubmit" || entry.action === "abandon")
            input.log(
              `${entry.action} ${entry.intent.family} ${entry.intent.workflowKey} (${entry.intent.txHash.toString("hex")})`,
            );
        left.push(...reconcileHolds(last, waits));
        rejections.observe(last);
      }
    } catch (error) {
      left.push(failureHold(INTENT_RECONCILE_FAILED, message(error), error));
    }
    left.push(...rejections.holds());
    holds = left;
    return left;
  };
  return {
    run,
    holds: () => holds,
    lastReport: () => last,
    close: () => seeder.close(),
  };
};
