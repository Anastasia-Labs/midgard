import {
  type FraudProofWorkflowReconcileResult,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import type { WatcherOrphanCancelWallet } from "../../src/funding/prover-funding-orphan-cancel.transaction.js";
import type { WatcherProverFundingReservationStore } from "../../src/funding/prover-funding-reservation.js";
import {
  key,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { signedReadObservation } from "../support/fault-proof-funding-fixture.signed-read.js";
import {
  fundWallet,
  loseJournals,
  orphanReadingFactory,
} from "./orphaned-reservation-fixture.js";
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import {
  type Fixture,
  replaceAfterExpiry,
} from "./superseded-attempt-replacement.js";

/** What the chain says of one signed transaction. */
export type CancelChainState =
  /** Not in the mempool, every input unspent. */
  | "dropped"
  /** Absent at the tip, an input spent. */
  | "spent"
  /** In the mempool. */
  | "mempool"
  /** Landed within release finality. */
  | "landed"
  | "landed final"
  | "invalidated final"
  | "expired final";

const answers: Record<
  Exclude<CancelChainState, "dropped" | "spent">,
  (txHash: string) => FraudProofWorkflowReconcileResult
> = {
  mempool: (txHash) => ({ kind: "pending", txHash }),
  landed: (txHash) => ({ kind: "confirmed", txHash }),
  "landed final": (txHash) => ({
    kind: "pending",
    txHash,
    retirement: { ...retirementFixture(txHash), reason: "included" },
  }),
  "invalidated final": (txHash) => ({
    kind: "not_found",
    retirement: retirementFixture(txHash),
  }),
  "expired final": (txHash) => ({
    kind: "not_found",
    retirement: { ...retirementFixture(txHash), reason: "expired" },
  }),
};

/** The prover wallet a cancel signs with: one-second slots, the fixture's
 * key, and every signature and submission kept. */
export const cancelWallet = (fail?: { submit?: boolean }) => {
  const signed: string[] = [];
  const submitted: string[] = [];
  const failures = { submit: fail?.submit ?? false };
  const wallet: WatcherOrphanCancelWallet = {
    address: walletAddress,
    open: async () => ({
      slotOf: (unixTimeMs) => Math.floor(unixTimeMs / 1000),
      timeOf: (slot) => slot * 1000,
      sign: async (transaction) => {
        const hash = CML.hash_transaction(transaction.body());
        signed.push(hash.to_hex());
        const witnesses = CML.TransactionWitnessSet.new();
        const keys = CML.VkeywitnessList.new();
        keys.add(
          CML.Vkeywitness.new(key.to_public(), key.sign(hash.to_raw_bytes())),
        );
        witnesses.set_vkeywitnesses(keys);
        return witnesses;
      },
    }),
    submit: async (signedTransactionCborHex) => {
      submitted.push(signedTransactionCborHex);
      if (failures.submit) throw new Error("the node is unreachable");
    },
  };
  return { wallet, signed, submitted, failures };
};

/** The hash of signed bytes. */
export const hashOf = (signedTransactionCborHex: string) =>
  CML.hash_transaction(
    CML.Transaction.from_cbor_hex(signedTransactionCborHex).body(),
  ).to_hex();

/**
 * The journals are lost while the original attempt is in flight, and the
 * mempool drops it. Its target is gone when `gone.value` says so. Each
 * transaction reads as `chain` says; a hash it does not name reads dropped.
 * `ttl` is the original attempt's expiry; by default it has none. A process
 * opened after `overrides.current` is set uses those store operations.
 */
export const orphanToCancel = async (
  options: {
    ttl?: bigint | null;
    wallet?: ReturnType<typeof cancelWallet>;
    /** A replacement, with an expiry, was recorded before the loss. */
    replaced?: boolean;
  } = {},
) => {
  const fixture: Fixture = await setupFundingRecoveryFixture(
    false,
    false,
    false,
    true,
    false,
    20n,
    44,
    3,
    options.ttl ?? null,
  );
  const replacement = options.replaced
    ? (await replaceAfterExpiry(fixture)).replacement
    : undefined;
  await loseJournals(fixture, [fixture.fresh]);
  supersededAttemptReadSchedule.forget(fixture.transactionHash);
  fixture.useUnspentPendingInputs();
  fundWallet(fixture);
  fixture.chain.mockResolvedValue({ kind: "not_found" });
  const chain = new Map<string, CancelChainState>();
  fixture.observeSignedTransaction.mockImplementation(async (input) => {
    const state = chain.get(input.transactionHash) ?? "dropped";
    if (state === "dropped" || state === "spent")
      return {
        ...signedReadObservation(input, { kind: "unknown", reason: "" }),
        ...(state === "dropped"
          ? {
              status: "rebroadcast",
              reason:
                "Canonical transaction absent and every recorded input remains unspent",
            }
          : {
              status: "invalidated_at_tip",
              reason: "an input is spent at the tip",
            }),
      };
    return signedReadObservation(input, answers[state](input.transactionHash));
  });
  const gone = { value: true };
  const targets: { category: string; headerHash: string }[] = [];
  const alerts: { subjectDigest: string; active: boolean }[] = [];
  let clock = 1_000_000_000;
  const wallet = options.wallet ?? cancelWallet();
  const overrides: { current: Partial<WatcherProverFundingReservationStore> } =
    { current: {} };
  /** A process over the fixture's store: a restart opens another. */
  const open = () =>
    orphanReadingFactory(
      fixture,
      {
        observeSignedTransaction: fixture.observeSignedTransaction,
        now: () => clock,
        alertSink: () => ({
          setAlert: ({ subjectDigest, active }) => {
            alerts.push({ subjectDigest, active });
          },
        }),
        orphanCancel: {
          wallet: wallet.wallet,
          targetGone: (target) => {
            targets.push(target);
            return gone.value;
          },
        },
      },
      overrides.current,
    );
  const process = { current: open() };
  /** One sweep, after every read's backoff has passed. */
  const sweep = async () => {
    clock += 600_000;
    await process.current.factory.releaseUnused();
    return process.current.factory.decisionHolds();
  };
  const orphan = async () =>
    (await fixture.records()).find(
      ({ reservationId }) => reservationId === fixture.plan.reservationId,
    )!;
  return {
    fixture,
    chain,
    gone,
    targets,
    alerts,
    wallet,
    process,
    overrides,
    replacement,
    sweep,
    orphan,
    /** The process restarts over the same store. */
    restart: async () => {
      await fixture.restartStore();
      process.current = open();
    },
    now: () => clock,
  };
};

/** Sweeps until the cancel is signed, the third agreeing read. */
export const sweepToCancel = async (
  setup: Awaited<ReturnType<typeof orphanToCancel>>,
) => {
  for (let read = 0; read < 3; read += 1) await setup.sweep();
  return setup.wallet.submitted.at(-1);
};

/** The cancel recorded for the orphan, as the store holds it. */
export const recordedCancel = async (fixture: Fixture) =>
  await fixture.store.readOrphanCancel!({
    reservationId: fixture.plan.reservationId,
  });
