/**
 * The restart wait of a transaction that spends only wallet coins.
 *
 * Such a transaction has no input of ours that excludes a duplicate, so a
 * copy built before a restart could still land beside a rebuilt one. Each
 * carries an upper validity bound at most `WALLET_ONLY_VALIDITY_MS` past
 * its build (`wallet-only-validity.ts`); a process that has just started
 * sends none until one window has passed, and from then on the chain alone
 * says whether the earlier copy landed. A transaction that spends the proof
 * thread or any other specific output needs no wait: its own input already
 * excludes a duplicate.
 */
import { type TxSigned } from "@lucid-evolution/lucid";

import { WALLET_ONLY_VALIDITY_MS } from "../wallet-only-validity.js";
import type { WorkflowSends } from "./in-flight.js";
import type { FraudProofWorkflowPreflight } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowNamedState } from "./orchestrator.fraud-proof-workflow-run-result.js";
import {
  workflowPreflightTransaction,
  workflowTransactionInputOutRefs,
} from "./transaction-boundary.js";

/**
 * A process's restart wait and the latest upper bound a wallet-only
 * transaction may carry. The caller builds it once per process; a requeue or
 * a later admission never moves the instant `open` turns true at.
 *
 * Both members answer on the caller's clock, the one its wallet builds
 * bounds on (`walletOnlyValidTo`) and its restart wait is measured on.
 */
export type WalletOnlySend = Readonly<{
  /** Whether this process may send a transaction that spends only wallet
   * coins: false until one wallet-only validity window has passed since the
   * process started, on the clock and on the chain the process has applied. */
  open: () => boolean | Promise<boolean>;
  /** The slot one wallet-only validity window past the caller's clock now,
   * floored: the bound `walletOnlyValidTo` builds at this instant. */
  latestValidToSlot: () => number;
}>;

/** The bound clock of a process whose wallet is not restricted: none of its
 * runs lists a wallet coin (`spendsOnlyWalletCoins`), so no bound is ever
 * checked against it. */
export const latestValidToSlotWithoutRestrictedWallet = (): never => {
  throw new Error(
    "no slot clock: this process restricts no wallet, so it checks no wallet-only upper bound",
  );
};

/** The restart wait of a process that follows no L1 chain and restricts no
 * wallet: open once the clock is one window past `startedAtMs`. */
export const clockOnlyWalletOnlySend = (
  startedAtMs: number,
): WalletOnlySend => ({
  open: () => Date.now() >= startedAtMs + WALLET_ONLY_VALIDITY_MS,
  latestValidToSlot: latestValidToSlotWithoutRestrictedWallet,
});

/** The named state of an action held by the restart wait. Not an alarm: it
 * clears by itself once the window has passed. */
export const FAULT_PROOF_RESTART_WAIT = "fault_proof_restart_wait" as const;

export const faultProofRestartWaitNamedState: FraudProofWorkflowNamedState =
  Object.freeze({ name: FAULT_PROOF_RESTART_WAIT, alarm: false });

/** The pending reason of an action held by the restart wait. */
export const faultProofRestartWaitReason = (actionId: string): string =>
  `${FAULT_PROOF_RESTART_WAIT}: ${actionId} spends only wallet coins and waits until one validity window has passed since this process started`;

/**
 * Whether every input of the signed body of `preflight` is a wallet coin: a
 * plain coin at the prover's funding address that the objective's
 * restricted wallet listed for the build (`workflow-wallet.ts`). An output
 * at the proof-materials address is a specific output of ours, not a wallet
 * coin; so is any other input (a thread, a certificate, a staging output, a
 * script output).
 *
 * The wallet was listed while the action was built, so this reads no L1
 * state. A run whose wallet is not restricted lists no wallet coin, so
 * neither the restart wait nor the validity-bound check applies; production
 * always restricts it (`runtime.ts`).
 */
export const spendsOnlyWalletCoins = ({
  sends,
  preflight,
}: {
  readonly sends: WorkflowSends;
  readonly preflight: FraudProofWorkflowPreflight;
}): boolean => {
  const signed = workflowPreflightTransaction(preflight);
  if (signed === undefined) return false;
  const inputs = workflowTransactionInputOutRefs(signed);
  return inputs.length > 0 && inputs.every(sends.isWalletCoin);
};

/** A wallet-only transaction with no upper validity bound, or one later
 * than one window past the caller's clock. */
export class WalletOnlyValidityBoundError extends Error {
  constructor(
    readonly actionId: string,
    detail: string,
  ) {
    super(
      `wallet-only transaction of ${actionId} ${detail}; every transaction spending only wallet coins, other than a field-preimage carriage publication, must carry the wallet-only validity window`,
    );
    this.name = "WalletOnlyValidityBoundError";
  }
}

/**
 * The slots past `latestValidToSlot()` a wallet-only upper bound may reach:
 * the one slot a slot clock that rounds up rather than down could add
 * (`walletOnlyValidTo`).
 */
export const WALLET_ONLY_BOUND_ROUNDING_SLOTS = 1n;

/**
 * Refuses a wallet-only transaction, other than the field-preimage carriage
 * publication, that carries no upper validity bound or one later than
 * `latestValidToSlot()`. The transaction was built earlier on the same
 * clock, so a bound from `walletOnlyValidTo` is never later than that.
 */
export const assertWalletOnlyValidityBound = ({
  actionId,
  preflight,
  latestValidToSlot,
}: {
  readonly actionId: string;
  readonly preflight: FraudProofWorkflowPreflight;
  readonly latestValidToSlot: WalletOnlySend["latestValidToSlot"];
}): void => {
  if (preflight.unboundedCarriagePublication === true) return;
  const signed: TxSigned | undefined = workflowPreflightTransaction(preflight);
  if (signed === undefined) return;
  const ttl = signed.toTransaction().body().ttl();
  if (ttl === undefined)
    throw new WalletOnlyValidityBoundError(
      actionId,
      "carries no upper validity bound",
    );
  const latest = BigInt(latestValidToSlot()) + WALLET_ONLY_BOUND_ROUNDING_SLOTS;
  if (ttl > latest)
    throw new WalletOnlyValidityBoundError(
      actionId,
      `carries upper validity bound slot ${ttl.toString()}, later than slot ${latest.toString()}, one window past the caller's clock`,
    );
};
