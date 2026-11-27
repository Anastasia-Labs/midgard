import {
  slotAlignedLowerBoundAtOrAfter,
  type SlotClock,
} from "@al-ft/midgard-sdk/core";
import {
  type LucidEvolution,
  type Network,
  type TxSignBuilder,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
  STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
} from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import type { StateQueueMutationLeaseCoordinator } from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import { type ResolvedProverSigner } from "./runtime.js";

/**
 * The ledger refused a correction step because its inputs are already
 * spent: an earlier step, ours or another actor's, is still in flight or
 * has just landed. Nothing was sent. The next run reads the chain again and
 * builds the step it calls for.
 */
export class TimeoutCorrectionAttemptInFlightError extends Error {
  readonly txHash: string;
  constructor(txHash: string, options?: { readonly cause?: unknown }) {
    super(
      `Timeout correction ${txHash} was refused: its inputs are already spent, so an earlier attempt is still in flight or has just landed. Run the command again once it settles.`,
      options,
    );
    this.name = "TimeoutCorrectionAttemptInFlightError";
    this.txHash = txHash;
  }
}

/**
 * The wallet a timeout correction is funded and signed from: the inputs its
 * coin selection may spend (`presetWalletInputs`, read just before the
 * build) and its signing.
 */
export type TimeoutCorrectionWallet = {
  readonly utxos: () => Promise<UTxO[]>;
  readonly sign: (unsigned: TxSignBuilder) => Promise<TxSigned>;
};

/**
 * `wallet`, or where none is given (a CLI run) the provider's UTxOs at the
 * selected wallet's address and the selected wallet's own signature.
 */
export const timeoutCorrectionWallet = (
  lucid: LucidEvolution,
  wallet: TimeoutCorrectionWallet | undefined,
): TimeoutCorrectionWallet =>
  wallet ?? {
    utxos: async () => lucid.utxosAt(await lucid.wallet().address()),
    sign: (unsigned) => unsigned.sign.withWallet().complete(),
  };

export type SubmitUnattestedTimeoutCorrectionParams = {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  /**
   * Wait for each submitted step to land and build the next one in the same
   * run. Otherwise the run returns `pending` after its first submission.
   */
  readonly awaitConfirmation?: boolean;
  readonly nowMs?: () => number;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  /**
   * The node's wallet view (§8.5) and its signing over it. Without one (a
   * CLI run), the provider's UTxOs at the selected wallet's address, and the
   * selected wallet's own signature. Nothing is pinned on `lucid` either way.
   */
  readonly wallet?: TimeoutCorrectionWallet;
};

/**
 * The validity range of a timeout-correction transaction built at `nowMs`.
 *
 * The validator requires `inclusive lower bound >= node.block_time +
 * da_attestation_timeout`. Commit times usually end in 999 ms, and the
 * ledger presents the lower bound as the start of its slot, so the deadline
 * itself is rounded up to the next slot boundary before it is used as the
 * earliest admissible lower bound. The lower bound is otherwise backdated to
 * tolerate submit latency and provider clock skew.
 */
export const resolveTimeoutCorrectionValidityRange = (
  slotClock: SlotClock,
  deadlineMs: bigint,
  nowMs: bigint,
): { readonly validFrom: bigint; readonly validTo: bigint } => {
  const earliestAdmitted = slotAlignedLowerBoundAtOrAfter(
    slotClock,
    deadlineMs,
  );
  const backdated = nowMs - STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS;
  const validFrom = backdated > earliestAdmitted ? backdated : earliestAdmitted;
  return {
    validFrom,
    validTo: validFrom + STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
  };
};
