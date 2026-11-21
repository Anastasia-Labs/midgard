/**
 * The cancel of a computation thread of ours whose header is gone, built
 * and sent with no workflow behind it.
 *
 * The cancel spends the thread itself, so it cannot land twice: a second
 * cancel of the same thread, whoever sends it and whenever, fails at build
 * or at submission for want of its input. That needs no stored record, no
 * restart wait and no reconcile: what is left to cancel is what the chain
 * still holds. A cancel that cannot be built or funded now is simply built
 * again later. Submission refuses any transaction that is not exactly such
 * a cancel: it spends the thread, burns its one token, and pays only our
 * wallet.
 *
 * The fee is paid from the signer's wallet. The caller passes a signer
 * whose wallet offers no coin a workflow holds (`excludeHeldWalletCoins`),
 * and the coins the cancel spends or offers as collateral are held through
 * `holdWalletInputs` until its validity ends, so no workflow takes them
 * while it may still land.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { HoldWalletInputs } from "./proven-correction-removal.js";
import {
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "./runtime.js";
import { walletOnlyValidTo } from "./wallet-only-validity.js";
import {
  FAMILY_OWN_THREAD_CANCELS,
  type OwnThreadCancelDeployment,
  type OwnThreadCancelPlan,
  type OwnThreadCancelSubject,
  planOwnThreadCancel,
} from "./workflow/family-application-registry.own-thread-cancel.js";
import { addMintedAssets } from "./workflow/minted-assets.js";
import {
  type LocallyEvaluatedTransaction,
  submitCapturedTransaction,
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
} from "./workflow/transaction-boundary.js";
import { isWorkflowFundingShortfall } from "./workflow/workflow-funding-shortfall.js";

/**
 * What one cancel attempt came to. `spent_elsewhere`: the thread is no
 * longer on chain, cancelled or spent by another route, so nothing is left
 * to do. `withdrawn`: the thread stopped being due before submission, so
 * nothing was sent. `unfunded`, `unavailable` and `failed` are tried again
 * at a later attempt; `uncancellable` never is.
 */
export type OwnThreadCancelOutcome =
  | Readonly<{
      kind: "submitted";
      txHash: string;
      /** The lovelace the thread held, back to our wallet less the fee. */
      reclaimedLovelace: bigint;
      /** When the cancel can no longer land (wall clock, ms). */
      validToMs: number;
    }>
  | Readonly<{ kind: "spent_elsewhere" }>
  | Readonly<{ kind: "withdrawn" }>
  | Readonly<{ kind: "uncancellable"; reason: string }>
  | Readonly<{ kind: "unavailable"; reason: string }>
  | Readonly<{ kind: "unfunded"; reason: string }>
  | Readonly<{ kind: "failed"; reason: string }>;

export type OwnThreadCanceller = Readonly<{
  /** The key whose threads this canceller cancels. */
  proverKeyHash: string;
  /** How the thread can be cancelled, reading nothing. */
  plan(
    subject: Omit<OwnThreadCancelSubject, "threadOutRef">,
  ): OwnThreadCancelPlan;
  /**
   * Builds, signs and submits the thread's cancel. `stillDue` is asked
   * once the cancel is built and before anything is sent: false withdraws
   * it, so a header returned by a rollback meanwhile keeps its thread.
   */
  cancel(input: {
    readonly subject: OwnThreadCancelSubject;
    readonly stillDue: () => Promise<boolean>;
  }): Promise<OwnThreadCancelOutcome>;
}>;

const message = (cause: unknown): string =>
  cause instanceof Error ? cause.message : String(cause);

/**
 * Refuses a signed transaction that is not exactly the cancel of the
 * thread at `threadOutRef` holding `unit`: it spends that thread and
 * otherwise only `walletAddress`'s coins, burns exactly that one token, and
 * pays only `walletAddress`.
 */
const requireOwnThreadCancel = async ({
  lucid,
  transaction,
  threadOutRef,
  unit,
  walletAddress,
}: {
  readonly lucid: LucidEvolution;
  readonly transaction: LocallyEvaluatedTransaction;
  readonly threadOutRef: string;
  readonly unit: string;
  readonly walletAddress: string;
}): Promise<readonly string[]> => {
  const refuse = (what: string): never => {
    throw new Error(`own-thread cancel ${transaction.txHash} ${what}`);
  };
  const inputs = workflowTransactionInputOutRefs(transaction.signed);
  if (!inputs.includes(threadOutRef))
    refuse(`does not spend thread ${threadOutRef}`);
  const resolved = await lucid.utxosByOutRef(
    inputs.map((input) => parseOutRef(input, "cancel input")),
  );
  if (resolved.length !== inputs.length) refuse("spends an unknown input");
  if (
    resolved.some(
      (utxo) =>
        outRefLabel(utxo) !== threadOutRef && utxo.address !== walletAddress,
    )
  )
    refuse("spends an input that is neither the thread nor our wallet's");
  const body = transaction.signed.toTransaction().body();
  const minted = new Map<string, bigint>();
  addMintedAssets(minted, body.mint());
  if (minted.size !== 1 || minted.get(unit) !== -1n)
    refuse(`does not burn exactly the thread's token ${unit}`);
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1)
    if (outputs.get(index).address().to_bech32() !== walletAddress)
      refuse("pays an address other than our wallet");
  const collateralReturn = body.collateral_return();
  if (
    collateralReturn !== undefined &&
    collateralReturn.address().to_bech32() !== walletAddress
  )
    refuse("returns collateral to an address other than our wallet");
  return [
    ...new Set([
      ...inputs.filter((input) => input !== threadOutRef),
      ...workflowTransactionCollateralInputOutRefs(transaction.signed),
    ]),
  ];
};

/**
 * The canceller of our threads over one deployment: `signer` signs and its
 * wallet pays, `holdWalletInputs` holds the coins each cancel spends until
 * its validity ends.
 */
export const createOwnThreadCanceller = ({
  lucid,
  signer,
  deployment,
  holdWalletInputs,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: OwnThreadCancelDeployment;
  readonly holdWalletInputs: HoldWalletInputs;
}): OwnThreadCanceller => {
  const threadGone = async (threadOutRef: string): Promise<boolean> =>
    (await lucid.utxosByOutRef([parseOutRef(threadOutRef, "thread")]))
      .length === 0;
  /** A failure that leaves the thread on chain is tried again later; one
   * that met it gone means another cancel or route got there first. */
  const settle = async (
    threadOutRef: string,
    cause: unknown,
    kind: "unfunded" | "unavailable" | "failed",
  ): Promise<OwnThreadCancelOutcome> => {
    try {
      if (await threadGone(threadOutRef))
        return Object.freeze({ kind: "spent_elsewhere" });
    } catch {
      // Unknown: the attempt failed and is tried again.
    }
    return Object.freeze({ kind, reason: message(cause) });
  };
  return Object.freeze({
    proverKeyHash: signer.paymentKeyHash,
    plan: (subject) =>
      planOwnThreadCancel(subject, deployment.contractScriptHashes),
    cancel: async ({ subject, stillDue }) => {
      const plan = planOwnThreadCancel(
        subject,
        deployment.contractScriptHashes,
      );
      if (plan.kind !== "cancellable") return plan;
      const { threadOutRef } = subject;
      let built;
      try {
        built = await FAMILY_OWN_THREAD_CANCELS[plan.category]({
          lucid,
          signer,
          deployment,
          subject,
          validTo: walletOnlyValidTo(lucid),
        });
      } catch (cause) {
        return await settle(
          threadOutRef,
          cause,
          isWorkflowFundingShortfall(cause) ? "unfunded" : "unavailable",
        );
      }
      if (built.kind !== "built") return built;
      const { transaction, computationThreadUnit, reclaimedLovelace } =
        built.cancel;
      if (!(await stillDue())) return Object.freeze({ kind: "withdrawn" });
      const ttl = transaction.signed.toTransaction().body().ttl();
      if (ttl === undefined)
        return Object.freeze({
          kind: "failed",
          reason: `own-thread cancel ${transaction.txHash} carries no validity bound`,
        });
      const validToMs = lucid.slotToUnixTime(Number(ttl));
      try {
        const walletCoins = await requireOwnThreadCancel({
          lucid,
          transaction,
          threadOutRef,
          unit: computationThreadUnit,
          walletAddress: signer.address,
        });
        // Released only by expiry: once submitted, the cancel may land
        // until its validity ends, whatever the provider answered.
        holdWalletInputs({ outRefs: walletCoins, untilMs: validToMs });
      } catch (cause) {
        return await settle(threadOutRef, cause, "failed");
      }
      try {
        const txHash = await submitCapturedTransaction(
          transaction,
          `own-thread cancel of ${threadOutRef}`,
        );
        return Object.freeze({
          kind: "submitted",
          txHash,
          reclaimedLovelace,
          validToMs,
        });
      } catch (cause) {
        return await settle(threadOutRef, cause, "failed");
      }
    },
  });
};

export {
  type OwnThreadCancelDeployment,
  type OwnThreadCancelPlan,
  type OwnThreadCancelSubject,
  ownThreadCategory,
} from "./workflow/family-application-registry.own-thread-cancel.js";
