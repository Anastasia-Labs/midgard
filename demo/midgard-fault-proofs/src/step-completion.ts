/**
 * How every fault-proof step transaction is funded, completed and submitted.
 *
 * A step builder states only what is specific to its step: the inputs it
 * spends, the outputs it pays, its redeemers, reference scripts and carriage.
 * It starts from {@link fundStep}, which funds the transaction from the
 * prover's wallet under one rule, hands the finished chain to
 * {@link completeStep}, and passes the signed transaction to
 * {@link submitStep}, which crosses the Q51 pre-submit boundary before any
 * provider I/O.
 */
import type {
  LucidEvolution,
  OutRef,
  TxBuilder,
  TxSigned,
  UTxO,
} from "@lucid-evolution/lucid";

import { stepWalletFunding } from "./funding/plain-funding-output.js";
import { DEFAULT_CONFIRMATION_POLL_MS } from "./runtime.js";
import type { ResolvedProverSigner } from "./runtime.resolve-prover-signer.js";
import type { FraudProofWorkflowReferenceScript } from "./workflow/orchestrator.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  submitCapturedTransaction,
} from "./workflow/transaction-boundary.js";

/** The prover that funds and signs a step: its key and its wallet. */
export type StepSigner = Pick<
  ResolvedProverSigner,
  "paymentKeyHash" | "selectWallet"
>;

/** A step transaction begun by {@link fundStep}, and the wallet coins its
 * completion may select from. */
export type StepFunding = Readonly<{
  tx: TxBuilder;
  walletInputs: { presetWalletInputs?: UTxO[] };
}>;

/**
 * Begins a step transaction funded by `signer`'s wallet, read once: it spends
 * the wallet's fee input and requires the signer's key. Coin selection and
 * collateral may take only plain coins outside `excluding`, the outputs the
 * step itself spends or references (`stepWalletFunding`).
 */
export const fundStep = async (
  lucid: LucidEvolution,
  {
    signer,
    excluding,
  }: {
    readonly signer: StepSigner;
    readonly excluding?: readonly OutRef[];
  },
): Promise<StepFunding> => {
  signer.selectWallet(lucid);
  const { feeInput, walletInputs } = await stepWalletFunding(lucid, {
    excluding,
  });
  return {
    tx: lucid
      .newTx()
      .collectFrom([feeInput])
      .addSignerKey(signer.paymentKeyHash),
    walletInputs,
  };
};

/**
 * Completes `tx` with every script evaluated locally, balancing it from
 * `funding`'s wallet coins, and signs it with the selected wallet. A step
 * with no fee input, such as Init, completes without `funding` and lets coin
 * selection read the wallet.
 */
export const completeStep = async (
  tx: TxBuilder,
  funding?: StepFunding,
): Promise<TxSigned> => {
  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...funding?.walletInputs,
  });
  return await unsigned.sign.withWallet().complete();
};

/**
 * Submits a signed step: it reaches `boundary` with the transaction and the
 * reference scripts it uses, submits only after the boundary returns,
 * refuses a provider that reports another hash, and waits for confirmation
 * when asked. Returns the transaction hash.
 */
export const submitStep = async (
  lucid: LucidEvolution,
  signed: TxSigned,
  {
    referenceScripts,
    boundary,
    awaitConfirmation,
  }: {
    readonly referenceScripts: readonly FraudProofWorkflowReferenceScript[];
    readonly boundary?: FraudProofPreSubmitBoundary;
    readonly awaitConfirmation: boolean;
  },
): Promise<string> => {
  const txHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts,
    boundary,
  });
  await submitCapturedTransaction({ txHash, signed, referenceScripts });
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return txHash;
};
