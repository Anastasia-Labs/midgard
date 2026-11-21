/**
 * How every fault-proof step transaction is funded, completed and finished.
 *
 * A step builder states only what is specific to its step: the inputs it
 * spends, the outputs it pays, its redeemers, reference scripts and carriage.
 * It starts from {@link fundStep}, which funds the transaction from the
 * prover's wallet under one rule, hands the finished chain to
 * {@link completeStep}, and passes the signed transaction to
 * {@link finishStep}, which returns it with the reference scripts it uses.
 * A builder submits nothing: its caller does.
 */
import type {
  LucidEvolution,
  OutRef,
  Script,
  TxBuilder,
  TxSigned,
  UTxO,
} from "@lucid-evolution/lucid";

import { stepWalletFunding } from "./funding/plain-funding-output.js";
import type { ResolvedProverSigner } from "./runtime.resolve-prover-signer.js";
import type { FraudProofWorkflowReferenceScript } from "./workflow/orchestrator.js";
import {
  type LocallyEvaluatedTransaction,
  workflowReferenceScriptsUsedByTransaction,
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
 * `funding`'s wallet coins, and signs it with the selected wallet. `funding`
 * is required: without it coin selection would read the whole wallet and
 * could spend the prover's own carriage.
 */
export const completeStep = async (
  tx: TxBuilder,
  funding: StepFunding,
): Promise<TxSigned> => {
  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...funding.walletInputs,
  });
  return await unsigned.sign.withWallet().complete();
};

/** A published reference script a step may read, and the script it must
 * carry. */
export type StepReferenceScriptCandidate = Readonly<{
  role: string;
  utxo: UTxO | undefined;
  expectedScript?: Script;
}>;

/**
 * The reference scripts a step transaction uses: either already resolved, or
 * the candidates it may read, of which exactly those the signed body
 * references are kept (`workflowReferenceScriptsUsedByTransaction`).
 */
export type StepReferenceScripts =
  | Readonly<{
      referenceScripts: readonly FraudProofWorkflowReferenceScript[];
    }>
  | Readonly<{
      referenceScriptCandidates: readonly StepReferenceScriptCandidate[];
    }>;

/**
 * Finishes a signed step as the transaction its caller submits: it refuses a
 * signed body whose hash is not 32 bytes of hex and pairs the transaction with
 * the reference scripts it uses. It performs no I/O.
 */
export const finishStep = (
  signed: TxSigned,
  scripts: StepReferenceScripts,
): LocallyEvaluatedTransaction => {
  const txHash = signed.toHash().toLowerCase();
  if (!/^[0-9a-f]{64}$/u.test(txHash)) {
    throw new Error(
      "locally evaluated transaction returned an invalid body hash",
    );
  }
  return {
    txHash,
    signed,
    referenceScripts:
      "referenceScripts" in scripts
        ? scripts.referenceScripts
        : workflowReferenceScriptsUsedByTransaction({
            signed,
            candidates: scripts.referenceScriptCandidates,
          }),
  };
};
