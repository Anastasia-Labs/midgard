/**
 * `missing-signature` step-03 submitter (offchain plan §4.2).
 *
 * The lift: hands the validator the verification-key preimage of the accused
 * signer hash (`get_verification_key_hash(vkey) == hash`, blake2b-224 on raw
 * bytes) and forwards the anchor. The one check the validator makes is made
 * locally first with the exact twin, so a wrong preimage — the §7.2
 * `UnknownVkeyPreimage` corner's tell — is refused before anything is paid
 * for. Pure computation on-chain: no reference inputs beyond the optional
 * step script, no withdrawal, no opening.
 */
import {
  MissingSignatureStep03Datum,
  MissingSignatureStep03SpendRedeemer,
  type MissingSignatureStep03State,
  MissingSignatureStep04Datum,
  type MissingSignatureStep04State,
  missingSignatureVkeyHash,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingSignatureContracts } from "./contracts-v1.js";
import {
  missingSignatureStepLabel,
  missingSignatureSubmitError,
  requireMissingSignatureReferenceScript,
  requireMissingSignatureStepState,
  requireMissingSignatureThreadUtxo,
} from "./submit-common-v1.js";

const STEP_LABEL = missingSignatureStepLabel(2);

export type SubmitMissingSignatureStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fourthStepAddress: string;
  readonly missingRequiredSignerVkey: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMissingSignatureStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  missingRequiredSignerVkey,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingSignatureContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The §3.3-resolved 32-byte verification key, hex. */
  readonly missingRequiredSignerVkey: string;
  /** §2.3: the published step-03 reference script (required; never inline). */
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingSignatureStep03Result> => {
  const { threadUtxo, threadToken } = await requireMissingSignatureThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 2,
    threadOutRef,
  });
  const state: MissingSignatureStep03State = requireMissingSignatureStepState({
    threadUtxo,
    signer,
    schema: MissingSignatureStep03Datum,
    stepIndex: 2,
  });

  const vkey = missingRequiredSignerVkey.toLowerCase();
  const derivedHash = missingSignatureVkeyHash(vkey);
  if (derivedHash !== state.missing_required_signer_hash) {
    throw missingSignatureSubmitError(
      `supplied verification key hashes to ${derivedHash}, not the thread's accused required signer ${state.missing_required_signer_hash} — no known preimage means this finding is the §7.2 UnknownVkeyPreimage corner, which this family cannot prove.`,
    );
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step04State: MissingSignatureStep04State = {
    missing_required_signer_vkey: vkey,
    verified_tx_id: state.verified_tx_id,
    verified_witness_set_hash: state.verified_witness_set_hash,
    field_walk_checkpoint_hash: "",
  };
  const step04Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step04State },
    MissingSignatureStep04Datum,
  );
  const step04OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: step04Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            missing_required_signer_vkey: vkey,
          },
        ],
      },
      MissingSignatureStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      contracts.steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .readFrom([
      requireMissingSignatureReferenceScript({
        utxo: referenceScriptUtxo,
        expectedScriptHash: contracts.steps[2].spendingScriptHash,
        stepIndex: 2,
      }),
    ]);

  const unsigned = await base.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw missingSignatureSubmitError(
      "BuildTxWithRedeemer did not resolve the step-03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof missing-signature step-03",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[2].spendingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingSignatureSubmitError(
      `step-03 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    fourthStepAddress: contracts.steps[3].spendingScriptAddress,
    missingRequiredSignerVkey: vkey,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
