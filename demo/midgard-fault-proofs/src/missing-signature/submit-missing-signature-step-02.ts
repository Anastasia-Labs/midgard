/**
 * `missing-signature` step-02 submitter (offchain plan §4.2, §5 frontier 2).
 *
 * Opens body field 4 (`required_signers`) through the §8.8 door against the
 * thread-anchored `verified_tx_id` — the **first offchain consumer of field
 * 4** — and selects the accused signer hash by its fixed-28-byte-stride
 * ordinal. Body field, so the opening needs no witness-set pairing;
 * `field_item_at` aborts on an out-of-domain ordinal on-chain, so the same
 * bound is refused here first.
 *
 * A realistic field 4 is tiny (a handful of 28-byte hashes) and rides tier
 * 1, but the tier is chosen by the door's planner from the actual bytes,
 * never assumed: when the plan demands carriage, this submitter publishes
 * the chunks first and reads them back, identically to the decoding
 * family's step-03.
 */
import {
  MissingSignatureStep02Datum,
  MissingSignatureStep02SpendRedeemer,
  type MissingSignatureStep02State,
  MissingSignatureStep03Datum,
  type MissingSignatureStep03State,
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
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingSignatureContracts } from "./contracts-v1.js";
import { planMissingSignatureRequiredSignersOpening } from "./evidence-v1.js";
import {
  missingSignatureStepLabel,
  missingSignatureSubmitError,
  requireMissingSignatureReferenceScript,
  requireMissingSignatureStepState,
  requireMissingSignatureThreadUtxo,
} from "./submit-common-v1.js";

const STEP_LABEL = missingSignatureStepLabel(1);

export type SubmitMissingSignatureStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  /** The accused required signer's hash, now pinned in thread state. */
  readonly missingRequiredSignerHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMissingSignatureStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  requiredSignerHashes,
  nativeTxCompactCbor,
  badRequiredSignerHashIndex,
  publishCarriage = false,
  certificateUtxo,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingSignatureContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Complete positional required-signer list — field 4's §5.1 preimage. */
  readonly requiredSignerHashes: readonly string[];
  /** The accused transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly badRequiredSignerHashIndex: bigint;
  /** Force §8 tier-2 carriage publication (testing knob). */
  readonly publishCarriage?: boolean;
  /** Pre-minted §8.6 certificate when the planner selects tier 3. */
  readonly certificateUtxo?: UTxO;
  /** §2.3: the published step-02 reference script (required; never inline). */
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingSignatureStep02Result> => {
  const { threadUtxo, threadToken } = await requireMissingSignatureThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 1,
    threadOutRef,
  });
  const state: MissingSignatureStep02State = requireMissingSignatureStepState({
    threadUtxo,
    signer,
    schema: MissingSignatureStep02Datum,
    stepIndex: 1,
  });

  // The validator's `field_item_at` aborts outside the domain; refuse the
  // same bound before paying for the attempt.
  const accusedHash =
    requiredSignerHashes[Number(badRequiredSignerHashIndex)]?.toLowerCase();
  if (badRequiredSignerHashIndex < 0n || accusedHash === undefined) {
    throw missingSignatureSubmitError(
      `accused ordinal ${badRequiredSignerHashIndex.toString()} is outside the ${requiredSignerHashes.length.toString()}-item required-signer list.`,
    );
  }

  // The §8.8 door: plan, publish whatever the tier demands, open.
  const planned = planMissingSignatureRequiredSignersOpening({
    anchorTxId: state.verified_tx_id,
    nativeTxCompactCbor,
    requiredSignerHashes,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
  });
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: `${STEP_LABEL} required-signers`,
  });
  const fieldReferenceInputs = [
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...carriageUtxos,
  ];
  const referenceInputs = [
    ...fieldReferenceInputs,
    requireMissingSignatureReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[1].spendingScriptHash,
      stepIndex: 1,
    }),
  ];
  const requiredSignersOpening = faultProofFieldOpening({
    planned,
    // §8.7 indices are into the complete reference-input set, including the
    // step's own reference script; resolving against carriage alone is only
    // accidentally correct for some out-ref orderings.
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} required-signers`,
  });

  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = carriageUtxos.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const step03State: MissingSignatureStep03State = {
    missing_required_signer_hash: accusedHash,
    verified_tx_id: state.verified_tx_id,
    verified_witness_set_hash: state.verified_witness_set_hash,
  };
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step03State },
    MissingSignatureStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: step03Datum,
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
        step03OutputMatches,
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
            required_signers_opening: requiredSignersOpening,
            bad_required_signer_hash_index: badRequiredSignerHashIndex,
          },
        ],
      },
      MissingSignatureStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  const withReferences =
    referenceInputs.length === 0
      ? withInputs
      : withInputs.readFrom(referenceInputs);
  const tx = withReferences.pay
    .ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: walletUtxosSansCarriage as UTxO[] }),
  });
  if (resolvedLayout === undefined) {
    throw missingSignatureSubmitError(
      "BuildTxWithRedeemer did not resolve the step-02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof missing-signature step-02",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[1].spendingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingSignatureSubmitError(
      `step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    missingRequiredSignerHash: accusedHash,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
