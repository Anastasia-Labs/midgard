import {
  encodeMidgardTxInputCanonical,
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  type MidgardTxInput,
  MissingNativeScriptTxStep02Datum,
  MissingNativeScriptTxStep02SpendRedeemer,
  type MissingNativeScriptTxStep02State,
  MissingNativeScriptTxStep03Datum,
  missingNativeScriptTxStep03State,
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
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
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
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { MissingNativeScriptTxContracts } from "./contracts.js";
import {
  missingNativeScriptTxStepLabel,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxReferenceScript,
  requireMissingNativeScriptTxStepState,
  requireMissingNativeScriptTxThreadUtxo,
} from "./submit-common.js";

const STEP_LABEL = missingNativeScriptTxStepLabel(1);

export type SubmitMissingNativeScriptTxStep02Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly inputWithMissingScript: MidgardTxInput;
  /** The §8.4 tier the ladder picked for field 0 — decided by size alone. */
  readonly carriageTier: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMissingNativeScriptTxStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  spendInputs,
  badInputIndex,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptTxContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly spendInputs: readonly MidgardTxInput[];
  readonly badInputIndex: bigint;
  /** Measurement/testing override; the tier otherwise follows preimage size. */
  readonly publishCarriage?: boolean;
  /** Pre-observed publications supplied by a journaled production action. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Pre-observed tier-3 certificate supplied by its journaled mint action. */
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  /** Durable boundary for each prerequisite carriage publication. */
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep02Result> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep02State =
    requireMissingNativeScriptTxStepState({
      threadUtxo,
      signer,
      schema: MissingNativeScriptTxStep02Datum,
      stepIndex: 1,
    });
  const inputWithMissingScript = spendInputs[Number(badInputIndex)];
  if (badInputIndex < 0n || inputWithMissingScript === undefined) {
    throw missingNativeScriptTxSubmitError(
      `bad input index ${badInputIndex.toString()} is outside ${spendInputs.length.toString()} items.`,
    );
  }
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: spendInputs.map(encodeMidgardTxInputCanonical),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: `${STEP_LABEL} spend inputs`,
  });

  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${STEP_LABEL} spend inputs`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepReference = requireMissingNativeScriptTxReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    stepIndex: 1,
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    stepReference,
  ];
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} spend inputs`,
  });
  const walletUtxos = await lucid.wallet().getUtxos();
  const usableWalletUtxos = carriageUtxos.reduce<readonly UTxO[]>(
    (utxos, carriage) => excludeUtxo(utxos, carriage),
    walletUtxos,
  );
  const feeInput = selectFeeInput(usableWalletUtxos);
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: missingNativeScriptTxStep03State({
        inputWithMissingScript,
        badTxId: state.bad_tx_id,
        badTxWitnessSetHash: state.bad_tx_witness_set_hash,
      }),
    },
    MissingNativeScriptTxStep03Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let layout: { inputIndex: bigint; outputIndex: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const resolved = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    layout = resolved;
    return Data.to(
      {
        Continue: [
          {
            input_index: resolved.inputIndex,
            output_index: resolved.outputIndex,
            bad_input_index: badInputIndex,
            spend_inputs_opening: opening,
          },
        ],
      },
      MissingNativeScriptTxStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  const paid = withInputs
    .readFrom(referenceInputs)
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await paid.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: usableWalletUtxos as UTxO[] }),
  });
  if (layout === undefined) {
    throw missingNativeScriptTxSubmitError(
      "BuildTxWithRedeemer did not resolve step-02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof missing-native-script-tx step-02",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[1].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingNativeScriptTxSubmitError(
      `step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputWithMissingScript,
    carriageTier: planned.plan.tier,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
