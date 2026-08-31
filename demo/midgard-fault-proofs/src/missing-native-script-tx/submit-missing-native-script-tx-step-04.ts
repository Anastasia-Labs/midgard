import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
  MissingNativeScriptTxStep04Datum,
  MissingNativeScriptTxStep04SpendRedeemer,
  type MissingNativeScriptTxStep04State,
  MissingNativeScriptTxStep05Datum,
  missingNativeScriptTxStep05StateV1,
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
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import {
  missingNativeScriptTxStepLabelV1,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxReferenceScriptV1,
  requireMissingNativeScriptTxStepStateV1,
  requireMissingNativeScriptTxThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = missingNativeScriptTxStepLabelV1(3);

export type SubmitMissingNativeScriptTxStep04Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly expectedMissingScriptHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMissingNativeScriptTxStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  outputItemCbors,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingNativeScriptTxContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Complete canonical field-2 item list of the producing transaction. */
  readonly outputItemCbors: readonly Uint8Array[];
  readonly nativeTxCompactCbor: string;
  readonly publishCarriage?: boolean;
  /** Pre-observed publications supplied by a journaled production action. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Pre-observed tier-3 certificate supplied by its journaled mint action. */
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  /** Durable boundary for each prerequisite carriage publication. */
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep04Result> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 3,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep04State =
    requireMissingNativeScriptTxStepStateV1({
      threadUtxo,
      signer,
      schema: MissingNativeScriptTxStep04Datum,
      stepIndex: 3,
    });
  const outputCbor = outputItemCbors[Number(state.bad_input_output_index)];
  if (state.bad_input_output_index < 0n || outputCbor === undefined) {
    throw missingNativeScriptTxSubmitError(
      `producing output index ${state.bad_input_output_index.toString()} is outside ${outputItemCbors.length.toString()} items.`,
    );
  }
  const credential = decodeMidgardAddressBytes(
    decodeMidgardTxOutput(outputCbor).address,
  ).paymentCredential;
  if (credential.kind !== "Script") {
    throw missingNativeScriptTxSubmitError(
      "the accused producing output is key-locked, not script-locked.",
    );
  }
  const expectedMissingScriptHash = credential.hash.toString("hex");
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
    anchorTxId: state.producing_tx_id,
    nativeTxCompactCbor,
    itemCbors: outputItemCbors,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: `${STEP_LABEL} outputs`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${STEP_LABEL} outputs`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepReference = requireMissingNativeScriptTxReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[3].spendingScriptHash,
    stepIndex: 3,
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    stepReference,
  ];
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} outputs`,
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
      data: missingNativeScriptTxStep05StateV1({
        expectedMissingScriptHash,
        badTxId: state.bad_tx_id,
        badTxWitnessSetHash: state.bad_tx_witness_set_hash,
      }),
    },
    MissingNativeScriptTxStep05Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[4].spendingScriptAddress,
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
            outputs_opening: opening,
          },
        ],
      },
      MissingNativeScriptTxStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  const paid = withInputs
    .readFrom(referenceInputs)
    .pay.ToContract(
      contracts.steps[4].spendingScriptAddress,
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
      "BuildTxWithRedeemer did not resolve step-04 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof missing-native-script-tx step-04",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[3].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingNativeScriptTxSubmitError(
      `step-04 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    expectedMissingScriptHash,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
