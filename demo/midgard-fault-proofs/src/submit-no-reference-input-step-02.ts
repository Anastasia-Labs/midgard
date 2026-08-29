/**
 * `no-reference-input` step-02 submitter (Goal task `Q18`, §9.1 output 8).
 *
 * Structural mirror of the `non-existent-input` chain's step 02
 * (`ne-submit-step-02.ts`): opens a field of the transaction carried by step 01
 * and forwards the single challenged input to step 03. Only the field differs —
 * §2.5 field **1**, the bad transaction's native reference inputs, rather than
 * field 0's spend inputs.
 *
 * **Re-derived onto the §8.8 door by #604.** The redeemer used to reproduce the
 * whole `reference_inputs_preimage: List<MidgardTxInput>`; it now carries a
 * `FieldOpeningV1`, and the validator reads item `bad_reference_input_index`
 * off the authenticated view.
 *
 * **What distinguishes field 1 from field 0 is position, not encoding.** The
 * header this replaces claimed the two fields' commitments differed because the
 * items were committed "under `bounded_collection_v1.from_items(1, ...)`"; §4
 * removed field-index domain separation, so identical items in fields 0 and 1
 * commit to identical hashes and only the slot the door reads tells them apart.
 * {@link planFaultProofFieldOpeningV1} names the slot explicitly and checks the
 * preimage against the commitment the compact body carries *there*, which is the
 * off-chain twin of `field_commitment_at`.
 *
 * Nothing in the prepared JSON is trusted for anything the chain re-derives: the
 * anchor and the block roots forwarded to step 03 are read from the **on-chain**
 * step-01 output datum, never from the file.
 */
import {
  encodeMidgardTxInputCanonicalV1,
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxInput,
  NoReferenceInputStep02Datum,
  NoReferenceInputStep02SpendRedeemer,
  NoReferenceInputStep03Datum,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpeningV1,
  parseNativeTxCompactCborV1,
  planFaultProofFieldOpeningV1,
} from "./field-opening-v1.js";
import { type NoReferenceInputPreimageEntry } from "./prepare-no-reference-input.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveNoReferenceInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import { witnessSpendingValidatorCarriageV1 } from "./witness-reference-scripts-v1.js";

const toMidgardTxInput = (
  entry: NoReferenceInputPreimageEntry,
): MidgardTxInput => ({
  tx_id: entry.txId,
  output_index: BigInt(entry.index),
});

export type SubmitNoReferenceInputStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly referenceInputsPreimagePath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — the disputed transaction's
   * compact structure. New in #604: the door authenticates field 1 against it.
   */
  readonly nativeTxCompactPath: string;
  readonly badReferenceInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitNoReferenceInputStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  readonly missingReferenceInput: MidgardTxInput;
  /** §4's flat commitment for field 1 — re-derived here and by the door. */
  readonly verifiedTxReferenceInputsHash: string;
  /** The door's authenticated item count for field 1. */
  readonly referenceInputsItemCount: number;
  readonly badReferenceInputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step02DatumWithState = NoReferenceInputStep02Datum & {
  readonly data: NonNullable<NoReferenceInputStep02Datum["data"]>;
};

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, NoReferenceInputStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 02 input datum must carry bad-tx-reference-inputs state data.",
    );
  }
  return datum as Step02DatumWithState;
};

export const submitNoReferenceInputStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  referenceInputsPreimage,
  nativeTxCompactCbor,
  badReferenceInputIndex,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceInputsPreimage: readonly NoReferenceInputPreimageEntry[];
  /** The disputed transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly badReferenceInputIndex: bigint;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNoReferenceInputStep02Result> => {
  const { noReferenceInputCategory, contracts } =
    await resolveNoReferenceInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const steps = contracts.noReferenceInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "no-reference-input step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at no-reference-input step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: noReferenceInputCategory.categoryId,
    categoryLabel: "no-reference-input",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });

  if (
    badReferenceInputIndex < 0n ||
    badReferenceInputIndex >= BigInt(referenceInputsPreimage.length)
  ) {
    throw new Error(
      `badReferenceInputIndex ${badReferenceInputIndex.toString()} is out of bounds for ${referenceInputsPreimage.length.toString()} reference inputs.`,
    );
  }
  const midgardReferenceInputs = referenceInputsPreimage.map(toMidgardTxInput);
  // Re-run the on-chain rule of step 02 before spending. The supplied list must
  // be the exact §5.1 preimage the bad transaction committed **at field 1**:
  // reference inputs and spend inputs share the per-item encoder *and*, since §4
  // removed domain separation, the resulting commitment, so it is the field
  // index named here — never the encoding — that stops a spend-inputs preimage
  // from opening this slot.
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.referenceInputs,
    anchorTxId: inputDatum.data.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: midgardReferenceInputs.map(encodeMidgardTxInputCanonicalV1),
    owner: signer.paymentKeyHash,
    label: "No-reference-input step 02 reference-inputs",
  });
  const verifiedTxReferenceInputsHash = planned.commitment;
  const stepScriptCarriage = witnessSpendingValidatorCarriageV1({
    script: steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "no-reference-input step 02 validator",
  });
  // The complete reference-input set the built transaction will declare, in
  // build order — the opening derivation must see all of it (bug fc635c8f).
  const referenceInputs = [...stepScriptCarriage.referenceInputs];
  const referenceInputsOpening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    label: "No-reference-input step 02 reference-inputs",
  });
  const missingReferenceInput =
    midgardReferenceInputs[Number(badReferenceInputIndex)]!;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        missing_reference_input: missingReferenceInput,
        blocks_prev_utxos_root: inputDatum.data.blocks_prev_utxos_root,
        blocks_transactions_root: inputDatum.data.blocks_transactions_root,
      },
    },
    NoReferenceInputStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: { inputIndex: bigint; outputIndex: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "no-reference-input step 02");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "no-reference-input step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "no-reference-input step 02 output",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            reference_inputs_opening: referenceInputsOpening,
            bad_reference_input_index: badReferenceInputIndex,
          },
        ],
      },
      NoReferenceInputStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const collected = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  // Without a published witness this step reads nothing, and `readFrom([])`
  // is an error rather than a no-op, so the branch is on whether the carriage
  // produced reference inputs at all.
  const tx = (
    referenceInputs.length === 0
      ? collected
      : collected.readFrom([...referenceInputs])
  ).pay
    .ToContract(
      steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = stepScriptCarriage.attach(tx);
  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve no-reference-input step 02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
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
    secondStepAddress: steps[1].spendingScriptAddress,
    thirdStepAddress: steps[2].spendingScriptAddress,
    missingReferenceInput,
    verifiedTxReferenceInputsHash,
    referenceInputsItemCount: planned.itemCount,
    badReferenceInputIndex: Number(badReferenceInputIndex),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitNoReferenceInputStep02FromFiles = async (
  config: SubmitNoReferenceInputStep02CliConfig,
): Promise<SubmitNoReferenceInputStep02Result> => {
  const [
    blueprint,
    deploymentInfo,
    referenceInputsJson,
    nativeTxCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.referenceInputsPreimagePath),
    readJsonFile(config.nativeTxCompactPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitNoReferenceInputStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    referenceInputsPreimage:
      referenceInputsJson as readonly NoReferenceInputPreimageEntry[],
    nativeTxCompactCbor: parseNativeTxCompactCborV1(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    badReferenceInputIndex: BigInt(config.badReferenceInputIndex),
    awaitConfirmation: config.awaitConfirmation,
  });
};
