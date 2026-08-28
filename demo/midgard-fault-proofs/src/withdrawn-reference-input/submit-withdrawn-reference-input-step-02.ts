/**
 * `withdrawn-reference-input` step-02 submitter (Q19 offchain plan §4.2).
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
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  WithdrawnReferenceInputStep02Datum,
  WithdrawnReferenceInputStep02SpendRedeemer,
  WithdrawnReferenceInputStep03Datum,
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
import type { WithdrawnReferenceInputContractsV1 } from "./contracts-v1.js";
import {
  requireWithdrawnReferenceInputReferenceScriptV1,
  requireWithdrawnReferenceInputStepStateV1,
  requireWithdrawnReferenceInputThreadUtxoV1,
  withdrawnReferenceInputSubmitError,
} from "./submit-common-v1.js";

export type SubmitWithdrawnReferenceInputStep02Result = {
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
  /** The §8.4 tier the ladder picked for field 1 — decided by size alone. */
  readonly carriageTier: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly referenceScriptOutRef: string;
  readonly awaitedConfirmation: boolean;
};

export const submitWithdrawnReferenceInputStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceInputs,
  nativeTxCompactCbor,
  badReferenceInputIndex,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnReferenceInputContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceInputs: readonly MidgardTxInput[];
  /** The disputed transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly badReferenceInputIndex: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawnReferenceInputStep02Result> => {
  const steps = contracts.steps;
  const { threadUtxo, threadToken } =
    await requireWithdrawnReferenceInputThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const inputState = requireWithdrawnReferenceInputStepStateV1({
    threadUtxo,
    signer,
    schema: WithdrawnReferenceInputStep02Datum,
    stepIndex: 1,
  });

  if (
    badReferenceInputIndex < 0n ||
    badReferenceInputIndex >= BigInt(referenceInputs.length)
  ) {
    throw withdrawnReferenceInputSubmitError(
      `badReferenceInputIndex ${badReferenceInputIndex.toString()} is out of bounds for ${referenceInputs.length.toString()} reference inputs.`,
    );
  }
  // Re-run the on-chain rule of step 02 before spending. The supplied list must
  // be the exact §5.1 preimage the bad transaction committed **at field 1**:
  // reference inputs and spend inputs share the per-item encoder *and*, since §4
  // removed domain separation, the resulting commitment, so it is the field
  // index named here — never the encoding — that stops a spend-inputs preimage
  // from opening this slot.
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.referenceInputs,
    anchorTxId: inputState.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: referenceInputs.map(encodeMidgardTxInputCanonicalV1),
    owner: signer.paymentKeyHash,
    label: "Withdrawn-reference-input step 02 reference-inputs",
  });
  const verifiedTxReferenceInputsHash = planned.commitment;
  const stepReference = requireWithdrawnReferenceInputReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: steps[1].spendingScriptHash,
    stepIndex: 1,
  });
  signer.selectWallet(lucid);
  // §8's ladder decides whether anything has to exist on-chain first. Tier 1
  // publishes nothing; tier 2 publishes raw carriage located by content
  // (§8.7), and a publication that already exists at this address is reused
  // rather than republished.
  const carriageUtxos = await publishFaultProofFieldCarriageV1({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Withdrawn-reference-input step 02 reference-inputs",
  });
  const transactionReferenceInputs = [...carriageUtxos, stepReference];
  const referenceInputsOpening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs: transactionReferenceInputs,
    label: "Withdrawn-reference-input step 02 reference-inputs",
  });
  const missingReferenceInput =
    referenceInputs[Number(badReferenceInputIndex)]!;

  // A fresh tier-2 publication carries enough min-ADA to top the fee sort;
  // never spend anything this transaction must read.
  const feeInput = selectFeeInput(
    transactionReferenceInputs.reduce<readonly UTxO[]>(
      (candidates, utxo) => excludeUtxo(candidates, utxo),
      await lucid.wallet().getUtxos(),
    ),
  );
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        missing_reference_input: missingReferenceInput,
        blocks_withdrawals_root: inputState.blocks_withdrawals_root,
        blocks_withdrawal_count: inputState.blocks_withdrawal_count,
      },
    },
    WithdrawnReferenceInputStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: { inputIndex: bigint; outputIndex: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "withdrawn-reference-input step 02",
    );
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "withdrawn-reference-input step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "withdrawn-reference-input step 02 output",
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
      WithdrawnReferenceInputStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([...transactionReferenceInputs])
    .pay.ToContract(
      steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw withdrawnReferenceInputSubmitError(
      "BuildTxWithRedeemer did not resolve withdrawn-reference-input step 02 layout.",
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
    carriageTier: planned.plan.tier,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    referenceScriptOutRef: `${stepReference.txHash}#${stepReference.outputIndex.toString()}`,
    awaitedConfirmation: awaitConfirmation,
  };
};
