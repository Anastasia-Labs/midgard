/**
 * `value-not-preserved` step-03 submitter (offchain plan §4): outputs, mint
 * and fee.
 *
 * Takes the challenged transaction's STRUCTURED outputs and mint items and
 * derives the committed field preimages itself
 * (`encodeMidgardFieldPreimageForFieldV1`), so the expected `final_delta`
 * can be computed locally with the same restricted fold the validator runs:
 * every output's claimed quantity is outflow; a token claim adds the mint
 * field's claimed entries; an ADA claim subtracts the committed fee and must
 * carry NO mint carriage (ADA is structurally unmintable, plan §1.2). Both
 * carriages are v1 tier-1 (`Inline`) per plan §5.
 */
import type {
  MidgardMintPolicyItemV1,
  MidgardTxOutput,
} from "@al-ft/midgard-core";
import { encodeMidgardFieldPreimageForFieldV1 } from "@al-ft/midgard-core";
import {
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
import type { ValueNotPreservedContractsV1 } from "./contracts-v1.js";
import {
  claimedQuantityOfValueV1,
  inlineFieldCarriageV1,
} from "./evidence-v1.js";
import {
  type ClaimedAssetV1,
  ValueNotPreservedStep03Datum,
  ValueNotPreservedStep03SpendRedeemer,
  type ValueNotPreservedStep03State,
  ValueNotPreservedStep04Datum,
  type ValueNotPreservedStep04State,
} from "./schemas-v1.js";
import {
  requireValueNotPreservedReferenceScriptV1,
  requireValueNotPreservedStepStateV1,
  requireValueNotPreservedThreadUtxoV1,
  valueNotPreservedStepLabelV1,
  valueNotPreservedSubmitError,
} from "./submit-common-v1.js";

const STEP_LABEL = valueNotPreservedStepLabelV1(2);

/** The mint field's claimed-asset total — the validator's stage-four fold. */
const mintClaimedQuantityV1 = (
  claim: ClaimedAssetV1,
  mintItems: readonly MidgardMintPolicyItemV1[],
): bigint => {
  if (claim === "AdaAsset") return 0n;
  let total = 0n;
  for (const item of mintItems) {
    if (
      Buffer.from(item.policyId).toString("hex") !== claim.TokenAsset.policy_id
    ) {
      continue;
    }
    for (const asset of item.assets) {
      if (
        Buffer.from(asset.assetName).toString("hex") ===
        claim.TokenAsset.asset_name
      ) {
        total += asset.quantity;
      }
    }
  }
  return total;
};

export type SubmitValueNotPreservedStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fourthStepAddress: string;
  /** The completed fold the thread now carries. */
  readonly completedState: ValueNotPreservedStep04State;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValueNotPreservedStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  outputs,
  mintItems,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ValueNotPreservedContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The challenged transaction's compact bytes — the §3 anchor. */
  readonly nativeTxCompactCbor: string;
  /** The transaction's outputs, exactly as committed (field 2). */
  readonly outputs: readonly MidgardTxOutput[];
  /**
   * The transaction's mint items, exactly as committed (field 5) — for a
   * token claim. MUST be null for an ADA claim (no mint carriage).
   */
  readonly mintItems: readonly MidgardMintPolicyItemV1[] | null;
  /** The published step-03 reference script; inline-attached when absent. */
  readonly referenceScriptUtxo?: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValueNotPreservedStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireValueNotPreservedThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state: ValueNotPreservedStep03State =
    requireValueNotPreservedStepStateV1({
      threadUtxo,
      signer,
      schema: ValueNotPreservedStep03Datum,
      stepIndex: 2,
    });

  // The validator's stage-three fold, run locally over the same preimage.
  const outflow = outputs.reduce(
    (total, output) =>
      total + claimedQuantityOfValueV1(state.claimed_asset, output.value),
    0n,
  );
  const deltaAfterOutputs = state.claimed_delta - outflow;
  let finalDelta: bigint;
  if (state.claimed_asset === "AdaAsset") {
    if (mintItems !== null) {
      throw valueNotPreservedSubmitError(
        "an ADA claim has no mint term (ADA is structurally unmintable); pass mintItems: null.",
      );
    }
    finalDelta = deltaAfterOutputs - state.committed_fee;
  } else {
    if (mintItems === null) {
      throw valueNotPreservedSubmitError(
        "a token claim must open the whole mint field; pass the committed mint items (possibly empty).",
      );
    }
    finalDelta =
      deltaAfterOutputs + mintClaimedQuantityV1(state.claimed_asset, mintItems);
  }

  const outputsPreimage = encodeMidgardFieldPreimageForFieldV1({
    fieldIndex: 2,
    items: outputs,
  });
  const mintCarriage =
    mintItems === null
      ? null
      : inlineFieldCarriageV1(
          encodeMidgardFieldPreimageForFieldV1({
            fieldIndex: 5,
            items: mintItems,
          }),
        );

  const completedState: ValueNotPreservedStep04State = {
    bad_tx_id: state.bad_tx_id,
    claimed_asset: state.claimed_asset,
    claimed_direction: state.claimed_direction,
    final_delta: finalDelta,
  };

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step04Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: completedState },
    ValueNotPreservedStep04Datum,
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
            native_tx_compact_cbor: nativeTxCompactCbor,
            outputs_carriage: inlineFieldCarriageV1(outputsPreimage),
            mint_carriage: mintCarriage,
          },
        ],
      },
      ValueNotPreservedStep03SpendRedeemer,
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
    .addSignerKey(signer.paymentKeyHash);
  const tx =
    referenceScriptUtxo === undefined
      ? base.attach.SpendingValidator(contracts.steps[2].spendingScript)
      : base.readFrom([
          requireValueNotPreservedReferenceScriptV1({
            utxo: referenceScriptUtxo,
            expectedScriptHash: contracts.steps[2].spendingScriptHash,
            stepIndex: 2,
          }),
        ]);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw valueNotPreservedSubmitError(
      "BuildTxWithRedeemer did not resolve the step-03 layout.",
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
    fourthStepAddress: contracts.steps[3].spendingScriptAddress,
    completedState,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
