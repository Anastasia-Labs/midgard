/**
 * `value-not-preserved` step-03 submitter (offchain plan §4): outputs, mint
 * and fee.
 *
 * Takes the challenged transaction's STRUCTURED outputs and mint items and
 * derives the committed field preimages itself
 * (`encodeMidgardFieldItemsV1` under the §5.1 envelope), so the expected
 * `final_delta` can be computed locally with the same restricted fold the
 * validator runs: every output's claimed quantity is outflow; a token claim
 * adds the mint field's claimed entries; an ADA claim subtracts the
 * committed fee and must carry NO mint carriage (ADA is structurally
 * unmintable, plan §1.2).
 *
 * Both carriages go through the §8.8 door's planner
 * (`planFaultProofFieldOpeningV1`), which pre-validates every check the door
 * makes and selects the tier purely from the preimage's own length (§8.4): a
 * small field rides tier-1 (`Inline`) in this step's own redeemer, and a
 * preimage over the 14,336-byte tier-1 cap is published as a tier-2
 * (`RawUtxo`) nothing-but-bytes datum first and consumed here as a reference
 * input, with the positional index resolved against this transaction's
 * COMPLETE reference-input set (carriage publications, any injected UTxO,
 * and the step's own reference script).
 */
import type {
  MidgardMintPolicyItemV1,
  MidgardTxOutput,
} from "@al-ft/midgard-core";
import { encodeMidgardFieldItemsV1 } from "@al-ft/midgard-core";
import {
  MIDGARD_FIELD_INDEX_V1,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldCarriageV1,
  type FaultProofFieldOpeningPlanV1,
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
import type { ValueNotPreservedContractsV1 } from "./contracts-v1.js";
import { claimedQuantityOfValueV1 } from "./evidence-v1.js";
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

/** Injective out-ref key for de-duplicating a reference-input set. */
const outRefKey = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const dedupUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const seen = new Set<string>();
  const unique: UTxO[] = [];
  for (const utxo of utxos) {
    const key = outRefKey(utxo);
    if (seen.has(key)) continue;
    seen.add(key);
    unique.push(utxo);
  }
  return unique;
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
  /** §8.4 tier the planner selected for the outputs field. */
  readonly outputsCarriageTier: "Inline" | "RawUtxo" | "Certified";
  /** §8.4 tier of the mint field; null exactly for an ADA claim. */
  readonly mintCarriageTier: "Inline" | "RawUtxo" | "Certified" | null;
  /** The §8 carriage publications this step consumed as reference inputs. */
  readonly carriageUtxos: readonly UTxO[];
  /** The §5.1 outputs-field preimage length in bytes (tier evidence). */
  readonly outputsPreimageBytes: number;
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
  unsafeSpendFieldRawUtxoForTest,
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
  /**
   * Never set outside tests. Substitutes the OUTPUTS carriage with a raw
   * tier-2 arm naming this UTxO positionally — bypassing the honest
   * content-addressed resolution (`resolveChunkReferenceIndicesV1` matches
   * publications by exact §8.5 datum bytes, so a tampered publication can
   * never resolve through it). Adversarial suites use this to prove the §8.8
   * door's `field_commitment(preimage) == expected_hash` re-hash refuses a
   * publication whose bytes differ from the committed field hash.
   */
  readonly unsafeSpendFieldRawUtxoForTest?: UTxO;
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

  // The §8.8 door: plan each field, publish whatever the tier demands, open.
  // The tier is a pure function of the preimage's length — nothing here
  // forces one.
  const outputsPlan = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: encodeMidgardFieldItemsV1({ fieldIndex: 2, items: outputs }),
    owner: signer.paymentKeyHash,
    label: `${STEP_LABEL} outputs`,
  });
  const mintPlan: FaultProofFieldOpeningPlanV1 | null =
    mintItems === null
      ? null
      : planFaultProofFieldOpeningV1({
          fieldIndex: MIDGARD_FIELD_INDEX_V1.mint,
          anchorTxId: state.bad_tx_id,
          nativeTxCompactCbor,
          itemCbors: encodeMidgardFieldItemsV1({
            fieldIndex: 5,
            items: mintItems,
          }),
          owner: signer.paymentKeyHash,
          label: `${STEP_LABEL} mint`,
        });
  signer.selectWallet(lucid);
  // With an injected raw UTxO the honest outputs publication is skipped —
  // the whole point is that ONLY the injected bytes ride as the carriage.
  const outputsCarriageUtxos =
    unsafeSpendFieldRawUtxoForTest === undefined
      ? await publishFaultProofFieldCarriageV1({
          lucid,
          signer,
          planned: outputsPlan,
          publisherAddress: signer.address,
          label: `${STEP_LABEL} outputs`,
        })
      : [];
  const mintCarriageUtxos =
    mintPlan === null
      ? []
      : await publishFaultProofFieldCarriageV1({
          lucid,
          signer,
          planned: mintPlan,
          publisherAddress: signer.address,
          label: `${STEP_LABEL} mint`,
        });
  const carriageUtxos = [...outputsCarriageUtxos, ...mintCarriageUtxos];
  const referenceInputs = dedupUtxos([
    ...carriageUtxos,
    ...(unsafeSpendFieldRawUtxoForTest === undefined
      ? []
      : [unsafeSpendFieldRawUtxoForTest]),
    ...(referenceScriptUtxo === undefined
      ? []
      : [
          requireValueNotPreservedReferenceScriptV1({
            utxo: referenceScriptUtxo,
            expectedScriptHash: contracts.steps[2].spendingScriptHash,
            stepIndex: 2,
          }),
        ]),
  ]);
  // §8.7 indices are into the complete reference-input set, including the
  // step's own reference script; resolving against carriage alone is only
  // accidentally correct for some out-ref orderings.
  const outputsCarriage =
    unsafeSpendFieldRawUtxoForTest === undefined
      ? faultProofFieldCarriageV1({
          planned: outputsPlan,
          referenceInputs,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: `${STEP_LABEL} outputs`,
        })
      : null;
  const mintCarriage =
    mintPlan === null
      ? null
      : faultProofFieldCarriageV1({
          planned: mintPlan,
          referenceInputs,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: `${STEP_LABEL} mint`,
        });

  const completedState: ValueNotPreservedStep04State = {
    bad_tx_id: state.bad_tx_id,
    claimed_asset: state.claimed_asset,
    claimed_direction: state.claimed_direction,
    final_delta: finalDelta,
  };

  // The publications live at the prover's own key address, so coin
  // selection must never consume them mid-flow — nor any OTHER datum-bearing
  // UTxO (a reusable publication from an earlier flow, or an injected test
  // fixture): fee input and preset candidates are datum-free only.
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansReferenced = referenceInputs.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const datumFreeWalletUtxos = walletUtxosSansReferenced.filter(
    (utxo) => utxo.datum == null && utxo.datumHash == null,
  );
  const feeInput = selectFeeInput(datumFreeWalletUtxos);
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
  const injectedRawUtxo = unsafeSpendFieldRawUtxoForTest;
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
    if (outputsCarriage === null && injectedRawUtxo === undefined) {
      throw valueNotPreservedSubmitError(
        "step-03 resolved neither an honest outputs carriage nor an injected one.",
      );
    }
    const outputsCarriageForLayout =
      outputsCarriage ??
      ({
        RawUtxo: {
          ref_input_index: requireReferenceInputIndex(
            ctx,
            // Non-null by the guard above.
            injectedRawUtxo as UTxO,
            `${STEP_LABEL} injected raw carriage`,
          ),
        },
      } as const);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            native_tx_compact_cbor: nativeTxCompactCbor,
            outputs_carriage: outputsCarriageForLayout,
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

  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  const withReferences =
    referenceInputs.length === 0
      ? withInputs
      : withInputs.readFrom([...referenceInputs]);
  const withPayment = withReferences.pay
    .ToContract(
      contracts.steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx =
    referenceScriptUtxo === undefined
      ? withPayment.attach.SpendingValidator(contracts.steps[2].spendingScript)
      : withPayment;

  const unsigned = await tx.complete({
    localUPLCEval: true,
    presetWalletInputs: datumFreeWalletUtxos as UTxO[],
  });
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
    outputsCarriageTier: outputsPlan.plan.tier,
    mintCarriageTier: mintPlan === null ? null : mintPlan.plan.tier,
    carriageUtxos,
    outputsPreimageBytes: outputsPlan.preimage.length,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
