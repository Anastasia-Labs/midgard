/**
 * `value-not-preserved` step-02 submitters (offchain plan §4): the
 * spent-input accumulation self-loop.
 *
 * - `submitValueNotPreservedStep02Fold` — one `FoldInput` iteration: opens
 *   the challenged transaction's field-0 preimage through the §8.8 door,
 *   carries the pre-state value witness for the input at the thread's
 *   cursor, and self-loops with the cursor advanced and the claimed asset's
 *   quantity accumulated. The expected next state is computed locally with
 *   the same fold the validator runs (`witnessClaimedQuantityV1`), so a
 *   witness that would not verify on-chain fails here first.
 * - `submitValueNotPreservedStep02Finish` — the `FinishInputs` arm: the
 *   cursor must equal the field's item count, and the accumulated inflow
 *   moves to step-03.
 *
 * One transaction per input by design (plan §4.1): each iteration is an
 * independent, resumable L1 transaction, so a fold interrupted at any depth
 * continues from the thread UTxO it left behind.
 */
import type { FieldOpeningV1 } from "@al-ft/midgard-sdk";
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
import { witnessSpendingValidatorCarriageV1 } from "../witness-reference-scripts-v1.js";
import type { ValueNotPreservedContractsV1 } from "./contracts-v1.js";
import { witnessClaimedQuantityV1 } from "./evidence-v1.js";
import {
  type SpentInputValueWitnessV1,
  ValueNotPreservedStep02Datum,
  ValueNotPreservedStep02SpendRedeemer,
  type ValueNotPreservedStep02State,
  ValueNotPreservedStep03Datum,
  type ValueNotPreservedStep03State,
} from "./schemas-v1.js";
import {
  requireValueNotPreservedReferenceScriptV1,
  requireValueNotPreservedStepStateV1,
  requireValueNotPreservedThreadUtxoV1,
  valueNotPreservedStepLabelV1,
  valueNotPreservedSubmitError,
} from "./submit-common-v1.js";

const STEP_LABEL = valueNotPreservedStepLabelV1(1);

export type SubmitValueNotPreservedStep02FoldResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  /** The advanced fold state the thread now carries. */
  readonly foldState: ValueNotPreservedStep02State;
  /** The folded input's claimed-asset contribution. */
  readonly claimedQuantity: bigint;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValueNotPreservedStep02Fold = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  spendInputsOpening,
  valueWitness,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ValueNotPreservedContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The §8.8 field-0 opening (see `spendInputsOpeningV1`). */
  readonly spendInputsOpening: FieldOpeningV1;
  /** The pre-state value witness for the input at the thread's cursor. */
  readonly valueWitness: SpentInputValueWitnessV1;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValueNotPreservedStep02FoldResult> => {
  const { threadUtxo, threadToken } =
    await requireValueNotPreservedThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const state: ValueNotPreservedStep02State =
    requireValueNotPreservedStepStateV1({
      threadUtxo,
      signer,
      schema: ValueNotPreservedStep02Datum,
      stepIndex: 1,
    });

  // The validator's own fold, run locally: the descriptor's claimed-asset
  // quantity, ADA from the descriptor scalar or tokens from the full
  // authenticated leaf walk.
  const claimedQuantity = witnessClaimedQuantityV1({
    claim: state.claimed_asset,
    witness: valueWitness,
  });
  const nextState: ValueNotPreservedStep02State = {
    ...state,
    input_cursor: state.input_cursor + 1n,
    claimed_delta: state.claimed_delta + claimedQuantity,
  };

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState },
    ValueNotPreservedStep02Datum,
  );
  // The self-loop pays back to step-02's own address.
  const nextOutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
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
        nextOutputMatches,
        `${STEP_LABEL} self-loop output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            FoldInput: [
              {
                input_index: layout.inputIndex,
                output_index: layout.outputIndex,
                spend_inputs_opening: spendInputsOpening,
                value_witness: valueWitness,
              },
            ],
          },
        ],
      },
      ValueNotPreservedStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };
  const stepReference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireValueNotPreservedReferenceScriptV1({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[1].spendingScriptHash,
          stepIndex: 1,
        });
  const stepCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.steps[1].spendingScript,
    referenceUtxo: stepReference,
    label: `${STEP_LABEL} fold spending validator`,
  });

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .readFrom([...stepCarriage.referenceInputs]);
  const tx = stepCarriage.attach(base);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw valueNotPreservedSubmitError(
      "BuildTxWithRedeemer did not resolve the step-02 fold layout.",
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
    foldState: nextState,
    claimedQuantity,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValueNotPreservedStep02FinishResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  /** The completed-inflow state the thread now carries. */
  readonly outflowState: ValueNotPreservedStep03State;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValueNotPreservedStep02Finish = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  spendInputsOpening,
  spendInputCount,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ValueNotPreservedContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The §8.8 field-0 opening; the validator reads the item count off it. */
  readonly spendInputsOpening: FieldOpeningV1;
  /** The transaction's spend-input count, for the local cursor check. */
  readonly spendInputCount: bigint;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValueNotPreservedStep02FinishResult> => {
  const { threadUtxo, threadToken } =
    await requireValueNotPreservedThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const state: ValueNotPreservedStep02State =
    requireValueNotPreservedStepStateV1({
      threadUtxo,
      signer,
      schema: ValueNotPreservedStep02Datum,
      stepIndex: 1,
    });
  // A premature finish would hide inflow; the validator compares against the
  // authenticated item count, so refuse the doomed transaction here.
  if (state.input_cursor !== spendInputCount) {
    throw valueNotPreservedSubmitError(
      `the fold has folded ${state.input_cursor.toString()} of ${spendInputCount.toString()} spend inputs; finish only after the last one.`,
    );
  }

  const outflowState: ValueNotPreservedStep03State = {
    bad_tx_id: state.bad_tx_id,
    claimed_asset: state.claimed_asset,
    claimed_direction: state.claimed_direction,
    committed_fee: state.committed_fee,
    claimed_delta: state.claimed_delta,
  };

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: outflowState },
    ValueNotPreservedStep03Datum,
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
            FinishInputs: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              spend_inputs_opening: spendInputsOpening,
            },
          },
        ],
      },
      ValueNotPreservedStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };
  const stepReference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireValueNotPreservedReferenceScriptV1({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[1].spendingScriptHash,
          stepIndex: 1,
        });
  const stepCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.steps[1].spendingScript,
    referenceUtxo: stepReference,
    label: `${STEP_LABEL} finish spending validator`,
  });

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .readFrom([...stepCarriage.referenceInputs]);
  const tx = stepCarriage.attach(base);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw valueNotPreservedSubmitError(
      "BuildTxWithRedeemer did not resolve the step-02 finish layout.",
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
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    outflowState,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
