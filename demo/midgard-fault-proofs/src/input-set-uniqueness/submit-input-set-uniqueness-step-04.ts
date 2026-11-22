import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import {
  InputSetUniquenessStep04DatumSchema,
  InputSetUniquenessStep04SpendRedeemerSchema,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import { buildLinearFaultFinalize } from "../linear-fault-finalize.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { ProofMaterialsAccess } from "../workflow/proof-materials-view.js";
import type { InputSetUniquenessContracts } from "./contracts.js";
import {
  requireInputSetUniquenessReferenceScript,
  requireInputSetUniquenessStepState,
  requireInputSetUniquenessThreadUtxo,
} from "./submit-common.js";
import {
  inputSetPairNamesNoDuplicate,
  inputSetUnionIsStrictlyIncreasing,
  inputSetUniquenessCheckpoint,
  type InputSetUniqueScanState,
} from "./wrongful-rejection.js";

const requireAuthenticState = (state: InputSetUniqueScanState) => {
  const expected = inputSetUniquenessCheckpoint({
    bound: state.bound,
    spendCount: state.spend_count,
    referenceCount: state.reference_count,
    cursor: state.cursor,
    previousItem: state.previous_item,
    nextExpectedScriptHash: state.next_expected_script_hash,
  });
  if (expected !== state.checkpoint_hash) {
    throw new Error("input-set-uniqueness: checkpoint changed");
  }
};

export const buildInputSetUniquenessStep04Advance = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  spendInputItemCbors,
  referenceInputItemCbors,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  materials,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: InputSetUniquenessContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}) => {
  const stepIndex = 3 as const;
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId,
      stepIndex,
      threadOutRef,
    },
  );
  const state = requireInputSetUniquenessStepState<InputSetUniqueScanState>({
    threadUtxo,
    signer,
    schema: InputSetUniquenessStep04DatumSchema as never,
    stepIndex,
  });
  requireAuthenticState(state);
  if (
    state.next_expected_script_hash !== contracts.steps[3].spendingScriptHash
  ) {
    throw new Error("input-set-uniqueness: wrong next expected script");
  }
  if (
    state.spend_count !== BigInt(spendInputItemCbors.length) ||
    state.reference_count !== BigInt(referenceInputItemCbors.length)
  ) {
    throw new Error("input-set-uniqueness: input counts changed");
  }
  const readingSpend = state.cursor < state.spend_count;
  const items = readingSpend ? spendInputItemCbors : referenceInputItemCbors;
  const itemIndex = readingSpend
    ? state.cursor
    : state.cursor - state.spend_count;
  const batch = items.slice(Number(itemIndex), Number(itemIndex) + 128);
  if (batch.length === 0)
    throw new Error("input-set-uniqueness: scan is complete");
  let previousItem = state.previous_item;
  for (const item of batch) {
    if (previousItem !== "" && previousItem >= item) {
      throw new Error("input-set-uniqueness: union is not strictly increasing");
    }
    previousItem = item;
  }
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: state.bound.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: readingSpend
      ? MIDGARD_FIELD_INDEX.spendInputs
      : MIDGARD_FIELD_INDEX.referenceInputs,
    anchorTxId: state.bound.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items.map((value) => Buffer.from(value, "hex")),
    owner: signer.paymentKeyHash,
    label: "input-set-uniqueness step-04",
  });
  const publications =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      materials,
      lucid,
      signer,
      planned,
      publisherAddress: signer.proofMaterialsAddress,
      label: "input-set-uniqueness step-04 field",
    }));
  if (isPrerequisiteTurn(publications)) return publications;
  const carriageUtxos = [
    ...publications,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
  ];
  const nextPartial = {
    ...state,
    cursor: state.cursor + BigInt(batch.length),
    previous_item: previousItem,
  };
  const nextState: InputSetUniqueScanState = {
    ...nextPartial,
    checkpoint_hash: inputSetUniquenessCheckpoint({
      bound: state.bound,
      spendCount: state.spend_count,
      referenceCount: state.reference_count,
      cursor: nextPartial.cursor,
      previousItem,
      nextExpectedScriptHash: contracts.steps[3].spendingScriptHash,
    }),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    InputSetUniquenessStep04DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireInputSetUniquenessReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "input-set-uniqueness step-04");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "input-set-uniqueness step-04",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "input-set-uniqueness step-04",
    );
    return Data.to(
      {
        Continue: [
          {
            Advance: {
              input_index: inputIndex,
              output_index: outputIndex,
              field_opening: faultProofFieldOpening({
                planned,
                referenceInputs: ctx.referenceInputs,
                certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
                label: "input-set-uniqueness step-04",
              }),
            },
          },
        ],
      } as never,
      InputSetUniquenessStep04SpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: "input-set-uniqueness step-04",
    nextAddress: contracts.steps[3].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
  });
  if (outputIndex === undefined)
    throw new Error("input-set-uniqueness: unresolved layout");
  return Object.freeze({
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
    state: nextState,
  });
};

export const buildInputSetUniquenessStep04Finalize = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  spendInputItemCbors,
  referenceInputItemCbors,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: InputSetUniquenessContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 3 as const;
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId,
      stepIndex,
      threadOutRef,
    },
  );
  const state = requireInputSetUniquenessStepState<InputSetUniqueScanState>({
    threadUtxo,
    signer,
    schema: InputSetUniquenessStep04DatumSchema as never,
    stepIndex,
  });
  requireAuthenticState(state);
  const pairNamesNoDuplicate = inputSetPairNamesNoDuplicate({
    bound: state.bound,
    spendCount: state.spend_count,
    referenceCount: state.reference_count,
  });
  if (
    !pairNamesNoDuplicate &&
    (state.cursor !== state.spend_count + state.reference_count ||
      !inputSetUnionIsStrictlyIncreasing({
        spendInputItemCbors,
        referenceInputItemCbors,
      }))
  ) {
    throw new Error("input-set-uniqueness: complete unique scan is absent");
  }
  return await buildLinearFaultFinalize({
    lucid,
    family: "input-set-uniqueness",
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: InputSetUniquenessStep04SpendRedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      Finalize: {
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      },
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
  });
};
