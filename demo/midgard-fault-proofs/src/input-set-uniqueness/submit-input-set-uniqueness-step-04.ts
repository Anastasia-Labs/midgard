import {
  InputSetUniquenessStep04DatumSchema,
  InputSetUniquenessStep04SpendRedeemerSchema,
  MIDGARD_FIELD_INDEX_V1,
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
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { InputSetUniquenessContractsV1 } from "./contracts-v1.js";
import {
  requireInputSetUniquenessReferenceScriptV1,
  requireInputSetUniquenessStepStateV1,
  requireInputSetUniquenessThreadUtxoV1,
} from "./submit-common-v1.js";
import {
  inputSetUnionIsStrictlyIncreasingV1,
  inputSetUniquenessCheckpointV1,
  type InputSetUniqueScanStateV1,
} from "./wrongful-rejection-v1.js";

const requireAuthenticState = (state: InputSetUniqueScanStateV1) => {
  const expected = inputSetUniquenessCheckpointV1({
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

export const submitInputSetUniquenessStep04AdvanceV1 = async ({
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
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: InputSetUniquenessContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3 as const;
  const { threadUtxo, threadToken } =
    await requireInputSetUniquenessThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex,
      threadOutRef,
    });
  const state = requireInputSetUniquenessStepStateV1<InputSetUniqueScanStateV1>(
    {
      threadUtxo,
      signer,
      schema: InputSetUniquenessStep04DatumSchema as never,
      stepIndex,
    },
  );
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
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: readingSpend
      ? MIDGARD_FIELD_INDEX_V1.spendInputs
      : MIDGARD_FIELD_INDEX_V1.referenceInputs,
    anchorTxId: state.bound.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items.map((value) => Buffer.from(value, "hex")),
    owner: signer.paymentKeyHash,
    label: "input-set-uniqueness step-04",
  });
  const carriageUtxos = [
    ...(publishedCarriageUtxos ??
      (await publishFaultProofFieldCarriageV1({
        lucid,
        signer,
        planned,
        publisherAddress: signer.address,
        label: "input-set-uniqueness step-04 field",
      }))),
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
  ];
  const nextPartial = {
    ...state,
    cursor: state.cursor + BigInt(batch.length),
    previous_item: previousItem,
  };
  const nextState: InputSetUniqueScanStateV1 = {
    ...nextPartial,
    checkpoint_hash: inputSetUniquenessCheckpointV1({
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
  const stepReference = requireInputSetUniquenessReferenceScriptV1({
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
              field_opening: faultProofFieldOpeningV1({
                planned,
                referenceInputs: [...carriageUtxos, stepReference],
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
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: "input-set-uniqueness step-04",
    nextAddress: contracts.steps[3].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("input-set-uniqueness: unresolved layout");
  return Object.freeze({
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    state: nextState,
  });
};

export const submitInputSetUniquenessStep04FinalizeV1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  spendInputItemCbors,
  referenceInputItemCbors,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: InputSetUniquenessContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3 as const;
  const { threadUtxo, threadToken } =
    await requireInputSetUniquenessThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex,
      threadOutRef,
    });
  const state = requireInputSetUniquenessStepStateV1<InputSetUniqueScanStateV1>(
    {
      threadUtxo,
      signer,
      schema: InputSetUniquenessStep04DatumSchema as never,
      stepIndex,
    },
  );
  requireAuthenticState(state);
  if (
    state.cursor !== state.spend_count + state.reference_count ||
    !inputSetUnionIsStrictlyIncreasingV1({
      spendInputItemCbors,
      referenceInputItemCbors,
    })
  ) {
    throw new Error("input-set-uniqueness: complete unique scan is absent");
  }
  return await submitLinearFaultFinalizeV1({
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
    preSubmitBoundary,
    awaitConfirmation,
  });
};
