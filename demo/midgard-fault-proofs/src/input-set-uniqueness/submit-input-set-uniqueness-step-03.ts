import {
  InputSetUniquenessStep03DatumSchema,
  InputSetUniquenessStep03SpendRedeemerSchema,
  type InputSetUniquenessStep03State,
  InputSetUniquenessStep04DatumSchema,
  MIDGARD_FIELD_INDEX,
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
  faultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { InputSetUniquenessContracts } from "./contracts-v1.js";
import {
  requireInputSetUniquenessReferenceScript,
  requireInputSetUniquenessStepState,
  requireInputSetUniquenessThreadUtxo,
} from "./submit-common-v1.js";
import {
  inputSetUniquenessCheckpoint,
  type InputSetUniqueScanState,
} from "./wrongful-rejection-v1.js";

const uniqueUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const seen = new Set<string>();
  return utxos.filter((utxo) => {
    const key = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
};

export const submitInputSetUniquenessStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  spendInputItemCbors,
  referenceInputItemCbors,
  publishedSpendCarriageUtxos,
  publishedReferenceCarriageUtxos,
  spendCertificateUtxo,
  referenceCertificateUtxo,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: InputSetUniquenessContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
  readonly publishedSpendCarriageUtxos?: readonly UTxO[];
  readonly publishedReferenceCarriageUtxos?: readonly UTxO[];
  readonly spendCertificateUtxo?: UTxO;
  readonly referenceCertificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2 as const;
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId,
      stepIndex,
      threadOutRef,
    },
  );
  const { bound } =
    requireInputSetUniquenessStepState<InputSetUniquenessStep03State>({
      threadUtxo,
      signer,
      schema: InputSetUniquenessStep03DatumSchema as never,
      stepIndex,
    });
  const plan = (fieldIndex: number, items: readonly string[]) =>
    planFaultProofFieldOpening({
      fieldIndex,
      anchorTxId: bound.subject.transaction_id,
      nativeTxCompactCbor,
      itemCbors: items.map((item) => Buffer.from(item, "hex")),
      owner: signer.paymentKeyHash,
      label: "input-set-uniqueness step-03",
    });
  const spendPlan = plan(MIDGARD_FIELD_INDEX.spendInputs, spendInputItemCbors);
  const referencePlan = plan(
    MIDGARD_FIELD_INDEX.referenceInputs,
    referenceInputItemCbors,
  );
  const resolve = async (
    planned: typeof spendPlan,
    supplied: readonly UTxO[] | undefined,
    certificate: UTxO | undefined,
  ) => [
    ...(supplied ??
      (await publishFaultProofFieldCarriage({
        lucid,
        signer,
        planned,
        publisherAddress: signer.address,
        label: "input-set-uniqueness step-03 field",
      }))),
    ...(certificate === undefined ? [] : [certificate]),
  ];
  const carriageUtxos = uniqueUtxos([
    ...(await resolve(
      spendPlan,
      publishedSpendCarriageUtxos,
      spendCertificateUtxo,
    )),
    ...(await resolve(
      referencePlan,
      publishedReferenceCarriageUtxos,
      referenceCertificateUtxo,
    )),
  ]);
  const spendCount = BigInt(spendInputItemCbors.length);
  const referenceCount = BigInt(referenceInputItemCbors.length);
  const inRange = (field: bigint, item: bigint) =>
    item >= 0n &&
    (field === 0n
      ? item < spendCount
      : field === 1n
        ? item < referenceCount
        : false);
  if (
    !inRange(bound.first_field_index, bound.first_item_index) ||
    !inRange(bound.second_field_index, bound.second_item_index) ||
    bound.first_field_index > bound.second_field_index ||
    (bound.first_field_index === bound.second_field_index &&
      bound.first_item_index >= bound.second_item_index)
  ) {
    throw new Error(
      "input-set-uniqueness: accused coordinates are non-canonical",
    );
  }
  const partial = {
    bound,
    spend_count: spendCount,
    reference_count: referenceCount,
    cursor: 0n,
    previous_item: "",
    next_expected_script_hash: contracts.steps[3].spendingScriptHash,
  };
  const scanState: InputSetUniqueScanState = {
    ...partial,
    checkpoint_hash: inputSetUniquenessCheckpoint({
      bound,
      spendCount,
      referenceCount,
      cursor: 0n,
      previousItem: "",
      nextExpectedScriptHash: contracts.steps[3].spendingScriptHash,
    }),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: scanState } as never,
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
    requireOwnSpendPurpose(ctx, threadUtxo, "input-set-uniqueness step-03");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "input-set-uniqueness step-03",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "input-set-uniqueness step-03",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            native_tx_compact_cbor: nativeTxCompactCbor,
            spend_inputs_carriage: faultProofFieldCarriage({
              planned: spendPlan,
              referenceInputs: [...carriageUtxos, stepReference],
              certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
              label: "input-set-uniqueness step-03 spend",
            }),
            reference_inputs_carriage: faultProofFieldCarriage({
              planned: referencePlan,
              referenceInputs: [...carriageUtxos, stepReference],
              certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
              label: "input-set-uniqueness step-03 reference",
            }),
          },
        ],
      } as never,
      InputSetUniquenessStep03SpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: "input-set-uniqueness step-03",
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
    state: scanState,
  });
};
