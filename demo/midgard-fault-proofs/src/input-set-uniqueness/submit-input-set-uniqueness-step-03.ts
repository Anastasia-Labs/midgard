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

import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import {
  faultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { InputSetUniquenessContracts } from "./contracts.js";
import {
  requireInputSetUniquenessReferenceScript,
  requireInputSetUniquenessStepState,
  requireInputSetUniquenessThreadUtxo,
} from "./submit-common.js";
import {
  inputSetUniquenessCheckpoint,
  type InputSetUniqueScanState,
} from "./wrongful-rejection.js";

const uniqueUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const seen = new Set<string>();
  return utxos.filter((utxo) => {
    const key = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
};

export const buildInputSetUniquenessStep03 = async ({
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
  ownOutputs = createOwnOutputJournal(),
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
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
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
      anchorSourceKind: bound.subject.source_kind === 1n ? 1n : 0n,
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
  ) => {
    const publications =
      supplied ??
      (await publishFaultProofFieldCarriage({
        ownOutputs,
        lucid,
        signer,
        planned,
        publisherAddress: signer.proofMaterialsAddress,
        label: "input-set-uniqueness step-03 field",
      }));
    if (isPrerequisiteTurn(publications)) return publications;
    return [
      ...publications,
      ...(certificate === undefined ? [] : [certificate]),
    ];
  };
  const spendCarriage = await resolve(
    spendPlan,
    publishedSpendCarriageUtxos,
    spendCertificateUtxo,
  );
  if (isPrerequisiteTurn(spendCarriage)) return spendCarriage;
  const referenceCarriage = await resolve(
    referencePlan,
    publishedReferenceCarriageUtxos,
    referenceCertificateUtxo,
  );
  if (isPrerequisiteTurn(referenceCarriage)) return referenceCarriage;
  const carriageUtxos = uniqueUtxos([...spendCarriage, ...referenceCarriage]);
  // The accused pair is not checked here: in either orientation the complete
  // scan answers it, and a pair naming no duplicate finalizes at cursor 0.
  const spendCount = BigInt(spendInputItemCbors.length);
  const referenceCount = BigInt(referenceInputItemCbors.length);
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
              referenceInputs: ctx.referenceInputs,
              certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
              label: "input-set-uniqueness step-03 spend",
            }),
            reference_inputs_carriage: faultProofFieldCarriage({
              planned: referencePlan,
              referenceInputs: ctx.referenceInputs,
              certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
              label: "input-set-uniqueness step-03 reference",
            }),
          },
        ],
      } as never,
      InputSetUniquenessStep03SpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: "input-set-uniqueness step-03",
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
    state: scanState,
  });
};
