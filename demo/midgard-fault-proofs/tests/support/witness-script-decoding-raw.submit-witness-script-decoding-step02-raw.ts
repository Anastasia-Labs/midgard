import {
  decodeMidgardNativeTxCompact,
  encodeMidgardForcedTxCompact,
} from "@al-ft/midgard-core";
import {
  type FieldOpening,
  type ForcedInclusionTxV1,
  type Header,
  MIDGARD_FIELD_INDEX,
  type OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type RootMembershipProof,
  type WitnessScriptDecodingBound,
  type WitnessScriptDecodingScanState,
  WitnessScriptDecodingStep01RedeemerSchema,
  WitnessScriptDecodingStep02DatumSchema,
  WitnessScriptDecodingStep02RedeemerSchema,
  WitnessScriptDecodingStep03DatumSchema,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../../src/field-opening.js";
import {
  requireLinearFaultInitialDatum,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { WitnessScriptDecodingContracts } from "../../src/witness-script-decoding/contracts.js";
import {
  FAMILY,
  type WitnessSetCarriage,
} from "./witness-script-decoding-raw.over-bound-field-carriage-plan.js";

// ---------------------------------------------------------------------------
// Raw submitters
// ---------------------------------------------------------------------------

/**
 * Forced step 01 with every bound value exposed: the header, membership,
 * direction, witness-set hash, coordinate, accused class and successor are
 * carried exactly as given, so a substitution at any of them is refused by
 * the applied validator.
 */
export const submitWitnessScriptDecodingStep01ForcedRaw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  direction,
  subject,
  witnessSetHash,
  scriptIndex,
  accusedClass,
  referenceScriptUtxo,
  nextStepIndex = 1,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly header: Header;
  readonly membership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  >;
  readonly direction: bigint;
  readonly subject: WitnessScriptDecodingBound["subject"];
  readonly witnessSetHash: string;
  readonly scriptIndex: bigint;
  readonly accusedClass: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly nextStepIndex?: 0 | 1 | 2 | 3;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  const bound: WitnessScriptDecodingBound = {
    subject,
    witness_set_hash: witnessSetHash,
    script_index: scriptIndex,
    accused_class: accusedClass,
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: bound } as never,
    WitnessScriptDecodingStep02DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex: 0,
  });
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw forced step 01`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} raw forced step 01`,
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: inputIndex,
                output_index: outputIndex,
                header,
                membership,
                direction,
              },
            },
            script_index: scriptIndex,
          },
        ],
      } as never,
      WitnessScriptDecodingStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: `${FAMILY} raw forced step 01`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${FAMILY}: raw layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/**
 * Step 02 without the off-chain bound-subject guard and with the opening and
 * successor state exposed. Carriage must already be published (and certified
 * when the tier requires it); `mutateOpening` rewrites the redeemer's opening
 * after the off-chain planner has passed, and `nextState` replaces the exact
 * successor the validator recomputes.
 */
export const submitWitnessScriptDecodingStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  anchorTxId,
  anchorWitnessSetHash,
  carriage,
  scriptWitnessItems,
  carriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  nextState,
  mutateOpening = (opening) => opening,
  nextStepIndex = 2,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly anchorTxId: string;
  readonly anchorWitnessSetHash: string;
  readonly carriage: WitnessSetCarriage;
  readonly scriptWitnessItems: readonly Uint8Array[];
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly nextState: WitnessScriptDecodingScanState;
  readonly mutateOpening?: (
    opening: FieldOpening,
    referenceInputs: readonly UTxO[],
  ) => FieldOpening;
  readonly nextStepIndex?: 0 | 1 | 2 | 3;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepState<WitnessScriptDecodingBound>({
    threadUtxo,
    signer,
    schema: WitnessScriptDecodingStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: bound.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId,
    nativeTxCompactCbor:
      bound.subject.source_kind === 1n
        ? encodeMidgardForcedTxCompact(
            decodeMidgardNativeTxCompact(
              Buffer.from(carriage.compactCbor, "hex"),
            ),
          ).toString("hex")
        : carriage.compactCbor,
    itemCbors: scriptWitnessItems,
    owner: signer.paymentKeyHash,
    publish: carriageUtxos.length > 0,
    witnessSet: carriage.witnessSet,
    anchorWitnessSetHash,
    label: `${FAMILY} raw step 02 field 6`,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const referenceInputs = [
    ...carriageUtxos,
    stepReference,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
  ];
  const opening = mutateOpening(
    faultProofFieldOpening({
      planned,
      referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${FAMILY} raw step 02 field 6`,
    }),
    referenceInputs,
  );
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    WitnessScriptDecodingStep03DatumSchema as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw step 02`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} raw step 02`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, opening },
        ],
      } as never,
      WitnessScriptDecodingStep02RedeemerSchema as never,
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
    stepRole: `${FAMILY} raw step 02`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${FAMILY}: raw layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
