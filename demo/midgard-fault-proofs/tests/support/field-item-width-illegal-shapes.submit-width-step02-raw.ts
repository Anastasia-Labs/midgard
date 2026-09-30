import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  type FieldOpening,
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

import type { FieldItemWidthIllegalContracts } from "../../src/field-item-width-illegal/contracts.js";
import type { FieldItemWidthEvidence } from "../../src/field-item-width-illegal/field-item-width-illegal.js";
import {
  FieldItemWidthStep02DatumSchema,
  FieldItemWidthStep02RedeemerSchema,
  FieldItemWidthStep03DatumSchema,
  FieldItemWidthStep03RedeemerSchema,
} from "../../src/field-item-width-illegal/schemas.js";
import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";

// ---------------------------------------------------------------------------
// Raw submitters
// ---------------------------------------------------------------------------

/**
 * Step 02 with the opening exposed. Carriage is published and certified the
 * way the production builder does it; `mutateOpening` then rewrites the
 * redeemer's opening after every off-chain check has passed, and
 * `nextStepIndex` names a successor other than the one the validator was
 * applied with. Both reach the validator, which is the point.
 */
export const submitWidthStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  referenceScriptUtxo,
  certificateReferenceScriptUtxo,
  mutateOpening = (opening) => opening,
  nextStepIndex = 2,
  publishedCarriageUtxos,
  certificateUtxo: suppliedCertificateUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FieldItemWidthIllegalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: FieldItemWidthEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly mutateOpening?: (
    opening: FieldOpening,
    referenceInputs: readonly UTxO[],
  ) => FieldOpening;
  readonly nextStepIndex?: 1 | 2;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "field-item-width-illegal",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    field_index: bigint;
    item_index: bigint;
  }>({
    threadUtxo,
    signer,
    schema: FieldItemWidthStep02DatumSchema as never,
    family: "field-item-width-illegal",
    stepIndex,
  });
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: evidence.fieldIndex,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: false,
    label: "field-item-width-illegal raw field opening",
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "field-item-width-illegal raw field opening",
    }));
  const certificateUtxo =
    suppliedCertificateUtxo ??
    (planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriage({
            lucid,
            network: lucid.config().network!,
            signer,
            planned,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo:
              certificateReferenceScriptUtxo ??
              (() => {
                throw new Error(
                  "field-item-width-illegal raw: certified opening requires certificate reference script",
                );
              })(),
            chunkUtxos: carriageUtxos,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
          })
        ).certificateUtxo
      : undefined);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "field-item-width-illegal",
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
      label: "field-item-width-illegal raw field opening",
    }),
    referenceInputs,
  );
  // The successor datum carries what the thread bound, not what the evidence
  // says: a coordinate mutation is bound at step 01 and must be refused here
  // by the door, not masked by an off-chain equality check.
  const item = items[Number(state.item_index)];
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        field_index: state.field_index,
        item_index: state.item_index,
        item_width: BigInt(item?.length ?? 0),
      },
    } as never,
    FieldItemWidthStep03DatumSchema as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "field-item-width-illegal raw step-02",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "field-item-width-illegal raw",
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "field-item-width-illegal raw step-02 output",
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, opening },
        ],
      } as never,
      FieldItemWidthStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: "field-item-width-illegal raw step-02",
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    awaitConfirmation: true,
  });
};

/**
 * Step 03 without the off-chain `fieldItemWidthEvidenceCloses` guard, so an
 * honest verdict reaches `terminal_contradiction_v1` and is refused there.
 */
export const submitWidthStep03Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FieldItemWidthIllegalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "field-item-width-illegal",
    stepIndex,
    threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: "field-item-width-illegal",
    stepIndex,
    step: contracts.steps[2],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: FieldItemWidthStep03RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};
