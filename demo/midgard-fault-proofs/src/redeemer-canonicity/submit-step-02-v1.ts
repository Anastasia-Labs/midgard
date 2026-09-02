import {
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  type FieldOpeningV1,
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
  certifyFaultProofFieldCarriageV1,
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { RedeemerCanonicityContractsV1 } from "./contracts-v1.js";
import {
  REDEEMER_CANONICITY_FIELD_INDEX_V1,
  type RedeemerCanonicityEvidenceV1,
} from "./family-v1.js";
import {
  RedeemerCanonicityStep02DatumV1Schema,
  RedeemerCanonicityStep02RedeemerV1Schema,
  RedeemerCanonicityStep03DatumV1Schema,
} from "./schemas-v1.js";

export const submitRedeemerCanonicityStep02V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo: suppliedCertificateUtxo,
  certificateReferenceScriptUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  certificatePreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: RedeemerCanonicityContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: RedeemerCanonicityEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "redeemer-canonicity",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    witness_set_hash: string;
    redeemer_index: bigint;
  }>({
    threadUtxo,
    signer,
    schema: RedeemerCanonicityStep02DatumV1Schema as never,
    family: "redeemer-canonicity",
    stepIndex,
  });
  if (state.redeemer_index !== BigInt(evidence.redeemerIndex))
    throw new Error(
      "redeemer-canonicity: opening coordinate differs from thread",
    );
  const items = decodeMidgardFieldPreimageV1(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const witnessSet = decodeMidgardNativeTxWitnessSetCompactV1(
    Buffer.from(witnessSetCompactCbor, "hex"),
  );
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: REDEEMER_CANONICITY_FIELD_INDEX_V1,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet: {
      addr_tx_wits_hash: Buffer.from(witnessSet.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(witnessSet.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(
        witnessSet.redeemerTxWitsHash,
      ).toString("hex"),
    },
    anchorWitnessSetHash: state.witness_set_hash,
    label: "redeemer-canonicity field opening",
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "redeemer-canonicity field opening",
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const certificateUtxo =
    suppliedCertificateUtxo ??
    (planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriageV1({
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
                  "redeemer-canonicity: certified opening requires certificate reference script",
                );
              })(),
            chunkUtxos: carriageUtxos,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
            preSubmitBoundary: certificatePreSubmitBoundary,
          })
        ).certificateUtxo
      : undefined);
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "redeemer-canonicity",
    stepIndex,
  });
  const opening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "redeemer-canonicity field opening",
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bound: {
          subject: evidence.subject,
          witness_set_hash: state.witness_set_hash,
          redeemer_index: BigInt(evidence.redeemerIndex),
        },
        canonical: evidence.canonical,
      },
    } as never,
    RedeemerCanonicityStep03DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "redeemer-canonicity step-02");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "redeemer-canonicity",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "redeemer-canonicity step-02 output",
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, opening },
        ],
      } as never,
      RedeemerCanonicityStep02RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: "redeemer-canonicity step-02",
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("redeemer-canonicity: step-02 layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    carriageTier: planned.plan.tier,
  };
};
