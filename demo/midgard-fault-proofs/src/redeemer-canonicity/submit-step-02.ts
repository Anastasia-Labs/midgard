import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
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

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { RedeemerCanonicityContracts } from "./contracts.js";
import {
  REDEEMER_CANONICITY_FIELD_INDEX,
  type RedeemerCanonicityEvidence,
} from "./family.js";
import {
  RedeemerCanonicityStep02DatumSchema,
  RedeemerCanonicityStep02RedeemerSchema,
  RedeemerCanonicityStep03DatumSchema,
} from "./schemas.js";

export const submitRedeemerCanonicityStep02 = async ({
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
  readonly contracts: RedeemerCanonicityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: RedeemerCanonicityEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "redeemer-canonicity",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    witness_set_hash: string;
    redeemer_index: bigint;
  }>({
    threadUtxo,
    signer,
    schema: RedeemerCanonicityStep02DatumSchema as never,
    family: "redeemer-canonicity",
    stepIndex,
  });
  if (state.redeemer_index !== BigInt(evidence.redeemerIndex))
    throw new Error(
      "redeemer-canonicity: opening coordinate differs from thread",
    );
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(witnessSetCompactCbor, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    fieldIndex: REDEEMER_CANONICITY_FIELD_INDEX,
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
    (await publishFaultProofFieldCarriage({
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
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "redeemer-canonicity",
    stepIndex,
  });
  const opening: FieldOpening = faultProofFieldOpening({
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
    RedeemerCanonicityStep03DatumSchema as never,
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
      RedeemerCanonicityStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
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
