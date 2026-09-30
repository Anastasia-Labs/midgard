import {
  type FieldOpening,
  missingSignatureFieldWalkCheckpoint,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../src/linear-fault-family.js";
import { submitLinearFaultContinue } from "../src/linear-fault-submit.js";
import { planSpendInputSignerWitnessOpening } from "../src/spend-input-signer-missing/field-plans.js";
import { type SpendInputSignerMissingEvidence } from "../src/spend-input-signer-missing/index.js";
import {
  SpendInputSignerStep03RedeemerSchema,
  SpendInputSignerStep04DatumSchema,
} from "../src/spend-input-signer-missing/schemas.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import {
  FAMILY,
  type Family,
  type Harness,
  network,
} from "./spend-input-signer-missing-lifecycle.registered-contracts.js";

/**
 * Step 03 over `threadOutRef` with a certificate and chunks honestly minted
 * for `foreign`'s witness field, carried under the thread's own compact
 * structure and witness set. Everything the family builder does is done here
 * with the same primitives; only the carriage is another transaction's.
 */
export const submitStep03WithForeignCertificate = async ({
  harness,
  family,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  foreign,
  referenceScriptUtxo,
  certificateReference,
}: {
  readonly harness: Harness;
  readonly family: Family;
  readonly threadOutRef: string;
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly foreign: {
    readonly evidence: SpendInputSignerMissingEvidence;
    readonly nativeTxCompactCbor: string;
    readonly witnessSetCompactCbor: string;
  };
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReference: UTxO;
}) => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const { contracts, category } = family;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId: category.categoryId,
    family: FAMILY,
    stepIndex: 2,
    threadOutRef,
  });
  const own = planSpendInputSignerWitnessOpening({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
  });
  const planned = planSpendInputSignerWitnessOpening({
    evidence: foreign.evidence,
    nativeTxCompactCbor: foreign.nativeTxCompactCbor,
    witnessSetCompactCbor: foreign.witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
  });
  expect(planned.plan.tier).toBe("Certified");
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "foreign address witnesses",
  });
  const { certificateUtxo } = await certifyFaultProofFieldCarriage({
    lucid,
    network,
    signer,
    planned,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    certificateMintingScript: contracts.fieldPreimageCertificateMintingScript,
    certificateReferenceScriptUtxo: certificateReference,
    chunkUtxos: carriageUtxos,
    compactCbor: foreign.nativeTxCompactCbor,
    witnessSetCompactCbor: foreign.witnessSetCompactCbor,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: FAMILY,
    stepIndex: 2,
  });
  const referenceInputs = [...carriageUtxos, certificateUtxo, stepReference];
  const foreignOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "foreign address witnesses",
  });
  if (
    !("WitnessFieldOpening" in foreignOpening) ||
    own.witnessSet === undefined
  )
    throw new Error("witness field openings expected");
  const opening: FieldOpening = {
    WitnessFieldOpening: {
      ...foreignOpening.WitnessFieldOpening,
      native_tx_compact_cbor: own.nativeTxCompactCbor,
      witness_set: own.witnessSet,
    },
  };
  const initial = missingSignatureFieldWalkCheckpoint({
    txId: evidence.subject.transaction_id,
    itemCount: own.itemCount,
    totalLength: own.preimage.length,
    nextItemIndex: 0,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        authenticated: {
          subject: evidence.subject,
          transaction_id: evidence.subject.transaction_id,
          witness_set_hash: evidence.witnessSetHashHex,
          payment_credential: evidence.paymentCredentialHex,
        },
        checkpoint_hash: initial.checkpointHash,
      },
    } as never,
    SpendInputSignerStep04DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "foreign-certificate step-03");
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "foreign-certificate step-03",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "foreign-certificate step-03 output",
            ),
            witnesses_opening: opening,
          },
        ],
      } as never,
      SpendInputSignerStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: "foreign-certificate step-03",
    nextAddress: contracts.steps[3].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs: [certificateUtxo],
    awaitConfirmation: true,
  });
};
