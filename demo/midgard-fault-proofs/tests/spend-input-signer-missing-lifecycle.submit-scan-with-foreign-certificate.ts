import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import {
  type FieldOpening,
  missingSignatureFieldWalkCheckpoint,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { faultProofFieldOpening } from "../src/field-opening.publish-fault-proof-field-carriage.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../src/linear-fault-family.js";
import { buildLinearFaultContinue } from "../src/linear-fault-submit.js";
import { planSignerWitnessOpening } from "../src/signer-witness-scan/field-plans.js";
import {
  SignerWitnessScanDatumSchema,
  SignerWitnessScanRedeemerSchema,
} from "../src/signer-witness-scan/schemas.js";
import { SIGNER_WITNESS_SCAN_BATCH } from "../src/signer-witness-scan/signer-witness-scan.js";
import { type SpendInputSignerMissingEvidence } from "../src/spend-input-signer-missing/index.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import {
  FAMILY,
  type Family,
  type Harness,
  network,
} from "./spend-input-signer-missing-lifecycle.registered-contracts.js";
import {
  buildAndSubmitTransactionUnjournaled,
  certifyCarriageUnjournaled,
  publishCarriageUnjournaled,
} from "./support/submit-unjournaled.js";

/**
 * The opening scan turn over `threadOutRef` with a certificate and chunks
 * honestly minted for `foreign`'s witness field, carried under the thread's
 * own compact structure and witness set. Everything the shared scan builder
 * does is done here with the same primitives; only the carriage is another
 * transaction's.
 */
export const submitScanWithForeignCertificate = async ({
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
  const own = planSignerWitnessOpening({
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
    label: "own address witnesses",
  });
  const planned = planSignerWitnessOpening({
    evidence: foreign.evidence,
    nativeTxCompactCbor: foreign.nativeTxCompactCbor,
    witnessSetCompactCbor: foreign.witnessSetCompactCbor,
    owner: signer.paymentKeyHash,
    label: "foreign address witnesses",
  });
  expect(planned.plan.tier).toBe("Certified");
  signer.selectWallet(lucid);
  const carriageUtxos = await publishCarriageUnjournaled({
    lucid,
    signer,
    planned,
    publisherAddress: signer.proofMaterialsAddress,
    label: "foreign address witnesses",
  });
  const { certificateUtxo } = await certifyCarriageUnjournaled({
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
  // The honest field holds its signer in the last batch, so the opening turn
  // continues at the scan with the first batch boundary's checkpoint.
  const next = missingSignatureFieldWalkCheckpoint({
    txId: evidence.subject.transaction_id,
    itemCount: own.itemCount,
    totalLength: own.preimage.length,
    nextItemIndex: SIGNER_WITNESS_SCAN_BATCH,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        witness_set_hash: evidence.witnessSetHashHex,
        payment_credential: evidence.paymentCredentialHex,
        checkpoint_hash: next.checkpointHash,
      },
    } as never,
    SignerWitnessScanDatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "foreign-certificate scan");
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "foreign-certificate scan",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "foreign-certificate scan output",
            ),
            witnesses_opening: opening,
            checkpoint_cbor: null,
          },
        ],
      } as never,
      SignerWitnessScanRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return buildAndSubmitTransactionUnjournaled(buildLinearFaultContinue, {
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: "foreign-certificate scan",
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs: [certificateUtxo],
    awaitConfirmation: true,
  });
};
