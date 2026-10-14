import {
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import {
  type Emulator,
  type LucidEvolution,
  type MintingPolicy,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import { type ResolvedProverSigner } from "../../src/runtime.js";
import { network } from "./emulator/blueprints.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./emulator/measurement.js";
import { EXECUTION_RESERVE_FRACTION } from "./emulator/proof-fit.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./emulator/protocol-parameters.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.js";

export type CertifiedCommittedField = Readonly<{
  plan: MidgardFieldCarriagePlan;
  chunks: readonly UTxO[];
  certificateUtxo: UTxO;
  /** Every carriage reference a step must read, in no particular order. */
  referenceInputs: readonly UTxO[];
}>;

/**
 * Ledger fit against the real L1 envelope and per-transaction execution
 * limits, as the field-preimage-length maximum lifecycle asserts it. The line
 * logged per stage also reports the margin to GOAL_SPEC 3.3's 20% execution
 * reserve, which a maximum-shape step may consume.
 */
export const expectStageFit = (
  label: string,
  stage: string,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  const { maxTxExMem, maxTxExSteps } = EMULATOR_PROTOCOL_PARAMETERS;
  const reserveMemory =
    (maxTxExMem * (100n - EXECUTION_RESERVE_FRACTION)) / 100n;
  console.info(
    `[${label}-fit] ${JSON.stringify({
      stage,
      signedBytes: measurement.completeSignedBytes,
      byteMargin: measurement.l1ByteMargin,
      referenceInputs: measurement.referenceInputCount,
      memory: measurement.executionMemory.toString(),
      memoryMargin: (maxTxExMem - measurement.executionMemory).toString(),
      reserveMemoryMargin: (
        reserveMemory - measurement.executionMemory
      ).toString(),
      cpu: measurement.executionSteps.toString(),
      cpuMargin: (maxTxExSteps - measurement.executionSteps).toString(),
    })}`,
  );
  expect(measurement.l1ByteMargin).toBeGreaterThan(0);
  expect(measurement.executionMemory).toBeLessThanOrEqual(maxTxExMem);
  expect(measurement.executionSteps).toBeLessThanOrEqual(maxTxExSteps);
};

/**
 * Publishes a committed field through the shared §8 carriage path: the planned
 * raw chunks, the certificate mint's reference script, and the §8.6
 * certificate itself. Each submitted transaction is measured for fit.
 */
export const publishCertifiedCommittedField = async ({
  emulator,
  lucid,
  signer,
  certificate,
  nativeTxId,
  fieldIndex,
  preimage,
  compactCbor,
  witnessSetCompactCbor,
  label,
}: {
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly certificate: Readonly<{
    policyId: string;
    mintingScript: MintingPolicy;
  }>;
  readonly nativeTxId: string;
  readonly fieldIndex: number;
  readonly preimage: Buffer;
  readonly compactCbor: string;
  /**
   * The §8.6 policy re-derives the tx id and binds the compact witness set to
   * its hash for every field, body fields included.
   */
  readonly witnessSetCompactCbor: string;
  readonly label: string;
}): Promise<CertifiedCommittedField> => {
  const plan = planMidgardFieldCarriage({
    owner: Buffer.from(signer.paymentKeyHash, "hex"),
    txId: Buffer.from(nativeTxId, "hex"),
    fieldIndex,
    preimage,
  });
  if (plan.tier !== "Certified") {
    throw new Error(`${label} expected a Certified plan, got ${plan.tier}`);
  }
  // Publication reads only the §8 plan; the committed bytes need not decode
  // into §5.1 items, so no item count is claimed for them.
  const planned: FaultProofFieldOpeningPlan = {
    sourceKind: 0n,
    fieldIndex,
    nativeTxId,
    nativeTxCompactCbor: compactCbor,
    preimage,
    itemCount: 0,
    commitment: plan.commitment.toString("hex"),
    plan,
  };
  const publication = await captureEmulatorSubmission(emulator, () =>
    publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label,
    }),
  );
  publication.measurements.forEach((measurement, index) =>
    expectStageFit(label, `chunk-${(index + 1).toString()}`, measurement),
  );
  const certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid,
      script: certificate.mintingScript,
      label: `${label} certificate mint`,
    })
  ).utxo;
  const minted = await captureEmulatorSubmission(emulator, () =>
    certifyFaultProofFieldCarriage({
      lucid,
      network,
      signer,
      planned: { plan, sourceKind: 0n },
      certificatePolicyId: certificate.policyId,
      certificateMintingScript: certificate.mintingScript,
      certificateReferenceScriptUtxo: certificateReference,
      chunkUtxos: publication.result,
      compactCbor,
      witnessSetCompactCbor,
    }),
  );
  expectStageFit(label, "certificate-mint", minted.measurement);
  return Object.freeze({
    plan,
    chunks: publication.result,
    certificateUtxo: minted.result.certificateUtxo,
    referenceInputs: [...publication.result, minted.result.certificateUtxo],
  });
};
