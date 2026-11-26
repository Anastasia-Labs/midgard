import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core/codec/native-tx-carriage";
import { type MidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { type UTxO, validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  deriveFieldPreimageCertification,
  resolveCertificateReferenceIndex,
  resolveChunkReferenceIndices,
} from "./field-preimage-carriage.resolve-certificate-reference-index.js";

/**
 * Resolves one field's §8 carriage against the reference-input set of the
 * transaction **whose door will read it**.
 *
 * The tier is §8.4's partition over the preimage's length — never a choice made
 * here — and the indices are resolved by content through the two resolvers
 * above, so nothing about a carriage produced here is asserted rather than
 * located.
 *
 * `referenceInputs` must be the door-running transaction's **complete**
 * reference-input set, not just the carriage UTxOs. Positional indices count
 * into the ledger's canonically-sorted list, and a dispute step references more
 * than its carriage — the published spending validator arrives the same way
 * (`readFrom`) and sorts into the same list. Passing only the carriage produces
 * indices that are right until the first transaction that references anything
 * else, which is every real one.
 *
 * This resolver **is** the production seam: the
 * dispute builder resolves the plan against the door transaction's own
 * reference-input set at build time and puts the result on the observe wire,
 * so no earlier transaction's index survives to disagree with it.
 */
export const resolveMidgardFieldCarriageAgainstReferenceInputs = ({
  plan,
  referenceInputs,
  certificatePolicyId,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly referenceInputs: readonly UTxO[];
  readonly certificatePolicyId?: string;
}): MidgardFieldCarriage => {
  if (plan.tier === "Inline") {
    if (plan.inlinePreimage === null) {
      throw new Error(
        `tier-1 plan for field ${plan.fieldIndex.toString()} carries no preimage`,
      );
    }
    return { carriage: "Inline", preimage: plan.inlinePreimage };
  }
  const chunkIndices = resolveChunkReferenceIndices({
    plan,
    referenceInputs,
  });
  if (plan.tier === "RawUtxo") {
    const refInputIndex = chunkIndices[0];
    if (refInputIndex === undefined || chunkIndices.length !== 1) {
      throw new Error(
        `tier-2 plan for field ${plan.fieldIndex.toString()} must resolve exactly one publication`,
      );
    }
    return { carriage: "RawUtxo", refInputIndex };
  }
  if (certificatePolicyId === undefined) {
    throw new Error(
      `tier-3 carriage for field ${plan.fieldIndex.toString()} requires the §8.6 certificate policy id`,
    );
  }
  if (plan.certificate === null) {
    throw new Error(
      `tier-3 plan for field ${plan.fieldIndex.toString()} carries no certificate`,
    );
  }
  return {
    carriage: "Certified",
    certRefInputIndex: resolveCertificateReferenceIndex({
      certificatePolicyId,
      txIdHex: plan.txId.toString("hex"),
      fieldIndex: plan.fieldIndex,
      fieldHashHex: plan.commitment.toString("hex"),
      referenceInputs,
      label: `field ${plan.fieldIndex.toString()}`,
    }),
    chunkRefInputIndices: chunkIndices,
  };
};

/**
 * Admits the published certificate minting policy used by strict production
 * certification. The UTxO's own reference script is the witness: a matching
 * policy id supplied beside a substituted script is rejected before building.
 */
export const requireFieldPreimageCertificateReferenceScript = ({
  certificatePolicyId,
  referenceUtxo,
}: {
  readonly certificatePolicyId: string;
  readonly referenceUtxo: UTxO;
}): UTxO => {
  if (!/^[0-9a-f]{56}$/u.test(certificatePolicyId)) {
    throw new Error(
      "field-preimage certificate policy id must be 28-byte lowercase hex",
    );
  }
  if (referenceUtxo.scriptRef == null) {
    throw new Error(
      `field-preimage certificate reference UTxO ${referenceUtxo.txHash}#${referenceUtxo.outputIndex.toString()} carries no reference script`,
    );
  }
  const actualPolicyId = validatorToScriptHash(referenceUtxo.scriptRef);
  if (actualPolicyId !== certificatePolicyId) {
    throw new Error(
      `field-preimage certificate reference script hashes to ${actualPolicyId}, expected ${certificatePolicyId}`,
    );
  }
  return referenceUtxo;
};

type FieldPreimageCertificationReferenceLayout = {
  /**
   * Complete reference-input set the certification transaction reads. The
   * Certify redeemer's chunk indices resolve against the builder's final
   * order, never against this list.
   */
  readonly referenceInputs: readonly UTxO[];
};

export const resolveFieldPreimageCertificationReferenceLayout = ({
  plan,
  certificatePolicyId,
  certificatePolicyReferenceUtxo,
  chunkUtxos,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly certificatePolicyId: string;
  readonly certificatePolicyReferenceUtxo: UTxO;
  readonly chunkUtxos: readonly UTxO[];
}): FieldPreimageCertificationReferenceLayout => {
  const certification = deriveFieldPreimageCertification(plan);
  if (chunkUtxos.length !== certification.chunkCount) {
    throw new Error("certification must reference exactly the plan's chunks");
  }
  const policyReference = requireFieldPreimageCertificateReferenceScript({
    certificatePolicyId,
    referenceUtxo: certificatePolicyReferenceUtxo,
  });
  const referenceInputs = [...chunkUtxos, policyReference];
  const labels = new Set<string>();
  for (const referenceInput of referenceInputs) {
    const label = `${referenceInput.txHash}#${referenceInput.outputIndex.toString()}`;
    if (labels.has(label)) {
      throw new Error(
        `field-preimage certification reference input ${label} is duplicated`,
      );
    }
    labels.add(label);
  }
  return { referenceInputs };
};
