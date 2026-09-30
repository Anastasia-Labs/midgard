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
 * transaction **whose door will read it** (#600).
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
 * Since Option B (#619/#621) this resolver **is** the production seam: the
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
 * Refuses a carriage whose positional indices would not resolve to the same
 * UTxOs in the transaction that actually runs the §8.8 door (#600 ruling
 * D3-A, re-scoped by #619/#621).
 *
 * When this guard was written, the auxiliary — carriage included — was hashed
 * into `evidence_hash` at PrepareSelected and dereferenced three transactions
 * later, so a reference-input set that moved in between failed on L1 after
 * the evidence was already staged; re-checking the frozen indices here, off
 * chain, was the one defense. Since Option B the committed evidence is
 * transition-only and the production builder
 * (`submitValidationDisputeSemanticResolution` in
 * `demo/midgard-fault-proofs/src/validation-dispute/submit.ts`) calls
 * {@link resolveMidgardFieldCarriageAgainstReferenceInputs} at build time
 * and puts the freshly-resolved carriage on the observe wire itself — there
 * is no committed index left to re-check, so this guard is no longer on the
 * production path.
 *
 * It remains the re-checker for any caller that carries a *pre-resolved*
 * carriage to a door transaction: it is pinned by its own unit tests
 * (`demo/midgard-sdk/tests/field-preimage-carriage-door.test.ts`) and
 * driven over real ledger-resolved carriage by the tiers-2/3 emulator leg
 * (`demo/midgard-validation/tests/complete-item-carriage-tiers-emulator.test.ts`),
 * which submits the same door transaction the guard admits and the one it
 * refuses.
 */
export const assertMidgardFieldCarriageResolvesAtDoor = ({
  carriage,
  plan,
  doorReferenceInputs,
  certificatePolicyId,
  label,
}: {
  readonly carriage: MidgardFieldCarriage;
  readonly plan: MidgardFieldCarriagePlan;
  readonly doorReferenceInputs: readonly UTxO[];
  readonly certificatePolicyId?: string;
  readonly label: string;
}): void => {
  if (carriage.carriage === "Inline") {
    // Tier 1 indexes nothing, so there is nothing that can drift.
    return;
  }
  const atDoor = resolveMidgardFieldCarriageAgainstReferenceInputs({
    plan,
    referenceInputs: doorReferenceInputs,
    ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
  });
  const disagreement = ((): string | null => {
    if (atDoor.carriage !== carriage.carriage) {
      return `tier ${carriage.carriage} committed, ${atDoor.carriage} at the door`;
    }
    if (carriage.carriage === "RawUtxo" && atDoor.carriage === "RawUtxo") {
      return carriage.refInputIndex === atDoor.refInputIndex
        ? null
        : `reference-input index ${carriage.refInputIndex.toString()} committed, ${atDoor.refInputIndex.toString()} at the door`;
    }
    if (carriage.carriage === "Certified" && atDoor.carriage === "Certified") {
      if (carriage.certRefInputIndex !== atDoor.certRefInputIndex) {
        return `certificate index ${carriage.certRefInputIndex.toString()} committed, ${atDoor.certRefInputIndex.toString()} at the door`;
      }
      const committed = carriage.chunkRefInputIndices;
      const observed = atDoor.chunkRefInputIndices;
      return committed.length === observed.length &&
        committed.every((index, position) => index === observed[position])
        ? null
        : `chunk indices [${committed.join(",")}] committed, [${observed.join(",")}] at the door`;
    }
    return null;
  })();
  if (disagreement !== null) {
    throw new Error(
      `${label} §8 carriage does not resolve at the door-running transaction: ${disagreement}. ` +
        "Positional indices are §8.7 content addresses into one concrete transaction's " +
        "reference-input set, so a carriage resolved against a different set names different " +
        "UTxOs; resolve against the set the door will read (#600, re-scoped by #619/#621: " +
        "production builds resolve at build time and put the fresh carriage on the wire).",
    );
  }
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

export type FieldPreimageCertificationReferenceLayout = {
  /** Complete reference-input set used by the certification transaction. */
  readonly referenceInputs: readonly UTxO[];
  /** Indices into that complete, ledger-sorted set for the Certify redeemer. */
  readonly chunkRefInputIndices: readonly number[];
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
  return {
    referenceInputs,
    chunkRefInputIndices: resolveChunkReferenceIndices({
      plan,
      referenceInputs,
    }),
  };
};
