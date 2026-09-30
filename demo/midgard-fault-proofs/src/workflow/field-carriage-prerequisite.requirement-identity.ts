import {
  computeHash32,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxCompact,
  encodeMidgardNativeTxCompact,
  midgardFieldCarriagePlansAreInterchangeable,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import {
  decodeMidgardForcedTxCompact,
  encodeMidgardForcedTxCompact,
} from "@al-ft/midgard-core/codec/forced";
import {
  deriveFieldPreimageCertification,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  fieldPreimagePublicationBytes,
  fieldPreimagePublicationDatumCbor,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  exact,
  FIELD_CARRIAGE_PREREQUISITE,
  type FieldCarriageRequirement,
  outRef,
  type PreimageCarriageRequirement,
  RAW_DATUM_PREIMAGE_PREREQUISITE,
  record,
  type Requirement,
  sha256,
} from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import type { JournalJsonObject } from "./journal.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import { rawDatumPreimagePublicationPlan } from "./raw-datum-preimage.js";

export const requirementIdentity = (
  requirement: PreimageCarriageRequirement,
): Requirement => {
  if ("kind" in requirement) {
    const planned = rawDatumPreimagePublicationPlan(requirement);
    return Object.freeze({
      ...requirement,
      planned,
      identitySha256: sha256(
        JSON.stringify({
          kind: requirement.kind,
          preimageHex: requirement.preimageHex,
          publicationDatums: planned.publicationDatums,
          publicationDigests: planned.publicationDigests,
        }),
      ),
      publicationDatums: planned.publicationDatums,
      publicationDigests: planned.publicationDigests,
      certificateDatumCbor: null,
      certificateUnit: null,
    });
  }
  const { planned } = requirement;
  const normalizedCompactCbor = requirement.compactCbor.toLowerCase();
  let decodedCompact: ReturnType<typeof decodeMidgardForcedTxCompact>;
  let canonicalCompactCbor: Buffer;
  try {
    const bytes = Buffer.from(normalizedCompactCbor, "hex");
    if (planned.sourceKind === 1n) {
      const forced = decodeMidgardForcedTxCompact(bytes);
      decodedCompact = forced;
      canonicalCompactCbor = encodeMidgardForcedTxCompact(forced);
    } else {
      const normal = decodeMidgardNativeTxCompact(bytes);
      decodedCompact = normal;
      canonicalCompactCbor = encodeMidgardNativeTxCompact(normal);
    }
  } catch (cause) {
    throw new Error(
      `field carriage compact CBOR does not decode: ${String(cause)}`,
    );
  }
  const planBytes =
    planned.plan.inlinePreimage ??
    Buffer.concat(planned.plan.publications.map(({ bytes }) => bytes));
  const replayedPlan = planMidgardFieldCarriage({
    owner: planned.plan.certificate?.owner ?? Buffer.alloc(28),
    txId: planned.plan.txId,
    fieldIndex: planned.plan.fieldIndex,
    preimage: planBytes,
    publish:
      planned.plan.tier === "RawUtxo" && planned.plan.inlinePreimage === null,
  });
  if (
    !/^(?:[0-9a-f]{2})+$/u.test(requirement.compactCbor) ||
    canonicalCompactCbor.toString("hex") !== normalizedCompactCbor ||
    computeMidgardNativeTxId(decodedCompact).toString("hex") !==
      planned.nativeTxId ||
    planned.plan.txId.toString("hex") !== planned.nativeTxId ||
    planned.plan.fieldIndex !== planned.fieldIndex ||
    planned.plan.totalLength !== planned.preimage.length ||
    !planBytes.equals(planned.preimage) ||
    !midgardFieldCarriagePlansAreInterchangeable(replayedPlan, planned.plan) ||
    computeHash32(planned.preimage).toString("hex") !== planned.commitment ||
    planned.plan.commitment.toString("hex") !== planned.commitment ||
    !/^[0-9a-f]{56}$/u.test(requirement.certificate.policyId) ||
    requirement.certificate.referenceScriptUtxo.scriptRef == null ||
    validatorToScriptHash(
      requirement.certificate.referenceScriptUtxo.scriptRef,
    ) !== validatorToScriptHash(requirement.certificate.mintingScript) ||
    ("nativeTxCompactCbor" in planned &&
      requirement.compactCbor !== planned.nativeTxCompactCbor)
  ) {
    throw new Error(
      "field carriage requires the exact manifest-bound certificate policy reference",
    );
  }
  const publicationDatums = Object.freeze(
    planned.plan.publications.map((publication) =>
      fieldPreimagePublicationDatumCbor(publication.bytes),
    ),
  );
  const publicationDigests = Object.freeze(
    planned.plan.publications.map((publication) =>
      publication.digest.toString("hex"),
    ),
  );
  const certification =
    planned.plan.tier === "Certified"
      ? deriveFieldPreimageCertification(planned.plan)
      : null;
  const certificateUnit =
    certification === null
      ? null
      : `${requirement.certificate.policyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;
  const identitySha256 = sha256(
    JSON.stringify({
      sourceKind: planned.sourceKind.toString(),
      fieldIndex: planned.fieldIndex,
      nativeTxId: planned.nativeTxId,
      nativeTxCompactCbor: requirement.compactCbor,
      planKind: "kind" in planned ? planned.kind : "decoded_field_opening_v1",
      preimage: planned.preimage.toString("hex"),
      itemCount: "itemCount" in planned ? planned.itemCount : null,
      commitment: planned.commitment,
      tier: planned.plan.tier,
      publicationDatums,
      publicationDigests,
      certificateDatumCbor: certification?.datumCbor ?? null,
      certificatePolicyId: requirement.certificate.policyId,
      certificateReferenceOutRef: outRef(
        requirement.certificate.referenceScriptUtxo,
      ),
      certificateReferenceScriptHash: validatorToScriptHash(
        requirement.certificate.mintingScript,
      ),
      compactCbor: requirement.compactCbor,
      witnessSetCompactCbor: requirement.witnessSetCompactCbor ?? null,
    }),
  );
  return Object.freeze({
    ...requirement,
    identitySha256,
    publicationDatums,
    publicationDigests,
    certificateDatumCbor: certification?.datumCbor ?? null,
    certificateUnit,
  });
};

export const certifiedRequirement = (
  requirement: Requirement,
): FieldCarriageRequirement => {
  if ("kind" in requirement || requirement.planned.plan.tier !== "Certified")
    throw new Error(
      "raw preimage publication cannot request field certification",
    );
  return requirement;
};

const frozenBaseAction = (
  action: FraudProofWorkflowAction,
): FraudProofWorkflowAction =>
  Object.freeze({
    actionId: action.actionId,
    input: Object.freeze({ ...action.input }),
  });

/**
 * §8.5 raw carriage publishes a nothing-but-bytes inline datum, so its content
 * address is taken over the unwrapped payload. A structured evidence
 * publication *is* the Data its consumer reads, so its content address is taken
 * over the datum itself. Journal-only recovery cannot rebuild a removed
 * requirement to tell the two apart, so the publication action records which
 * encoding it published under.
 */
export const PUBLICATION_ENCODINGS = [
  "nothing_but_bytes",
  "structured_data",
] as const;

export type PublicationEncoding = (typeof PUBLICATION_ENCODINGS)[number];

const publicationEncoding = (requirement: Requirement): PublicationEncoding =>
  "kind" in requirement && requirement.kind === "structured_data_preimage"
    ? "structured_data"
    : "nothing_but_bytes";

export const publishedContentDigest = (
  encoding: PublicationEncoding,
  datumCbor: string,
): string =>
  computeHash32(
    encoding === "structured_data"
      ? Buffer.from(datumCbor, "hex")
      : fieldPreimagePublicationBytes(datumCbor),
  ).toString("hex");

export const publicationAction = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  baseAction,
  requirement,
  publicationIndex,
}: {
  readonly category: Category;
  readonly baseAction: FraudProofWorkflowAction;
  readonly requirement: Requirement;
  readonly publicationIndex: number;
}): FraudProofWorkflowAction =>
  Object.freeze({
    actionId: `publish-${"kind" in requirement ? "raw-datum-preimage" : "field-carriage"}:${baseAction.actionId}:${requirement.identitySha256}:${publicationIndex.toString()}`,
    input: Object.freeze({
      schemaVersion:
        "kind" in requirement
          ? RAW_DATUM_PREIMAGE_PREREQUISITE
          : FIELD_CARRIAGE_PREREQUISITE,
      category,
      stage: "publish_field_carriage",
      forAction: frozenBaseAction(baseAction),
      requirementSha256: requirement.identitySha256,
      publicationIndex,
      publicationEncoding: publicationEncoding(requirement),
      publicationDigest: requirement.publicationDigests[publicationIndex]!,
      datumCborSha256: sha256(requirement.publicationDatums[publicationIndex]!),
    }),
  });

export const certificateAction = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  baseAction,
  requirement,
}: {
  readonly category: Category;
  readonly baseAction: FraudProofWorkflowAction;
  readonly requirement: Requirement;
}): FraudProofWorkflowAction =>
  Object.freeze({
    actionId: `certify-field-carriage:${baseAction.actionId}:${requirement.identitySha256}`,
    input: Object.freeze({
      schemaVersion: FIELD_CARRIAGE_PREREQUISITE,
      category,
      stage: "certify_field_carriage",
      forAction: frozenBaseAction(baseAction),
      requirementSha256: requirement.identitySha256,
      certificateDatumCborSha256: sha256(requirement.certificateDatumCbor!),
      certificateUnit: requirement.certificateUnit!,
    }),
  });

export const isCarriagePrerequisiteAction = (
  action: FraudProofWorkflowAction,
  rawDatum: boolean,
): boolean =>
  action.input.schemaVersion ===
    (rawDatum
      ? RAW_DATUM_PREIMAGE_PREREQUISITE
      : FIELD_CARRIAGE_PREREQUISITE) &&
  (action.input.stage === "publish_field_carriage" ||
    action.input.stage === "certify_field_carriage");

export const parseBaseAction = (
  value: unknown,
  label: string,
): FraudProofWorkflowAction => {
  const parsed = exact(value, ["actionId", "input"], label);
  if (typeof parsed.actionId !== "string") {
    throw new Error(`${label} actionId is malformed`);
  }
  return {
    actionId: parsed.actionId,
    input: record(parsed.input, `${label} input`) as JournalJsonObject,
  };
};
