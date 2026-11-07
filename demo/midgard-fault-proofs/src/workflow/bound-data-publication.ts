import { computeHash32 } from "@al-ft/midgard-core";
import { MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES } from "@al-ft/midgard-core/codec/native-tx-carriage";
import { CML, Data } from "@lucid-evolution/lucid";

import type { Requirement } from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";

export const BOUND_DATA_PUBLICATION_PREREQUISITE =
  "midgard-bound-data-publication-prerequisite-v1" as const;

/** One complete typed datum at the consumer's exact address; never split. */
export type BoundDataPublicationRequirement = Readonly<{
  kind: "bound_data_publication";
  publicationAddress: string;
  datumCbor: string;
  sourceIdentity: JournalJsonObject;
}>;

export const createBoundDataPublicationRequirement = ({
  publicationAddress,
  datumCbor,
  sourceIdentity,
}: Omit<
  BoundDataPublicationRequirement,
  "kind"
>): BoundDataPublicationRequirement => {
  CML.Address.from_bech32(publicationAddress);
  if (
    typeof sourceIdentity.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(sourceIdentity.headerHash) ||
    typeof sourceIdentity.action !== "object" ||
    sourceIdentity.action === null ||
    Array.isArray(sourceIdentity.action)
  )
    throw new Error("bound publication omitted its source action or header");
  if (
    !/^(?:[0-9a-f]{2})+$/u.test(datumCbor) ||
    Data.to(Data.from(datumCbor)) !== datumCbor ||
    datumCbor.length / 2 > MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES
  )
    throw new Error(
      "bound publication requires one bounded canonical Data datum",
    );
  return Object.freeze({
    kind: "bound_data_publication",
    publicationAddress,
    datumCbor,
    sourceIdentity: normalizeJournalJson(sourceIdentity) as JournalJsonObject,
  });
};

export const boundDataPublicationPlan = (
  requirement: BoundDataPublicationRequirement,
) => {
  if (
    Object.keys(requirement).sort().join(",") !==
    "datumCbor,kind,publicationAddress,sourceIdentity"
  )
    throw new Error("bound publication requirement changed identity");
  const checked = createBoundDataPublicationRequirement(requirement);
  const bytes = Buffer.from(checked.datumCbor, "hex");
  const digest = computeHash32(bytes);
  return Object.freeze({
    plan: Object.freeze({
      // This internal publication plan does not change a consumer's wire tier.
      tier: "RawDatums" as const,
      publications: Object.freeze([{ chunkIndex: 0, bytes, digest }]),
    }),
    publicationDatums: Object.freeze([checked.datumCbor]),
    publicationDigests: Object.freeze([digest.toString("hex")]),
  });
};

export const requireBoundPublicationContext = (
  required: Requirement,
  headerHash: string,
  action: FraudProofWorkflowAction,
) => {
  if (
    "kind" in required &&
    required.kind === "bound_data_publication" &&
    (required.sourceIdentity.headerHash !== headerHash ||
      journalJsonDigest(
        normalizeJournalJson(required.sourceIdentity.action),
      ) !== journalJsonDigest(normalizeJournalJson(action)))
  )
    throw new Error("bound publication changed its source action or header");
};

export const boundPublicationAddress = (
  required: Requirement,
  publisherAddress: string,
): string =>
  "kind" in required && required.kind === "bound_data_publication"
    ? required.publicationAddress
    : publisherAddress;

export const requireBoundPublicationRecoveryHeader = (
  action: FraudProofWorkflowAction,
  headerHash: string,
): void => {
  if (
    action.input.schemaVersion === BOUND_DATA_PUBLICATION_PREREQUISITE &&
    (action.input.sourceIdentity as JournalJsonObject).headerHash !== headerHash
  )
    throw new Error("bound publication changed its source header");
};
