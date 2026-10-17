import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { computeHash28 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { commitCountedRoot } from "../transition-trace/phas.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
import {
  invalidSignatureEvidenceFromForcedSource,
  invalidSignatureWrongfulRejectionCloses,
  type PreparedInvalidSignatureWrongfulRejection,
  prepareInvalidSignatureWrongfulRejection,
} from "./wrongful-rejection.js";

export const INVALID_SIGNATURE_FORCED_ARTIFACT =
  "midgard-invalid-signature-forced-artifact-v1";
export type InvalidSignatureForcedArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof INVALID_SIGNATURE_FORCED_ARTIFACT;
    headerHash: string;
    forcedSourceCbor: string;
    fullTransactionCbor: string;
  }>;

/** Restarts re-open the retained source, membership, and preimage before acting. */
export const admitInvalidSignatureForcedArtifact = async (
  value: unknown,
): Promise<PreparedInvalidSignatureWrongfulRejection> => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error("invalidSignature: artifact must be an object");
  const artifact = value as Record<string, unknown>;
  if (
    Object.keys(artifact).sort().join(",") !==
      "forcedSourceCbor,fullTransactionCbor,headerHash,schemaVersion" ||
    artifact.schemaVersion !== INVALID_SIGNATURE_FORCED_ARTIFACT
  )
    throw new Error("invalidSignature: artifact shape changed");
  for (const key of [
    "headerHash",
    "forcedSourceCbor",
    "fullTransactionCbor",
  ] as const)
    if (
      typeof artifact[key] !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(artifact[key])
    )
      throw new Error(`invalidSignature: noncanonical ${key}`);
  const source = Data.from(
    artifact.forcedSourceCbor as string,
    SDK.InvalidSignatureForcedSourcePayloadSchema as never,
  ) as {
    header: SDK.Header;
    membership: SDK.ForcedTransactionSourceMembershipProof;
    direction: bigint;
  };
  const { membership, header } = source;
  if (
    source.direction !== 1n ||
    membership.domain !== SDK.ROOT_DOMAINS.forcedTransactionsV1 ||
    membership.root !== header.forcedTransactionsRoot ||
    membership.count !== header.forcedTransactionCount ||
    (await commitCountedRoot({
      domain: membership.domain,
      phasRoot: membership.phas_root,
      count: membership.count,
    })) !== membership.root ||
    computeHash28(SDK.encodeHeaderCbor(header)).toString("hex") !==
      artifact.headerHash
  )
    throw new Error("invalidSignature: artifact source/header changed");
  const proof = membership.proof.map((step) => {
    if ("Branch" in step)
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    if ("Fork" in step)
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });
  const opened = MpfProof.fromJSON(
    Buffer.from(Data.to(membership.key, SDK.OutputReference), "hex"),
    Buffer.from(
      Data.to(
        membership.value as never,
        SDK.ForcedInclusionTxV1Schema as never,
      ),
      "hex",
    ),
    proof,
  ).verify(true);
  if (opened?.toString("hex") !== membership.phas_root)
    throw new Error("invalidSignature: artifact membership changed");
  const evidence = invalidSignatureEvidenceFromForcedSource({
    key: membership.key,
    value: membership.value,
    fullTransactionCbor: Buffer.from(
      artifact.fullTransactionCbor as string,
      "hex",
    ),
  });
  if (evidence === null || !invalidSignatureWrongfulRejectionCloses(evidence))
    throw new Error("invalidSignature: artifact has no contradiction");
  return Object.freeze({
    headerHash: artifact.headerHash as string,
    evidence,
    forcedSource: { header, membership, direction: 1n as const },
  });
};
export const prepareInvalidSignatureForcedArtifact = async ({
  block,
}: Readonly<{
  block: CanonicalBlockEvidence;
}>): Promise<InvalidSignatureForcedArtifact> => {
  const prepared = await prepareInvalidSignatureWrongfulRejection({ block });
  const forced = block.reconstruction.forcedTransactions.find(
    (entry) =>
      Data.to(entry.key, SDK.OutputReference) ===
      Data.to(prepared.forcedSource.membership.key, SDK.OutputReference),
  );
  if (forced === undefined)
    throw new Error("invalidSignature: retained forced source absent");
  const artifact = normalizeJournalJson({
    schemaVersion: INVALID_SIGNATURE_FORCED_ARTIFACT,
    headerHash: prepared.headerHash,
    forcedSourceCbor: Data.to(
      prepared.forcedSource as never,
      SDK.InvalidSignatureForcedSourcePayloadSchema as never,
    ),
    fullTransactionCbor: Buffer.from(forced.fullTransactionCbor).toString(
      "hex",
    ),
  }) as InvalidSignatureForcedArtifact;
  await admitInvalidSignatureForcedArtifact(artifact);
  return artifact;
};
