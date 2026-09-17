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
import { detectMinAdaForcedReplay, prepareMinAdaForcedPlan } from "./forced.js";

export const MIN_ADA_FORCED_ARTIFACT = "midgard-min-ada-forced-artifact-v1";
export type MinAdaForcedArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof MIN_ADA_FORCED_ARTIFACT;
    headerHash: string;
    forcedSourceCbor: string;
    fullTransactionCbor: string;
    detectionId: string;
    forcedIndex: number;
  }>;

/** Restarts re-open the retained source, membership, and preimage before acting. */
export const admitMinAdaForcedArtifact = async (
  value: unknown,
): Promise<Awaited<ReturnType<typeof prepareMinAdaForcedPlan>>> => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error("minAda: artifact must be an object");
  const artifact = value as Record<string, unknown>;
  if (
    Object.keys(artifact).sort().join(",") !==
      "detectionId,forcedIndex,forcedSourceCbor,fullTransactionCbor,headerHash,schemaVersion" ||
    artifact.schemaVersion !== MIN_ADA_FORCED_ARTIFACT
  )
    throw new Error("minAda: artifact shape changed");
  for (const key of [
    "headerHash",
    "forcedSourceCbor",
    "fullTransactionCbor",
  ] as const)
    if (
      typeof artifact[key] !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(artifact[key])
    )
      throw new Error(`minAda: noncanonical ${key}`);
  if (
    !Number.isSafeInteger(artifact.forcedIndex) ||
    Number(artifact.forcedIndex) < 0
  )
    throw new Error("minAda: invalid forced index");
  const source = Data.from(
    artifact.forcedSourceCbor as string,
    SDK.MinAdaForcedSourcePayloadSchema as never,
  ) as {
    header: SDK.Header;
    membership: SDK.ForcedTransactionSourceMembershipProof;
    direction: bigint;
  };
  if (
    Data.to(source as never, SDK.MinAdaForcedSourcePayloadSchema as never) !==
    artifact.forcedSourceCbor
  )
    throw new Error("minAda: noncanonical forced source");
  const { membership, header } = source;
  if (
    source.direction !== 1n ||
    header.protocolVersion !== 1n ||
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
    throw new Error("minAda: artifact source/header changed");
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
    throw new Error("minAda: artifact membership changed");
  const detection = detectMinAdaForcedReplay({
    headerHash: artifact.headerHash,
    header,
    reconstruction: {
      forcedTransactions: [
        {
          key: membership.key,
          value: membership.value,
          fullTransactionCbor: Buffer.from(
            artifact.fullTransactionCbor as string,
            "hex",
          ),
        },
      ],
    },
  } as unknown as CanonicalBlockEvidence)[0];
  if (detection === undefined)
    throw new Error("minAda: artifact has no contradiction");
  const detectionId =
    `min-ada:forced:${String(artifact.forcedIndex)}:${membership.value.tx_id}:${detection.evidence.badOutputIndex.toString()}` as const;
  if (artifact.detectionId !== detectionId)
    throw new Error("minAda: detection identity changed");
  return Object.freeze({
    ...detection,
    detectionId,
    forcedIndex: Number(artifact.forcedIndex),
    position: BigInt(Number(artifact.forcedIndex)),
    forcedSource: { header, membership, direction: 1n as const },
  });
};
export const prepareMinAdaForcedArtifact = async ({
  block,
  detectionId,
}: Readonly<{
  block: CanonicalBlockEvidence;
  detectionId?: string;
}>): Promise<MinAdaForcedArtifact> => {
  const prepared = await prepareMinAdaForcedPlan({
    block,
    detectionId,
  });
  const forced = block.reconstruction.forcedTransactions.find(
    (entry) =>
      Data.to(entry.key, SDK.OutputReference) ===
      Data.to(prepared.forcedSource.membership.key, SDK.OutputReference),
  );
  if (forced === undefined)
    throw new Error("minAda: retained forced source absent");
  const artifact = normalizeJournalJson({
    schemaVersion: MIN_ADA_FORCED_ARTIFACT,
    detectionId: prepared.detectionId,
    forcedIndex: prepared.forcedIndex,
    headerHash: prepared.headerHash,
    forcedSourceCbor: Data.to(
      prepared.forcedSource as never,
      SDK.MinAdaForcedSourcePayloadSchema as never,
    ),
    fullTransactionCbor: Buffer.from(forced.fullTransactionCbor).toString(
      "hex",
    ),
  }) as MinAdaForcedArtifact;
  await admitMinAdaForcedArtifact(artifact);
  return artifact;
};
