import type { DistinctAssetAccumulationEvidence } from "./family.js";

export const DISTINCT_ASSET_ACCUMULATION_COMPLETE_REPLAY_TOKEN =
  "distinct-asset-accumulation-complete-replay-v1" as const;
export type DistinctAssetAccumulationStage =
  | "step01"
  | "step02"
  | "step03"
  | "step04"
  | "step05"
  | "step06"
  | "removeTarget"
  | "removeDescendant"
  | "complete"
  | "cancelled";
export type DistinctAssetAccumulationJournalEntry = Readonly<{
  sequence: number;
  stage: DistinctAssetAccumulationStage;
  txId: string;
  evidenceIdentity: string;
}>;
const order: readonly DistinctAssetAccumulationStage[] = [
  "step01",
  "step02",
  "step03",
  "step04",
  "step05",
  "step06",
  "removeTarget",
  "removeDescendant",
  "complete",
];
export const nextDistinctAssetAccumulationStage = (
  entries: readonly DistinctAssetAccumulationJournalEntry[],
  expectedEvidenceIdentity?: string,
): DistinctAssetAccumulationStage => {
  const transactionIds = new Set<string>();
  for (let index = 0; index < entries.length; index += 1) {
    const entry = entries[index]!;
    if (entry.sequence !== index || !/^[0-9a-f]{64}$/u.test(entry.txId))
      throw new Error(
        "distinctAssetAccumulationLimit: journal continuity changed",
      );
    if (
      !/^[0-9a-f]{64}$/u.test(entry.evidenceIdentity) ||
      (expectedEvidenceIdentity !== undefined &&
        entry.evidenceIdentity !== expectedEvidenceIdentity) ||
      transactionIds.has(entry.txId)
    )
      throw new Error(
        "distinctAssetAccumulationLimit: journal identity changed",
      );
    transactionIds.add(entry.txId);
    if (entry.stage === "cancelled") {
      if (index !== entries.length - 1)
        throw new Error(
          "distinctAssetAccumulationLimit: journal continued after cancellation",
        );
      return "cancelled";
    }
    if (entry.stage !== order[index])
      throw new Error(
        "distinctAssetAccumulationLimit: journal continuity changed",
      );
  }
  return order[entries.length] ?? "complete";
};
export type DistinctAssetAccumulationReplayArtifact = Readonly<{
  evidence: DistinctAssetAccumulationEvidence;
  retainedWitnessCborHex: string;
  authenticatedHeaderHash: string;
}>;
export const admitDistinctAssetAccumulationReplayArtifact = (
  artifact: DistinctAssetAccumulationReplayArtifact,
): DistinctAssetAccumulationReplayArtifact => {
  if (!/^(?:[0-9a-f]{2})+$/u.test(artifact.retainedWitnessCborHex))
    throw new Error(
      "distinctAssetAccumulationLimit: retained witness is not canonical hex",
    );
  if (!/^[0-9a-f]{64}$/u.test(artifact.authenticatedHeaderHash))
    throw new Error("distinctAssetAccumulationLimit: header hash is malformed");
  return Object.freeze(artifact);
};
