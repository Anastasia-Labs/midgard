import type { DistinctAssetAccumulationEvidenceV1 } from "./family-v1.js";

export const DISTINCT_ASSET_ACCUMULATION_COMPLETE_REPLAY_TOKEN_V1 =
  "distinct-asset-accumulation-complete-replay-v1" as const;
export type DistinctAssetAccumulationStageV1 =
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
export type DistinctAssetAccumulationJournalEntryV1 = Readonly<{
  sequence: number;
  stage: DistinctAssetAccumulationStageV1;
  txId: string;
  evidenceIdentity: string;
}>;
const order: readonly DistinctAssetAccumulationStageV1[] = [
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
export const nextDistinctAssetAccumulationStageV1 = (
  entries: readonly DistinctAssetAccumulationJournalEntryV1[],
  expectedEvidenceIdentity?: string,
): DistinctAssetAccumulationStageV1 => {
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
export type DistinctAssetAccumulationReplayArtifactV1 = Readonly<{
  evidence: DistinctAssetAccumulationEvidenceV1;
  retainedWitnessCborHex: string;
  authenticatedHeaderHash: string;
}>;
export const admitDistinctAssetAccumulationReplayArtifactV1 = (
  artifact: DistinctAssetAccumulationReplayArtifactV1,
): DistinctAssetAccumulationReplayArtifactV1 => {
  if (!/^(?:[0-9a-f]{2})+$/u.test(artifact.retainedWitnessCborHex))
    throw new Error(
      "distinctAssetAccumulationLimit: retained witness is not canonical hex",
    );
  if (!/^[0-9a-f]{64}$/u.test(artifact.authenticatedHeaderHash))
    throw new Error("distinctAssetAccumulationLimit: header hash is malformed");
  return Object.freeze(artifact);
};
