import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { reconstructDaPayload } from "../transition-trace/reconstruct.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
import { prepareCrossBlockDuplicateEventRootOpenings } from "./prepare.js";
import {
  type CrossBlockDuplicateCoordinate,
  crossBlockDuplicateDetectionId,
  detectCrossBlockDuplicateEvents,
} from "./replay.js";
import {
  type CrossBlockSettlementContext,
  crossBlockSettlementRecords,
} from "./settlement-authority.js";
export const CROSS_BLOCK_DUPLICATE_ARTIFACT =
  "midgard-cross-block-duplicate-event-workflow-artifact-v1";
export const admitCrossBlockDuplicateArtifact = async (
  value: JournalJsonObject,
) => {
  if (
    Object.keys(value).sort().join(",") !==
      "authorityDigest,coordinate,detectionId,headerHash,payloadEnvelopeCbor,schemaVersion,settledPayloadEnvelopeCbor,settlementDatumCbor,settlementOutRef,settlementPolicyId" ||
    value.schemaVersion !== CROSS_BLOCK_DUPLICATE_ARTIFACT
  )
    throw new Error("cross-block duplicate artifact shape changed");
  for (const key of ["headerHash", "settlementPolicyId"] as const)
    if (typeof value[key] !== "string" || !/^[0-9a-f]{56}$/u.test(value[key]))
      throw new Error("cross-block duplicate artifact hash is invalid");
  for (const key of [
    "payloadEnvelopeCbor",
    "settledPayloadEnvelopeCbor",
    "settlementDatumCbor",
  ] as const)
    if (
      typeof value[key] !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(value[key])
    )
      throw new Error("cross-block duplicate artifact bytes are invalid");
  if (
    typeof value.authorityDigest !== "string" ||
    !/^[0-9a-f]{64}$/u.test(value.authorityDigest) ||
    typeof value.settlementOutRef !== "string" ||
    !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(value.settlementOutRef)
  )
    throw new Error(
      "cross-block duplicate artifact authority coordinate is invalid",
    );
  const c = value.coordinate as JournalJsonObject;
  if (
    typeof c !== "object" ||
    c === null ||
    Array.isArray(c) ||
    Object.keys(c).sort().join(",") !==
      "kind,outputIndex,settledHeaderHash,transactionId" ||
    typeof c.settledHeaderHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(c.settledHeaderHash) ||
    typeof c.transactionId !== "string" ||
    !/^[0-9a-f]{64}$/u.test(c.transactionId) ||
    typeof c.outputIndex !== "string" ||
    !/^(?:0|[1-9][0-9]*)$/u.test(c.outputIndex) ||
    typeof c.kind !== "string" ||
    !["deposit", "withdrawal", "forced-transaction"].includes(c.kind)
  )
    throw new Error(
      "cross-block duplicate artifact event coordinate is invalid",
    );
  const coordinate = c as CrossBlockDuplicateCoordinate;
  if (value.detectionId !== crossBlockDuplicateDetectionId(coordinate))
    throw new Error("cross-block duplicate artifact identity changed");
  const current = await reconstructDaPayload({
    payloadEnvelopeCbor: Buffer.from(
      value.payloadEnvelopeCbor as string,
      "hex",
    ),
    expectedHeaderHash: value.headerHash as string,
  });
  const settled = await reconstructDaPayload({
    payloadEnvelopeCbor: Buffer.from(
      value.settledPayloadEnvelopeCbor as string,
      "hex",
    ),
    expectedHeaderHash: coordinate.settledHeaderHash,
  });
  const datum = Data.from(
    value.settlementDatumCbor as string,
    SDK.SettlementDatum,
  );
  if (
    datum.deposits_root !== settled.header.depositsRoot ||
    datum.withdrawals_root !== settled.header.withdrawalsRoot ||
    datum.forced_transactions_root !== settled.header.forcedTransactionsRoot ||
    datum.transactions_root !== settled.header.transactionsRoot
  )
    throw new Error("cross-block duplicate artifact settlement roots changed");
  const prepared = await prepareCrossBlockDuplicateEventRootOpenings({
    challenged: current,
    settled,
    settlementPolicyId: value.settlementPolicyId as string,
    kind: coordinate.kind,
    eventKey: {
      transactionId: coordinate.transactionId,
      outputIndex: BigInt(coordinate.outputIndex),
    },
  });
  return {
    current,
    settled,
    coordinate,
    prepared,
    settlementOutRef: value.settlementOutRef,
    settlementPolicyId: value.settlementPolicyId as string,
    settlementDatumCbor: value.settlementDatumCbor as string,
  };
};
export const prepareCrossBlockDuplicateArtifact = async ({
  evidence,
  context,
  classification,
}: {
  evidence: CanonicalBlockEvidence;
  context: CrossBlockSettlementContext;
  classification: Extract<
    CanonicalBlockClassification,
    { decision: "fault_detected" }
  >;
}) => {
  const selected = detectCrossBlockDuplicateEvents({ evidence, context }).find(
    (d) =>
      classification.category === "crossBlockDuplicateEvent" &&
      d.headerHash === classification.selected.headerHash &&
      d.violationId === classification.selected.violationId &&
      d.position === classification.selected.position &&
      d.detectionId === classification.selected.detectionId,
  );
  if (selected === undefined)
    throw new Error(
      "cross-block duplicate classification does not select authenticated evidence",
    );
  const coordinate = JSON.parse(
    selected.diagnostic!,
  ) as CrossBlockDuplicateCoordinate;
  const settled = crossBlockSettlementRecords(evidence, context).find(
    (record) => record.headerHash === coordinate.settledHeaderHash,
  )!;
  const artifact = normalizeJournalJson({
    schemaVersion: CROSS_BLOCK_DUPLICATE_ARTIFACT,
    headerHash: evidence.headerHash,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    coordinate,
    detectionId: selected.detectionId,
    authorityDigest: context.contextDigest,
    settlementOutRef: settled.outRef,
    settlementPolicyId: settled.policyId,
    settlementDatumCbor: settled.datumCbor,
    settledPayloadEnvelopeCbor: settled.payloadEnvelopeCbor,
  }) as JournalJsonObject;
  await admitCrossBlockDuplicateArtifact(artifact);
  return artifact;
};
