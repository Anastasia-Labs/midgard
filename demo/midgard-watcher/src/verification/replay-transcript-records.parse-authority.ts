import { buildCanonicalTransitionEffect } from "@al-ft/midgard-validation";

import type { WatcherAuthenticatedReplayTranscript } from "./authenticated-replay-transcript.js";
import {
  type WatcherBlockReplayEventAuthorityRecord,
  type WatcherBlockReplayEventOriginRecord,
  type WatcherBlockReplayResult,
} from "./block-replay.js";
import {
  bindWatcherOriginEventClaim,
  type WatcherCommittedEventClaim,
} from "./event-claims.js";
import {
  eventKeyFor,
  parseEvent,
  phaseFor,
} from "./replay-transcript-records.parse-event.js";
import {
  equal,
  HEX,
  HEX28,
  HEX32,
  list,
  NATURAL,
  nullableText,
  OUT_REF,
  record,
  requireCondition,
  stringFields,
  stringList,
  text,
} from "./replay-transcript-records.w25-keys.js";

export const parseAuthority = (
  value: unknown,
): WatcherBlockReplayEventAuthorityRecord => {
  const r = record(
    value,
    [
      "phase",
      "eventKey",
      "event",
      "network",
      "origin",
      "committedClaim",
      "canonicalNativeTxCborHex",
      "programMaterialSidecarCborHex",
      "transitionEffect",
    ],
    "event authority",
  );
  const event = parseEvent(r.event);
  const phase = phaseFor(event);
  const eventKey = eventKeyFor(event);
  equal(r.phase, phase, "event phase");
  equal(r.eventKey, eventKey, "event key");
  requireCondition(
    r.network === "Mainnet" ||
      r.network === "Preprod" ||
      r.network === "Custom" ||
      r.network === "Preview",
    "event network",
  );
  requireCondition(
    typeof r.origin === "object" && r.origin !== null,
    "event origin",
  );
  const origin = record(
    r.origin,
    [
      "source",
      "deploymentManifestId",
      "blueprintHash",
      "checkpointDigest",
      "checkpointPayloadDigest",
      "snapshotDigest",
      "headEntryDigest",
      "historyEntryDigests",
      "throughHeader",
    ],
    "event origin",
  );
  equal(origin.source, "local_publication", "event authority source");
  stringFields(
    origin,
    [
      "deploymentManifestId",
      "blueprintHash",
      "checkpointDigest",
      "checkpointPayloadDigest",
      "snapshotDigest",
      "headEntryDigest",
    ],
    "event origin",
    HEX32,
  );
  stringList(origin.historyEntryDigests, "event history", HEX32);
  requireCondition(
    list(origin.historyEntryDigests, "event history").length > 0,
    "event history",
  );
  if (origin.throughHeader !== null) {
    const cutoff = record(
      origin.throughHeader,
      [
        "headerHash",
        "headerCborHex",
        "queueOutRef",
        "observedTransactionHash",
        "observedBlockHash",
        "observedSlot",
        "observedBlockNo",
        "transactionIndex",
        "historyEntryDigest",
      ],
      "event header cutoff",
    );
    text(cutoff.headerHash, "event cutoff header hash", HEX28);
    text(cutoff.headerCborHex, "event cutoff header CBOR", HEX);
    text(cutoff.queueOutRef, "event cutoff queue outRef", OUT_REF);
    stringFields(
      cutoff,
      ["observedTransactionHash", "observedBlockHash", "historyEntryDigest"],
      "event cutoff",
      HEX32,
    );
    stringFields(
      cutoff,
      ["observedSlot", "observedBlockNo", "transactionIndex"],
      "event cutoff",
      NATURAL,
    );
    requireCondition(
      list(origin.historyEntryDigests, "event history").includes(
        cutoff.historyEntryDigest,
      ),
      "event cutoff history membership",
    );
  }
  const c = record(
    r.committedClaim,
    ["phase", "eventIdCborHex", "valueCborHex", "canonicalNativeTxCborHex"],
    "event committedClaim",
  );
  equal(c.phase, phase, "event claim phase");
  equal(c.eventIdCborHex, event.eventId, "event claim id");
  text(c.valueCborHex, "event claim value", HEX);
  nullableText(c.canonicalNativeTxCborHex, "event claim native", HEX);
  nullableText(r.canonicalNativeTxCborHex, "event native", HEX);
  nullableText(r.programMaterialSidecarCborHex, "event sidecar", HEX);
  if (phase === "ForcedTransaction") {
    text(r.canonicalNativeTxCborHex, "forced native", HEX);
    equal(
      r.canonicalNativeTxCborHex,
      c.canonicalNativeTxCborHex,
      "forced native claim binding",
    );
  } else {
    equal(r.canonicalNativeTxCborHex, null, "ordinary event native");
    equal(r.programMaterialSidecarCborHex, null, "ordinary event sidecar");
    equal(c.canonicalNativeTxCborHex, null, "ordinary event claim native");
  }
  const committedClaim = c as unknown as WatcherCommittedEventClaim;
  bindWatcherOriginEventClaim(event, committedClaim);
  const effect = record(
    r.transitionEffect,
    ["canonicalCborHex", "digest", "operations"],
    "event effect",
  );
  text(effect.canonicalCborHex, "event effect CBOR", HEX);
  text(effect.digest, "event effect digest", HEX32);
  const operations = list(effect.operations, "event effect operations").map(
    (value) => {
      requireCondition(
        typeof value === "object" && value !== null,
        "event operation",
      );
      const insert = Reflect.get(value, "type") === "insert";
      const item = record(
        value,
        insert
          ? ["type", "outRefCborHex", "outputCborHex"]
          : ["type", "outRefCborHex"],
        "event operation",
      );
      equal(item.type, insert ? "insert" : "delete", "event operation type");
      const outRefCbor = Buffer.from(
        text(item.outRefCborHex, "event operation outRef", HEX),
        "hex",
      );
      return insert
        ? {
            type: "insert" as const,
            outRefCbor,
            outputCbor: Buffer.from(
              text(item.outputCborHex, "event operation output", HEX),
              "hex",
            ),
          }
        : { type: "delete" as const, outRefCbor };
    },
  );
  const rebuilt = buildCanonicalTransitionEffect(operations);
  equal(
    rebuilt.canonicalCbor.toString("hex"),
    effect.canonicalCborHex,
    "event effect bytes",
  );
  equal(rebuilt.digest, effect.digest, "event effect digest");
  return {
    phase,
    eventKey,
    event,
    network: r.network,
    origin: origin as unknown as WatcherBlockReplayEventOriginRecord,
    committedClaim,
    canonicalNativeTxCborHex: r.canonicalNativeTxCborHex as string | null,
    programMaterialSidecarCborHex: r.programMaterialSidecarCborHex as
      | string
      | null,
    transitionEffect:
      effect as unknown as WatcherBlockReplayEventAuthorityRecord["transitionEffect"],
  };
};

export const TRANSCRIPT_KEYS = [
  "schemaVersion",
  "deploymentFingerprint",
  "stateQueueObservationDigest",
  "headerHash",
  "inclusionPoint",
  "coordinate",
  "payloadEnvelopeCborHex",
  "payloadEnvelopeSha256",
  "payloadSha256",
  "daProvenanceCborHex",
  "authenticatedHeaderObservationCborHex",
  "stateQueueHeaderObservationCborHex",
  "priorState",
  "reconstructionRecordCborHex",
  "phaseARecordCborHex",
  "ruleBundleCborHex",
  "ruleBundleCommitment",
  "eventAuthorityRecordsCborHex",
  "blockReplayRecordCborHex",
  "blockReplayResultDigest",
  "transcriptDigest",
] as const;

export const HEADER_OBSERVATION_KEYS = [
  "headerHash",
  "headerCborHex",
  "stateQueueNodeCborHex",
  "linkedListDatumCborHex",
  "daAvailability",
  "queueOutRef",
  "nextHeaderHash",
  "observedTransactionHash",
  "observedBlockHash",
  "observedSlot",
  "observedBlockNo",
  "observedChainPointId",
  "finalityDepth",
] as const;

export const INCLUSION_KEYS = [
  "transactionHash",
  "blockHash",
  "blockNo",
  "slot",
  "chainPointId",
  "finalityDepth",
] as const;

export const provenance = (value: unknown, trustClass: string): void => {
  const r = record(value, ["trustClass", "sourceId", "grade"], "provenance");
  equal(r.trustClass, trustClass, "provenance trust class");
  equal(r.grade, "security", "provenance grade");
  requireCondition(
    text(r.sourceId, "provenance source").trim().length > 0,
    "provenance source",
  );
};

export type WatcherReplayTranscriptRecords = Readonly<{
  transcript: WatcherAuthenticatedReplayTranscript;
  headerCborHex: string;
  reconstruction: Record<string, unknown>;
  phaseA: Record<string, unknown>;
  blockReplay: WatcherBlockReplayResult;
  events: readonly WatcherBlockReplayEventAuthorityRecord[];
}>;
