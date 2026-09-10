import { createHash } from "node:crypto";

import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { reconstructDaPayload } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalTransitionEffect } from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";

import type {
  WatcherIndexedUserEvent,
  WatcherTerminalUserEvent,
} from "../indexers/user-event-indexer.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import type { WatcherAuthenticatedReplayTranscript } from "./authenticated-replay-transcript.js";
import {
  watcherBlockReplayCommittedSteps,
  watcherBlockReplayDownstreamInputDigest,
  watcherBlockReplayEventAuthorityManifest,
  type WatcherBlockReplayEventAuthorityRecord,
  type WatcherBlockReplayEventOriginRecord,
  watcherBlockReplayPriorState,
  type WatcherBlockReplayResult,
} from "./block-replay.js";
import {
  bindWatcherOriginEventClaim,
  type WatcherCommittedEventClaim,
} from "./event-claims.js";
import {
  WATCHER_HEADER_COUNT_FIELDS,
  WATCHER_HEADER_ROOT_FIELDS,
} from "./header-root-reconstruction.js";
import {
  computeWatcherRuleBundleCommitment,
  parseWatcherRuleBundle,
} from "./rule-bundle.js";

const HEX = /^(?:[0-9a-f]{2})+$/u;
const HEX32 = /^[0-9a-f]{64}$/u;
const HEX28 = /^[0-9a-f]{56}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
function requireCondition(
  condition: unknown,
  field: string,
): asserts condition {
  if (!condition)
    throw new Error(`persisted replay record binding is invalid: ${field}`);
}
const text = (value: unknown, field: string, pattern?: RegExp): string => {
  requireCondition(
    typeof value === "string" && (pattern === undefined || pattern.test(value)),
    field,
  );
  return value;
};
const count = (value: unknown, field: string): number => {
  requireCondition(
    typeof value === "number" &&
      Number.isSafeInteger(value) &&
      value >= 0 &&
      !Object.is(value, -0),
    field,
  );
  return value;
};
const list = (value: unknown, field: string): readonly unknown[] => {
  requireCondition(Array.isArray(value), field);
  return value;
};
const record = (
  value: unknown,
  keys: readonly string[],
  field: string,
): Record<string, unknown> => {
  requireCondition(
    typeof value === "object" &&
      value !== null &&
      Object.getPrototypeOf(value) === Object.prototype,
    field,
  );
  requireCondition(
    Reflect.ownKeys(value).length === keys.length &&
      keys.every((key) => Object.prototype.hasOwnProperty.call(value, key)),
    `${field}.keys`,
  );
  return value as Record<string, unknown>;
};
const equal = (left: unknown, right: unknown, field: string): void => {
  requireCondition(encodeCbor(left).equals(encodeCbor(right)), field);
};
const nullableText = (
  value: unknown,
  field: string,
  pattern?: RegExp,
): void => {
  if (value !== null) text(value, field, pattern);
};
const stringFields = (
  value: Record<string, unknown>,
  keys: readonly string[],
  field: string,
  pattern?: RegExp,
): void => {
  keys.forEach((key) => text(value[key], `${field}.${key}`, pattern));
};
const countFields = (
  value: Record<string, unknown>,
  keys: readonly string[],
  field: string,
): void => {
  keys.forEach((key) => count(value[key], `${field}.${key}`));
};
const stringList = (value: unknown, field: string, pattern?: RegExp): void => {
  list(value, field).forEach((entry) => text(entry, field, pattern));
};

const plainCbor = (value: unknown): unknown => {
  if (value instanceof Map) {
    const result: Record<string, unknown> = {};
    for (const [key, entry] of value) {
      requireCondition(
        typeof key === "string" &&
          !Object.prototype.hasOwnProperty.call(result, key),
        "CBOR map key",
      );
      Object.defineProperty(result, key, {
        enumerable: true,
        configurable: true,
        writable: true,
        value: plainCbor(entry),
      });
    }
    return result;
  }
  if (Array.isArray(value)) return value.map(plainCbor);
  return value;
};

/** Descriptive decoding only. This never admits an L1, event or replay authority. */
export const decodeWatcherReplayRawRecord = (cborHex: string): unknown => {
  text(cborHex, "CBOR", HEX);
  const decoded = plainCbor(decodeSingleCbor(Buffer.from(cborHex, "hex")));
  equal(encodeCbor(decoded).toString("hex"), cborHex, "canonical CBOR");
  return decoded;
};

const resultDigest = (value: Record<string, unknown>, field: string): void => {
  const { resultDigest: digest, ...material } = value;
  equal(
    text(digest, `${field}.resultDigest`, HEX32),
    watcherSha256CanonicalJson(material),
    `${field}.resultDigest`,
  );
};

const ROOT_KEYS = [
  "sequence",
  "txIndex",
  "txId",
  "stepIndex",
  "phase",
  "operation",
  "outRef",
  "preRoot",
  "postRoot",
] as const;
const TX_ROOT_KEYS = [
  "txIndex",
  "txId",
  "preRoot",
  "postRoot",
  "mutationCount",
  "committedStepIndex",
  "committedPreRoot",
  "committedPostRoot",
] as const;
const EVENT_ROOT_KEYS = [
  "stepIndex",
  "phase",
  "eventKeyFingerprint",
  "preRoot",
  "postRoot",
  "mutationCount",
] as const;
const FACT_KEYS = [
  "eventKeyFingerprint",
  "stepIndex",
  "authenticatedOperatorValidity",
  "canonicalOperatorValidity",
  "phaseAStatus",
  "phaseARejectCode",
  "phaseBStatus",
  "phaseBRejectCode",
  "canonicalEffectDigest",
  "canonicalEffectMutationCount",
] as const;
const REJECTION_KEYS = [
  "index",
  "txId",
  "code",
  "consensusPhase",
  "consensusPhasePriority",
  "stage",
  "detail",
] as const;
const W25_KEYS = [
  "schemaVersion",
  "action",
  "reasonCodes",
  "verifiedRequires",
  "downstreamPrerequisite",
  "rejectionSelection",
  "consensusProfileId",
  "headerHash",
  "payloadEnvelopeSha256",
  "payloadSha256",
  "reconstructionDigest",
  "phaseAResultDigest",
  "ruleBundleCommitment",
  "authorityManifestDigest",
  "sourceManifestDigest",
  "effectManifestDigest",
  "priorStateRoot",
  "expectedPriorStateRoot",
  "postStateRoot",
  "expectedPostStateRoot",
  "transactionCount",
  "acceptedCount",
  "acceptedTxIds",
  "intermediateRoots",
  "transactionRoots",
  "eventRoots",
  "forcedValidationFacts",
  "stageMismatches",
  "rejections",
  "selectedRejection",
  "resultDigest",
] as const;
const parseW25 = (value: unknown): WatcherBlockReplayResult => {
  const r = record(value, W25_KEYS, "W25");
  stringFields(
    r,
    [
      "schemaVersion",
      "verifiedRequires",
      "rejectionSelection",
      "consensusProfileId",
    ],
    "W25",
  );
  requireCondition(
    r.action === "accept" || r.action === "reject",
    "W25.action",
  );
  text(r.headerHash, "W25.headerHash", HEX28);
  stringFields(
    r,
    [
      "payloadEnvelopeSha256",
      "payloadSha256",
      "reconstructionDigest",
      "phaseAResultDigest",
      "ruleBundleCommitment",
      "authorityManifestDigest",
      "sourceManifestDigest",
      "effectManifestDigest",
      "priorStateRoot",
      "expectedPriorStateRoot",
      "postStateRoot",
      "expectedPostStateRoot",
      "resultDigest",
    ],
    "W25",
    HEX32,
  );
  countFields(r, ["transactionCount", "acceptedCount"], "W25");
  stringList(r.acceptedTxIds, "W25.acceptedTxIds", HEX32);
  stringList(r.reasonCodes, "W25.reasonCodes");
  const prerequisite = record(
    r.downstreamPrerequisite,
    ["schemaVersion", "requiredVerifier", "inputDigest", "w29Eligibility"],
    "W25.downstreamPrerequisite",
  );
  stringFields(
    prerequisite,
    ["schemaVersion", "requiredVerifier", "w29Eligibility"],
    "W25.downstreamPrerequisite",
  );
  text(
    prerequisite.inputDigest,
    "W25.downstreamPrerequisite.inputDigest",
    HEX32,
  );
  for (const value of list(r.intermediateRoots, "W25.intermediateRoots")) {
    const item = record(value, ROOT_KEYS, "W25.intermediateRoot");
    count(item.sequence, "W25.intermediateRoot.sequence");
    for (const key of ["txIndex", "stepIndex"])
      if (item[key] !== null) count(item[key], `W25.intermediateRoot.${key}`);
    nullableText(item.txId, "W25.intermediateRoot.txId", HEX32);
    nullableText(item.phase, "W25.intermediateRoot.phase");
    requireCondition(
      item.operation === "delete" || item.operation === "insert",
      "W25.intermediateRoot.operation",
    );
    text(item.outRef, "W25.intermediateRoot.outRef", HEX);
    stringFields(item, ["preRoot", "postRoot"], "W25.intermediateRoot", HEX32);
  }
  for (const value of list(r.transactionRoots, "W25.transactionRoots")) {
    const item = record(value, TX_ROOT_KEYS, "W25.transactionRoot");
    countFields(item, ["txIndex", "mutationCount"], "W25.transactionRoot");
    if (item.committedStepIndex !== null)
      count(item.committedStepIndex, "W25.transactionRoot.committedStepIndex");
    stringFields(
      item,
      ["txId", "preRoot", "postRoot"],
      "W25.transactionRoot",
      HEX32,
    );
    nullableText(
      item.committedPreRoot,
      "W25.transactionRoot.committedPreRoot",
      HEX32,
    );
    nullableText(
      item.committedPostRoot,
      "W25.transactionRoot.committedPostRoot",
      HEX32,
    );
  }
  for (const value of list(r.eventRoots, "W25.eventRoots")) {
    const item = record(value, EVENT_ROOT_KEYS, "W25.eventRoot");
    countFields(item, ["stepIndex", "mutationCount"], "W25.eventRoot");
    stringFields(item, ["phase", "eventKeyFingerprint"], "W25.eventRoot");
    stringFields(item, ["preRoot", "postRoot"], "W25.eventRoot", HEX32);
  }
  for (const value of list(
    r.forcedValidationFacts,
    "W25.forcedValidationFacts",
  )) {
    const item = record(value, FACT_KEYS, "W25.forcedValidationFact");
    stringFields(
      item,
      [
        "eventKeyFingerprint",
        "authenticatedOperatorValidity",
        "canonicalOperatorValidity",
        "phaseAStatus",
        "phaseBStatus",
      ],
      "W25.forcedValidationFact",
    );
    nullableText(
      item.phaseARejectCode,
      "W25.forcedValidationFact.phaseARejectCode",
    );
    nullableText(
      item.phaseBRejectCode,
      "W25.forcedValidationFact.phaseBRejectCode",
    );
    text(
      item.canonicalEffectDigest,
      "W25.forcedValidationFact.canonicalEffectDigest",
      HEX32,
    );
    countFields(
      item,
      ["stepIndex", "canonicalEffectMutationCount"],
      "W25.forcedValidationFact",
    );
  }
  for (const value of list(r.stageMismatches, "W25.stageMismatches")) {
    const keys = ["stage", "reasonCode", "field", "expected", "actual"];
    stringFields(
      record(value, keys, "W25.stageMismatch"),
      keys,
      "W25.stageMismatch",
    );
  }
  const rejection = (value: unknown): void => {
    const item = record(value, REJECTION_KEYS, "W25.rejection");
    countFields(item, ["index", "consensusPhasePriority"], "W25.rejection");
    text(item.txId, "W25.rejection.txId", HEX32);
    stringFields(item, ["code", "consensusPhase", "stage"], "W25.rejection");
    nullableText(item.detail, "W25.rejection.detail");
  };
  list(r.rejections, "W25.rejections").forEach(rejection);
  if (r.selectedRejection !== null) rejection(r.selectedRejection);
  resultDigest(r, "W25");
  // Every key and scalar/container type has been checked at this descriptive wire boundary.
  // Enum meanings and all actual outcomes are compared against independent fresh W25 below.
  const parsed = r as unknown as WatcherBlockReplayResult;
  equal(
    prerequisite.inputDigest,
    watcherBlockReplayDownstreamInputDigest(parsed),
    "W25 downstream digest",
  );
  return parsed;
};

const W22_KEYS = [
  "schemaVersion",
  "action",
  "reasonCodes",
  "rootMismatches",
  "countMismatches",
  "reconstructedRoots",
  "reconstructedCounts",
  "headerRoots",
  "headerCounts",
  "headerHash",
  "headerPrevUtxosRoot",
  "payloadEnvelopeSha256",
  "payloadSha256",
  "resultDigest",
] as const;
const W24_KEYS = [
  "schemaVersion",
  "action",
  "reasonCodes",
  "rejectionSelection",
  "consensusProfileId",
  "headerHash",
  "payloadEnvelopeSha256",
  "payloadSha256",
  "reconstructionDigest",
  "ruleBundleCommitment",
  "transactionCount",
  "acceptedCount",
  "acceptedTxIds",
  "rejections",
  "selectedRejection",
  "resultDigest",
] as const;
const parseW22 = (value: unknown): Record<string, unknown> => {
  const r = record(value, W22_KEYS, "W22");
  requireCondition(r.action === "accept", "W22.action");
  text(r.schemaVersion, "W22.schemaVersion");
  for (const key of ["reasonCodes", "rootMismatches", "countMismatches"])
    equal(r[key], [], `W22.${key}`);
  for (const key of ["reconstructedRoots", "headerRoots"])
    stringFields(
      record(r[key], WATCHER_HEADER_ROOT_FIELDS, `W22.${key}`),
      WATCHER_HEADER_ROOT_FIELDS,
      `W22.${key}`,
      HEX32,
    );
  for (const key of ["reconstructedCounts", "headerCounts"])
    stringFields(
      record(r[key], WATCHER_HEADER_COUNT_FIELDS, `W22.${key}`),
      WATCHER_HEADER_COUNT_FIELDS,
      `W22.${key}`,
      NATURAL,
    );
  text(r.headerHash, "W22.headerHash", HEX28);
  stringFields(
    r,
    [
      "headerPrevUtxosRoot",
      "payloadEnvelopeSha256",
      "payloadSha256",
      "resultDigest",
    ],
    "W22",
    HEX32,
  );
  equal(r.reconstructedRoots, r.headerRoots, "W22 roots");
  equal(r.reconstructedCounts, r.headerCounts, "W22 counts");
  resultDigest(r, "W22");
  return r;
};
const parseW24 = (value: unknown): Record<string, unknown> => {
  const r = record(value, W24_KEYS, "W24");
  requireCondition(r.action === "accept", "W24.action");
  stringFields(
    r,
    ["schemaVersion", "rejectionSelection", "consensusProfileId"],
    "W24",
  );
  equal(r.reasonCodes, [], "W24.reasonCodes");
  equal(r.rejections, [], "W24.rejections");
  equal(r.selectedRejection, null, "W24.selectedRejection");
  text(r.headerHash, "W24.headerHash", HEX28);
  stringFields(
    r,
    [
      "payloadEnvelopeSha256",
      "payloadSha256",
      "reconstructionDigest",
      "ruleBundleCommitment",
      "resultDigest",
    ],
    "W24",
    HEX32,
  );
  countFields(r, ["transactionCount", "acceptedCount"], "W24");
  stringList(r.acceptedTxIds, "W24.acceptedTxIds", HEX32);
  equal(r.transactionCount, r.acceptedCount, "W24 counts");
  equal(
    r.acceptedCount,
    list(r.acceptedTxIds, "W24.acceptedTxIds").length,
    "W24 accepted ids",
  );
  resultDigest(r, "W24");
  return r;
};

const EVENT_KEYS = [
  "kind",
  "eventId",
  "outRef",
  "transactionHash",
  "outputIndex",
  "nonceOutRef",
  "policyId",
  "spendScriptHash",
  "addressHex",
  "assetNameHex",
  "witnessScriptHash",
  "inclusionTime",
  "eventCborHex",
  "datumCborHex",
  "outputCborHex",
  "eventContentDigest",
  "datumDigest",
  "outputDigest",
  "originPointDigest",
  "originChainPointId",
  "originBlockHash",
  "originSlot",
  "originBlockNo",
  "finalityStatus",
] as const;
const TERMINAL_KEYS = [
  "terminalStatus",
  "terminalTransactionHash",
  "terminalPointDigest",
  "terminalBlockHash",
  "terminalSlot",
  "terminalBlockNo",
  "terminalFinalityStatus",
] as const;
const parseEvent = (
  value: unknown,
): WatcherIndexedUserEvent | WatcherTerminalUserEvent => {
  requireCondition(typeof value === "object" && value !== null, "event");
  const terminal = Object.prototype.hasOwnProperty.call(
    value,
    "terminalStatus",
  );
  const classified = Object.prototype.hasOwnProperty.call(
    value,
    "terminalClassification",
  );
  const keys = [
    ...EVENT_KEYS,
    ...(terminal ? TERMINAL_KEYS : []),
    ...(classified ? ["terminalClassification"] : []),
  ];
  const r = record(value, keys, "event");
  requireCondition(
    r.kind === "deposit" ||
      r.kind === "withdrawal" ||
      r.kind === "forced_order",
    "event.kind",
  );
  stringFields(
    r,
    [
      "eventId",
      "addressHex",
      "assetNameHex",
      "eventCborHex",
      "datumCborHex",
      "outputCborHex",
    ],
    "event",
    HEX,
  );
  stringFields(
    r,
    [
      "transactionHash",
      "eventContentDigest",
      "datumDigest",
      "outputDigest",
      "originPointDigest",
      "originChainPointId",
      "originBlockHash",
    ],
    "event",
    HEX32,
  );
  stringFields(
    r,
    ["policyId", "spendScriptHash", "witnessScriptHash"],
    "event",
    HEX28,
  );
  stringFields(
    r,
    ["outputIndex", "inclusionTime", "originSlot", "originBlockNo"],
    "event",
    NATURAL,
  );
  stringFields(r, ["outRef", "nonceOutRef"], "event", OUT_REF);
  equal(
    r.outRef,
    `${String(r.transactionHash)}#${String(r.outputIndex)}`,
    "event outRef",
  );
  equal(r.finalityStatus, "final", "event finality");
  for (const [bytes, digest] of [
    ["eventCborHex", "eventContentDigest"],
    ["datumCborHex", "datumDigest"],
    ["outputCborHex", "outputDigest"],
  ])
    equal(
      sha256(Buffer.from(text(r[bytes!], `event.${bytes}`, HEX), "hex")),
      r[digest!],
      `event.${digest}`,
    );
  const id = Data.from(text(r.eventId, "event.eventId"), SDK.OutputReference);
  equal(Data.to(id, SDK.OutputReference), r.eventId, "event id CBOR");
  equal(
    `${id.transactionId}#${id.outputIndex.toString()}`,
    r.nonceOutRef,
    "event nonce",
  );
  if (terminal) {
    text(r.terminalStatus, "event.terminalStatus");
    stringFields(
      r,
      ["terminalTransactionHash", "terminalPointDigest", "terminalBlockHash"],
      "event",
      HEX32,
    );
    stringFields(r, ["terminalSlot", "terminalBlockNo"], "event", NATURAL);
    equal(r.terminalFinalityStatus, "final", "event terminal finality");
  }
  if (classified) {
    requireCondition(
      terminal && r.kind === "forced_order",
      "event terminal classification kind",
    );
    const item = record(
      r.terminalClassification,
      [
        "schemaVersion",
        "operatorValidity",
        "terminalTransactionHash",
        "terminalPointDigest",
      ],
      "event.terminalClassification",
    );
    stringFields(
      item,
      ["schemaVersion", "operatorValidity"],
      "event.terminalClassification",
    );
    equal(
      item.terminalTransactionHash,
      r.terminalTransactionHash,
      "terminal transaction",
    );
    equal(item.terminalPointDigest, r.terminalPointDigest, "terminal point");
  }
  return r as unknown as WatcherIndexedUserEvent | WatcherTerminalUserEvent;
};

const eventKeyFor = (event: WatcherIndexedUserEvent): SDK.EventKey => {
  const id = Data.from(event.eventId, SDK.OutputReference);
  if (event.kind === "deposit") return { DepositEventKey: { deposit_id: id } };
  if (event.kind === "withdrawal")
    return { WithdrawalEventKey: { withdrawal_id: id } };
  return { ForcedTransactionEventKey: { tx_order_id: id } };
};
const phaseFor = (
  event: WatcherIndexedUserEvent,
): WatcherCommittedEventClaim["phase"] =>
  event.kind === "deposit"
    ? "Deposit"
    : event.kind === "withdrawal"
      ? "Withdrawal"
      : "ForcedTransaction";
const parseAuthority = (
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
      r.network === "Preview",
    "event network",
  );
  requireCondition(
    typeof r.origin === "object" && r.origin !== null,
    "event origin",
  );
  const local = Reflect.get(r.origin, "source") === "local_publication";
  const origin = record(
    r.origin,
    local
      ? [
          "source",
          "deploymentManifestId",
          "blueprintHash",
          "checkpointDigest",
          "checkpointPayloadDigest",
          "snapshotDigest",
          "headEntryDigest",
          "historyEntryDigests",
          "throughHeader",
        ]
      : [
          "source",
          "resultDigest",
          "stateDigest",
          "snapshotDigest",
          "historyEntryDigests",
        ],
    "event origin",
  );
  equal(
    origin.source,
    local ? "local_publication" : "parser_replay",
    "event authority source",
  );
  stringFields(
    origin,
    local
      ? [
          "deploymentManifestId",
          "blueprintHash",
          "checkpointDigest",
          "checkpointPayloadDigest",
          "snapshotDigest",
          "headEntryDigest",
        ]
      : ["resultDigest", "stateDigest", "snapshotDigest"],
    "event origin",
    HEX32,
  );
  stringList(origin.historyEntryDigests, "event history", HEX32);
  requireCondition(
    list(origin.historyEntryDigests, "event history").length > 0,
    "event history",
  );
  if (local && origin.throughHeader !== null) {
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

const TRANSCRIPT_KEYS = [
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
const HEADER_OBSERVATION_KEYS = [
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
const INCLUSION_KEYS = [
  "transactionHash",
  "blockHash",
  "blockNo",
  "slot",
  "chainPointId",
  "finalityDepth",
] as const;
const provenance = (value: unknown, trustClass: string): void => {
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

/** Checks historical bytes for integrity and semantic consistency only. Original
 * source provenance is descriptive; current authority must be acquired separately.
 */
export const readWatcherReplayTranscriptRecords = async (
  cborHex: string,
  minimumConfirmationDepth: number,
): Promise<WatcherReplayTranscriptRecords> => {
  const t = record(
    decodeWatcherReplayRawRecord(cborHex),
    TRANSCRIPT_KEYS,
    "transcript",
  );
  equal(
    t.schemaVersion,
    "midgard-watcher-production-authenticated-replay-transcript-v1",
    "transcript schema",
  );
  stringFields(
    t,
    [
      "deploymentFingerprint",
      "stateQueueObservationDigest",
      "payloadEnvelopeSha256",
      "payloadSha256",
      "ruleBundleCommitment",
      "blockReplayResultDigest",
      "transcriptDigest",
    ],
    "transcript",
    HEX32,
  );
  text(t.headerHash, "transcript headerHash", HEX28);
  const { transcriptDigest, ...transcriptMaterial } = t;
  equal(
    transcriptDigest,
    computeDeploymentManifestJsonDigest(transcriptMaterial),
    "transcript digest",
  );
  const inclusion = record(t.inclusionPoint, INCLUSION_KEYS, "inclusionPoint");
  stringFields(
    inclusion,
    ["transactionHash", "blockHash", "chainPointId"],
    "inclusionPoint",
    HEX32,
  );
  stringFields(
    inclusion,
    ["blockNo", "slot", "finalityDepth"],
    "inclusionPoint",
    NATURAL,
  );
  const coordinate = record(t.coordinate, ["domain", "index"], "coordinate");
  requireCondition(
    ["block", "transaction", "mutation", "event", "transition_step"].includes(
      text(coordinate.domain, "coordinate domain"),
    ),
    "coordinate domain",
  );
  text(coordinate.index, "coordinate index", NATURAL);
  const priorState = list(t.priorState, "priorState").map((value) => {
    const r = record(value, ["outRef", "outputCbor"], "priorState entry");
    return {
      outRef: text(r.outRef, "priorState outRef", HEX),
      outputCbor: text(r.outputCbor, "priorState output", HEX),
    };
  });
  const prior = await watcherBlockReplayPriorState(priorState);
  equal(
    priorState,
    [...priorState].sort((a, b) => a.outRef.localeCompare(b.outRef)),
    "priorState order",
  );
  const raw = (key: string): unknown =>
    decodeWatcherReplayRawRecord(text(t[key], `transcript.${key}`, HEX));
  provenance(raw("daProvenanceCborHex"), "public_or_permissionless_da");
  const headerRecord = record(
    raw("stateQueueHeaderObservationCborHex"),
    HEADER_OBSERVATION_KEYS,
    "header observation",
  );
  equal(headerRecord.headerHash, t.headerHash, "header hash copies");
  const headerCborHex = text(headerRecord.headerCborHex, "HeaderV1 CBOR", HEX);
  const header = Data.from(headerCborHex, SDK.Header);
  equal(Data.to(header, SDK.Header), headerCborHex, "canonical HeaderV1");
  equal(
    computeHash28(Buffer.from(headerCborHex, "hex")).toString("hex"),
    t.headerHash,
    "HeaderV1 hash",
  );
  const nodeCbor = text(
    headerRecord.stateQueueNodeCborHex,
    "StateQueueNode CBOR",
    HEX,
  );
  const node = Data.from(nodeCbor, SDK.StateQueueNode);
  equal(
    Data.to(node, SDK.StateQueueNode),
    nodeCbor,
    "canonical StateQueueNode",
  );
  equal(
    Data.to(node.header, SDK.Header),
    headerCborHex,
    "StateQueueNode header",
  );
  equal(
    node.da_attestation,
    headerRecord.daAvailability,
    "StateQueueNode availability",
  );
  const linkedCbor = text(
    headerRecord.linkedListDatumCborHex,
    "linked list CBOR",
    HEX,
  );
  const linked = Data.from(linkedCbor, SDK.LinkedListDatum);
  equal(
    CML.PlutusData.from_cbor_hex(linkedCbor).to_canonical_cbor_hex(),
    linkedCbor,
    "canonical linked list",
  );
  const view = SDK.linkedListDatumToNodeView(
    linked,
    `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${text(t.headerHash, "headerHash")}`,
  );
  equal(
    Data.to(
      Data.castFrom(view.data, SDK.StateQueueNode) as SDK.StateQueueNode,
      SDK.StateQueueNode,
    ),
    nodeCbor,
    "linked list node",
  );
  equal(view.key, { Key: { key: t.headerHash } }, "linked list key");
  equal(
    view.next === "Empty" ? null : view.next.Key.key,
    headerRecord.nextHeaderHash,
    "linked list next",
  );
  text(headerRecord.queueOutRef, "queue outRef", OUT_REF);
  nullableText(headerRecord.nextHeaderHash, "next header", HEX28);
  for (const [a, b] of [
    ["observedTransactionHash", "transactionHash"],
    ["observedBlockHash", "blockHash"],
    ["observedBlockNo", "blockNo"],
    ["observedSlot", "slot"],
    ["observedChainPointId", "chainPointId"],
    ["finalityDepth", "finalityDepth"],
  ])
    equal(headerRecord[a!], inclusion[b!], `inclusion ${a}`);
  const observed = record(
    raw("authenticatedHeaderObservationCborHex"),
    [
      "schemaVersion",
      "sourceMode",
      "provenance",
      "chainPoint",
      "confirmationDepth",
      "headerHash",
      "header",
    ],
    "authenticated header description",
  );
  equal(
    observed.schemaVersion,
    SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    "SDK observation schema",
  );
  equal(observed.sourceMode, "local_node", "SDK observation source mode");
  provenance(observed.provenance, "authenticated_cardano_l1");
  equal(observed.header, header, "SDK observation HeaderV1");
  equal(observed.headerHash, t.headerHash, "SDK observation header hash");
  const chainPoint = record(
    observed.chainPoint,
    ["slot", "blockHash"],
    "SDK observation point",
  );
  equal(chainPoint.blockHash, inclusion.blockHash, "SDK block hash");
  equal(
    chainPoint.slot,
    BigInt(text(inclusion.slot, "inclusion slot")),
    "SDK slot",
  );
  equal(
    count(observed.confirmationDepth, "SDK confirmation depth").toString(),
    inclusion.finalityDepth,
    "SDK depth",
  );
  const ruleBundle = parseWatcherRuleBundle(raw("ruleBundleCborHex"));
  equal(
    computeWatcherRuleBundleCommitment(ruleBundle),
    t.ruleBundleCommitment,
    "rule bundle commitment",
  );
  equal(
    ruleBundle.deploymentManifestId,
    t.deploymentFingerprint,
    "rule deployment",
  );
  requireCondition(
    BigInt(text(inclusion.finalityDepth, "inclusion depth")) >=
      BigInt(
        count(minimumConfirmationDepth, "current release confirmation depth"),
      ),
    "persisted release depth",
  );
  const envelopeHex = text(t.payloadEnvelopeCborHex, "payload envelope", HEX);
  const envelope = Buffer.from(envelopeHex, "hex");
  equal(sha256(envelope), t.payloadEnvelopeSha256, "payload envelope hash");
  const reconstructed = await reconstructDaPayload({
    payloadEnvelopeCbor: envelope,
    expectedHeaderHash: text(t.headerHash, "headerHash"),
    committedHeader: header,
  });
  equal(sha256(reconstructed.payloadCbor), t.payloadSha256, "payload hash");
  equal(prior.root, header.prevUtxosRoot, "prior state root");
  const w22 = parseW22(raw("reconstructionRecordCborHex"));
  const w24 = parseW24(raw("phaseARecordCborHex"));
  const w25 = parseW25(raw("blockReplayRecordCborHex"));
  for (const r of [w22, w24, w25]) {
    equal(r.headerHash, t.headerHash, "result header");
    equal(r.payloadEnvelopeSha256, t.payloadEnvelopeSha256, "result envelope");
    equal(r.payloadSha256, t.payloadSha256, "result payload");
  }
  for (const r of [w24, w25]) {
    equal(
      r.reconstructionDigest,
      w22.resultDigest,
      "result reconstruction digest",
    );
    equal(r.ruleBundleCommitment, t.ruleBundleCommitment, "result rule bundle");
  }
  equal(w25.phaseAResultDigest, w24.resultDigest, "W25 Phase A digest");
  equal(w25.resultDigest, t.blockReplayResultDigest, "W25 result digest");
  equal(w25.priorStateRoot, prior.root, "W25 prior root");
  equal(
    w25.expectedPriorStateRoot,
    header.prevUtxosRoot,
    "W25 expected prior root",
  );
  equal(w25.expectedPostStateRoot, header.utxosRoot, "W25 expected post root");
  const events = list(t.eventAuthorityRecordsCborHex, "event records").map(
    (value) =>
      parseAuthority(
        decodeWatcherReplayRawRecord(text(value, "event record CBOR", HEX)),
      ),
  );
  const claims: WatcherCommittedEventClaim[] = [
    ...reconstructed.deposits.map((entry) => ({
      phase: "Deposit" as const,
      eventIdCborHex: entry.keyBytes.toString("hex"),
      valueCborHex: entry.valueBytes.toString("hex"),
      canonicalNativeTxCborHex: null,
    })),
    ...reconstructed.withdrawals.map((entry) => ({
      phase: "Withdrawal" as const,
      eventIdCborHex: entry.keyBytes.toString("hex"),
      valueCborHex: entry.valueBytes.toString("hex"),
      canonicalNativeTxCborHex: null,
    })),
    ...reconstructed.forcedTransactions.map((entry) => ({
      phase: "ForcedTransaction" as const,
      eventIdCborHex: entry.keyBytes.toString("hex"),
      valueCborHex: entry.valueBytes.toString("hex"),
      canonicalNativeTxCborHex: entry.fullTransactionCbor.toString("hex"),
    })),
  ];
  equal(events.length, claims.length, "event authority count");
  const steps = watcherBlockReplayCommittedSteps({
    transitionTrace: reconstructed.transitionTrace,
    eventToStep: reconstructed.eventToStep,
  });
  equal(
    events.map(
      (event) =>
        watcherBlockReplayEventAuthorityManifest(event).eventKeyFingerprint,
    ),
    steps
      .filter((step) => step.phase !== "L2Transaction")
      .map((step) => step.eventKeyFingerprint),
    "event authority order",
  );
  for (const event of events) {
    const matching = claims.filter(
      (claim) =>
        claim.phase === event.phase &&
        claim.eventIdCborHex === event.event.eventId,
    );
    equal(matching, [event.committedClaim], "event DA claim");
    equal(event.network, ruleBundle.network, "event network");
    if (event.origin.source === "local_publication") {
      equal(
        event.origin.deploymentManifestId,
        t.deploymentFingerprint,
        "event deployment",
      );
      equal(
        event.origin.blueprintHash,
        ruleBundle.blueprintHash,
        "event release",
      );
      const cutoff = event.origin.throughHeader;
      if (cutoff !== null) {
        for (const key of [
          "headerHash",
          "headerCborHex",
          "queueOutRef",
          "observedTransactionHash",
          "observedBlockHash",
          "observedSlot",
          "observedBlockNo",
        ] as const) {
          equal(cutoff[key], headerRecord[key], `event cutoff ${key}`);
        }
      }
    }
  }
  equal(
    w25.authorityManifestDigest,
    watcherSha256CanonicalJson(
      events.map(watcherBlockReplayEventAuthorityManifest),
    ),
    "W25 authority manifest",
  );
  equal(
    w25.sourceManifestDigest,
    watcherSha256CanonicalJson(
      steps.map(
        ({
          stepIndex,
          phase,
          eventKeyFingerprint,
          preRoot,
          postRoot,
          eventToStepIndex,
          eventToStepPhase,
        }) => ({
          stepIndex,
          phase,
          eventKeyFingerprint,
          preRoot,
          postRoot,
          eventToStepIndex,
          eventToStepPhase,
        }),
      ),
    ),
    "W25 source manifest",
  );
  equal(
    w25.effectManifestDigest,
    watcherSha256CanonicalJson(
      events.map((event) => ({
        phase: event.phase,
        eventKeyFingerprint:
          watcherBlockReplayEventAuthorityManifest(event).eventKeyFingerprint,
        effectDigest: event.transitionEffect.digest,
        effectCborSha256: sha256(
          Buffer.from(event.transitionEffect.canonicalCborHex, "hex"),
        ),
        operations: event.transitionEffect.operations.map((op) => ({
          type: op.type,
          outRefCbor: op.outRefCborHex,
          ...(op.type === "insert"
            ? { outputCborSha256: sha256(Buffer.from(op.outputCborHex, "hex")) }
            : {}),
        })),
      })),
    ),
    "W25 effect manifest",
  );
  // This cast follows exact structural/content validation. The returned record is
  // descriptive and is deliberately never inserted into an authority WeakSet.
  return {
    transcript: t as unknown as WatcherAuthenticatedReplayTranscript,
    headerCborHex,
    reconstruction: w22,
    phaseA: w24,
    blockReplay: w25,
    events,
  };
};

/** Explicit equality surface: source freshness stays with its own capture. */
export const watcherReplayTranscriptSemanticProjection = (
  records: WatcherReplayTranscriptRecords,
): unknown => {
  const { transcript: t, blockReplay: w25 } = records;
  return {
    schemaVersion: t.schemaVersion,
    deploymentFingerprint: t.deploymentFingerprint,
    headerHash: t.headerHash,
    headerCborHex: records.headerCborHex,
    inclusionPoint: {
      transactionHash: t.inclusionPoint.transactionHash,
      blockHash: t.inclusionPoint.blockHash,
      blockNo: t.inclusionPoint.blockNo,
      slot: t.inclusionPoint.slot,
      chainPointId: t.inclusionPoint.chainPointId,
    },
    coordinate: t.coordinate,
    payloadEnvelopeCborHex: t.payloadEnvelopeCborHex,
    payloadEnvelopeSha256: t.payloadEnvelopeSha256,
    payloadSha256: t.payloadSha256,
    priorState: t.priorState,
    ruleBundleCborHex: t.ruleBundleCborHex,
    ruleBundleCommitment: t.ruleBundleCommitment,
    reconstruction: records.reconstruction,
    phaseA: records.phaseA,
    blockReplay: {
      schemaVersion: w25.schemaVersion,
      action: w25.action,
      reasonCodes: w25.reasonCodes,
      verifiedRequires: w25.verifiedRequires,
      rejectionSelection: w25.rejectionSelection,
      consensusProfileId: w25.consensusProfileId,
      headerHash: w25.headerHash,
      payloadEnvelopeSha256: w25.payloadEnvelopeSha256,
      payloadSha256: w25.payloadSha256,
      reconstructionDigest: w25.reconstructionDigest,
      phaseAResultDigest: w25.phaseAResultDigest,
      ruleBundleCommitment: w25.ruleBundleCommitment,
      sourceManifestDigest: w25.sourceManifestDigest,
      effectManifestDigest: w25.effectManifestDigest,
      priorStateRoot: w25.priorStateRoot,
      expectedPriorStateRoot: w25.expectedPriorStateRoot,
      postStateRoot: w25.postStateRoot,
      expectedPostStateRoot: w25.expectedPostStateRoot,
      transactionCount: w25.transactionCount,
      acceptedCount: w25.acceptedCount,
      acceptedTxIds: w25.acceptedTxIds,
      intermediateRoots: w25.intermediateRoots,
      transactionRoots: w25.transactionRoots,
      eventRoots: w25.eventRoots,
      forcedValidationFacts: w25.forcedValidationFacts,
      stageMismatches: w25.stageMismatches,
      rejections: w25.rejections,
      selectedRejection: w25.selectedRejection,
      downstreamPrerequisite: {
        schemaVersion: w25.downstreamPrerequisite.schemaVersion,
        requiredVerifier: w25.downstreamPrerequisite.requiredVerifier,
        w29Eligibility: w25.downstreamPrerequisite.w29Eligibility,
      },
    },
    events: records.events.map((event) => ({
      phase: event.phase,
      eventKey: event.eventKey,
      event: {
        kind: event.event.kind,
        eventId: event.event.eventId,
        outRef: event.event.outRef,
        transactionHash: event.event.transactionHash,
        outputIndex: event.event.outputIndex,
        nonceOutRef: event.event.nonceOutRef,
        policyId: event.event.policyId,
        spendScriptHash: event.event.spendScriptHash,
        addressHex: event.event.addressHex,
        assetNameHex: event.event.assetNameHex,
        witnessScriptHash: event.event.witnessScriptHash,
        inclusionTime: event.event.inclusionTime,
        eventCborHex: event.event.eventCborHex,
        datumCborHex: event.event.datumCborHex,
        outputCborHex: event.event.outputCborHex,
        eventContentDigest: event.event.eventContentDigest,
        datumDigest: event.event.datumDigest,
        outputDigest: event.event.outputDigest,
        originBlockHash: event.event.originBlockHash,
        originSlot: event.event.originSlot,
        originBlockNo: event.event.originBlockNo,
        finalityStatus: event.event.finalityStatus,
        // Capture-specific point digests are verified within each record's
        // authority manifest. A fresh local owner regenerates those digests.
        ...("terminalStatus" in event.event
          ? {
              terminalStatus: event.event.terminalStatus,
              terminalTransactionHash: event.event.terminalTransactionHash,
              terminalBlockHash: event.event.terminalBlockHash,
              terminalSlot: event.event.terminalSlot,
              terminalBlockNo: event.event.terminalBlockNo,
              terminalFinalityStatus: event.event.terminalFinalityStatus,
              ...(event.event.terminalClassification === undefined
                ? {}
                : {
                    terminalClassification: {
                      schemaVersion:
                        event.event.terminalClassification.schemaVersion,
                      operatorValidity:
                        event.event.terminalClassification.operatorValidity,
                      terminalTransactionHash:
                        event.event.terminalClassification
                          .terminalTransactionHash,
                    },
                  }),
            }
          : {}),
      },
      network: event.network,
      committedClaim: event.committedClaim,
      canonicalNativeTxCborHex: event.canonicalNativeTxCborHex,
      programMaterialSidecarCborHex: event.programMaterialSidecarCborHex,
      transitionEffect: event.transitionEffect,
      deploymentManifestId:
        event.origin.source === "local_publication"
          ? event.origin.deploymentManifestId
          : t.deploymentFingerprint,
      throughHeader:
        event.origin.source === "local_publication" &&
        event.origin.throughHeader !== null
          ? {
              headerHash: event.origin.throughHeader.headerHash,
              headerCborHex: event.origin.throughHeader.headerCborHex,
              queueOutRef: event.origin.throughHeader.queueOutRef,
              observedTransactionHash:
                event.origin.throughHeader.observedTransactionHash,
              observedBlockHash: event.origin.throughHeader.observedBlockHash,
              observedSlot: event.origin.throughHeader.observedSlot,
              observedBlockNo: event.origin.throughHeader.observedBlockNo,
              transactionIndex: event.origin.throughHeader.transactionIndex,
            }
          : null,
    })),
  };
};
