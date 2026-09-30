import {
  watcherBlockReplayDownstreamInputDigest,
  type WatcherBlockReplayResult,
} from "./block-replay.js";
import {
  WATCHER_HEADER_COUNT_FIELDS,
  WATCHER_HEADER_ROOT_FIELDS,
} from "./header-root-reconstruction.js";
import {
  count,
  countFields,
  equal,
  EVENT_ROOT_KEYS,
  FACT_KEYS,
  HEX,
  HEX28,
  HEX32,
  list,
  NATURAL,
  nullableText,
  record,
  REJECTION_KEYS,
  requireCondition,
  resultDigest,
  ROOT_KEYS,
  stringFields,
  stringList,
  text,
  TX_ROOT_KEYS,
  W25_KEYS,
} from "./replay-transcript-records.w25-keys.js";

export const parseW25 = (value: unknown): WatcherBlockReplayResult => {
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

export const parseW22 = (value: unknown): Record<string, unknown> => {
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

export const parseW24 = (value: unknown): Record<string, unknown> => {
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

export const EVENT_KEYS = [
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
