import {
  CANONICAL_NATURAL,
  CANONICAL_POSITIVE,
  exactLiteral,
  exactRecord,
  exactString,
  fail,
  HEX_32,
  parsePayload,
  STABLE_NAME,
} from "./durable-store.canonical-json.js";
import {
  parseProtocolUtxo,
  type RecordParser,
  WATCHER_BLOCK_DECISIONS,
  WATCHER_DEADLINE_KINDS,
  type WatcherBlockDecision,
  type WatcherConfirmation,
  type WatcherDaProofInput,
  type WatcherDeadline,
  type WatcherFault,
  type WatcherReconstructedState,
  type WatcherRetry,
  type WatcherSpentProtocolUtxo,
  type WatcherSubmission,
} from "./durable-store.parse-l1-observation.js";

export const parseSpentProtocolUtxo: RecordParser<WatcherSpentProtocolUtxo> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, [
    "outRef",
    "role",
    "chainPointId",
    "output",
    "spentAtChainPointId",
  ]);
  const active = parseProtocolUtxo(
    {
      outRef: record.outRef,
      role: record.role,
      chainPointId: record.chainPointId,
      output: record.output,
    },
    path,
  );
  return {
    ...active,
    spentAtChainPointId: exactString(
      record.spentAtChainPointId,
      `${path}.spentAtChainPointId`,
      HEX_32,
    ),
  };
};

export const parseDaProofInput: RecordParser<WatcherDaProofInput> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, ["inputId", "kind", "payload"]);
  return {
    inputId: exactString(record.inputId, `${path}.inputId`, HEX_32),
    kind: exactLiteral(record.kind, `${path}.kind`, [
      "da_payload",
      "proof_input",
    ]),
    payload: parsePayload(record.payload, `${path}.payload`),
  };
};

const parseSortedDigests = (
  value: unknown,
  path: string,
): readonly string[] => {
  if (!Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const members = value as readonly unknown[];
  const digests = members.map((member, index) =>
    exactString(member, `${path}[${index.toString()}]`, HEX_32),
  );
  for (let index = 1; index < digests.length; index += 1) {
    const previous = digests[index - 1] as string;
    const current = digests[index] as string;
    if (current === previous) {
      fail("duplicate_key", `${path}[${index.toString()}]`);
    }
    if (current < previous) {
      fail("unsorted_records", path);
    }
  }
  return digests;
};

export const parseReconstructedState: RecordParser<
  WatcherReconstructedState
> = (value, path) => {
  const record = exactRecord(value, path, [
    "blockHash",
    "chainPointId",
    "priorStateRoot",
    "postStateRoot",
    "inputIds",
    "state",
  ]);
  return {
    blockHash: exactString(record.blockHash, `${path}.blockHash`, HEX_32),
    chainPointId: exactString(
      record.chainPointId,
      `${path}.chainPointId`,
      HEX_32,
    ),
    priorStateRoot: exactString(
      record.priorStateRoot,
      `${path}.priorStateRoot`,
      HEX_32,
    ),
    postStateRoot: exactString(
      record.postStateRoot,
      `${path}.postStateRoot`,
      HEX_32,
    ),
    inputIds: parseSortedDigests(record.inputIds, `${path}.inputIds`),
    state: parsePayload(record.state, `${path}.state`),
  };
};

export const parseDecision: RecordParser<WatcherBlockDecision> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, [
    "blockHash",
    "decision",
    "reconstructionDigest",
    "evidenceDigest",
  ]);
  return {
    blockHash: exactString(record.blockHash, `${path}.blockHash`, HEX_32),
    decision: exactLiteral(
      record.decision,
      `${path}.decision`,
      WATCHER_BLOCK_DECISIONS,
    ),
    reconstructionDigest: exactString(
      record.reconstructionDigest,
      `${path}.reconstructionDigest`,
      HEX_32,
    ),
    evidenceDigest: exactString(
      record.evidenceDigest,
      `${path}.evidenceDigest`,
      HEX_32,
    ),
  };
};

export const parseFault: RecordParser<WatcherFault> = (value, path) => {
  const record = exactRecord(value, path, [
    "faultId",
    "blockHash",
    "familyId",
    "evidence",
  ]);
  return {
    faultId: exactString(record.faultId, `${path}.faultId`, HEX_32),
    blockHash: exactString(record.blockHash, `${path}.blockHash`, HEX_32),
    familyId: exactString(record.familyId, `${path}.familyId`, STABLE_NAME),
    evidence: parsePayload(record.evidence, `${path}.evidence`),
  };
};

export const parseSubmission: RecordParser<WatcherSubmission> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, [
    "submissionId",
    "faultId",
    "txBodyHash",
    "status",
  ]);
  return {
    submissionId: exactString(
      record.submissionId,
      `${path}.submissionId`,
      HEX_32,
    ),
    faultId: exactString(record.faultId, `${path}.faultId`, HEX_32),
    txBodyHash: exactString(record.txBodyHash, `${path}.txBodyHash`, HEX_32),
    status: exactLiteral(record.status, `${path}.status`, [
      "prepared",
      "submitted",
      "ambiguous",
    ]),
  };
};

export const parseConfirmation: RecordParser<WatcherConfirmation> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, [
    "confirmationId",
    "submissionId",
    "txHash",
    "chainPointId",
    "depth",
    "status",
  ]);
  return {
    confirmationId: exactString(
      record.confirmationId,
      `${path}.confirmationId`,
      HEX_32,
    ),
    submissionId: exactString(
      record.submissionId,
      `${path}.submissionId`,
      HEX_32,
    ),
    txHash: exactString(record.txHash, `${path}.txHash`, HEX_32),
    chainPointId: exactString(
      record.chainPointId,
      `${path}.chainPointId`,
      HEX_32,
    ),
    depth: exactString(record.depth, `${path}.depth`, CANONICAL_NATURAL),
    status: exactLiteral(record.status, `${path}.status`, [
      "observed",
      "confirmed",
      "rolled_back",
    ]),
  };
};

export const parseRetry: RecordParser<WatcherRetry> = (value, path) => {
  const record = exactRecord(value, path, [
    "retryId",
    "submissionId",
    "attempt",
    "nextEligibleSlot",
    "reason",
  ]);
  return {
    retryId: exactString(record.retryId, `${path}.retryId`, HEX_32),
    submissionId: exactString(
      record.submissionId,
      `${path}.submissionId`,
      HEX_32,
    ),
    attempt: exactString(record.attempt, `${path}.attempt`, CANONICAL_POSITIVE),
    nextEligibleSlot: exactString(
      record.nextEligibleSlot,
      `${path}.nextEligibleSlot`,
      CANONICAL_NATURAL,
    ),
    reason: exactLiteral(record.reason, `${path}.reason`, [
      "provider_unavailable",
      "submission_ambiguous",
      "confirmation_timeout",
      "rollback",
      "topology_changed",
    ]),
  };
};

export const parseDeadline: RecordParser<WatcherDeadline> = (value, path) => {
  const record = exactRecord(value, path, [
    "deadlineId",
    "subjectKind",
    "subjectId",
    "kind",
    "expiresAtSlot",
  ]);
  return {
    deadlineId: exactString(record.deadlineId, `${path}.deadlineId`, HEX_32),
    subjectKind: exactLiteral(record.subjectKind, `${path}.subjectKind`, [
      "fault",
      "submission",
    ]),
    subjectId: exactString(record.subjectId, `${path}.subjectId`, HEX_32),
    kind: exactLiteral(record.kind, `${path}.kind`, WATCHER_DEADLINE_KINDS),
    expiresAtSlot: exactString(
      record.expiresAtSlot,
      `${path}.expiresAtSlot`,
      CANONICAL_NATURAL,
    ),
  };
};
