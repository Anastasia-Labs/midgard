import { WATCHER_CARDANO_SECURITY_PARAMETER_K } from "midgard-watcher";

import type {
  HistoryWindowActor,
  HistoryWindowSeal,
} from "./history-native-window-proof.js";
import {
  digest,
  type HistoryWindowPoint,
  point,
} from "./history-window-canonical.js";

export const HISTORY_CHILD_SCHEMA = "history-child-proof-v1";
export const HISTORY_CHILD_ROLES = [
  "history-recorder",
  "history-archive-a",
  "history-archive-b",
  "history-tunnel",
] as const;
export type HistoryChildRole = (typeof HISTORY_CHILD_ROLES)[number];
export type HistoryChildActor = Readonly<{
  role: HistoryChildRole;
  runId: string;
  deploymentFingerprint: string;
  codeStamp: string;
  serviceSpecsDigest: string;
  attemptId: string;
  childPid: number;
}>;
/** Availability is separately proven; a native promotion cannot recreate a file. */
export type HistoryWindowOffer = Readonly<{
  window: HistoryWindowSeal;
  predecessor: HistoryWindowPoint | null;
}>;
export type HistoryChildChallenge = Readonly<{
  schema: typeof HISTORY_CHILD_SCHEMA;
  challengeId: string;
  actor: HistoryChildActor;
  operation: "seal" | "revalidate" | "prove";
  budgetMs: number;
  offer: HistoryWindowOffer | null;
}>;
export type HistoryChildReply = Readonly<{
  schema: typeof HISTORY_CHILD_SCHEMA;
  challengeId: string;
  actor: HistoryChildActor;
  offer: HistoryWindowOffer | null;
}>;
const object = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);
const keys = (value: Record<string, unknown>, expected: readonly string[]) =>
  Object.keys(value).sort().join() === [...expected].sort().join();
const hex = (value: unknown): value is string =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);
const uuid = (value: unknown): value is string =>
  typeof value === "string" &&
  /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/u.test(value);
const actorKeys = [
  "role",
  "runId",
  "deploymentFingerprint",
  "codeStamp",
  "serviceSpecsDigest",
  "attemptId",
];
export const parseHistoryChildActor = (
  value: unknown,
): HistoryChildActor | null => {
  if (
    !object(value) ||
    !keys(value, [...actorKeys, "childPid"]) ||
    !HISTORY_CHILD_ROLES.some((role) => role === value.role) ||
    typeof value.runId !== "string" ||
    value.runId.length === 0 ||
    value.runId.length > 200 ||
    !hex(value.deploymentFingerprint) ||
    !hex(value.codeStamp) ||
    !hex(value.serviceSpecsDigest) ||
    !uuid(value.attemptId) ||
    typeof value.childPid !== "number" ||
    !Number.isSafeInteger(value.childPid) ||
    value.childPid <= 0
  )
    return null;
  const role = HISTORY_CHILD_ROLES.find((role) => role === value.role);
  if (role === undefined) return null;
  return Object.freeze({
    role,
    runId: value.runId,
    deploymentFingerprint: value.deploymentFingerprint,
    codeStamp: value.codeStamp,
    serviceSpecsDigest: value.serviceSpecsDigest,
    attemptId: value.attemptId,
    childPid: value.childPid,
  });
};
export const historyActorsMatch = (
  expected: HistoryChildActor,
  actual: HistoryChildActor,
): boolean =>
  expected.role === actual.role &&
  expected.runId === actual.runId &&
  expected.deploymentFingerprint === actual.deploymentFingerprint &&
  expected.codeStamp === actual.codeStamp &&
  expected.serviceSpecsDigest === actual.serviceSpecsDigest &&
  expected.attemptId === actual.attemptId &&
  expected.childPid === actual.childPid;
const offerScopeMatches = (
  owner: HistoryChildActor,
  offer: HistoryWindowOffer,
) =>
  owner.runId === offer.window.actor.runId &&
  owner.deploymentFingerprint === offer.window.actor.deploymentFingerprint &&
  owner.codeStamp === offer.window.actor.codeStamp &&
  owner.serviceSpecsDigest === offer.window.actor.serviceSpecsDigest &&
  (owner.role !== "history-recorder" ||
    owner.attemptId === offer.window.actor.attemptId);
const windowActor = (value: unknown): HistoryWindowActor | null => {
  if (!object(value) || !keys(value, actorKeys)) return null;
  const parsed = parseHistoryChildActor({ ...value, childPid: 1 });
  if (parsed === null || parsed.role !== "history-recorder") return null;
  return {
    role: parsed.role,
    runId: parsed.runId,
    deploymentFingerprint: parsed.deploymentFingerprint,
    codeStamp: parsed.codeStamp,
    serviceSpecsDigest: parsed.serviceSpecsDigest,
    attemptId: parsed.attemptId,
  };
};
const window = (value: unknown): HistoryWindowSeal | null => {
  if (
    !object(value) ||
    !keys(value, [
      "actor",
      "sourceEpoch",
      "authorityDigest",
      "startupDigest",
      "sealId",
      "promotionDigest",
      "first",
      "last",
      "rowCount",
      "recoveryWindow",
      "rangeDigest",
      "generation",
    ]) ||
    !uuid(value.sourceEpoch) ||
    !hex(value.authorityDigest) ||
    !hex(value.startupDigest) ||
    !uuid(value.sealId) ||
    !hex(value.promotionDigest) ||
    !hex(value.rangeDigest) ||
    !hex(value.generation) ||
    value.recoveryWindow !== WATCHER_CARDANO_SECURITY_PARAMETER_K ||
    typeof value.rowCount !== "number" ||
    !Number.isSafeInteger(value.rowCount) ||
    value.rowCount <= 0 ||
    value.rowCount > WATCHER_CARDANO_SECURITY_PARAMETER_K
  )
    return null;
  const owner = windowActor(value.actor);
  if (owner === null) return null;
  const first = point(value.first);
  const last = point(value.last);
  const required = BigInt(last.blockNo) + 1n;
  const count =
    required < BigInt(value.recoveryWindow)
      ? required
      : BigInt(value.recoveryWindow);
  if (
    count !== BigInt(value.rowCount) ||
    BigInt(first.blockNo) !== required - count ||
    BigInt(first.slot) > BigInt(last.slot)
  )
    return null;
  const fields = {
    actor: owner,
    sourceEpoch: value.sourceEpoch,
    authorityDigest: value.authorityDigest,
    startupDigest: value.startupDigest,
    sealId: value.sealId,
    promotionDigest: value.promotionDigest,
    first,
    last,
    rowCount: value.rowCount,
    recoveryWindow: value.recoveryWindow,
    rangeDigest: value.rangeDigest,
  };
  if (
    digest(JSON.stringify(["history-full-window-seal-v1", fields])) !==
    value.generation
  )
    return null;
  return { ...fields, generation: value.generation };
};
export const parseHistoryWindowOffer = (
  value: unknown,
): HistoryWindowOffer | null => {
  try {
    if (!object(value) || !keys(value, ["window", "predecessor"])) return null;
    const parsed = window(value.window);
    if (parsed === null) return null;
    const first = BigInt(parsed.first.blockNo);
    const predecessor =
      value.predecessor === null ? null : point(value.predecessor);
    if (
      (first === 0n && predecessor !== null) ||
      (first > 0n &&
        (predecessor === null ||
          BigInt(predecessor.blockNo) !== first - 1n ||
          BigInt(predecessor.slot) >= BigInt(parsed.first.slot)))
    )
      return null;
    return { window: parsed, predecessor };
  } catch {
    return null;
  }
};
export const parseHistoryChildChallenge = (
  value: unknown,
): HistoryChildChallenge | null => {
  if (
    !object(value) ||
    !keys(value, [
      "schema",
      "challengeId",
      "actor",
      "operation",
      "budgetMs",
      "offer",
    ]) ||
    value.schema !== HISTORY_CHILD_SCHEMA ||
    !uuid(value.challengeId) ||
    typeof value.budgetMs !== "number" ||
    !Number.isSafeInteger(value.budgetMs) ||
    value.budgetMs <= 0 ||
    value.budgetMs > 120_000
  )
    return null;
  const owner = parseHistoryChildActor(value.actor);
  const offered =
    value.offer === null ? null : parseHistoryWindowOffer(value.offer);
  if (
    owner === null ||
    (value.offer !== null && offered === null) ||
    (offered !== null && !offerScopeMatches(owner, offered))
  )
    return null;
  if (value.operation === "seal") {
    if (owner.role !== "history-recorder" || offered !== null) return null;
  } else if (value.operation === "revalidate") {
    if (owner.role !== "history-recorder" || offered === null) return null;
  } else if (
    value.operation !== "prove" ||
    owner.role === "history-recorder" ||
    offered === null
  )
    return null;
  return {
    schema: HISTORY_CHILD_SCHEMA,
    challengeId: value.challengeId,
    actor: owner,
    operation: value.operation,
    budgetMs: value.budgetMs,
    offer: offered,
  };
};
export const parseHistoryChildReply = (
  value: unknown,
  challengeId: string,
  expected: HistoryChildActor,
): HistoryChildReply | null => {
  if (
    !object(value) ||
    !keys(value, ["schema", "challengeId", "actor", "offer"]) ||
    value.schema !== HISTORY_CHILD_SCHEMA ||
    value.challengeId !== challengeId
  )
    return null;
  const owner = parseHistoryChildActor(value.actor);
  if (owner === null || !historyActorsMatch(expected, owner)) return null;
  const offered =
    value.offer === null ? null : parseHistoryWindowOffer(value.offer);
  if (
    (value.offer !== null && offered === null) ||
    (offered !== null && !offerScopeMatches(owner, offered))
  )
    return null;
  return {
    schema: HISTORY_CHILD_SCHEMA,
    challengeId,
    actor: owner,
    offer: offered,
  };
};
