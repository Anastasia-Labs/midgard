import {
  type HistoryWindowOffer,
  parseHistoryWindowOffer,
} from "./history-child-evidence.js";
import { historyOfferAvailable } from "./history-offer-availability.js";
import {
  historyProofDeadline,
  historyProofRemaining,
} from "./history-proof-deadline.js";

export const HISTORY_LISTENER_PATH = "/midgard/v1/history-recovery-range";
export const HISTORY_LISTENER_SCHEMA = "history-listener-range-proof-v1";
export type HistoryListenerBinding = Readonly<{
  sourceId: string;
  operatorIdentitySha256: string;
  deploymentIdentityDigest: string;
  blueprintHash: string;
  policyDigest: string;
}>;
export type HistoryListenerChallenge = Readonly<{
  schema: typeof HISTORY_LISTENER_SCHEMA;
  challengeId: string;
  budgetMs: number;
  offer: HistoryWindowOffer;
}>;
const object = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);
const exact = (value: Record<string, unknown>, keys: readonly string[]) =>
  Object.keys(value).sort().join() === [...keys].sort().join();
export const parseHistoryListenerBinding = (
  value: unknown,
): HistoryListenerBinding | null => {
  const hex = (field: unknown): field is string =>
    typeof field === "string" && /^[0-9a-f]{64}$/u.test(field);
  if (
    !object(value) ||
    !exact(value, [
      "sourceId",
      "operatorIdentitySha256",
      "deploymentIdentityDigest",
      "blueprintHash",
      "policyDigest",
    ]) ||
    typeof value.sourceId !== "string" ||
    value.sourceId.length === 0 ||
    value.sourceId.length > 200 ||
    !hex(value.operatorIdentitySha256) ||
    !hex(value.deploymentIdentityDigest) ||
    !hex(value.blueprintHash) ||
    !hex(value.policyDigest)
  )
    return null;
  return {
    sourceId: value.sourceId,
    operatorIdentitySha256: value.operatorIdentitySha256,
    deploymentIdentityDigest: value.deploymentIdentityDigest,
    blueprintHash: value.blueprintHash,
    policyDigest: value.policyDigest,
  };
};
const uuid = (value: unknown): value is string =>
  typeof value === "string" &&
  /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/u.test(value);
export const parseHistoryListenerChallenge = (
  value: unknown,
): HistoryListenerChallenge | null => {
  if (
    !object(value) ||
    !exact(value, ["schema", "challengeId", "budgetMs", "offer"]) ||
    value.schema !== HISTORY_LISTENER_SCHEMA ||
    !uuid(value.challengeId) ||
    typeof value.budgetMs !== "number" ||
    !Number.isSafeInteger(value.budgetMs) ||
    value.budgetMs <= 0 ||
    value.budgetMs > 120_000
  )
    return null;
  const offer = parseHistoryWindowOffer(value.offer);
  return offer === null
    ? null
    : {
        schema: HISTORY_LISTENER_SCHEMA,
        challengeId: value.challengeId,
        budgetMs: value.budgetMs,
        offer,
      };
};
export const historyListenerAnswer = (input: {
  readonly directory: string;
  readonly binding: HistoryListenerBinding;
  readonly request: HistoryListenerChallenge;
  readonly maximumBudgetMs: number;
}) => {
  const { binding, request } = input;
  const deadline = historyProofDeadline(
    Math.min(request.budgetMs, input.maximumBudgetMs),
  );
  if (deadline === null || historyProofRemaining(deadline) === 0) return null;
  if (
    request.offer.window.actor.deploymentFingerprint !==
      binding.deploymentIdentityDigest ||
    !historyOfferAvailable([input.directory], request.offer) ||
    historyProofRemaining(deadline) === 0
  )
    return null;
  return {
    schema: HISTORY_LISTENER_SCHEMA,
    challengeId: request.challengeId,
    ...binding,
    offer: request.offer,
  };
};
export const historyListenerReplyMatches = (
  value: unknown,
  request: HistoryListenerChallenge,
  binding: HistoryListenerBinding,
): boolean => {
  if (
    !object(value) ||
    !exact(value, [
      "schema",
      "challengeId",
      "sourceId",
      "operatorIdentitySha256",
      "deploymentIdentityDigest",
      "blueprintHash",
      "policyDigest",
      "offer",
    ]) ||
    value.schema !== HISTORY_LISTENER_SCHEMA ||
    value.challengeId !== request.challengeId ||
    value.sourceId !== binding.sourceId ||
    value.operatorIdentitySha256 !== binding.operatorIdentitySha256 ||
    value.deploymentIdentityDigest !== binding.deploymentIdentityDigest ||
    value.blueprintHash !== binding.blueprintHash ||
    value.policyDigest !== binding.policyDigest
  )
    return false;
  const offered = parseHistoryWindowOffer(value.offer);
  return (
    offered !== null &&
    JSON.stringify(offered) === JSON.stringify(request.offer)
  );
};
