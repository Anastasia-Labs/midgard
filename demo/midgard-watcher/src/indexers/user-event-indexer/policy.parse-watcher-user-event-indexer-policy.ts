import {
  type DeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { EVENT_WAIT_DURATION_MS } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import {
  parseWatcherCustomNetwork,
  type WatcherCustomNetwork,
} from "../../runtime/custom-network.js";
import {
  exactRecord,
  isHex28,
  isHex32,
  isHexBytes,
  isNatural,
  isNetwork,
  parseForcedTerminalClassification,
  sha256Canonical,
} from "./policy.evidence-within-bounds.js";
import {
  type EventPolicyFields,
  type PlainRecord,
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION,
  type WatcherUserEventIndexerPolicy,
  type WatcherUserEventKind,
} from "./types.js";

export const snapshotTerminalClassificationsAreExact = (
  value: unknown,
): boolean => {
  if (typeof value !== "object" || value === null) {
    return false;
  }
  const snapshot = value as PlainRecord;
  if (
    !Array.isArray(snapshot.activeEvents) ||
    !Array.isArray(snapshot.terminalEvents)
  ) {
    return false;
  }
  for (const event of snapshot.activeEvents) {
    if (
      typeof event !== "object" ||
      event === null ||
      Object.hasOwn(event, "terminalClassification")
    ) {
      return false;
    }
  }
  for (const event of snapshot.terminalEvents) {
    if (typeof event !== "object" || event === null) {
      return false;
    }
    const terminal = event as PlainRecord;
    const mustHaveClassification =
      terminal.kind === "forced_order" &&
      terminal.terminalStatus === "processed";
    const hasClassification = Object.hasOwn(terminal, "terminalClassification");
    if (mustHaveClassification !== hasClassification) {
      return false;
    }
    if (!hasClassification) {
      continue;
    }
    const classification = parseForcedTerminalClassification(
      terminal.terminalClassification,
    );
    if (
      classification === null ||
      classification.terminalTransactionHash !==
        terminal.terminalTransactionHash ||
      classification.terminalPointDigest !== terminal.terminalPointDigest
    ) {
      return false;
    }
  }
  return true;
};

export const cloneMarker = (value: unknown): DeploymentMarker | null => {
  const record = exactRecord(value, ["schemaVersion", "manifestId"]);
  if (
    record === null ||
    record.schemaVersion !== MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION ||
    !isHex32(record.manifestId)
  ) {
    return null;
  }
  return Object.freeze({
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
    manifestId: record.manifestId,
  });
};

const policyFields = (value: unknown): EventPolicyFields | null => {
  const record = exactRecord(value, [
    "policyId",
    "spendScriptHash",
    "addressHex",
  ]);
  if (
    record === null ||
    !isHex28(record.policyId) ||
    !isHex28(record.spendScriptHash) ||
    !isHexBytes(record.addressHex) ||
    record.addressHex.length === 0
  ) {
    return null;
  }
  try {
    const address = CML.Address.from_hex(record.addressHex);
    if (
      address.payment_cred()?.as_script()?.to_hex() !== record.spendScriptHash
    ) {
      return null;
    }
  } catch {
    return null;
  }
  return Object.freeze({
    policyId: record.policyId,
    spendScriptHash: record.spendScriptHash,
    addressHex: record.addressHex,
  });
};

const policyWithoutDigest = (
  value: Omit<WatcherUserEventIndexerPolicy, "policyDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION,
  network: value.network,
  ...(value.customNetwork === undefined
    ? {}
    : { customNetwork: value.customNetwork }),
  blueprintHash: value.blueprintHash,
  deploymentMarker: value.deploymentMarker,
  deposit: value.deposit,
  withdrawal: value.withdrawal,
  forcedOrder: value.forcedOrder,
  bootstrapStoreDigest: value.bootstrapStoreDigest,
  deploymentTrustRootId: value.deploymentTrustRootId,
  eventWaitDurationMs: value.eventWaitDurationMs,
  requiredFinalityDepth: value.requiredFinalityDepth,
  maximumActiveHistoryEntries: value.maximumActiveHistoryEntries,
  maximumAuditHistoryEntries: value.maximumAuditHistoryEntries,
});

export const makeWatcherUserEventIndexerPolicy = (
  value: Omit<
    WatcherUserEventIndexerPolicy,
    "schemaVersion" | "policyDigest" | "eventWaitDurationMs"
  >,
): WatcherUserEventIndexerPolicy | null => {
  const canonical = policyWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION,
    ...value,
    eventWaitDurationMs: EVENT_WAIT_DURATION_MS.toString(),
  });
  return parseWatcherUserEventIndexerPolicy({
    ...canonical,
    policyDigest: sha256Canonical(canonical),
  });
};

export const parseWatcherUserEventIndexerPolicy = (
  value: unknown,
): WatcherUserEventIndexerPolicy | null => {
  const custom =
    typeof value === "object" &&
    value !== null &&
    Object.getOwnPropertyDescriptor(value, "network")?.value === "Custom";
  const record = exactRecord(value, [
    "schemaVersion",
    "network",
    ...(custom ? ["customNetwork"] : []),
    "blueprintHash",
    "deploymentMarker",
    "deposit",
    "withdrawal",
    "forcedOrder",
    "bootstrapStoreDigest",
    "deploymentTrustRootId",
    "eventWaitDurationMs",
    "requiredFinalityDepth",
    "maximumActiveHistoryEntries",
    "maximumAuditHistoryEntries",
    "policyDigest",
  ]);
  const marker = record === null ? null : cloneMarker(record.deploymentMarker);
  const deposit = record === null ? null : policyFields(record.deposit);
  const withdrawal = record === null ? null : policyFields(record.withdrawal);
  const forcedOrder = record === null ? null : policyFields(record.forcedOrder);
  if (
    record === null ||
    marker === null ||
    deposit === null ||
    withdrawal === null ||
    forcedOrder === null ||
    record.schemaVersion !== WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION ||
    !isNetwork(record.network) ||
    !isHex32(record.blueprintHash) ||
    !isHex32(record.bootstrapStoreDigest) ||
    !isHex32(record.deploymentTrustRootId) ||
    !isNatural(record.eventWaitDurationMs) ||
    record.eventWaitDurationMs !== EVENT_WAIT_DURATION_MS.toString() ||
    !isNatural(record.requiredFinalityDepth) ||
    BigInt(record.requiredFinalityDepth) === 0n ||
    BigInt(record.requiredFinalityDepth) >
      BigInt(WATCHER_USER_EVENT_INDEXER_BOUNDS.requiredFinalityDepthMaximum) ||
    !isNatural(record.maximumActiveHistoryEntries) ||
    BigInt(record.maximumActiveHistoryEntries) === 0n ||
    BigInt(record.maximumActiveHistoryEntries) >
      BigInt(WATCHER_USER_EVENT_INDEXER_BOUNDS.activeHistoryEntries) ||
    !isNatural(record.maximumAuditHistoryEntries) ||
    BigInt(record.maximumAuditHistoryEntries) === 0n ||
    BigInt(record.maximumAuditHistoryEntries) >
      BigInt(WATCHER_USER_EVENT_INDEXER_BOUNDS.auditHistoryEntries) ||
    BigInt(record.maximumAuditHistoryEntries) <
      BigInt(record.maximumActiveHistoryEntries) ||
    !isHex32(record.policyDigest) ||
    new Set([deposit.policyId, withdrawal.policyId, forcedOrder.policyId])
      .size !== 3
  ) {
    return null;
  }
  const expectedNetworkId = record.network === "Mainnet" ? 1 : 0;
  for (const fields of [deposit, withdrawal, forcedOrder]) {
    try {
      if (
        CML.Address.from_hex(fields.addressHex).network_id() !==
        expectedNetworkId
      ) {
        return null;
      }
    } catch {
      return null;
    }
  }
  let customNetwork: WatcherCustomNetwork | undefined;
  if (custom) {
    try {
      customNetwork = parseWatcherCustomNetwork(record.customNetwork);
    } catch {
      return null;
    }
  }
  const canonical = policyWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_INDEXER_POLICY_SCHEMA_VERSION,
    network: record.network,
    ...(customNetwork === undefined ? {} : { customNetwork }),
    blueprintHash: record.blueprintHash,
    deploymentMarker: marker,
    deposit,
    withdrawal,
    forcedOrder,
    bootstrapStoreDigest: record.bootstrapStoreDigest,
    deploymentTrustRootId: record.deploymentTrustRootId,
    eventWaitDurationMs: record.eventWaitDurationMs,
    requiredFinalityDepth: record.requiredFinalityDepth,
    maximumActiveHistoryEntries: record.maximumActiveHistoryEntries,
    maximumAuditHistoryEntries: record.maximumAuditHistoryEntries,
  });
  if (sha256Canonical(canonical) !== record.policyDigest) {
    return null;
  }
  return Object.freeze({ ...canonical, policyDigest: record.policyDigest });
};

export const eventPolicy = (
  policy: WatcherUserEventIndexerPolicy,
  kind: WatcherUserEventKind,
): EventPolicyFields =>
  kind === "deposit"
    ? policy.deposit
    : kind === "withdrawal"
      ? policy.withdrawal
      : policy.forcedOrder;

export const kindForPolicy = (
  policy: WatcherUserEventIndexerPolicy,
  policyId: string,
): WatcherUserEventKind | null =>
  policy.deposit.policyId === policyId
    ? "deposit"
    : policy.withdrawal.policyId === policyId
      ? "withdrawal"
      : policy.forcedOrder.policyId === policyId
        ? "forced_order"
        : null;
