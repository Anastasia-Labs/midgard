import { getAddressDetails } from "@lucid-evolution/lucid";

import {
  requireExactKeys,
  requireFinalOutRef,
  requireHex,
  requireRecord,
  requireString,
} from "./primitives.js";
import { type DeploymentManifestEventHistoryBounds } from "./types.js";

export const parseDeploymentManifestEventHistoryBounds = (
  value: unknown,
  field = "eventHistoryBounds",
): DeploymentManifestEventHistoryBounds => {
  const record = requireRecord(value, `Deployment manifest ${field}`);
  requireExactKeys(
    record,
    ["inlineLimitBytes", "maxPayloadBytes", "maxPayloadNodes"],
    [],
    field,
  );
  const integer = (key: string): string => {
    const value = record[key];
    if (
      typeof value !== "string" ||
      !/^[1-9][0-9]{0,15}$/u.test(value) ||
      BigInt(value) > BigInt(Number.MAX_SAFE_INTEGER)
    )
      throw new Error(
        `Deployment manifest ${field}.${key} must be a positive canonical safe integer string`,
      );
    return value;
  };
  const bounds = {
    inlineLimitBytes: integer("inlineLimitBytes"),
    maxPayloadBytes: integer("maxPayloadBytes"),
    maxPayloadNodes: integer("maxPayloadNodes"),
  };
  if (BigInt(bounds.inlineLimitBytes) > BigInt(bounds.maxPayloadBytes))
    throw new Error(
      `Deployment manifest ${field} inline bound exceeds total payload bound`,
    );
  return Object.freeze(bounds);
};

/** Exact list deployment parameters, included in the manifest identity. */
export type DeploymentManifestEventHistoryRecipe = Readonly<{
  kind: "Deposit" | "Withdrawal";
  hubPolicyId: string;
  initializationNonce: Readonly<{ txHash: string; outputIndex: number }>;
  protectionDurationMs: string;
  bounds: DeploymentManifestEventHistoryBounds;
}>;

export const parseDeploymentManifestEventHistoryRecipe = (
  value: unknown,
  field = "eventHistoryRecipe",
): DeploymentManifestEventHistoryRecipe => {
  const record = requireRecord(value, `Deployment manifest ${field}`);
  requireExactKeys(
    record,
    [
      "kind",
      "hubPolicyId",
      "initializationNonce",
      "protectionDurationMs",
      "bounds",
    ],
    [],
    field,
  );
  if (record.kind !== "Deposit" && record.kind !== "Withdrawal")
    throw new Error(`Deployment manifest ${field}.kind is invalid`);
  const hubPolicyId = requireHex(
    record.hubPolicyId,
    28,
    `${field}.hubPolicyId`,
  );
  const nonce = requireFinalOutRef(
    record.initializationNonce,
    `${field}.initializationNonce`,
  );
  const protectionDurationMs = record.protectionDurationMs;
  if (
    typeof protectionDurationMs !== "string" ||
    !/^[1-9][0-9]{0,15}$/u.test(protectionDurationMs) ||
    BigInt(protectionDurationMs) > BigInt(Number.MAX_SAFE_INTEGER)
  )
    throw new Error(
      `Deployment manifest ${field}.protectionDurationMs must be a positive canonical safe integer string`,
    );
  return Object.freeze({
    kind: record.kind,
    hubPolicyId,
    initializationNonce: Object.freeze(nonce),
    protectionDurationMs,
    bounds: parseDeploymentManifestEventHistoryBounds(
      record.bounds,
      `${field}.bounds`,
    ),
  });
};

export const parseDeploymentManifestEventHistoryRetentionAddress = (
  value: unknown,
): string => {
  const address = requireString(value, "eventHistoryRetentionAddress");
  if (getAddressDetails(address).paymentCredential?.type !== "Script")
    throw new Error(
      "Deployment manifest eventHistoryRetentionAddress must have a script payment credential",
    );
  return address;
};

export type DeploymentManifestEventHistoryRetentionAddresses = Readonly<{
  deposit: string;
  withdrawal: string;
}>;

export const parseDeploymentManifestEventHistoryRetentionAddresses = (
  value: unknown,
): DeploymentManifestEventHistoryRetentionAddresses => {
  const record = requireRecord(value, "eventHistoryRetentionAddresses");
  requireExactKeys(
    record,
    ["deposit", "withdrawal"],
    [],
    "eventHistoryRetentionAddresses",
  );
  return Object.freeze({
    deposit: parseDeploymentManifestEventHistoryRetentionAddress(
      record.deposit,
    ),
    withdrawal: parseDeploymentManifestEventHistoryRetentionAddress(
      record.withdrawal,
    ),
  });
};
