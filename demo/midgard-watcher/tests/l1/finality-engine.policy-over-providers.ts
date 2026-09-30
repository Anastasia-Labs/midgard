import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  evaluateWatcherFinality,
  makeWatcherFinalityPolicy,
  watcherFinalityConfiguredSource,
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from "../../src/l1/finality-engine.js";
import {
  makeWatcherL1PublicBytes,
  normalizeWatcherL1Block,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from "../../src/l1/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistency as evaluateWatcherMultiProviderConsistencyRaw } from "../../src/l1/multi-provider-consistency.js";
import { sha256Canonical as sha256CanonicalForTest } from "../support/canonical-json.js";
import {
  config,
  deploymentIdentity,
  externalEndpoints,
  externalSource,
  hex32,
  observationAttestations,
  transportContexts,
} from "./finality-engine.config.js";

// A policy over an explicit configured provider set, so a test can name a
// provider allowlist that is strictly wider than the set a W11 record binds.
export const policyOverProviders = (
  providers: readonly (readonly [string, string])[],
  depth = 3,
): WatcherFinalityPolicy => {
  const base = config(depth, depth);
  const value = makeWatcherFinalityPolicy(
    {
      ...base,
      l1: {
        ...base.l1,
        source: {
          sourceMode: "external_providers",
          providers: providers.map(([identity, identityByte]) => ({
            identity,
            operatorIdentitySha256: hex32(identityByte),
            endpoint: externalEndpoints.get(
              `${identity}:${identityByte}:${identityByte}`,
            )!,
          })),
        },
      },
    },
    deploymentIdentity(),
  );
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicy;
};

// Re-stamps a genuine W11 record onto another policy's configured source, so
// the source-authority binding cannot mask the provider-coverage question.
// Everything else in the record - bindings, counts, evidence, agreement - is
// exactly what W11 produced. Against a policy the record already matches this
// is the identity function, which the fully bound control asserts.
export const rebindConsistencyToPolicy = (
  record: unknown,
  targetPolicy: WatcherFinalityPolicy,
): Record<string, unknown> => {
  const rebound: Record<string, unknown> = {
    ...(record as Record<string, unknown>),
    configuredSourceDigest: sha256CanonicalForTest(
      watcherFinalityConfiguredSource(targetPolicy),
    ),
  };
  delete rebound.consistencyDigest;
  return { ...rebound, consistencyDigest: sha256CanonicalForTest(rebound) };
};

export const provider = (
  providerId: string,
  identityByte: string,
  operatorIdentityByte = identityByte,
) =>
  transportContexts.get(
    `external:${providerId}:${identityByte}:${operatorIdentityByte}`,
  )!;

const transaction = (bodySeedHex: string) => {
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    BigInt(`0x${bodySeedHex}`),
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  const fullTransaction = CML.Transaction.new(
    body,
    witnessSet,
    true,
    undefined,
  );
  const bodyHex = body.to_canonical_cbor_hex();
  return {
    txHash: computeHash32(Buffer.from(bodyHex, "hex")).toString("hex"),
    fullTransaction: makeWatcherL1PublicBytes(
      fullTransaction.to_canonical_cbor_hex(),
    ),
    body: makeWatcherL1PublicBytes(bodyHex),
    witnessSet: makeWatcherL1PublicBytes(witnessSet.to_canonical_cbor_hex()),
    utxos: [],
    scripts: [],
    datums: [],
    redeemers: [],
  };
};

export type ObservationOptions = Readonly<{
  blockHash?: string;
  parentBlockHash?: string | null;
  slot?: string;
  blockNo?: string;
  depth?: string;
  bodyHex?: string;
  operatorIdentityByte?: string;
}>;

export const observation = (
  providerId: string,
  identityByte: string,
  options: ObservationOptions = {},
): WatcherNormalizedL1Block => {
  const attestation = provider(
    providerId,
    identityByte,
    options.operatorIdentityByte ?? identityByte,
  );
  const normalized = normalizeWatcherL1Block(attestation, {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
    network: "Preprod",
    providerId,
    chainPoint: {
      blockHash: options.blockHash ?? hex32("aa"),
      parentBlockHash: options.parentBlockHash ?? null,
      slot: options.slot ?? "1000",
      blockNo: options.blockNo ?? "100",
      depth: options.depth ?? "0",
    },
    transactions:
      options.bodyHex === undefined ? [] : [transaction(options.bodyHex)],
  });
  observationAttestations.set(normalized, attestation);
  return normalized;
};

export const evaluateWatcherMultiProviderConsistency = (
  configuredSource: unknown,
  observations: unknown,
  explicitAttestations?: readonly WatcherL1TransportAttestationContext[],
) => {
  const inferred = Array.isArray(observations)
    ? observations.flatMap((candidate) => {
        if (typeof candidate !== "object" || candidate === null) return [];
        const attestation = observationAttestations.get(candidate);
        return attestation === undefined ? [] : [attestation];
      })
    : [];
  return evaluateWatcherMultiProviderConsistencyRaw(
    configuredSource,
    observations,
    explicitAttestations ?? [...new Set(inferred)],
  );
};

export const agreement = (
  depth: string,
  options: ObservationOptions = {},
  reverse = false,
) => {
  const observations = [
    observation("provider-a", "a1", { ...options, depth }),
    observation("provider-b", "b2", { ...options, depth }),
  ];
  return evaluateWatcherMultiProviderConsistency(
    externalSource(),
    reverse ? observations.reverse() : observations,
  );
};

export const pendingAt = (
  finalityPolicy: WatcherFinalityPolicy,
  depth: string,
  options: ObservationOptions = {},
): WatcherFinalityState => {
  const result = evaluateWatcherFinality(
    finalityPolicy,
    null,
    agreement(depth, options),
  );
  expect(result.action).toBe("observe_pending");
  return result.state as WatcherFinalityState;
};

export const finalizeAtThreshold = (
  finalityPolicy: WatcherFinalityPolicy,
  options: ObservationOptions = {},
): WatcherFinalityState => {
  const pending = pendingAt(finalityPolicy, "2", options);
  const result = evaluateWatcherFinality(
    finalityPolicy,
    pending,
    agreement("3", options),
  );
  expect(result.action).toBe("finalize");
  return result.state as WatcherFinalityState;
};
