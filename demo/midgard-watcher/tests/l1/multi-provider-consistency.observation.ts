import { execFile } from "node:child_process";
import { type Server } from "node:net";
import { promisify } from "node:util";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";

import {
  makeWatcherL1PublicBytes,
  normalizeWatcherL1Block,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from "../../src/l1/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistency as evaluateWatcherMultiProviderConsistencyRaw } from "../../src/l1/multi-provider-consistency.js";

export const reorderObjectKeysForTest = (value: unknown): unknown => {
  if (Array.isArray(value)) return value.map(reorderObjectKeysForTest);
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.keys(value as Record<string, unknown>)
        .reverse()
        .map((key) => [
          key,
          reorderObjectKeysForTest((value as Record<string, unknown>)[key]),
        ]),
    );
  }
  return value;
};

export const observationAttestations = new WeakMap<
  object,
  WatcherL1TransportAttestationContext
>();

export const execFileAsync = promisify(execFile);

export const transportContexts = new Map<
  string,
  WatcherL1TransportAttestationContext
>();

export const tlsIdentities = new Map<string, string>();

export const listen = async (
  server: Server,
  target: string | number,
): Promise<void> =>
  await new Promise((resolve, reject) => {
    server.once("error", reject);
    const onListen = () => {
      server.off("error", reject);
      resolve();
    };
    if (typeof target === "string") server.listen(target, onListen);
    else server.listen(target, "127.0.0.1", onListen);
  });

const provider = (
  providerId: string,
  identityByte: string,
  operatorIdentityByte = identityByte,
) =>
  transportContexts.get(
    `external:${providerId}:${identityByte}:${operatorIdentityByte}`,
  )!;

export const localConfig = () => ({
  sourceMode: "local_node",
  network: "Preprod",
  authorityNodeId: "watcher-node-a",
  genesisIdentitySha256: "aa".repeat(32),
  chainSyncSocketPath: "/run/cardano/node.socket",
  queryServices: [],
});

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

export const observation = (
  providerId: string,
  identityByte: string,
  options: {
    blockHash?: string;
    parentBlockHash?: string | null;
    slot?: string;
    blockNo?: string;
    depth?: string;
    bodyHex?: string;
    operatorIdentityByte?: string;
  } = {},
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
      blockHash: options.blockHash ?? "11".repeat(32),
      parentBlockHash: options.parentBlockHash ?? null,
      slot: options.slot ?? "1000",
      blockNo: options.blockNo ?? "100",
      depth: options.depth ?? "15",
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
        if (typeof candidate !== "object" || candidate === null) {
          return [];
        }
        const context = observationAttestations.get(candidate);
        return context === undefined ? [] : [context];
      })
    : [];
  return evaluateWatcherMultiProviderConsistencyRaw(
    configuredSource,
    observations,
    explicitAttestations ?? [...new Set(inferred)],
  );
};
