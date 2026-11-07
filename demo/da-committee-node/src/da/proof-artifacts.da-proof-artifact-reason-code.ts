import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  computeDaSha256Hash,
  type DaProofBundleByHeaderResponse,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CommitteeStore } from "../store.js";
import { hexToBytes } from "../utils/hex.js";
import { type DaPayloadCountSet, type DaPayloadRootSet } from "./payload.js";

export type DataSchema = Parameters<typeof LucidData.Nullable>[0];

export type DaProofArtifactReasonCode =
  | "committed_header_count_mismatch"
  | "committed_header_missing"
  | "committed_header_root_mismatch"
  | "deployment_fingerprint_mismatch"
  | "event_key_malformed"
  | "event_to_step_not_found"
  | "payload_header_hash_mismatch"
  | "payload_root_derivation_failed"
  | "proof_bundle_too_large_for_inline_response"
  | "record_deployment_fingerprint_mismatch"
  | "record_header_hash_mismatch"
  | "stored_payload_bytes_malformed"
  | "stored_payload_hash_malformed"
  | "stored_payload_hash_mismatch"
  | "stored_payload_malformed"
  | "stored_payload_not_found"
  | "stored_payload_not_verified"
  | "stored_root_summary_malformed"
  | "stored_root_summary_mismatch"
  | "stored_root_summary_missing"
  | "trace_step_not_found"
  | "witness_construction_failed";

export type DaProofArtifactDerivation<TResponse> = {
  readonly response: TResponse;
  readonly reasonCode: DaProofArtifactReasonCode | null;
};

export type DaProofArtifactStore = Pick<
  CommitteeStore,
  "getDaPayload" | "getStateQueueHeader"
>;

export type DaProofArtifactDeriverOptions = {
  readonly deploymentFingerprint: string | Uint8Array;
  readonly store: DaProofArtifactStore;
};

export type DecodedRootEntry<K, V> = {
  readonly key: K;
  readonly value: V;
  readonly keyBytes: Buffer;
  readonly valueBytes: Buffer;
};

type KeyValuePhasEntry = {
  readonly key: Buffer;
  readonly value: Buffer;
};

export type KeyValuePhasRoot = {
  readonly root: string;
  readonly count: bigint;
  readonly entries: readonly KeyValuePhasEntry[];
};

export type CountedRoot = KeyValuePhasRoot & {
  readonly domain: SDK.RootDomain;
  readonly phasRoot: string;
};

export type ProofRootSetInput = DaPayloadRootSet;

export type ProofCountSetInput = DaPayloadCountSet;

export type TraceProofReconstruction = {
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly rootSummary: DaPayloadRootSet;
  readonly countSummary: DaPayloadCountSet;
  readonly transitionTrace: ReadonlyMap<
    bigint,
    DecodedRootEntry<bigint, SDK.TransitionStep>
  >;
  readonly eventToStep: ReadonlyMap<
    string,
    DecodedRootEntry<SDK.EventKey, SDK.EventToStepValue>
  >;
  readonly rootData: {
    readonly transitionTrace: CountedRoot;
    readonly eventToStep: CountedRoot;
  };
};

export type VerifiedPayloadResolution =
  | { readonly kind: "missing" }
  | {
      readonly kind: "rejected";
      readonly reasonCode: DaProofArtifactReasonCode;
    }
  | {
      readonly kind: "found";
      readonly reconstruction: TraceProofReconstruction;
    };

const MIDGARD_EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;

export const NON_MEMBERSHIP_DUMMY_VALUE = Buffer.alloc(0);

export const PROOF_BUNDLE_VERSION = 1n;

export const rejectedProofBundleResponse = (
  headerHash: Buffer,
  reasonCode: DaProofArtifactReasonCode,
): DaProofBundleByHeaderResponse => ({
  status: "rejected",
  headerHash,
  proofBundleHash: null,
  proofBundleBytes: null,
  chunkManifest: null,
  reasonCode,
});

export const encodeProofBundle = (
  reconstruction: TraceProofReconstruction,
): Buffer =>
  encodeCbor([
    PROOF_BUNDLE_VERSION,
    reconstruction.headerHash,
    reconstruction.payloadHash,
    rootSummaryHash(reconstruction.rootSummary),
    rootSummaryValues(reconstruction.rootSummary),
    countSummaryValues(reconstruction.countSummary),
  ]);

const rootSummaryHash = (rootSummary: DaPayloadRootSet): Buffer =>
  computeDaSha256Hash(Buffer.concat(rootSummaryValues(rootSummary)));

const rootSummaryValues = (
  rootSummary: DaPayloadRootSet,
): readonly Buffer[] => [
  hexToBytes(rootSummary.utxosRoot, "utxos root", 32),
  hexToBytes(rootSummary.withdrawalsRoot, "withdrawals root", 32),
  hexToBytes(
    rootSummary.forcedTransactionsRoot,
    "forced transactions root",
    32,
  ),
  hexToBytes(rootSummary.transactionsRoot, "transactions root", 32),
  hexToBytes(rootSummary.depositsRoot, "deposits root", 32),
  hexToBytes(rootSummary.transitionTraceRoot, "transition trace root", 32),
  hexToBytes(rootSummary.eventToStepRoot, "event to step root", 32),
  hexToBytes(rootSummary.validationTracesRoot, "validation traces root", 32),
];

const countSummaryValues = (
  countSummary: DaPayloadCountSet,
): readonly bigint[] => [
  countSummary.withdrawalCount,
  countSummary.forcedTransactionCount,
  countSummary.l2TransactionCount,
  countSummary.depositCount,
  countSummary.totalEventCount,
  countSummary.transitionStepCount,
  countSummary.validationTraceCount,
];

export const decodeCanonicalEventKey = (bytes: Buffer): SDK.EventKey | null => {
  const eventKeyHex = bytes.toString("hex");
  try {
    const eventKey = LucidData.from(
      eventKeyHex,
      SDK.EventKeySchema as never,
    ) as SDK.EventKey;
    return LucidData.to(eventKey as never, SDK.EventKeySchema as never) ===
      eventKeyHex
      ? eventKey
      : null;
  } catch {
    return null;
  }
};

export const decodeTypedEntries = <K, V>({
  fieldName,
  entries,
  keySchema,
  valueSchema,
}: {
  readonly fieldName: string;
  readonly entries: readonly SDK.DaPayloadEntry[];
  readonly keySchema: DataSchema;
  readonly valueSchema: DataSchema;
}): readonly DecodedRootEntry<K, V>[] =>
  entries.map(([keyHex, valueHex], index) => {
    const keyBytes = entryBuffer(keyHex, `${fieldName}[${index}].key`);
    const valueBytes = entryBuffer(valueHex, `${fieldName}[${index}].value`);
    return {
      key: decodeData<K>(keyHex, keySchema),
      value: decodeData<V>(valueHex, valueSchema),
      keyBytes,
      valueBytes,
    };
  });

export const rawEntries = (
  fieldName: string,
  entries: readonly SDK.DaPayloadEntry[],
): readonly KeyValuePhasEntry[] =>
  canonicalizeKeyValuePhasEntries(
    entries.map(([key, value], index) => ({
      key: entryBuffer(key, `${fieldName}[${index}].key`),
      value: entryBuffer(value, `${fieldName}[${index}].value`),
    })),
  );

const entryBuffer = (value: string, fieldName: string): Buffer =>
  hexToBytes(value, fieldName);

const decodeData = <A>(hex: string, schema: DataSchema): A => {
  const value = LucidData.from(hex, schema as never) as A;
  if (LucidData.to(value as never, schema as never) !== hex) {
    throw new Error("non-canonical data");
  }
  return value;
};

export const eventKeyFingerprint = (eventKey: SDK.EventKey): string =>
  LucidData.to(eventKey as never, SDK.EventKeySchema as never);

const canonicalizeKeyValuePhasEntries = (
  entries: readonly KeyValuePhasEntry[],
): readonly KeyValuePhasEntry[] => {
  const sorted = entries
    .map((entry) => ({
      key: Buffer.from(entry.key),
      value: Buffer.from(entry.value),
    }))
    .sort((left, right) => Buffer.compare(left.key, right.key));
  const seen = new Set<string>();
  for (const entry of sorted) {
    const keyHex = entry.key.toString("hex");
    if (seen.has(keyHex)) {
      throw new Error(`duplicate PHAS key ${keyHex}`);
    }
    seen.add(keyHex);
  }
  return sorted;
};

export const normalizeRoot = (root: Uint8Array | null | undefined): string => {
  if (root === null || root === undefined) {
    return MIDGARD_EMPTY_ROOT;
  }
  const hex = Buffer.from(root).toString("hex");
  return hex === "00".repeat(32) ? MIDGARD_EMPTY_ROOT : hex;
};

export const trieFromEntries = async (
  entries: readonly KeyValuePhasEntry[],
): Promise<Trie> =>
  await Trie.fromList(
    entries.map((entry) => ({
      key: Buffer.from(entry.key),
      value: Buffer.from(entry.value),
    })),
  );

const keyValuePhasRootWithCount = async (
  entries: readonly KeyValuePhasEntry[],
): Promise<KeyValuePhasRoot> => {
  const canonical = canonicalizeKeyValuePhasEntries(entries);
  if (canonical.length === 0) {
    return {
      root: MIDGARD_EMPTY_ROOT,
      count: 0n,
      entries: canonical,
    };
  }
  const trie = await trieFromEntries(canonical);
  return {
    root: normalizeRoot(trie.hash),
    count: BigInt(canonical.length),
    entries: canonical,
  };
};

export const buildCountedRoot = async (
  domain: SDK.RootDomain,
  entries: readonly KeyValuePhasEntry[],
): Promise<CountedRoot> => {
  const phas = await keyValuePhasRootWithCount(entries);
  const root = await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain,
      phasRoot: phas.root,
      count: phas.count,
    }),
  );
  return {
    ...phas,
    root,
    phasRoot: phas.root,
    domain,
  };
};
