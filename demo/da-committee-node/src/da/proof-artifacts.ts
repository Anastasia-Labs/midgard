import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
  daDeploymentFingerprintFromHex,
  type DaEventToStepByEventRequestV1,
  type DaEventToStepByEventResponseV1,
  type DaProofBundleByHeaderRequestV1,
  type DaProofBundleByHeaderResponseV1,
  type DaTraceStepByIndexRequestV1,
  type DaTraceStepByIndexResponseV1,
  normalizeDaDeploymentFingerprintHex,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type {
  DaPayloadRecord,
  Header,
  PayloadCountSet,
  PayloadRootSet,
  StateQueueHeaderRecord,
} from "../domain.js";
import { hashBlockHeader } from "../l1/state-queue-scanner.js";
import type { WatcherStore } from "../store.js";
import { hexToBytes, normalizeHex } from "../utils/hex.js";
import {
  computeDaPayloadRoots,
  daPayloadSha256,
  decodeDaPayloadV2Strict,
  type TransactionRootValueProjector,
} from "./payload.js";

type DataSchema = Parameters<typeof LucidData.Nullable>[0];

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
  WatcherStore,
  "getDaPayload" | "getStateQueueHeader"
>;

export type DaProofArtifactDeriverOptions = {
  readonly deploymentFingerprint: string | Uint8Array;
  readonly store: DaProofArtifactStore;
  readonly transactionProjector?: TransactionRootValueProjector;
};

type DecodedRootEntry<K, V> = {
  readonly key: K;
  readonly value: V;
  readonly keyBytes: Buffer;
  readonly valueBytes: Buffer;
};

type KeyValuePhasEntry = {
  readonly key: Buffer;
  readonly value: Buffer;
};

type KeyValuePhasRoot = {
  readonly root: string;
  readonly count: bigint;
  readonly entries: readonly KeyValuePhasEntry[];
};

type CountedRoot = KeyValuePhasRoot & {
  readonly domain: SDK.RootDomain;
  readonly phasRoot: string;
};

type TraceProofReconstruction = {
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly rootSummary: PayloadRootSet;
  readonly countSummary: PayloadCountSet;
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

type VerifiedPayloadResolution =
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
const NON_MEMBERSHIP_DUMMY_VALUE = Buffer.alloc(0);
const PROOF_BUNDLE_V1_VERSION = 1n;

export class DaProofArtifactDeriver {
  private readonly deploymentFingerprint: string;
  private readonly deploymentFingerprintBytes: Buffer;
  private readonly store: DaProofArtifactStore;
  private readonly transactionProjector?: TransactionRootValueProjector;

  constructor(options: DaProofArtifactDeriverOptions) {
    this.deploymentFingerprint = normalizeDaDeploymentFingerprintHex(
      options.deploymentFingerprint,
    );
    this.deploymentFingerprintBytes = daDeploymentFingerprintFromHex(
      this.deploymentFingerprint,
    );
    this.store = options.store;
    this.transactionProjector = options.transactionProjector;
  }

  async proofBundleByHeader(
    request: DaProofBundleByHeaderRequestV1,
  ): Promise<DaProofArtifactDerivation<DaProofBundleByHeaderResponseV1>> {
    const headerHash = request.headerHash;
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return {
        reasonCode: "deployment_fingerprint_mismatch",
        response: rejectedProofBundleResponse(
          headerHash,
          "deployment_fingerprint_mismatch",
        ),
      };
    }
    const resolution = await this.resolveVerifiedPayload(headerHash);
    if (resolution.kind === "missing") {
      return {
        reasonCode: "stored_payload_not_found",
        response: {
          status: "not_found",
          headerHash,
          proofBundleHash: null,
          proofBundleBytes: null,
          chunkManifest: null,
          reasonCode: "stored_payload_not_found",
        },
      };
    }
    if (resolution.kind === "rejected") {
      return {
        reasonCode: resolution.reasonCode,
        response: rejectedProofBundleResponse(
          headerHash,
          resolution.reasonCode,
        ),
      };
    }
    const proofBundleBytes = encodeProofBundleV1(resolution.reconstruction);
    const proofBundleHash = computeDaSha256Hash(proofBundleBytes);
    if (proofBundleBytes.length > request.maxInlineBytes) {
      return {
        reasonCode: "proof_bundle_too_large_for_inline_response",
        response: {
          status: "rejected",
          headerHash,
          proofBundleHash,
          proofBundleBytes: null,
          chunkManifest: null,
          reasonCode: "proof_bundle_too_large_for_inline_response",
        },
      };
    }
    return {
      reasonCode: null,
      response: {
        status: "found_inline",
        headerHash,
        proofBundleHash,
        proofBundleBytes,
        chunkManifest: null,
        reasonCode: null,
      },
    };
  }

  async traceStepByIndex(
    request: DaTraceStepByIndexRequestV1,
  ): Promise<DaProofArtifactDerivation<DaTraceStepByIndexResponseV1>> {
    const absentResponse = (
      status: DaTraceStepByIndexResponseV1["status"],
    ): DaTraceStepByIndexResponseV1 => ({
      status,
      headerHash: request.headerHash,
      stepIndex: request.stepIndex,
      transitionStepBytes: null,
      membershipProofBytes: null,
    });
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return {
        reasonCode: "deployment_fingerprint_mismatch",
        response: absentResponse("rejected"),
      };
    }
    const resolution = await this.resolveVerifiedPayload(request.headerHash);
    if (resolution.kind === "missing") {
      return {
        reasonCode: "stored_payload_not_found",
        response: absentResponse("not_found"),
      };
    }
    if (resolution.kind === "rejected") {
      return {
        reasonCode: resolution.reasonCode,
        response: absentResponse("rejected"),
      };
    }
    const entry = resolution.reconstruction.transitionTrace.get(
      BigInt(request.stepIndex),
    );
    if (entry === undefined) {
      return {
        reasonCode: "trace_step_not_found",
        response: absentResponse("not_found"),
      };
    }
    try {
      const proof = await membershipProof(
        resolution.reconstruction.rootData.transitionTrace,
        entry,
      );
      return {
        reasonCode: null,
        response: {
          status: "found",
          headerHash: request.headerHash,
          stepIndex: request.stepIndex,
          transitionStepBytes: entry.valueBytes,
          membershipProofBytes: encodeData(
            proof,
            SDK.TransitionTraceMembershipProofSchema,
          ),
        },
      };
    } catch {
      return {
        reasonCode: "witness_construction_failed",
        response: absentResponse("rejected"),
      };
    }
  }

  async eventToStepByEvent(
    request: DaEventToStepByEventRequestV1,
  ): Promise<DaProofArtifactDerivation<DaEventToStepByEventResponseV1>> {
    const absentResponse = (
      status: DaEventToStepByEventResponseV1["status"],
    ): DaEventToStepByEventResponseV1 => ({
      status,
      headerHash: request.headerHash,
      eventKey: request.eventKey,
      eventToStepEntryBytes: null,
      membershipOrNonmembershipProofBytes: null,
    });
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return {
        reasonCode: "deployment_fingerprint_mismatch",
        response: absentResponse("rejected"),
      };
    }
    const eventKey = decodeCanonicalEventKey(request.eventKey);
    if (eventKey === null) {
      return {
        reasonCode: "event_key_malformed",
        response: absentResponse("rejected"),
      };
    }
    const resolution = await this.resolveVerifiedPayload(request.headerHash);
    if (resolution.kind === "missing") {
      return {
        reasonCode: "stored_payload_not_found",
        response: absentResponse("not_found"),
      };
    }
    if (resolution.kind === "rejected") {
      return {
        reasonCode: resolution.reasonCode,
        response: absentResponse("rejected"),
      };
    }
    const entry = resolution.reconstruction.eventToStep.get(
      eventKeyFingerprint(eventKey),
    );
    try {
      const proof =
        entry === undefined
          ? await buildEventToStepNonMembershipProof(
              resolution.reconstruction.rootData.eventToStep,
              eventKey,
            )
          : await buildEventToStepMembershipProof(
              resolution.reconstruction.rootData.eventToStep,
              entry,
            );
      return {
        reasonCode: entry === undefined ? "event_to_step_not_found" : null,
        response: {
          status: "found",
          headerHash: request.headerHash,
          eventKey: request.eventKey,
          eventToStepEntryBytes: entry?.valueBytes ?? null,
          membershipOrNonmembershipProofBytes: encodeData(
            proof,
            SDK.EventToStepProofSchema,
          ),
        },
      };
    } catch {
      return {
        reasonCode: "witness_construction_failed",
        response: absentResponse("rejected"),
      };
    }
  }

  private async resolveVerifiedPayload(
    headerHash: Buffer,
  ): Promise<VerifiedPayloadResolution> {
    const headerHashHex = headerHash.toString("hex");
    const record = await this.store.getDaPayload(headerHashHex);
    if (record === undefined) {
      return { kind: "missing" };
    }
    const recordCheck = this.validateRecordEnvelope(record, headerHashHex);
    if (recordCheck !== null) {
      return { kind: "rejected", reasonCode: recordCheck };
    }
    const committedHeader = await this.store.getStateQueueHeader(headerHashHex);
    if (committedHeader === undefined) {
      return { kind: "rejected", reasonCode: "committed_header_missing" };
    }
    const headerCheck = this.validateCommittedHeader(
      committedHeader,
      headerHashHex,
    );
    if (headerCheck !== null) {
      return { kind: "rejected", reasonCode: headerCheck };
    }
    let payloadBytes: Buffer;
    try {
      payloadBytes = hexToBytes(record.payloadCborHex, "stored payload CBOR");
    } catch {
      return { kind: "rejected", reasonCode: "stored_payload_bytes_malformed" };
    }
    if (payloadBytes.length === 0) {
      return { kind: "rejected", reasonCode: "stored_payload_bytes_malformed" };
    }
    let storedHash: string;
    try {
      storedHash = normalizeHex(record.payloadSha256, {
        fieldName: "stored payload sha256",
        byteLength: 32,
      });
    } catch {
      return { kind: "rejected", reasonCode: "stored_payload_hash_malformed" };
    }
    if (storedHash !== daPayloadSha256(payloadBytes)) {
      return { kind: "rejected", reasonCode: "stored_payload_hash_mismatch" };
    }
    let payload: SDK.DaPayloadV2;
    try {
      const unwrapped = await unwrapDaPayload(payloadBytes, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
        // Pre-V3 persisted records are raw DaPayloadV2 by construction.
        schemaVersion: record.payloadSchemaVersion ?? 2,
      });
      payload = decodeDaPayloadV2Strict(unwrapped.innerBytes);
    } catch {
      return { kind: "rejected", reasonCode: "stored_payload_malformed" };
    }
    if (
      payload.block_body.header_hash !== headerHashHex ||
      headerCborHex(payload.block_body.header) !==
        headerCborHex(committedHeader.header)
    ) {
      return { kind: "rejected", reasonCode: "payload_header_hash_mismatch" };
    }
    let roots: PayloadRootSet;
    try {
      roots = await computeDaPayloadRoots(payload, this.transactionProjector);
    } catch {
      return {
        kind: "rejected",
        reasonCode: "payload_root_derivation_failed",
      };
    }
    if (rootMismatches(record.rootSummary!, roots).length > 0) {
      return { kind: "rejected", reasonCode: "stored_root_summary_mismatch" };
    }
    if (rootMismatches(headerRoots(committedHeader.header), roots).length > 0) {
      return { kind: "rejected", reasonCode: "committed_header_root_mismatch" };
    }
    if (
      countMismatches(
        headerCounts(committedHeader.header),
        payload.block_body.counts,
      ).length > 0
    ) {
      return {
        kind: "rejected",
        reasonCode: "committed_header_count_mismatch",
      };
    }
    try {
      return {
        kind: "found",
        reconstruction: await reconstructTraceProofs(payload, {
          headerHash: Buffer.from(headerHashHex, "hex"),
          payloadHash: Buffer.from(storedHash, "hex"),
          rootSummary: roots,
          countSummary: payload.block_body.counts,
        }),
      };
    } catch {
      return { kind: "rejected", reasonCode: "witness_construction_failed" };
    }
  }

  private validateRecordEnvelope(
    record: DaPayloadRecord,
    headerHashHex: string,
  ): DaProofArtifactReasonCode | null {
    if (record.validationStatus !== "verified") {
      return "stored_payload_not_verified";
    }
    if (record.rootSummary === undefined) {
      return "stored_root_summary_missing";
    }
    try {
      if (
        normalizeDaDeploymentFingerprintHex(record.deploymentFingerprint) !==
        this.deploymentFingerprint
      ) {
        return "record_deployment_fingerprint_mismatch";
      }
      if (
        normalizeHex(record.headerHash, {
          fieldName: "stored header hash",
          byteLength: 28,
        }) !== headerHashHex
      ) {
        return "record_header_hash_mismatch";
      }
    } catch {
      return "record_header_hash_mismatch";
    }
    try {
      normalizeRootSet(record.rootSummary, "stored root summary");
    } catch {
      return "stored_root_summary_malformed";
    }
    try {
      normalizeHex(record.payloadSha256, {
        fieldName: "stored payload sha256",
        byteLength: 32,
      });
    } catch {
      return "stored_payload_hash_malformed";
    }
    return null;
  }

  private validateCommittedHeader(
    record: StateQueueHeaderRecord,
    headerHashHex: string,
  ): DaProofArtifactReasonCode | null {
    try {
      if (
        normalizeDaDeploymentFingerprintHex(record.deploymentFingerprint) !==
        this.deploymentFingerprint
      ) {
        return "record_deployment_fingerprint_mismatch";
      }
      if (
        normalizeHex(record.headerHash, {
          fieldName: "committed header hash",
          byteLength: 28,
        }) !== headerHashHex
      ) {
        return "record_header_hash_mismatch";
      }
      if (
        normalizeHex(record.computedHeaderHash, {
          fieldName: "computed header hash",
          byteLength: 28,
        }) !== headerHashHex ||
        hashBlockHeader(record.header) !== headerHashHex
      ) {
        return "record_header_hash_mismatch";
      }
    } catch {
      return "record_header_hash_mismatch";
    }
    return null;
  }

  private matchesDeployment(value: Buffer): boolean {
    return value.equals(this.deploymentFingerprintBytes);
  }
}

const rejectedProofBundleResponse = (
  headerHash: Buffer,
  reasonCode: DaProofArtifactReasonCode,
): DaProofBundleByHeaderResponseV1 => ({
  status: "rejected",
  headerHash,
  proofBundleHash: null,
  proofBundleBytes: null,
  chunkManifest: null,
  reasonCode,
});

const encodeProofBundleV1 = (
  reconstruction: TraceProofReconstruction,
): Buffer =>
  encodeCbor([
    PROOF_BUNDLE_V1_VERSION,
    reconstruction.headerHash,
    reconstruction.payloadHash,
    rootSummaryHash(reconstruction.rootSummary),
    rootSummaryValues(reconstruction.rootSummary),
    countSummaryValues(reconstruction.countSummary),
  ]);

const rootSummaryHash = (rootSummary: PayloadRootSet): Buffer =>
  computeDaSha256Hash(Buffer.concat(rootSummaryValues(rootSummary)));

const rootSummaryValues = (rootSummary: PayloadRootSet): readonly Buffer[] => [
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
];

const countSummaryValues = (
  countSummary: PayloadCountSet,
): readonly bigint[] => [
  countSummary.withdrawalCount,
  countSummary.forcedTransactionCount,
  countSummary.l2TransactionCount,
  countSummary.depositCount,
  countSummary.totalEventCount,
  countSummary.transitionStepCount,
];

const decodeCanonicalEventKey = (bytes: Buffer): SDK.EventKey | null => {
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

const reconstructTraceProofs = async (
  payload: SDK.DaPayloadV2,
  context: {
    readonly headerHash: Buffer;
    readonly payloadHash: Buffer;
    readonly rootSummary: PayloadRootSet;
    readonly countSummary: PayloadCountSet;
  },
): Promise<TraceProofReconstruction> => {
  const { block_body: body } = payload;
  const transitionTraceEntries = decodeTypedEntries<bigint, SDK.TransitionStep>(
    {
      fieldName: "transition_trace",
      entries: body.transition_trace,
      keySchema: LucidData.Integer() as never,
      valueSchema: SDK.TransitionStepSchema,
    },
  );
  const eventToStepEntries = decodeTypedEntries<
    SDK.EventKey,
    SDK.EventToStepValue
  >({
    fieldName: "event_to_step",
    entries: body.event_to_step,
    keySchema: SDK.EventKeySchema,
    valueSchema: SDK.EventToStepValueSchema,
  });
  return {
    headerHash: Buffer.from(context.headerHash),
    payloadHash: Buffer.from(context.payloadHash),
    rootSummary: normalizeRootSet(context.rootSummary, "proof bundle roots"),
    countSummary: context.countSummary,
    transitionTrace: new Map(
      transitionTraceEntries.map((entry) => [entry.key, entry] as const),
    ),
    eventToStep: new Map(
      eventToStepEntries.map(
        (entry) => [eventKeyFingerprint(entry.key), entry] as const,
      ),
    ),
    rootData: {
      transitionTrace: await buildCountedRoot(
        SDK.ROOT_DOMAINS.transitionTrace,
        rawEntries("transition_trace", body.transition_trace),
      ),
      eventToStep: await buildCountedRoot(
        SDK.ROOT_DOMAINS.eventToStep,
        rawEntries("event_to_step", body.event_to_step),
      ),
    },
  };
};

const decodeTypedEntries = <K, V>({
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

const rawEntries = (
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

const encodeData = <A>(value: A, schema: DataSchema): Buffer =>
  Buffer.from(LucidData.to(value as never, schema as never), "hex");

const eventKeyFingerprint = (eventKey: SDK.EventKey): string =>
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

const normalizeRoot = (root: Uint8Array | null | undefined): string => {
  if (root === null || root === undefined) {
    return MIDGARD_EMPTY_ROOT;
  }
  const hex = Buffer.from(root).toString("hex");
  return hex === "00".repeat(32) ? MIDGARD_EMPTY_ROOT : hex;
};

const trieFromEntries = async (
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

const buildCountedRoot = async (
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

const countedPhasView = (root: CountedRoot): KeyValuePhasRoot => ({
  root: root.phasRoot,
  count: root.count,
  entries: root.entries,
});

const sdkProofFromCbor = (proofCbor: Uint8Array): SDK.Proof =>
  LucidData.from(
    Buffer.from(proofCbor).toString("hex"),
    SDK.Proof as never,
  ) as SDK.Proof;

const keyValuePhasProof = async (
  root: KeyValuePhasRoot,
  key: Buffer,
  value: Buffer,
): Promise<SDK.Proof> => {
  if (root.entries.length === 0) {
    throw new Error("cannot prove membership for empty tree");
  }
  const trie = await trieFromEntries(root.entries);
  const proof = await trie.prove(Buffer.from(key));
  if (normalizeRoot(proof.verify(true)) !== root.root) {
    throw new Error("generated membership proof root mismatch");
  }
  if (
    !root.entries.some(
      (entry) => entry.key.equals(key) && entry.value.equals(value),
    )
  ) {
    throw new Error("cannot prove membership for absent key/value");
  }
  return sdkProofFromCbor(proof.toCBOR());
};

const keyValuePhasNonMembershipProof = async (
  root: KeyValuePhasRoot,
  key: Buffer,
): Promise<SDK.Proof> => {
  if (root.entries.some((entry) => entry.key.equals(key))) {
    throw new Error("cannot prove non-membership for present key");
  }
  const trie = await trieFromEntries(root.entries);
  await trie.insert(Buffer.from(key), NON_MEMBERSHIP_DUMMY_VALUE);
  const proof = await trie.prove(Buffer.from(key));
  if (normalizeRoot(proof.verify(false)) !== root.root) {
    throw new Error("generated non-membership proof root mismatch");
  }
  return sdkProofFromCbor(proof.toCBOR());
};

const membershipProof = async <K, V>(
  root: CountedRoot,
  entry: DecodedRootEntry<K, V>,
): Promise<SDK.RootMembershipProof<K, V>> => ({
  domain: root.domain,
  root: root.root,
  phas_root: root.phasRoot,
  count: root.count,
  key: entry.key,
  value: entry.value,
  proof: await keyValuePhasProof(
    countedPhasView(root),
    entry.keyBytes,
    entry.valueBytes,
  ),
});

const nonMembershipProof = async <K>({
  root,
  key,
  keyBytes,
}: {
  readonly root: CountedRoot;
  readonly key: K;
  readonly keyBytes: Buffer;
}): Promise<SDK.RootNonMembershipProof<K>> => ({
  domain: root.domain,
  root: root.root,
  phas_root: root.phasRoot,
  count: root.count,
  key,
  proof: await keyValuePhasNonMembershipProof(countedPhasView(root), keyBytes),
});

const buildEventToStepMembershipProof = async (
  root: CountedRoot,
  entry: DecodedRootEntry<SDK.EventKey, SDK.EventToStepValue>,
): Promise<SDK.EventToStepProof> => ({
  EventToStepMembership: {
    membership: await membershipProof(root, entry),
  },
});

const buildEventToStepNonMembershipProof = async (
  root: CountedRoot,
  eventKey: SDK.EventKey,
): Promise<SDK.EventToStepProof> => ({
  EventToStepNonMembership: {
    non_membership: await nonMembershipProof({
      root,
      key: eventKey,
      keyBytes: encodeData(eventKey, SDK.EventKeySchema),
    }),
  },
});

const normalizeRootSet = (
  roots: PayloadRootSet,
  fieldName: string,
): PayloadRootSet => ({
  utxosRoot: normalizeHex(roots.utxosRoot, {
    fieldName: `${fieldName}.utxos_root`,
    byteLength: 32,
  }),
  withdrawalsRoot: normalizeHex(roots.withdrawalsRoot, {
    fieldName: `${fieldName}.withdrawals_root`,
    byteLength: 32,
  }),
  forcedTransactionsRoot: normalizeHex(roots.forcedTransactionsRoot, {
    fieldName: `${fieldName}.forced_transactions_root`,
    byteLength: 32,
  }),
  transactionsRoot: normalizeHex(roots.transactionsRoot, {
    fieldName: `${fieldName}.transactions_root`,
    byteLength: 32,
  }),
  depositsRoot: normalizeHex(roots.depositsRoot, {
    fieldName: `${fieldName}.deposits_root`,
    byteLength: 32,
  }),
  transitionTraceRoot: normalizeHex(roots.transitionTraceRoot, {
    fieldName: `${fieldName}.transition_trace_root`,
    byteLength: 32,
  }),
  eventToStepRoot: normalizeHex(roots.eventToStepRoot, {
    fieldName: `${fieldName}.event_to_step_root`,
    byteLength: 32,
  }),
});

const rootMismatches = (
  expected: PayloadRootSet,
  actual: PayloadRootSet,
): readonly string[] => {
  const normalizedExpected = normalizeRootSet(expected, "expected roots");
  const normalizedActual = normalizeRootSet(actual, "actual roots");
  return [
    normalizedExpected.utxosRoot === normalizedActual.utxosRoot
      ? null
      : "utxos_root",
    normalizedExpected.withdrawalsRoot === normalizedActual.withdrawalsRoot
      ? null
      : "withdrawals_root",
    normalizedExpected.forcedTransactionsRoot ===
    normalizedActual.forcedTransactionsRoot
      ? null
      : "forced_transactions_root",
    normalizedExpected.transactionsRoot === normalizedActual.transactionsRoot
      ? null
      : "transactions_root",
    normalizedExpected.depositsRoot === normalizedActual.depositsRoot
      ? null
      : "deposits_root",
    normalizedExpected.transitionTraceRoot ===
    normalizedActual.transitionTraceRoot
      ? null
      : "transition_trace_root",
    normalizedExpected.eventToStepRoot === normalizedActual.eventToStepRoot
      ? null
      : "event_to_step_root",
  ].filter((field): field is string => field !== null);
};

const countMismatches = (
  expected: PayloadCountSet,
  actual: PayloadCountSet,
): readonly string[] =>
  [
    expected.withdrawalCount === actual.withdrawalCount
      ? null
      : "withdrawal_count",
    expected.forcedTransactionCount === actual.forcedTransactionCount
      ? null
      : "forced_transaction_count",
    expected.l2TransactionCount === actual.l2TransactionCount
      ? null
      : "l2_transaction_count",
    expected.depositCount === actual.depositCount ? null : "deposit_count",
    expected.totalEventCount === actual.totalEventCount
      ? null
      : "total_event_count",
    expected.transitionStepCount === actual.transitionStepCount
      ? null
      : "transition_step_count",
  ].filter((field): field is string => field !== null);

const headerRoots = (header: Header): PayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
});

const headerCounts = (header: Header): PayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
});

const headerCborHex = (header: Header): string =>
  LucidData.to(header as never, SDK.Header as never);
