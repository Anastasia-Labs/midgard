import { dirname } from "node:path";

import { Proof, Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardTxOutput, outRefToCbor } from "@al-ft/lucid-midgard";
import {
  encodeMidgardCekProgramMaterialSidecarV1,
  type MidgardCekProgramEnvelopeV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  encodeCborBytes,
  encodeCborInteger,
  encodeCborTagRaw,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_TRANSITION_STEP_V1_SCHEMA_VERSION,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { validateMidgardConsensusV1TxCbor } from "@al-ft/midgard-core/consensus-validation-v1";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  collectMidgardV1AttachedProgramEnvelopes,
  collectMidgardV1ReferencedProgramEnvelopes,
} from "@al-ft/midgard-core/script-proof";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applyUTxOStatePatch,
  applyValidationMachineLedgerMutationStepV1,
  buildCanonicalMidgardLedgerEntryOutputMaterialV1,
  buildCanonicalTransitionEffectV1,
  buildDeterministicValidationMachineTrace,
  canonicalCommittedWithdrawalTransitionEffectV1,
  canonicalDepositTransitionEffectV1,
  canonicalTransitionEffectFromStatePatchV1,
  type CanonicalTransitionEffectV1,
  type RejectCode,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import type { UTxO } from "@lucid-evolution/lucid";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { Data, Effect, Fiber, Metric, Option } from "effect";
import * as FS from "fs";
import { Level } from "level";

import { positiveSafeInteger } from "@/artifact-schema.js";
import * as CekProgramMaterialDB from "@/database/cekProgramMaterial.js";
import * as ConfirmedLedgerDB from "@/database/confirmedLedger.js";
import * as DepositsDB from "@/database/deposits.js";
import * as ForcedTransactionsDB from "@/database/forcedTransactions.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as MempoolTxDeltasDB from "@/database/mempoolTxDeltas.js";
import * as MpfEngineStateDB from "@/database/mpfEngineState.js";
import * as PendingBlockFinalizationsDB from "@/database/pendingBlockFinalizations.js";
import * as TxAdmissionsDB from "@/database/txAdmissions.js";
import * as TxRejectionsDB from "@/database/txRejections.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import * as Tx from "@/database/utils/tx.js";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import { Database, NodeConfig, type NodeConfigDep } from "@/services/index.js";
import type { ContractDeploymentIdentityValue } from "@/services/midgard-contracts.js";
import {
  encodeNativeMpfEventLog,
  type NativeMpfGenerationHandle,
  type NativeMpfOwnerClient,
} from "@/services/mpf-native-owner/index.js";
import { sha256 } from "@/sha256.js";
import { FileSystemError, findSpentAndProducedUTxOs } from "@/utils.js";

import { keyValuePhasRoot, keyValuePhasRootWithCount } from "./mpf/phas.js";
import {
  type ClassifiedWithdrawal,
  classifyWithdrawal,
  indexSelectedLedgerOutputs,
} from "./mpf/withdrawal-classification.js";
import {
  type AuthenticatedPackedMpfRecord,
  authenticatePackedMpfRecord,
  EventFlatMutationArena,
  type EventFlatMutationDiagnostics,
  type PackedMpfStoredValue,
  type ParkedEventFlatOverlayV1,
  ResumedEventFlatOverlayV1,
} from "./mpf-event-flat.js";
import {
  eventFlatDigest,
  prepareEventFlatDigest,
} from "./mpf-event-flat-digest.js";
import {
  buildCountedMpfRootInWorker,
  configureMpfRootWorkers,
  prewarmMpfRootWorkers,
  shouldBuildMpfRootInWorker,
} from "./mpf-root-pool.js";

export {
  canonicalizeKeyValuePhasEntries,
  type KeyValuePhasEntry,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  type KeyValuePhasRoot,
  keyValuePhasRoot,
  keyValuePhasRootWithCount,
  rootFromPhasProof,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
} from "./mpf/phas.js";
export type { ParkedEventFlatOverlayV1 } from "./mpf-event-flat.js";

const consumeMpfMutationProof = Trie.prototype.consumeMidgardMutationProof;
const ROOT_KEY = "__root__";
const JSON_LEVEL_ENCODING_OPTS = { valueEncoding: "json" as const };
export const MPF_EMPTY_ROOT_HEX = SDK.EMPTY_MERKLE_TREE_ROOT;
const MPF_EMPTY_ROOT = Buffer.from(MPF_EMPTY_ROOT_HEX, "hex");
const MPF_INTERNAL_NULL_ROOT_HEX = "00".repeat(32);
const MPF_INTERNAL_NULL_ROOT = Buffer.alloc(32);

export type MpfBatchOp =
  | { readonly type: "insert"; readonly key: Buffer; readonly value: Buffer }
  | { readonly type: "delete"; readonly key: Buffer };

export type MpfInsertBatchOp = Extract<MpfBatchOp, { readonly type: "insert" }>;

const transitionEffectToRawLedgerOpsV1 = (
  effect: CanonicalTransitionEffectV1,
): readonly MpfBatchOp[] =>
  effect.operations.map((operation) =>
    operation.type === "delete"
      ? { type: "delete", key: Buffer.from(operation.outRefCbor) }
      : {
          type: "insert",
          key: Buffer.from(operation.outRefCbor),
          value: Buffer.from(operation.outputCbor),
        },
  );

const transitionEffectToLedgerOpsV1 = (
  effect: CanonicalTransitionEffectV1,
): readonly MpfBatchOp[] =>
  effect.operations.map((operation) =>
    operation.type === "delete"
      ? { type: "delete", key: Buffer.from(operation.outRefCbor) }
      : ledgerOutputToInsertBatchOpV1({
          outRef: operation.outRefCbor,
          outputCbor: operation.outputCbor,
        }),
  );

export type LedgerDelta = {
  readonly spent: readonly Buffer[];
  readonly produced: readonly UtxoPayloadEntry[];
};

export type UtxoPayloadSizeAggregate = SDK.DaPayloadEntrySizeAggregate;

export const utxoPayloadEntryEncodedSize = ({
  outref,
  output,
}: UtxoPayloadEntry): number =>
  SDK.daPayloadEntryEncodedSize([
    outref.toString("hex"),
    output.toString("hex"),
  ]);

export const utxoPayloadAggregateFromEntries = (
  entries: readonly UtxoPayloadEntry[],
): UtxoPayloadSizeAggregate => ({
  entryCount: entries.length,
  encodedTupleBytes: entries.reduce(
    (total, entry) => total + utxoPayloadEntryEncodedSize(entry),
    0,
  ),
});

export const ledgerPayloadAggregateFromEntries = (
  entries: readonly Ledger.MinimalEntry[],
): UtxoPayloadSizeAggregate =>
  utxoPayloadAggregateFromEntries(
    entries.map((entry) => ({
      outref: entry[Ledger.Columns.OUTREF],
      output: entry[Ledger.Columns.OUTPUT],
    })),
  );

const collapseLedgerDelta = (
  ops: readonly MpfBatchOp[],
  insertedValues: ReadonlyMap<string, Buffer>,
): LedgerDelta => {
  const finalByOutref = new Map<string, MpfBatchOp>();
  for (const op of ops) finalByOutref.set(op.key.toString("hex"), op);
  const spent: Buffer[] = [];
  const produced: UtxoPayloadEntry[] = [];
  for (const op of finalByOutref.values()) {
    if (op.type === "delete") spent.push(Buffer.from(op.key));
    else {
      const output = insertedValues.get(op.key.toString("hex"));
      if (output === undefined) {
        throw new Error(
          `Missing full output bytes for ledger delta insertion ${op.key.toString("hex")}`,
        );
      }
      produced.push({
        outref: Buffer.from(op.key),
        output: Buffer.from(output),
      });
    }
  }
  return { spent, produced };
};

type MpfStoredValue = string | Record<string, unknown>;
type MpfSerializableValue = { readonly serialise: () => MpfStoredValue };
type MpfReadableValue = MpfStoredValue | MpfSerializableValue;

export const estimateMpfStoredValueBytes = (value: MpfStoredValue): number => {
  if (typeof value === "string") {
    return 128 + 2 * Buffer.byteLength(value);
  }
  const kind = value.__kind;
  const prefix = value.prefix;
  if (kind === "Leaf") {
    if (
      typeof prefix !== "string" ||
      typeof value.key !== "string" ||
      typeof value.value !== "string"
    ) {
      throw new Error("Invalid serialized MPF leaf in block path cache");
    }
    return (
      512 +
      2 *
        (Buffer.byteLength(prefix) +
          Buffer.byteLength(value.key) +
          Buffer.byteLength(value.value))
    );
  }
  if (kind === "Branch") {
    if (
      typeof prefix !== "string" ||
      !Array.isArray(value.children) ||
      value.children.length !== 16 ||
      !value.children.every(
        (child) =>
          child === undefined || child === null || typeof child === "string",
      ) ||
      !Number.isSafeInteger(value.size)
    ) {
      throw new Error("Invalid serialized MPF branch in block path cache");
    }
    const childBytes = value.children.reduce<number>(
      (total, child) =>
        total + (typeof child === "string" ? Buffer.byteLength(child) : 8),
      0,
    );
    return 768 + 2 * (Buffer.byteLength(prefix) + childBytes);
  }
  throw new Error("Invalid serialized MPF node kind in block path cache");
};

export type MpfEngine = "legacy" | "overlay" | "event_flat";
export type MpfScratchBuild = "insert" | "fromlist";
export type MpfPathHydrationMode = "whole_block" | "chunked" | "chunked_arena";

export type MpfArenaLimits = {
  readonly pathCacheMaxNodes: number;
  readonly pathCacheMaxBytes: number;
  readonly liveArenaMaxNodes: number;
  readonly liveArenaMaxBytes: number;
};

export type ParkedMpfOverlayV1 = {
  readonly schemaVersion: 1;
  readonly trieName: string;
  readonly baseRoot: ArrayBuffer;
  readonly candidateRoot: ArrayBuffer;
  readonly closureDigest: ArrayBuffer;
  readonly nodeCount: number;
  readonly nodeHashes: ArrayBuffer;
  readonly nodeValues: ArrayBuffer;
  /** Uint32 pairs of JSON byte offset/length, one pair per node hash. */
  readonly nodeValueOffsets: ArrayBuffer;
  readonly encodedBytes: number;
};

const DEFAULT_MPF_ARENA_LIMITS: MpfArenaLimits = {
  pathCacheMaxNodes: 1_000_000,
  pathCacheMaxBytes: 1024 * 1024 * 1024,
  liveArenaMaxNodes: 1_000_000,
  liveArenaMaxBytes: 1024 * 1024 * 1024,
};

export type MpfPathHydrationConfig = {
  readonly mode: MpfPathHydrationMode;
  readonly chunkOps: number;
  readonly retainDepth: number;
};

let configuredMpfScratchBuild: MpfScratchBuild = "insert";
let configuredMpfPathHydration: MpfPathHydrationConfig = {
  mode: "whole_block",
  chunkOps: 512,
  retainDepth: 2,
};
let configuredMpfArenaLimits: MpfArenaLimits = {
  ...DEFAULT_MPF_ARENA_LIMITS,
};

export const setMpfScratchBuild = (mode: MpfScratchBuild): void => {
  configuredMpfScratchBuild = mode;
};

export const getMpfScratchBuild = (): MpfScratchBuild =>
  configuredMpfScratchBuild;

export const configureMpfPathHydration = ({
  mode,
  chunkOps,
  retainDepth,
}: MpfPathHydrationConfig): void => {
  if (!Number.isSafeInteger(chunkOps) || chunkOps <= 0) {
    throw new Error("MPF hydration chunk ops must be a positive safe integer");
  }
  if (
    !Number.isSafeInteger(retainDepth) ||
    retainDepth < 0 ||
    retainDepth > 8
  ) {
    throw new Error("MPF retained hydration depth must be between 0 and 8");
  }
  configuredMpfPathHydration = { mode, chunkOps, retainDepth };
};

export const getMpfPathHydrationConfig = (): MpfPathHydrationConfig => ({
  ...configuredMpfPathHydration,
});

export const configureMpfArenaLimits = (limits: MpfArenaLimits): void => {
  for (const [name, value] of Object.entries(limits)) {
    positiveSafeInteger(value, name);
  }
  configuredMpfArenaLimits = { ...limits };
};

export const resetMpfArenaLimits = (): void => {
  configuredMpfArenaLimits = { ...DEFAULT_MPF_ARENA_LIMITS };
};

const getMpfArenaLimits = (): MpfArenaLimits => ({
  ...configuredMpfArenaLimits,
});

const exactArrayBuffer = (bytes: Uint8Array): ArrayBuffer =>
  bytes.buffer.slice(
    bytes.byteOffset,
    bytes.byteOffset + bytes.byteLength,
  ) as ArrayBuffer;

const parkedOverlayDigest = ({
  trieName,
  baseRoot,
  candidateRoot,
  nodeCount,
  nodeHashes,
  nodeValues,
  nodeValueOffsets,
}: Omit<
  ParkedMpfOverlayV1,
  "schemaVersion" | "closureDigest" | "encodedBytes"
>): Buffer => {
  const trieNameBytes = Buffer.from(trieName);
  const header = Buffer.alloc(8);
  header.writeUInt32BE(trieNameBytes.length, 0);
  header.writeUInt32BE(nodeCount, 4);
  const hash = blake2b.create({ dkLen: 32 });
  hash.update(header);
  hash.update(trieNameBytes);
  hash.update(new Uint8Array(baseRoot));
  hash.update(new Uint8Array(candidateRoot));
  hash.update(new Uint8Array(nodeHashes));
  hash.update(new Uint8Array(nodeValueOffsets));
  hash.update(new Uint8Array(nodeValues));
  return Buffer.from(hash.digest());
};

export type MpfStoreDiagnostics = {
  readonly entries: number;
  readonly storePuts: number;
  readonly storeDels: number;
  readonly serialiseCalls: number;
  readonly serialiseMs: number;
  readonly deferredMaterializedEstimatedBytes: number;
  readonly deferredMaterializedActualBytes: number;
  readonly deferredLazyReads: number;
  readonly deferredLazySerialiseMs: number;
  readonly deferredLazySerialisedBytes: number;
  readonly arenaCheckpointCalls: number;
  readonly arenaCheckpointMs: number;
  readonly arenaCheckpointNodes: number;
  readonly arenaCheckpointBytes: number;
  readonly pathCacheEntries: number;
  readonly pathCacheBytes: number;
  readonly pathCacheHits: number;
  readonly liveArenaPrunedNodes: number;
  readonly liveArenaPromotedNodes: number;
  readonly liveArenaPromotedBytes: number;
  readonly retainedSnapshotAuthentications: number;
  readonly retainedSnapshotAuthenticationMs: number;
  readonly transientLiveNodes: number;
  readonly transientLiveBytes: number;
  readonly transientDirtyNodes: number;
  readonly transientSnapshotsCaptured: number;
  readonly eventAtomicFinalizations: number;
  readonly eventAtomicDirtyNodes: number;
  readonly eventAtomicMaxDirtyNodes: number;
  readonly levelGets: number;
  readonly levelGetManyCalls: number;
  readonly levelGetManyMaxKeys: number;
  readonly levelGetMs: number;
  readonly jsonCodecMs: number;
  readonly overlayHits: number;
  readonly readCacheHits: number;
  readonly levelBatchWrites: number;
  readonly bytesFlushed: number;
  readonly overlayEntries: number;
  readonly overlayBytes: number;
  readonly overlaySpills: number;
  readonly overlaySpillMs: number;
  readonly flushMs: number;
};

export type MpfPathHydrationDiagnostics = {
  readonly prefetchMs: number;
  readonly uniquePaths: number;
  readonly nodesRequested: number;
  readonly hydrationHits: number;
  readonly hydrationMisses: number;
  readonly loadedNodes: number;
  readonly maxInFlight: number;
  readonly maxBatchKeys: number;
  readonly maxFrontierPaths: number;
  readonly retainedBytesEstimate: number;
  readonly chunkCount: number;
  readonly checkpointMs: number;
  readonly authenticationMs: number;
  readonly materializeMs: number;
  readonly collapseMs: number;
  readonly checkpointSerializedNodes: number;
  readonly checkpointSerializedBytes: number;
  readonly verifiedUpperNodes: number;
  readonly retainedUpperNodes: number;
  readonly collapsedNodes: number;
  readonly peakDecodedNodes: number;
};

export type MpfArenaCheckpointDiagnostics = {
  readonly checkpointMs: number;
  readonly authenticationMs: number;
  readonly materializeMs: number;
  readonly collapseMs: number;
  readonly serializedNodes: number;
  readonly serializedBytes: number;
  readonly verifiedUpperNodes: number;
  readonly retainedUpperNodes: number;
  readonly collapsedNodes: number;
};

type LevelBatchOp =
  | {
      readonly type: "put";
      readonly key: string;
      readonly value: MpfStoredValue;
      readonly encodedBytes?: number;
    }
  | { readonly type: "del"; readonly key: string };

const normalizeRootMarkerHex = (rootHex: string, fieldName: string): string =>
  normalizeHex(rootHex, { fieldName, byteLength: 32 });

const normalizeStoredRootHex = (rootHex: string): string => {
  const normalized = normalizeRootMarkerHex(rootHex, "MPF root marker");
  return normalized === MPF_INTERNAL_NULL_ROOT_HEX
    ? MPF_EMPTY_ROOT_HEX
    : normalized;
};

const parseStoredRootHex = (rootHex: unknown): Buffer => {
  if (rootHex === undefined) {
    return MPF_EMPTY_ROOT;
  }
  if (typeof rootHex !== "string") {
    throw new Error("Persisted MPF root marker is not a string");
  }
  const normalized = normalizeRootMarkerHex(
    rootHex,
    "Persisted MPF root marker",
  );
  if (normalized === MPF_INTERNAL_NULL_ROOT_HEX) {
    throw new Error(
      "Persisted MPF root marker uses the library internal null root instead of the canonical Midgard empty root",
    );
  }
  return Buffer.from(normalized, "hex");
};

const applyPendingBatch = (
  key: string,
  value: MpfReadableValue | undefined,
  ops: readonly LevelBatchOp[] | undefined,
): MpfReadableValue | undefined =>
  (ops ?? []).reduce<MpfReadableValue | undefined>((current, op) => {
    if (op.key !== key) {
      return current;
    }
    return op.type === "put" ? op.value : undefined;
  }, value);

export const encodeTransactionRootValue = (
  txCanonicalCbor: Buffer,
  consensusProfile: MidgardConsensusProfileV1 = MIDGARD_CONSENSUS_PROFILE_V1,
): Buffer => {
  if (!isMidgardConsensusProfileV1(consensusProfile)) {
    throw new Error("Refusing transaction under a non-V1 consensus profile");
  }
  const violation = validateMidgardConsensusV1TxCbor(txCanonicalCbor);
  if (violation !== null) {
    throw new Error(
      `Refusing transaction outside the exact canonical V1 consensus profile: ${violation.code} ${violation.featureId} ${violation.detail}`,
    );
  }
  const source =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(txCanonicalCbor);
  const transactionId = computeMidgardNativeTxIdV1(
    decodeMidgardNativeTxFullV1FromCanonicalCbor(txCanonicalCbor),
  );
  const value: SDK.L2TransactionSourceV1 = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return Buffer.from(
    aikenSerialisedPlutusDataCbor(
      LucidData.to(value, SDK.L2TransactionSourceV1),
    ),
    "hex",
  );
};

export const COMMIT_REJECT_CODE_DECODE_FAILED = "E_COMMIT_CBOR_DESERIALIZATION";
export const COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT =
  "E_COMMIT_WITHDRAWN_REFERENCE_INPUT";
export const COMMIT_REJECT_CODE_SAME_BLOCK_DEPOSIT_INPUT =
  "E_COMMIT_SAME_BLOCK_DEPOSIT_INPUT";
export const COMMIT_REJECT_CODE_FORCED_TRANSACTION_INPUT =
  "E_COMMIT_FORCED_TRANSACTION_INPUT";

export type ResolvedTxDeltaForCommit =
  | {
      readonly _tag: "Decoded";
      readonly spent: readonly Buffer[];
      readonly produced: readonly Ledger.MinimalEntry[];
    }
  | {
      readonly _tag: "Rejected";
      readonly rejection: TxRejectionsDB.EntryNoTimestamp;
    };

export const commitTxDeltaCacheHitCounter = Metric.counter(
  "commit_tx_delta_cache_hit_total",
  {
    description:
      "Commit candidates resolved from the best-effort mempool tx-delta cache",
    bigint: true,
    incremental: true,
  },
);

export const commitTxDeltaFallbackDecodedCounter = Metric.counter(
  "commit_tx_delta_fallback_decoded_total",
  {
    description:
      "Commit candidates successfully decoded from canonical CBOR after a tx-delta cache miss",
    bigint: true,
    incremental: true,
  },
);

export const resolveTxDeltaForCommit = (
  entry: Tx.EntryWithTimeStamp,
  existingDelta: MempoolTxDeltasDB.TxDelta | undefined,
): Effect.Effect<ResolvedTxDeltaForCommit, never> =>
  Effect.gen(function* () {
    if (existingDelta !== undefined) {
      return {
        _tag: "Decoded",
        spent: existingDelta.spent.map((outRef) => Buffer.from(outRef)),
        produced: existingDelta.produced.map((deltaEntry) => ({
          [Ledger.Columns.OUTREF]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTREF],
          ),
          [Ledger.Columns.OUTPUT]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTPUT],
          ),
        })),
      };
    }

    const txId = entry[Tx.Columns.TX_ID];
    const txCbor = entry[Tx.Columns.TX];
    const decoded = yield* findSpentAndProducedUTxOs(txCbor, txId).pipe(
      Effect.either,
    );
    if (decoded._tag === "Left") {
      return {
        _tag: "Rejected",
        rejection: {
          [TxRejectionsDB.Columns.TX_ID]: Buffer.from(txId),
          [TxRejectionsDB.Columns.REJECT_CODE]:
            COMMIT_REJECT_CODE_DECODE_FAILED,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: decoded.left.message,
        },
      };
    }

    return {
      _tag: "Decoded",
      spent: decoded.right.spent,
      produced: decoded.right.produced,
    };
  });

export const configureCommitMpfRuntime = (
  nodeConfig: Pick<
    NodeConfigDep,
    | "MPF_SCRATCH_BUILD"
    | "MPF_PATH_HYDRATION_MODE"
    | "MPF_HYDRATION_CHUNK_OPS"
    | "MPF_RETAIN_HYDRATED_DEPTH"
    | "MPF_PARALLEL_ROOTS"
    | "MPF_ROOT_WORKERS"
    | "MPF_PARALLEL_ROOT_MIN_ENTRIES"
  >,
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    setMpfScratchBuild(nodeConfig.MPF_SCRATCH_BUILD);
    configureMpfPathHydration({
      mode: nodeConfig.MPF_PATH_HYDRATION_MODE,
      chunkOps: nodeConfig.MPF_HYDRATION_CHUNK_OPS,
      retainDepth: nodeConfig.MPF_RETAIN_HYDRATED_DEPTH,
    });
    configureMpfRootWorkers({
      enabled: nodeConfig.MPF_PARALLEL_ROOTS,
      workers: nodeConfig.MPF_ROOT_WORKERS,
      minEntries: nodeConfig.MPF_PARALLEL_ROOT_MIN_ENTRIES,
    });
    if (nodeConfig.MPF_PARALLEL_ROOTS) {
      yield* Effect.tryPromise({
        try: () => prewarmMpfRootWorkers(),
        catch: (cause) => MpfError.rootBuild("MPF root worker warmup", cause),
      });
    }
  });

export const makeMpfs: Effect.Effect<
  { ledgerMpf: MidgardMpf; transactionsMpf: MidgardMpf },
  DatabaseError | MpfError,
  Database | NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  if (nodeConfig.MPF_ENGINE === "architecture_g") {
    return yield* Effect.fail(
      MpfError.create(
        "architecture-g-owner",
        new Error(
          "Architecture G ledger ownership must be supplied by the main-process native owner; refusing to open its Level path in a commit worker",
        ),
      ),
    );
  }
  yield* configureCommitMpfRuntime(nodeConfig);
  const transactionsMpf = yield* MidgardMpf.create(
    "transactions",
    nodeConfig.TRANSACTIONS_MPF_DB_PATH,
    {
      engine: nodeConfig.MPF_ENGINE,
      spillThresholdBytes: nodeConfig.MPF_OVERLAY_SPILL_BYTES,
    },
  );
  const ledgerMpf = yield* MidgardMpf.create(
    "ledger",
    nodeConfig.LEDGER_MPF_DB_PATH,
    {
      engine: nodeConfig.MPF_ENGINE,
      spillThresholdBytes: nodeConfig.MPF_OVERLAY_SPILL_BYTES,
    },
  );
  const ledgerRootIsEmpty = yield* ledgerMpf.rootIsEmpty();
  if (ledgerRootIsEmpty) {
    yield* Effect.logInfo(
      "🔹 No previous ledger MPF root found - inserting genesis utxos",
    );
    const genesisEntries = yield* Effect.forEach(
      nodeConfig.GENESIS_UTXOS,
      (u: UTxO) =>
        utxoToLedgerInsertMaterialV1(u).pipe(
          Effect.mapError((e) => MpfError.rootBuild("ledger genesis", e)),
          Effect.map(({ ledgerOp, outputCbor }) => ({
            op: ledgerOp,
            ledgerEntry: {
              [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(u.txHash, "hex"),
              [MempoolLedgerDB.Columns.OUTREF]: ledgerOp.key,
              [MempoolLedgerDB.Columns.OUTPUT]: outputCbor,
              [MempoolLedgerDB.Columns.ADDRESS]: u.address,
              [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
            } satisfies MempoolLedgerDB.EntryNoTimeStamp,
          })),
        ),
    );
    yield* MempoolLedgerDB.insert(
      genesisEntries.map(({ ledgerEntry }) => ledgerEntry),
    );
    const ops = genesisEntries.map(({ op }) => op);
    yield* ledgerMpf.applyBatch(ops);
    const rootAfterGenesis = yield* ledgerMpf.rootHex();
    yield* Effect.logInfo(
      `🔹 New ledger MPF root after inserting genesis utxos: ${rootAfterGenesis}`,
    );
  }
  yield* MpfEngineStateDB.stampLedgerMigration(yield* ledgerMpf.rootHex());
  return {
    ledgerMpf,
    transactionsMpf,
  };
});

export const ledgerEntryToInsertBatchOp = (
  entry: Ledger.MinimalEntry,
): MpfInsertBatchOp =>
  ledgerOutputToInsertBatchOpV1({
    outRef: entry[Ledger.Columns.OUTREF],
    outputCbor: entry[Ledger.Columns.OUTPUT],
  });

export const ledgerOutputToInsertBatchOpV1 = ({
  outRef,
  outputCbor,
}: {
  readonly outRef: Uint8Array;
  readonly outputCbor: Uint8Array;
}): MpfInsertBatchOp => ({
  type: "insert",
  key: Buffer.from(outRef),
  value: buildCanonicalMidgardLedgerEntryOutputMaterialV1({
    outRef,
    outputCbor,
  }).descriptorCbor,
});

export const computeLedgerMpfRootFromLedgerEntries = (
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<string, MpfError> =>
  Effect.try({
    try: () => entries.map(ledgerEntryToInsertBatchOp),
    catch: (cause) => MpfError.rootBuild("ledger descriptor root", cause),
  }).pipe(
    Effect.flatMap((ops) =>
      keyValuePhasRoot(
        ops.map((op) => op.key),
        ops.map((op) => op.value),
      ),
    ),
  );

export const hydrateLedgerMpfFromLedgerEntries = (
  ledgerMpf: MidgardMpf,
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<string, MpfError> =>
  Effect.gen(function* () {
    yield* ledgerMpf.resetToEmpty();
    const ops = yield* Effect.try({
      try: () => entries.map(ledgerEntryToInsertBatchOp),
      catch: (cause) =>
        MpfError.rootBuild("ledger descriptor hydration", cause),
    });
    yield* ledgerMpf.applyBatch(ops);
    return yield* ledgerMpf.rootHex();
  });

export const synchronizeCommitMpfStoresFromLedgerEntries = (
  entries: readonly Ledger.MinimalEntry[],
): Effect.Effect<
  {
    readonly ledgerEntryCount: number;
    readonly ledgerRoot: string;
    readonly transactionsRoot: string;
  },
  MpfError,
  NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    if (nodeConfig.MPF_ENGINE === "architecture_g") {
      return yield* Effect.fail(
        MpfError.create(
          "architecture-g-owner",
          new Error(
            "Architecture G ledger ownership belongs to the live native owner; refusing to reopen its LevelDB path for persistent-store synchronization",
          ),
        ),
      );
    }
    const ledgerMpf = yield* MidgardMpf.create(
      "ledger",
      nodeConfig.LEDGER_MPF_DB_PATH,
    );
    const transactionsMpf = yield* MidgardMpf.create(
      "transactions",
      nodeConfig.TRANSACTIONS_MPF_DB_PATH,
    );
    const closeMpfs = Effect.all(
      [
        ledgerMpf.close().pipe(Effect.catchAll(() => Effect.void)),
        transactionsMpf.close().pipe(Effect.catchAll(() => Effect.void)),
      ],
      { discard: true },
    );
    return yield* Effect.gen(function* () {
      const ledgerRoot = yield* hydrateLedgerMpfFromLedgerEntries(
        ledgerMpf,
        entries,
      );
      yield* transactionsMpf.resetToEmpty();
      const transactionsRoot = yield* transactionsMpf.rootHex();
      yield* Effect.logInfo(
        `Synchronized commit MPF stores from confirmed ledger: ledger_entries=${entries.length.toString()},ledger_root=${ledgerRoot},transactions_root=${transactionsRoot}`,
      );
      return {
        ledgerEntryCount: entries.length,
        ledgerRoot,
        transactionsRoot,
      };
    }).pipe(Effect.ensuring(closeMpfs));
  });

export const synchronizeCommitMpfStoresFromConfirmedLedger: Effect.Effect<
  {
    readonly ledgerEntryCount: number;
    readonly ledgerRoot: string;
    readonly transactionsRoot: string;
  },
  DatabaseError | MpfError,
  Database | NodeConfig
> = Effect.gen(function* () {
  const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
  return yield* synchronizeCommitMpfStoresFromLedgerEntries(confirmedEntries);
});

export const utxoToLedgerInsertMaterialV1 = (
  utxo: UTxO,
): Effect.Effect<
  {
    readonly ledgerOp: MpfInsertBatchOp;
    readonly outputCbor: Buffer;
  },
  SDK.CmlDeserializationError
> =>
  Effect.gen(function* () {
    // The MPF trie key is the §5.3 field-0/1 item encoding, byte-for-byte what
    // on-chain `ledger_outref_key` derives through `encode_midgard_tx_input`.
    // CML's minimal-index TransactionInput CBOR is 36 bytes for indices 0–23 and
    // would key the trie where the on-chain side never looks.
    const outRef = yield* Effect.try({
      try: () => outRefToCbor(utxo),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to encode UTxO outref as the §5.3 ledger key",
          cause: e,
        }),
    });
    const output = yield* Effect.try({
      try: () =>
        encodeMidgardTxOutput(utxo.address, utxo.assets, {
          ...(utxo.datum == null
            ? {}
            : { datum: { kind: "inline" as const, data: utxo.datum } }),
        }),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to convert UTxO to Midgard output CBOR",
          cause: e,
        }),
    });
    const ledgerOp = yield* Effect.try({
      try: () => ledgerOutputToInsertBatchOpV1({ outRef, outputCbor: output }),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: "Failed to derive the canonical V1 genesis descriptor",
          cause: e,
        }),
    });
    return { ledgerOp, outputCbor: output };
  });

export const deleteMpfStore = (
  path: string,
  name: string,
): Effect.Effect<void, FileSystemError> =>
  Effect.try({
    try: () => FS.rmSync(path, { recursive: true, force: true }),
    catch: (e) =>
      new FileSystemError({
        message: `Failed to delete ${name}'s MPF LevelDB store from disk`,
        cause: e,
      }),
  }).pipe(Effect.withLogSpan(`Delete ${name} MPF store`));

export type ProcessMpfsConfig = {
  readonly consensusProfile?: ContractDeploymentIdentityValue["consensusProfile"];
  readonly forcedValidation?: {
    readonly expectedNetworkId: bigint;
    readonly minFeeA: bigint;
    readonly minFeeB: bigint;
    readonly bucketConcurrency: number;
    readonly slotForUnixTime: (unixTimeMs: number) => bigint;
  };
  readonly currentBlockStartTime?: Date;
  readonly processedOnlyEndTime?: Date;
  readonly depositOnlyEndTime?: Date;
  readonly depositVisibilityBarrierTime?: Date;
  readonly txOrderVisibilityBarrierTime?: Date;
  readonly withdrawalVisibilityBarrierTime?: Date;
  readonly initialLedgerEntries?: readonly Ledger.MinimalEntry[];
  readonly selectedBaseUtxoRoot?: string;
  readonly payloadRootCheck?: "every_block" | "periodic" | "off";
  readonly baseUtxoPayloadAggregate?: UtxoPayloadSizeAggregate;
  readonly recordCorpusPath?: string;
  readonly excludedDepositEventIds?: ReadonlySet<string>;
  readonly excludedForcedTransactionEventIds?: ReadonlySet<string>;
  readonly excludedWithdrawalEventIds?: ReadonlySet<string>;
  /**
   * A speculative build must leave every durable queue/event row untouched
   * until the submitter owns the state-queue lease. The caller is responsible
   * for applying the returned projection/rejection side effects immediately
   * before submission.
   */
  readonly deferDatabaseWrites?: boolean;
  readonly nativeMpf?: NativeMpfBuildContext;
  /**
   * V1 blocks must retain a descriptor for every forced and included
   * normal transaction. The provider is deliberately mandatory for that
   * generation: a header must never substitute an empty or synthetic trace
   * when the deterministic validation machine is unavailable.
   */
  readonly validationTraceBuilder?: (
    input: ValidationTraceBuildInput,
  ) => Effect.Effect<
    readonly RetainedValidationTraceMember[],
    MpfError | DatabaseError
  >;
};

export type NativeMpfBuildContext = {
  readonly client: NativeMpfOwnerClient;
  readonly handle: NativeMpfGenerationHandle;
  readonly ownerBinarySha256: string;
  eventLog?: Buffer;
  eventLogDigest?: string;
  eventRoots?: readonly string[];
  candidateRoot?: string;
};

export type NativeMpfReplayBuild = {
  readonly schema: 1;
  readonly ownerBinarySha256: Buffer;
  readonly baseRoot: Buffer;
  readonly candidateRoot: Buffer;
  readonly eventLog: Buffer;
  readonly eventLogDigest: Buffer;
  readonly eventRoots: Buffer;
  readonly eventCount: number;
};

/**
 * Applies a commit-stage rejection as one database mutation. CEK admission
 * ownership is released only if both the mempool removal and durable rejection
 * record commit successfully.
 */
export const persistCommitStageRejectedTransactions = ({
  rejectedTxHashes,
  rejectionEntries,
}: {
  readonly rejectedTxHashes: readonly Buffer[];
  readonly rejectionEntries: readonly TxRejectionsDB.EntryNoTimestamp[];
}): Effect.Effect<void, DatabaseError, Database> => {
  if (rejectedTxHashes.length === 0) return Effect.void;
  return Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* MempoolDB.clearTxs([...rejectedTxHashes]);
        yield* TxRejectionsDB.insertMany(rejectionEntries);
        yield* CekProgramMaterialDB.releaseAdmissionOwnership(rejectedTxHashes);
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      MempoolDB.tableName,
      "Failed to persist commit-stage transaction rejections",
    ),
  );
};

type CorpusHexEntry = { readonly key: string; readonly value: string };
type CorpusLedgerOp =
  | { readonly type: "insert"; readonly key: string; readonly value: string }
  | { readonly type: "delete"; readonly key: string };

export type MpfReplayCorpusBlock = {
  readonly version: 1;
  readonly label: string;
  readonly initialLedgerEntries: readonly CorpusHexEntry[];
  readonly sourceEvents: readonly {
    readonly phase: SDK.TransitionPhase;
    readonly eventKeyCbor: string;
    readonly ledgerOps: readonly CorpusLedgerOp[];
  }[];
  readonly transactionOps: readonly CorpusHexEntry[];
  readonly deposits: readonly CorpusHexEntry[];
  readonly withdrawals: readonly CorpusHexEntry[];
  readonly forcedTransactions: readonly CorpusHexEntry[];
  readonly finalUtxoEntries: readonly CorpusHexEntry[];
  readonly expected: {
    readonly utxoRoot: string;
    readonly rawTxRoot: string;
    readonly txRoot: string;
    readonly transitionTraceRoot: string;
    readonly eventToStepRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
    readonly forcedTransactionsRoot: string;
    readonly transitionRoots: readonly {
      readonly pre: string;
      readonly post: string;
    }[];
  };
};

export type UtxoPayloadEntry = {
  readonly outref: Buffer;
  readonly output: Buffer;
};

export const applyLedgerOpsToUtxoPayloadAggregateFromFullValues = (
  base: UtxoPayloadSizeAggregate,
  ops: readonly MpfBatchOp[],
  initialValues: ReadonlyMap<string, Buffer>,
  insertedValues: ReadonlyMap<string, Buffer>,
): Effect.Effect<UtxoPayloadSizeAggregate, MpfError> =>
  Effect.gen(function* () {
    let entryCount = base.entryCount;
    let encodedTupleBytes = base.encodedTupleBytes;
    const currentValues = new Map<string, Buffer | null>();
    for (const op of ops) {
      const keyHex = op.key.toString("hex");
      const current = currentValues.has(keyHex)
        ? currentValues.get(keyHex)
        : (initialValues.get(keyHex) ?? null);
      if (current !== null && current !== undefined) {
        entryCount -= 1;
        encodedTupleBytes -= utxoPayloadEntryEncodedSize({
          outref: op.key,
          output: current,
        });
      } else if (op.type === "delete") {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "DA UTxO size aggregate",
            new Error(`Cannot size deletion of missing UTxO ${keyHex}`),
          ),
        );
      }
      if (op.type === "insert") {
        const inserted = insertedValues.get(keyHex);
        if (inserted === undefined) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "DA UTxO size aggregate",
              new Error(`Missing full output bytes for insertion ${keyHex}`),
            ),
          );
        }
        entryCount += 1;
        encodedTupleBytes += utxoPayloadEntryEncodedSize({
          outref: op.key,
          output: inserted,
        });
        currentValues.set(keyHex, Buffer.from(inserted));
      } else {
        currentValues.set(keyHex, null);
      }
    }
    const aggregate = { entryCount, encodedTupleBytes };
    try {
      SDK.daPayloadEntriesEncodedSizeFromAggregate(aggregate);
    } catch (cause) {
      return yield* Effect.fail(
        MpfError.rootBuild("DA UTxO size aggregate", cause),
      );
    }
    return aggregate;
  });

export type TransitionTraceSourceEvent = {
  readonly eventKey: SDK.EventKey;
  readonly phase: SDK.TransitionPhase;
  readonly ledgerOps: readonly MpfBatchOp[];
};

export type RetainedTransitionTraceMember = {
  readonly stepIndex: bigint;
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
  readonly value: SDK.TransitionStep;
};

export type RetainedEventToStepMember = {
  readonly eventKey: SDK.EventKey;
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
  readonly value: SDK.EventToStepValue;
};

export type ValidationTraceTransactionInput = {
  readonly eventKey: SDK.EventKey;
  readonly transactionId: Buffer;
  readonly canonicalTransactionCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly sourceKind: "normal" | "forced";
  readonly priorUtxosRoot: string;
  readonly postUtxosRoot: string;
  readonly ledgerOps: readonly MpfBatchOp[];
  readonly ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  readonly ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[];
  readonly verdict: "accepted" | "rejected";
  readonly rejectionCode: RejectCode | null;
};

export type ValidationTraceBuildInput = {
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly blockEndTime: Date;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly blockSlot: bigint;
  readonly transactions: readonly ValidationTraceTransactionInput[];
};

export type RetainedValidationTraceMember = {
  readonly eventKey: SDK.EventKey;
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
  readonly value: SDK.ValidationTraceDescriptorV1;
};

export type ValidationTraceBuildResult = {
  readonly validationTracesRoot: string;
  readonly validationTraceMembers: readonly RetainedValidationTraceMember[];
  readonly validationTraceCount: number;
};

export const buildDeterministicValidationTraceMembers = (
  input: ValidationTraceBuildInput,
): Effect.Effect<
  readonly RetainedValidationTraceMember[],
  DatabaseError | MpfError
> =>
  Effect.forEach(
    input.transactions,
    (transaction) =>
      Effect.gen(function* () {
        const keyCbor = yield* eventKeyCbor(transaction.eventKey);
        const trace = yield* buildDeterministicValidationMachineTrace({
          consensusProfile: input.consensusProfile,
          eventKeyCbor: keyCbor,
          transactionId: transaction.transactionId,
          canonicalTransactionCbor: transaction.canonicalTransactionCbor,
          programMaterialSidecarCbor: transaction.programMaterialSidecarCbor,
          sourceKind: transaction.sourceKind,
          priorUtxosRoot: transaction.priorUtxosRoot,
          postUtxosRoot: transaction.postUtxosRoot,
          ledgerWitnessEntries: transaction.ledgerWitnessEntries,
          ledgerMutationSteps: transaction.ledgerMutationSteps,
          expectedLedgerOps: transaction.ledgerOps,
          expectedVerdict: transaction.verdict,
          expectedRejectionCode: transaction.rejectionCode,
          blockEndTimeMs: input.blockEndTime.getTime(),
          expectedNetworkId: input.expectedNetworkId,
          minFeeA: input.minFeeA,
          minFeeB: input.minFeeB,
          blockSlot: input.blockSlot,
        }).pipe(
          Effect.mapError(
            (cause) =>
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Deterministic validation-machine replay failed while building a V1 block",
                cause,
              }),
          ),
        );
        const descriptor: SDK.ValidationTraceDescriptorV1 = {
          schema_version: BigInt(trace.tree.descriptor.schemaVersion),
          machine_version: BigInt(trace.tree.descriptor.machineVersion),
          trace_root: trace.tree.descriptor.traceRoot.toString("hex"),
          step_count: BigInt(trace.tree.descriptor.stepCount),
          initial_state_hash:
            trace.tree.descriptor.initialStateHash.toString("hex"),
          terminal_state_hash:
            trace.tree.descriptor.terminalStateHash.toString("hex"),
          verdict:
            trace.tree.descriptor.verdict === "accepted"
              ? "Accepted"
              : "Rejected",
          rejection_code_hash:
            trace.tree.descriptor.rejectionCodeHash.toString("hex"),
        };
        return {
          eventKey: transaction.eventKey,
          keyCbor,
          valueCbor: Buffer.from(
            LucidData.to(
              descriptor as never,
              SDK.ValidationTraceDescriptorV1 as never,
            ),
            "hex",
          ),
          value: descriptor,
        };
      }),
    { concurrency: 1 },
  );

export type TransitionTraceBuildResult = {
  readonly finalUtxosRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
  readonly eventToStepMembers: readonly RetainedEventToStepMember[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly totalEventCount: number;
  readonly transitionStepCount: number;
  readonly pathHydration: MpfPathHydrationDiagnostics;
  readonly nativePhaseMs?: {
    readonly validation: number;
    readonly eventLogEncode: number;
    readonly ownerApply: number;
    readonly ownerProofArena: number;
    readonly ownerMutation: number;
    readonly memberAssembly: number;
    readonly retainedRoots: number;
  };
};

export type DecodedMempoolTxForCommit = {
  readonly entry: Tx.EntryWithTimeStamp;
  readonly txHash: Buffer;
  readonly txCbor: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly Ledger.MinimalEntry[];
};

export const establishEffectiveEndTimeFromDecodedMempool = (
  decodedMempoolTxs: readonly {
    readonly entry: Tx.EntryWithTimeStamp;
  }[],
  processedOnlyEndTime?: Date,
  depositOnlyEndTime?: Date,
): Date | undefined =>
  decodedMempoolTxs.at(-1)?.entry[Tx.Columns.TIMESTAMPTZ] ??
  processedOnlyEndTime ??
  depositOnlyEndTime;

const outputReferenceFromCbor = (
  cbor: Buffer,
  label: string,
): Effect.Effect<SDK.OutputReference, MpfError> =>
  Effect.try({
    try: () =>
      LucidData.from(
        cbor.toString("hex"),
        SDK.OutputReference,
      ) as SDK.OutputReference,
    catch: (cause) =>
      MpfError.rootBuild(
        "transition trace event key",
        new Error(`Failed to decode ${label} as OutputReference CBOR`, {
          cause,
        }),
      ),
  });

const withdrawalTraceEventKey = (
  entry: WithdrawalsDB.Entry,
): Effect.Effect<SDK.EventKey, MpfError> =>
  outputReferenceFromCbor(
    entry[WithdrawalsDB.Columns.ID],
    "withdrawal event id",
  ).pipe(
    Effect.map((withdrawalId) => ({
      WithdrawalEventKey: { withdrawal_id: withdrawalId },
    })),
  );

const forcedTransactionTraceEventKey = (
  entry: ForcedTransactionsDB.Entry,
): Effect.Effect<SDK.EventKey, MpfError> =>
  outputReferenceFromCbor(
    entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
    "forced transaction tx_order_id",
  ).pipe(
    Effect.map((txOrderId) => ({
      ForcedTransactionEventKey: { tx_order_id: txOrderId },
    })),
  );

const depositTraceEventKey = (
  entry: DepositsDB.Entry,
): Effect.Effect<SDK.EventKey, MpfError> =>
  outputReferenceFromCbor(
    entry[DepositsDB.Columns.ID],
    "deposit event id",
  ).pipe(
    Effect.map((depositId) => ({
      DepositEventKey: { deposit_id: depositId },
    })),
  );

const l2TransactionTraceEventKey = (txHash: Buffer): SDK.EventKey => ({
  L2TransactionEventKey: { tx_id: txHash.toString("hex") },
});

export const orderDecodedMempoolTxsForLedgerApplication = (
  decodedMempoolTxs: readonly DecodedMempoolTxForCommit[],
): Effect.Effect<readonly DecodedMempoolTxForCommit[], DatabaseError, never> =>
  Effect.gen(function* () {
    if (decodedMempoolTxs.length <= 1) {
      return decodedMempoolTxs;
    }

    const txByHash = new Map<string, DecodedMempoolTxForCommit>();
    const originalIndexByTxHash = new Map<string, number>();
    const producerByOutRef = new Map<string, string>();

    for (const [index, decoded] of decodedMempoolTxs.entries()) {
      const txHashHex = decoded.txHash.toString("hex");
      if (txByHash.has(txHashHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: MempoolDB.tableName,
            message:
              "Refusing to build a block because the mempool candidate contains duplicate transaction ids",
            cause: `tx_id=${txHashHex}`,
          }),
        );
      }
      txByHash.set(txHashHex, decoded);
      originalIndexByTxHash.set(txHashHex, index);

      for (const produced of decoded.produced) {
        const outRefHex = produced[Ledger.Columns.OUTREF].toString("hex");
        const priorProducer = producerByOutRef.get(outRefHex);
        if (priorProducer !== undefined) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because multiple mempool transactions produce the same outref",
              cause: `outref=${outRefHex},first_tx_id=${priorProducer},duplicate_tx_id=${txHashHex}`,
            }),
          );
        }
        producerByOutRef.set(outRefHex, txHashHex);
      }
    }

    const dependenciesByTxHash = new Map<string, Set<string>>();
    const dependentsByTxHash = new Map<string, Set<string>>();
    for (const txHashHex of txByHash.keys()) {
      dependenciesByTxHash.set(txHashHex, new Set());
      dependentsByTxHash.set(txHashHex, new Set());
    }

    for (const decoded of decodedMempoolTxs) {
      const txHashHex = decoded.txHash.toString("hex");
      const dependencies = dependenciesByTxHash.get(txHashHex)!;
      const spentByThisTx = new Set<string>();

      for (const spent of decoded.spent) {
        const spentHex = spent.toString("hex");
        if (spentByThisTx.has(spentHex)) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because a mempool transaction spends the same outref more than once",
              cause: `tx_id=${txHashHex},outref=${spentHex}`,
            }),
          );
        }
        spentByThisTx.add(spentHex);

        const producerTxHash = producerByOutRef.get(spentHex);
        if (producerTxHash === undefined) {
          continue;
        }
        if (producerTxHash === txHashHex) {
          return yield* Effect.fail(
            new DatabaseError({
              table: MempoolDB.tableName,
              message:
                "Refusing to build a block because a mempool transaction spends an outref it also produces",
              cause: `tx_id=${txHashHex},outref=${spentHex}`,
            }),
          );
        }

        dependencies.add(producerTxHash);
        dependentsByTxHash.get(producerTxHash)!.add(txHashHex);
      }
    }

    const byOriginalIndex = (left: string, right: string) =>
      originalIndexByTxHash.get(left)! - originalIndexByTxHash.get(right)!;
    const ready = [...dependenciesByTxHash.entries()]
      .filter(([, dependencies]) => dependencies.size === 0)
      .map(([txHashHex]) => txHashHex)
      .sort(byOriginalIndex);
    const queued = new Set(ready);
    const ordered: DecodedMempoolTxForCommit[] = [];

    while (ready.length > 0) {
      const txHashHex = ready.shift()!;
      ordered.push(txByHash.get(txHashHex)!);

      for (const dependentTxHash of dependentsByTxHash.get(txHashHex)!) {
        const dependencies = dependenciesByTxHash.get(dependentTxHash)!;
        dependencies.delete(txHashHex);
        if (dependencies.size === 0 && !queued.has(dependentTxHash)) {
          ready.push(dependentTxHash);
          queued.add(dependentTxHash);
          ready.sort(byOriginalIndex);
        }
      }
    }

    if (ordered.length !== decodedMempoolTxs.length) {
      const blockedTxIds = [...dependenciesByTxHash.entries()]
        .filter(([, dependencies]) => dependencies.size > 0)
        .map(([txHashHex, dependencies]) => ({
          tx_id: txHashHex,
          depends_on: [...dependencies].sort(),
        }));
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolDB.tableName,
          message:
            "Refusing to build a block because same-block mempool dependencies are cyclic",
          cause: JSON.stringify(blockedTxIds),
        }),
      );
    }

    return ordered;
  });

const compareBufferHex = (left: Buffer, right: Buffer): number => {
  const leftHex = left.toString("hex");
  const rightHex = right.toString("hex");
  return leftHex < rightHex ? -1 : leftHex > rightHex ? 1 : 0;
};

const materializeUtxoPayloadEntries = (
  initialLedgerEntries: readonly Ledger.MinimalEntry[],
  ledgerOps: readonly MpfBatchOp[],
  insertedValues: ReadonlyMap<string, Buffer>,
): readonly UtxoPayloadEntry[] => {
  const entries = new Map<string, UtxoPayloadEntry>();
  for (const entry of initialLedgerEntries) {
    entries.set(entry[Ledger.Columns.OUTREF].toString("hex"), {
      outref: Buffer.from(entry[Ledger.Columns.OUTREF]),
      output: Buffer.from(entry[Ledger.Columns.OUTPUT]),
    });
  }
  for (const op of ledgerOps) {
    const key = op.key.toString("hex");
    if (op.type === "delete") {
      entries.delete(key);
      continue;
    }
    const output = insertedValues.get(key);
    if (output === undefined) {
      throw new Error(`Missing full output bytes for insertion ${key}`);
    }
    entries.set(key, {
      outref: Buffer.from(op.key),
      output: Buffer.from(output),
    });
  }
  return [...entries.values()].sort((left, right) =>
    compareBufferHex(left.outref, right.outref),
  );
};

export const computeUtxoPayloadRoot = (
  entries: readonly UtxoPayloadEntry[],
): Effect.Effect<string, MpfError> =>
  Effect.try({
    try: () =>
      entries.map((entry) =>
        ledgerOutputToInsertBatchOpV1({
          outRef: entry.outref,
          outputCbor: entry.output,
        }),
      ),
    catch: (cause) => MpfError.rootBuild("DA UTxO descriptor root", cause),
  }).pipe(
    Effect.flatMap((ops) =>
      keyValuePhasRoot(
        ops.map((op) => op.key),
        ops.map((op) => op.value),
      ),
    ),
  );

const encodeUnsignedBigEndian = (value: bigint): Buffer => {
  if (value <= 0n) return Buffer.from([0]);
  const bytes: number[] = [];
  for (let remaining = value; remaining > 0n; remaining >>= 8n) {
    bytes.push(Number(remaining & 0xffn));
  }
  bytes.reverse();
  return Buffer.from(bytes);
};

export const encodeTransitionIntegerCbor = (value: bigint): Buffer => {
  const magnitude = value >= 0n ? value : -1n - value;
  if (magnitude <= 0xffff_ffff_ffff_ffffn) {
    return encodeCborInteger(value);
  }
  return encodeCborTagRaw(
    value >= 0n ? 2n : 3n,
    encodeCborBytes(encodeUnsignedBigEndian(magnitude)),
  );
};

const encodeTransitionConstr = (
  alternative: 0 | 1 | 2 | 3,
  fields: readonly Buffer[],
): Buffer => {
  const tag = Buffer.from([0xd8, 0x79 + alternative]);
  return fields.length === 0
    ? Buffer.concat([tag, Buffer.from([0x80])])
    : Buffer.concat([tag, Buffer.from([0x9f]), ...fields, Buffer.from([0xff])]);
};

const encodeTransitionFixedBytes = (value: string, fieldName: string): Buffer =>
  encodeCborBytes(
    Buffer.from(normalizeHex(value, { fieldName, byteLength: 32 }), "hex"),
  );

const encodeTransitionOutputReference = (
  outputReference: SDK.OutputReference,
): Buffer =>
  encodeTransitionConstr(0, [
    encodeTransitionFixedBytes(
      outputReference.transactionId,
      "transition event transaction id",
    ),
    encodeTransitionIntegerCbor(outputReference.outputIndex),
  ]);

export const encodeTransitionPhaseCbor = (
  phase: SDK.TransitionPhase,
): Buffer => {
  switch (phase) {
    case "Withdrawal":
      return encodeTransitionConstr(0, []);
    case "ForcedTransaction":
      return encodeTransitionConstr(1, []);
    case "L2Transaction":
      return encodeTransitionConstr(2, []);
    case "Deposit":
      return encodeTransitionConstr(3, []);
  }
};

export const encodeTransitionEventKeyCbor = (
  eventKey: SDK.EventKey,
): Buffer => {
  if ("WithdrawalEventKey" in eventKey) {
    return encodeTransitionConstr(0, [
      encodeTransitionOutputReference(
        eventKey.WithdrawalEventKey.withdrawal_id,
      ),
    ]);
  }
  if ("ForcedTransactionEventKey" in eventKey) {
    return encodeTransitionConstr(1, [
      encodeTransitionOutputReference(
        eventKey.ForcedTransactionEventKey.tx_order_id,
      ),
    ]);
  }
  if ("L2TransactionEventKey" in eventKey) {
    return encodeTransitionConstr(2, [
      encodeTransitionFixedBytes(
        eventKey.L2TransactionEventKey.tx_id,
        "transition event transaction hash",
      ),
    ]);
  }
  return encodeTransitionConstr(3, [
    encodeTransitionOutputReference(eventKey.DepositEventKey.deposit_id),
  ]);
};

export const encodeTransitionStepCbor = (value: SDK.TransitionStep): Buffer =>
  encodeTransitionConstr(0, [
    encodeTransitionIntegerCbor(value.schema_version),
    encodeTransitionIntegerCbor(value.step_index),
    encodeTransitionEventKeyCbor(value.event_key),
    encodeTransitionPhaseCbor(value.phase),
    encodeTransitionFixedBytes(
      value.pre_utxos_root,
      "transition pre UTxO root",
    ),
    encodeTransitionFixedBytes(
      value.post_utxos_root,
      "transition post UTxO root",
    ),
  ]);

export const encodeEventToStepValueCbor = (
  value: SDK.EventToStepValue,
): Buffer =>
  encodeTransitionConstr(0, [
    encodeTransitionIntegerCbor(value.step_index),
    encodeTransitionPhaseCbor(value.phase),
  ]);

const countedRootFromEncodedEntries = (
  domain: SDK.RootDomain,
  entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
): Effect.Effect<string, MpfError> =>
  Effect.gen(function* () {
    if (shouldBuildMpfRootInWorker(entries.length)) {
      return yield* Effect.tryPromise({
        try: () => buildCountedMpfRootInWorker(domain, entries),
        catch: (cause) => MpfError.rootBuild("parallel counted root", cause),
      });
    }
    const phas = yield* keyValuePhasRootWithCount(
      entries.map((entry) => entry.key),
      entries.map((entry) => entry.value),
    );
    return yield* SDK.commitCountedRootProgram({
      domain,
      phasRoot: phas.root,
      count: phas.count,
    }).pipe(
      Effect.mapError((cause) =>
        MpfError.rootBuild(
          "count-bound transition commitment",
          new Error("Failed to commit count-bound root", { cause }),
        ),
      ),
    );
  });

export const buildTransactionsSourceRoot = (
  entries: readonly MpfInsertBatchOp[],
  domain: SDK.RootDomain = SDK.ROOT_DOMAINS.transactionsV1,
): Effect.Effect<string, MpfError> =>
  countedRootFromEncodedEntries(domain, entries);

const eventKeyCbor = (
  eventKey: SDK.EventKey,
): Effect.Effect<Buffer, MpfError> =>
  Effect.try({
    try: () => encodeTransitionEventKeyCbor(eventKey),
    catch: (cause) =>
      MpfError.rootBuild(
        "transition trace",
        new Error("Failed to encode transition event key", { cause }),
      ),
  });

const eventKeyFingerprint = (
  eventKey: SDK.EventKey,
): Effect.Effect<string, MpfError> =>
  eventKeyCbor(eventKey).pipe(Effect.map((encoded) => encoded.toString("hex")));

export const indexTransitionTraceMembersByEventKey = (
  members: readonly RetainedTransitionTraceMember[],
): Effect.Effect<
  ReadonlyMap<string, RetainedTransitionTraceMember>,
  MpfError
> =>
  Effect.gen(function* () {
    const byEventKey = new Map<string, RetainedTransitionTraceMember>();
    for (const member of members) {
      const fingerprint = yield* eventKeyFingerprint(member.value.event_key);
      if (byEventKey.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "validation trace",
            new Error(
              `Transition trace contains duplicate event key ${fingerprint}`,
            ),
          ),
        );
      }
      byEventKey.set(fingerprint, member);
    }
    return byEventKey;
  });

export const validateValidationTraceEventKeySet = ({
  expectedEventKeys,
  transitionEventKeyCbors,
  members,
}: {
  readonly expectedEventKeys: readonly SDK.EventKey[];
  readonly transitionEventKeyCbors: ReadonlySet<string>;
  readonly members: readonly Pick<
    RetainedValidationTraceMember,
    "eventKey" | "keyCbor"
  >[];
}): Effect.Effect<void, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const expected = new Set<string>();
    for (const eventKey of expectedEventKeys) {
      const keyHex = (yield* eventKeyCbor(eventKey)).toString("hex");
      if (expected.has(keyHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Validation trace inputs contain a duplicate canonical event key",
            cause: `event_key_cbor=${keyHex}`,
          }),
        );
      }
      expected.add(keyHex);
    }
    if (members.length !== expected.size) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Validation trace provider returned the wrong descriptor count",
          cause: `expected=${expected.size.toString()},actual=${members.length.toString()}`,
        }),
      );
    }

    const seen = new Set<string>();
    for (const member of members) {
      const keyHex = member.keyCbor.toString("hex");
      if (
        seen.has(keyHex) ||
        !expected.has(keyHex) ||
        !transitionEventKeyCbors.has(keyHex) ||
        !member.keyCbor.equals(yield* eventKeyCbor(member.eventKey))
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Validation trace provider returned a duplicate, foreign, or non-canonical event key",
            cause: `event_key_cbor=${keyHex}`,
          }),
        );
      }
      seen.add(keyHex);
    }
    if (seen.size !== expected.size) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Validation trace provider omitted a required canonical event key",
          cause: `expected=${expected.size.toString()},actual=${seen.size.toString()}`,
        }),
      );
    }
  });

const assertUniqueTransitionSourceEvents = (
  sourceEvents: readonly TransitionTraceSourceEvent[],
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    const seen = new Set<string>();
    for (const [index, event] of sourceEvents.entries()) {
      const fingerprint = yield* eventKeyFingerprint(event.eventKey);
      if (seen.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Duplicate source event key at source index ${index.toString()}: ${fingerprint}`,
            ),
          ),
        );
      }
      seen.add(fingerprint);
    }
  });

const transitionPhaseRank = (phase: SDK.TransitionPhase): number => {
  switch (phase) {
    case "Withdrawal":
      return 0;
    case "ForcedTransaction":
      return 1;
    case "L2Transaction":
      return 2;
    case "Deposit":
      return 3;
  }
};

const assertCanonicalTransitionPhaseOrder = (
  sourceEvents: readonly TransitionTraceSourceEvent[],
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    let lastRank = -1;
    for (const [index, sourceEvent] of sourceEvents.entries()) {
      const rank = transitionPhaseRank(sourceEvent.phase);
      if (rank < lastRank) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Transition source events are not in canonical phase order at source index ${index.toString()}: phase=${sourceEvent.phase}`,
            ),
          ),
        );
      }
      lastRank = rank;
    }
  });

export const applyTraceLedgerOpsToMpf = (
  ledgerMpf: MidgardMpf,
  ops: readonly MpfBatchOp[],
  eventKeyDescription: string,
): Effect.Effect<void, MpfError> =>
  Effect.gen(function* () {
    if (ledgerMpf.usesStrictOverlayMutations()) {
      yield* ledgerMpf
        .applyBatch(ops)
        .pipe(
          Effect.mapError((cause) =>
            MpfError.rootBuild(
              "transition trace",
              new Error(
                `Transition event ${eventKeyDescription} failed strict ledger mutation`,
                { cause },
              ),
            ),
          ),
        );
      return;
    }
    const eventPresenceOverlay = new Map<string, boolean>();
    const isPresent = (key: Buffer): Effect.Effect<boolean, MpfError> => {
      const keyHex = key.toString("hex");
      const overlayPresence = eventPresenceOverlay.get(keyHex);
      if (overlayPresence !== undefined) {
        return Effect.succeed(overlayPresence);
      }
      return ledgerMpf.get(key).pipe(Effect.map(Option.isSome));
    };

    for (const op of ops) {
      const keyHex = op.key.toString("hex");
      const present = yield* isPresent(op.key);
      if (op.type === "delete") {
        if (!present) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "transition trace",
              new Error(
                `Transition event ${eventKeyDescription} deletes missing UTxO ${keyHex}`,
              ),
            ),
          );
        }
        eventPresenceOverlay.set(keyHex, false);
        continue;
      }
      if (present) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Transition event ${eventKeyDescription} inserts duplicate UTxO ${keyHex}`,
            ),
          ),
        );
      }
      eventPresenceOverlay.set(keyHex, true);
    }

    yield* ledgerMpf.applyBatch(ops);
  });

const validateTransitionTraceSourceEvents = ({
  sourceEvents,
  withdrawalCount,
  forcedTransactionCount,
  l2TransactionCount,
  depositCount,
  expectedTotalEventCount,
}: {
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly expectedTotalEventCount?: number;
}): Effect.Effect<
  {
    readonly totalEventCount: number;
    readonly eventKeyCbors: readonly Buffer[];
  },
  MpfError
> =>
  Effect.gen(function* () {
    const sourceCountBounds = [
      [
        "withdrawal",
        withdrawalCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxWithdrawalCount,
      ],
      [
        "forced transaction",
        forcedTransactionCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxForcedTransactionCount,
      ],
      [
        "L2 transaction",
        l2TransactionCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxL2TransactionCount,
      ],
      ["deposit", depositCount, MIDGARD_CONSENSUS_LIMITS_V1.maxDepositCount],
    ] as const;
    for (const [label, count, maximum] of sourceCountBounds) {
      if (!Number.isSafeInteger(count) || count < 0 || count > maximum) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `${label} count must be a safe integer between 0 and ${maximum.toString()}: ${count.toString()}`,
            ),
          ),
        );
      }
    }
    const totalEventCount =
      withdrawalCount +
      forcedTransactionCount +
      l2TransactionCount +
      depositCount;
    if (
      totalEventCount > MIDGARD_CONSENSUS_LIMITS_V1.maxTotalEventCount ||
      totalEventCount > MIDGARD_CONSENSUS_LIMITS_V1.maxTransitionStepCount
    ) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Transition source count ${totalEventCount.toString()} exceeds the launch event/step bound`,
          ),
        ),
      );
    }
    const ledgerOperationCount = sourceEvents.reduce(
      (total, sourceEvent) => total + sourceEvent.ledgerOps.length,
      0,
    );
    if (
      ledgerOperationCount > MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOperationCount
    ) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Ledger operation count ${ledgerOperationCount.toString()} exceeds the V1 consensus maximum ${MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOperationCount.toString()}`,
          ),
        ),
      );
    }
    if (
      expectedTotalEventCount !== undefined &&
      totalEventCount !== expectedTotalEventCount
    ) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Transition source count mismatch: expected=${expectedTotalEventCount.toString()},actual=${totalEventCount.toString()}`,
          ),
        ),
      );
    }
    if (sourceEvents.length !== totalEventCount) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "transition trace",
          new Error(
            `Transition source event array length does not match source counts: source_events=${sourceEvents.length.toString()},source_count_sum=${totalEventCount.toString()}`,
          ),
        ),
      );
    }
    const eventKeyCbors: Buffer[] = [];
    const seenEventKeys = new Set<string>();
    for (const [index, event] of sourceEvents.entries()) {
      const keyCbor = yield* eventKeyCbor(event.eventKey);
      const fingerprint = keyCbor.toString("hex");
      if (seenEventKeys.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "transition trace",
            new Error(
              `Duplicate source event key at source index ${index.toString()}: ${fingerprint}`,
            ),
          ),
        );
      }
      seenEventKeys.add(fingerprint);
      eventKeyCbors.push(keyCbor);
    }
    yield* assertCanonicalTransitionPhaseOrder(sourceEvents);
    return { totalEventCount, eventKeyCbors };
  });

export const buildEventToStepMembersFromTrace = ({
  sourceEvents,
  transitionTraceMembers,
}: {
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
}): Effect.Effect<readonly RetainedEventToStepMember[], MpfError> =>
  Effect.gen(function* () {
    yield* assertUniqueTransitionSourceEvents(sourceEvents);
    if (sourceEvents.length !== transitionTraceMembers.length) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "event-to-step root",
          new Error(
            `Transition source event count does not match trace step count: source_events=${sourceEvents.length.toString()},trace_steps=${transitionTraceMembers.length.toString()}`,
          ),
        ),
      );
    }

    const sourceByKey = new Map<string, TransitionTraceSourceEvent>();
    for (const sourceEvent of sourceEvents) {
      sourceByKey.set(
        yield* eventKeyFingerprint(sourceEvent.eventKey),
        sourceEvent,
      );
    }

    const seenTraceEvents = new Set<string>();
    const members: RetainedEventToStepMember[] = [];
    for (const traceMember of transitionTraceMembers) {
      const step = traceMember.value;
      const fingerprint = yield* eventKeyFingerprint(step.event_key);
      const source = sourceByKey.get(fingerprint);
      if (source === undefined) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "event-to-step root",
            new Error(
              `Transition trace step ${step.step_index.toString()} references an event key with no source-root member: ${fingerprint}`,
            ),
          ),
        );
      }
      if (seenTraceEvents.has(fingerprint)) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "event-to-step root",
            new Error(
              `Transition trace contains duplicate event key ${fingerprint}`,
            ),
          ),
        );
      }
      if (source.phase !== step.phase) {
        return yield* Effect.fail(
          MpfError.rootBuild(
            "event-to-step root",
            new Error(
              `Transition trace step phase does not match source phase: step_index=${step.step_index.toString()},source_phase=${source.phase},step_phase=${step.phase}`,
            ),
          ),
        );
      }
      seenTraceEvents.add(fingerprint);
      const value: SDK.EventToStepValue = {
        step_index: step.step_index,
        phase: step.phase,
      };
      members.push({
        eventKey: step.event_key,
        keyCbor: yield* eventKeyCbor(step.event_key),
        valueCbor: encodeEventToStepValueCbor(value),
        value,
      });
    }

    if (seenTraceEvents.size !== sourceByKey.size) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "event-to-step root",
          new Error(
            `Event-to-step root omits source events: source_events=${sourceByKey.size.toString()},mapped_events=${seenTraceEvents.size.toString()}`,
          ),
        ),
      );
    }
    return members;
  });

export const buildTransitionTraceResult = ({
  ledgerMpf,
  sourceEvents,
  withdrawalCount,
  forcedTransactionCount,
  l2TransactionCount,
  depositCount,
  expectedTotalEventCount,
}: {
  readonly ledgerMpf: MidgardMpf;
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly expectedTotalEventCount?: number;
}): Effect.Effect<TransitionTraceBuildResult, MpfError> =>
  Effect.gen(function* () {
    const { totalEventCount, eventKeyCbors } =
      yield* validateTransitionTraceSourceEvents({
        sourceEvents,
        withdrawalCount,
        forcedTransactionCount,
        l2TransactionCount,
        depositCount,
        expectedTotalEventCount,
      });
    const hydrationConfig = getMpfPathHydrationConfig();
    const indexedEvents = sourceEvents.map((sourceEvent, index) => ({
      index,
      sourceEvent,
    }));
    const eventChunks: (typeof indexedEvents)[] = [];
    if (hydrationConfig.mode === "whole_block") {
      if (indexedEvents.length > 0) eventChunks.push(indexedEvents);
    } else {
      let chunk: typeof indexedEvents = [];
      let chunkOps = 0;
      for (const indexedEvent of indexedEvents) {
        const eventOps = indexedEvent.sourceEvent.ledgerOps.length;
        if (
          chunk.length > 0 &&
          chunkOps + eventOps > hydrationConfig.chunkOps
        ) {
          eventChunks.push(chunk);
          chunk = [];
          chunkOps = 0;
        }
        chunk.push(indexedEvent);
        chunkOps += eventOps;
      }
      if (chunk.length > 0) eventChunks.push(chunk);
    }
    const uniqueTouchedPaths = new Set(
      sourceEvents.flatMap((sourceEvent) =>
        sourceEvent.ledgerOps.map((op) => op.key.toString("hex")),
      ),
    );
    const pathHydration: {
      -readonly [K in keyof MpfPathHydrationDiagnostics]: MpfPathHydrationDiagnostics[K];
    } = {
      prefetchMs: 0,
      uniquePaths: uniqueTouchedPaths.size,
      nodesRequested: 0,
      hydrationHits: 0,
      hydrationMisses: 0,
      loadedNodes: 0,
      maxInFlight: 0,
      maxBatchKeys: 0,
      maxFrontierPaths: 0,
      retainedBytesEstimate: 0,
      chunkCount: 0,
      checkpointMs: 0,
      authenticationMs: 0,
      materializeMs: 0,
      collapseMs: 0,
      checkpointSerializedNodes: 0,
      checkpointSerializedBytes: 0,
      verifiedUpperNodes: 0,
      retainedUpperNodes: 0,
      collapsedNodes: 0,
      peakDecodedNodes: 0,
    };
    const transitionTraceMembers: RetainedTransitionTraceMember[] = [];
    let runningUtxosRoot = yield* ledgerMpf.rootHex();
    let retainedUpperNodes = 0;
    if (hydrationConfig.mode === "chunked_arena") {
      const primed = yield* ledgerMpf.primeBlockPathArena(
        sourceEvents.flatMap((sourceEvent) => sourceEvent.ledgerOps),
        hydrationConfig.retainDepth,
        false,
      );
      pathHydration.prefetchMs += primed.hydration.prefetchMs;
      pathHydration.nodesRequested += primed.hydration.nodesRequested;
      pathHydration.hydrationHits += primed.hydration.hydrationHits;
      pathHydration.hydrationMisses += primed.hydration.hydrationMisses;
      pathHydration.loadedNodes += primed.hydration.loadedNodes;
      pathHydration.maxInFlight = primed.hydration.maxInFlight;
      pathHydration.maxBatchKeys = primed.hydration.maxBatchKeys;
      pathHydration.maxFrontierPaths = primed.hydration.maxFrontierPaths;
      pathHydration.retainedBytesEstimate =
        primed.hydration.retainedBytesEstimate;
      pathHydration.authenticationMs +=
        primed.authenticationMs + primed.checkpoint.authenticationMs;
      pathHydration.checkpointMs += primed.checkpoint.checkpointMs;
      pathHydration.collapseMs += primed.checkpoint.collapseMs;
      pathHydration.verifiedUpperNodes +=
        primed.verifiedNodes + primed.checkpoint.verifiedUpperNodes;
      pathHydration.collapsedNodes += primed.checkpoint.collapsedNodes;
      retainedUpperNodes = primed.checkpoint.retainedUpperNodes;
      pathHydration.retainedUpperNodes = retainedUpperNodes;
      pathHydration.peakDecodedNodes = Math.max(
        pathHydration.peakDecodedNodes,
        primed.hydration.loadedNodes,
      );
    }
    for (const eventChunk of eventChunks) {
      if (hydrationConfig.mode !== "chunked_arena") {
        const hydration = yield* ledgerMpf.prefetchTouchedPaths(
          eventChunk.flatMap(({ sourceEvent }) => sourceEvent.ledgerOps),
        );
        pathHydration.prefetchMs += hydration.prefetchMs;
        pathHydration.nodesRequested += hydration.nodesRequested;
        pathHydration.hydrationHits += hydration.hydrationHits;
        pathHydration.hydrationMisses += hydration.hydrationMisses;
        pathHydration.loadedNodes += hydration.loadedNodes;
        pathHydration.maxInFlight = Math.max(
          pathHydration.maxInFlight,
          hydration.maxInFlight,
        );
        pathHydration.maxBatchKeys = Math.max(
          pathHydration.maxBatchKeys,
          hydration.maxBatchKeys,
        );
        pathHydration.maxFrontierPaths = Math.max(
          pathHydration.maxFrontierPaths,
          hydration.maxFrontierPaths,
        );
        pathHydration.retainedBytesEstimate = Math.max(
          pathHydration.retainedBytesEstimate,
          hydration.retainedBytesEstimate,
        );
        pathHydration.peakDecodedNodes = Math.max(
          pathHydration.peakDecodedNodes,
          retainedUpperNodes + hydration.loadedNodes,
        );
      }
      pathHydration.chunkCount += 1;
      if (
        hydrationConfig.mode !== "whole_block" &&
        !ledgerMpf.usesEventFlatEngine()
      ) {
        const authentication = yield* ledgerMpf.authenticateDecodedArena(
          hydrationConfig.mode === "chunked_arena"
            ? 0
            : hydrationConfig.retainDepth,
        );
        pathHydration.verifiedUpperNodes += authentication.verifiedNodes;
        pathHydration.authenticationMs += authentication.authenticationMs;
      }
      for (const { index, sourceEvent } of eventChunk) {
        const preUtxosRoot = runningUtxosRoot;
        const eventKeyDescription = eventKeyCbors[index]!.toString("hex");
        if (ledgerMpf.usesStrictOverlayMutations()) {
          const postUtxosRoot = yield* ledgerMpf
            .applyBatch(sourceEvent.ledgerOps)
            .pipe(
              Effect.map((root) => root.toString("hex")),
              Effect.mapError((cause) =>
                MpfError.rootBuild(
                  "transition trace",
                  new Error(
                    `Transition event ${eventKeyDescription} failed strict ledger mutation`,
                    { cause },
                  ),
                ),
              ),
            );
          runningUtxosRoot = postUtxosRoot;
        } else {
          yield* applyTraceLedgerOpsToMpf(
            ledgerMpf,
            sourceEvent.ledgerOps,
            eventKeyDescription,
          );
          const postUtxosRoot = yield* ledgerMpf.rootHex();
          runningUtxosRoot = postUtxosRoot;
        }
        const value: SDK.TransitionStep = {
          schema_version: BigInt(MIDGARD_TRANSITION_STEP_V1_SCHEMA_VERSION),
          step_index: BigInt(index),
          event_key: sourceEvent.eventKey,
          phase: sourceEvent.phase,
          pre_utxos_root: preUtxosRoot,
          post_utxos_root: runningUtxosRoot,
        };
        const member: RetainedTransitionTraceMember = {
          stepIndex: value.step_index,
          keyCbor: encodeTransitionIntegerCbor(value.step_index),
          valueCbor: encodeTransitionStepCbor(value),
          value,
        };
        transitionTraceMembers.push(member);
      }
      if (
        hydrationConfig.mode !== "whole_block" &&
        !ledgerMpf.usesEventFlatEngine()
      ) {
        const checkpoint = yield* ledgerMpf.checkpointAndCollapseDecodedArena(
          hydrationConfig.retainDepth,
          hydrationConfig.mode !== "chunked_arena",
          hydrationConfig.mode !== "chunked_arena",
        );
        pathHydration.checkpointMs += checkpoint.checkpointMs;
        pathHydration.authenticationMs += checkpoint.authenticationMs;
        pathHydration.materializeMs += checkpoint.materializeMs;
        pathHydration.collapseMs += checkpoint.collapseMs;
        pathHydration.checkpointSerializedNodes += checkpoint.serializedNodes;
        pathHydration.checkpointSerializedBytes += checkpoint.serializedBytes;
        pathHydration.verifiedUpperNodes += checkpoint.verifiedUpperNodes;
        pathHydration.collapsedNodes += checkpoint.collapsedNodes;
        retainedUpperNodes = checkpoint.retainedUpperNodes;
        pathHydration.retainedUpperNodes = Math.max(
          pathHydration.retainedUpperNodes,
          retainedUpperNodes,
        );
      }
    }

    const eventToStepMembers: RetainedEventToStepMember[] = [];
    for (const [index, traceMember] of transitionTraceMembers.entries()) {
      const value: SDK.EventToStepValue = {
        step_index: traceMember.value.step_index,
        phase: traceMember.value.phase,
      };
      eventToStepMembers.push({
        eventKey: traceMember.value.event_key,
        keyCbor: eventKeyCbors[index]!,
        valueCbor: encodeEventToStepValueCbor(value),
        value,
      });
    }
    const [transitionTraceRoot, eventToStepRoot] = yield* Effect.all(
      [
        countedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.transitionTrace,
          transitionTraceMembers.map((member) => ({
            key: member.keyCbor,
            value: member.valueCbor,
          })),
        ),
        countedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.eventToStep,
          eventToStepMembers.map((member) => ({
            key: member.keyCbor,
            value: member.valueCbor,
          })),
        ),
      ],
      { concurrency: "unbounded" },
    );

    return {
      finalUtxosRoot: runningUtxosRoot,
      transitionTraceRoot,
      eventToStepRoot,
      transitionTraceMembers,
      eventToStepMembers,
      withdrawalCount,
      forcedTransactionCount,
      l2TransactionCount,
      depositCount,
      totalEventCount,
      transitionStepCount: transitionTraceMembers.length,
      pathHydration,
    };
  });

export const buildNativeTransitionTraceResult = ({
  nativeMpf,
  sourceEvents,
  withdrawalCount,
  forcedTransactionCount,
  l2TransactionCount,
  depositCount,
  expectedTotalEventCount,
}: {
  readonly nativeMpf: NativeMpfBuildContext;
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly withdrawalCount: number;
  readonly forcedTransactionCount: number;
  readonly l2TransactionCount: number;
  readonly depositCount: number;
  readonly expectedTotalEventCount?: number;
}): Effect.Effect<TransitionTraceBuildResult, MpfError> =>
  Effect.gen(function* () {
    const validationStartedAt = performance.now();
    const { totalEventCount, eventKeyCbors } =
      yield* validateTransitionTraceSourceEvents({
        sourceEvents,
        withdrawalCount,
        forcedTransactionCount,
        l2TransactionCount,
        depositCount,
        expectedTotalEventCount,
      });
    const validationMs = performance.now() - validationStartedAt;
    const eventLogEncodeStartedAt = performance.now();
    const eventLog = yield* Effect.try({
      try: () =>
        encodeNativeMpfEventLog(
          nativeMpf.handle.baseRoot,
          sourceEvents.map((sourceEvent) => sourceEvent.ledgerOps),
        ),
      catch: (cause) => MpfError.rootBuild("Architecture G event log", cause),
    });
    const eventLogEncodeMs = performance.now() - eventLogEncodeStartedAt;
    const ownerApplyStartedAt = performance.now();
    const applied = yield* Effect.tryPromise({
      try: () => nativeMpf.client.applyEvents(nativeMpf.handle, eventLog),
      catch: (cause) =>
        MpfError.rootBuild("Architecture G native owner", cause),
    });
    const ownerApplyMs = performance.now() - ownerApplyStartedAt;
    if (applied.eventRoots.length !== sourceEvents.length) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "Architecture G native owner",
          new Error(
            `Native owner returned the wrong event-root count: expected=${sourceEvents.length.toString()},actual=${applied.eventRoots.length.toString()}`,
          ),
        ),
      );
    }
    nativeMpf.eventLog = eventLog;
    nativeMpf.eventLogDigest = applied.eventLogDigest;
    nativeMpf.eventRoots = applied.eventRoots;
    nativeMpf.candidateRoot = applied.candidateRoot;

    let runningUtxosRoot = nativeMpf.handle.baseRoot;
    const transitionTraceMembers: RetainedTransitionTraceMember[] = [];
    const eventToStepMembers: RetainedEventToStepMember[] = [];
    const memberAssemblyStartedAt = performance.now();
    for (const [index, sourceEvent] of sourceEvents.entries()) {
      const preUtxosRoot = runningUtxosRoot;
      runningUtxosRoot = applied.eventRoots[index]!;
      const value: SDK.TransitionStep = {
        schema_version: BigInt(MIDGARD_TRANSITION_STEP_V1_SCHEMA_VERSION),
        step_index: BigInt(index),
        event_key: sourceEvent.eventKey,
        phase: sourceEvent.phase,
        pre_utxos_root: preUtxosRoot,
        post_utxos_root: runningUtxosRoot,
      };
      transitionTraceMembers.push({
        stepIndex: value.step_index,
        keyCbor: encodeTransitionIntegerCbor(value.step_index),
        valueCbor: encodeTransitionStepCbor(value),
        value,
      });
      const eventToStepValue: SDK.EventToStepValue = {
        step_index: value.step_index,
        phase: value.phase,
      };
      eventToStepMembers.push({
        eventKey: value.event_key,
        keyCbor: eventKeyCbors[index]!,
        valueCbor: encodeEventToStepValueCbor(eventToStepValue),
        value: eventToStepValue,
      });
    }
    const memberAssemblyMs = performance.now() - memberAssemblyStartedAt;
    const retainedRootsStartedAt = performance.now();
    const [transitionTraceRoot, eventToStepRoot] = yield* Effect.all(
      [
        countedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.transitionTrace,
          transitionTraceMembers.map((member) => ({
            key: member.keyCbor,
            value: member.valueCbor,
          })),
        ),
        countedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.eventToStep,
          eventToStepMembers.map((member) => ({
            key: member.keyCbor,
            value: member.valueCbor,
          })),
        ),
      ],
      { concurrency: "unbounded" },
    );
    const retainedRootsMs = performance.now() - retainedRootsStartedAt;
    return {
      finalUtxosRoot: runningUtxosRoot,
      transitionTraceRoot,
      eventToStepRoot,
      transitionTraceMembers,
      eventToStepMembers,
      withdrawalCount,
      forcedTransactionCount,
      l2TransactionCount,
      depositCount,
      totalEventCount,
      transitionStepCount: transitionTraceMembers.length,
      nativePhaseMs: {
        validation: validationMs,
        eventLogEncode: eventLogEncodeMs,
        ownerApply: ownerApplyMs,
        ownerProofArena: applied.proofArenaDurationNs / 1_000_000,
        ownerMutation: applied.mutationDurationNs / 1_000_000,
        memberAssembly: memberAssemblyMs,
        retainedRoots: retainedRootsMs,
      },
      pathHydration: {
        prefetchMs: 0,
        uniquePaths: new Set(
          sourceEvents.flatMap((sourceEvent) =>
            sourceEvent.ledgerOps.map((op) => op.key.toString("hex")),
          ),
        ).size,
        nodesRequested: 0,
        hydrationHits: 0,
        hydrationMisses: 0,
        loadedNodes: 0,
        maxInFlight: 0,
        maxBatchKeys: 0,
        maxFrontierPaths: 0,
        retainedBytesEstimate: 0,
        chunkCount: sourceEvents.length === 0 ? 0 : 1,
        checkpointMs: 0,
        authenticationMs: 0,
        materializeMs: 0,
        collapseMs: 0,
        checkpointSerializedNodes: 0,
        checkpointSerializedBytes: 0,
        verifiedUpperNodes: 0,
        retainedUpperNodes: 0,
        collapsedNodes: 0,
        peakDecodedNodes: 0,
      },
    };
  });

export type NativeProductionRootProbeResult = {
  readonly utxoRoot: string;
  readonly rawTxRoot: string;
  readonly txRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly depositsRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transitionRoots: readonly {
    readonly pre: string;
    readonly post: string;
  }[];
  readonly durationMs: number;
  readonly phaseMs: {
    readonly transactionSourceRoot: number;
    readonly transitionTraceBuild: number;
    readonly transactionMpfApply: number;
    readonly auxiliaryRoots: number;
  };
  readonly transitionTraceBuild: TransitionTraceBuildResult;
};

/**
 * Runs the complete root-building portion of the Architecture G production
 * commit path without the database-specific transaction classification phase.
 * Inputs are the exact encoded entries that processMpfs applies after decoding.
 * This is intentionally colocated with processMpfs so benchmarks cannot replace
 * production root algorithms with harness-specific approximations.
 */
export const buildNativeProductionRootProbe = ({
  nativeMpf,
  sourceEvents,
  transactionOps,
  deposits = [],
  withdrawals = [],
  forcedTransactions = [],
}: {
  readonly nativeMpf: NativeMpfBuildContext;
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly transactionOps: readonly MpfInsertBatchOp[];
  readonly deposits?: readonly MpfInsertBatchOp[];
  readonly withdrawals?: readonly MpfInsertBatchOp[];
  readonly forcedTransactions?: readonly MpfInsertBatchOp[];
}): Effect.Effect<NativeProductionRootProbeResult, MpfError> =>
  Effect.acquireUseRelease(
    MidgardMpf.createScratch("architecture-g-production-probe-transactions"),
    (transactionsMpf) =>
      Effect.gen(function* () {
        const startedAt = performance.now();
        const timedTransactionSourceRoot = yield* Effect.gen(function* () {
          const phaseStartedAt = performance.now();
          const root = yield* buildTransactionsSourceRoot(transactionOps);
          return { root, durationMs: performance.now() - phaseStartedAt };
        }).pipe(Effect.fork);
        const timedTransactionMpfApply = yield* Effect.gen(function* () {
          const phaseStartedAt = performance.now();
          yield* transactionsMpf.applyBatch(transactionOps);
          const rawTxRoot = yield* transactionsMpf.rootHex();
          return {
            rawTxRoot,
            durationMs: performance.now() - phaseStartedAt,
          };
        }).pipe(Effect.fork);
        const transitionTraceStartedAt = performance.now();
        const transitionTraceBuild = yield* buildNativeTransitionTraceResult({
          nativeMpf,
          sourceEvents,
          withdrawalCount: withdrawals.length,
          forcedTransactionCount: forcedTransactions.length,
          l2TransactionCount: transactionOps.length,
          depositCount: deposits.length,
        });
        const transitionTraceBuildMs =
          performance.now() - transitionTraceStartedAt;
        const [timedTxRoot, timedRawTxRoot] = yield* Effect.all(
          [
            Fiber.join(timedTransactionSourceRoot),
            Fiber.join(timedTransactionMpfApply),
          ],
          { concurrency: "unbounded" },
        );

        const auxiliaryRootsStartedAt = performance.now();
        const productionEventRoot = (
          domain: SDK.RootDomain,
          entries: readonly MpfInsertBatchOp[],
        ): Effect.Effect<string, MpfError> =>
          entries.length === 0
            ? Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT)
            : countedRootFromEncodedEntries(domain, entries);
        const [depositsRoot, withdrawalsRoot, forcedTransactionsRoot] =
          yield* Effect.all(
            [
              productionEventRoot(SDK.ROOT_DOMAINS.deposits, deposits),
              productionEventRoot(SDK.ROOT_DOMAINS.withdrawals, withdrawals),
              productionEventRoot(
                SDK.ROOT_DOMAINS.forcedTransactionsV1,
                forcedTransactions,
              ),
            ],
            { concurrency: "unbounded" },
          );
        const auxiliaryRootsMs = performance.now() - auxiliaryRootsStartedAt;
        const utxoRoot = nativeMpf.candidateRoot;
        if (utxoRoot === undefined) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "Architecture G production probe",
              new Error("Native owner did not return a candidate UTxO root"),
            ),
          );
        }
        if (transitionTraceBuild.finalUtxosRoot !== utxoRoot) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "Architecture G production probe",
              new Error(
                `Transition trace final root mismatch: trace=${transitionTraceBuild.finalUtxosRoot},candidate=${utxoRoot}`,
              ),
            ),
          );
        }
        return {
          utxoRoot,
          rawTxRoot: timedRawTxRoot.rawTxRoot,
          txRoot: timedTxRoot.root,
          transitionTraceRoot: transitionTraceBuild.transitionTraceRoot,
          eventToStepRoot: transitionTraceBuild.eventToStepRoot,
          depositsRoot,
          withdrawalsRoot,
          forcedTransactionsRoot,
          transitionRoots: transitionTraceBuild.transitionTraceMembers.map(
            (member) => ({
              pre: member.value.pre_utxos_root,
              post: member.value.post_utxos_root,
            }),
          ),
          durationMs: performance.now() - startedAt,
          phaseMs: {
            transactionSourceRoot: timedTxRoot.durationMs,
            transitionTraceBuild: transitionTraceBuildMs,
            transactionMpfApply: timedRawTxRoot.durationMs,
            auxiliaryRoots: auxiliaryRootsMs,
          },
          transitionTraceBuild,
        };
      }),
    (transactionsMpf) => transactionsMpf.close().pipe(Effect.orDie),
  );

export const resolveIncludedDepositEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
  persistProjection = true,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
  readonly persistProjection?: boolean;
}): Effect.Effect<readonly DepositsDB.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* DepositsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime);
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: DepositsDB.tableName,
              message:
                "Refusing to build a block because one or more deposits due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) => entry[DepositsDB.Columns.ID].toString("hex"))
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] !== DepositsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected deposit UTxO(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime(),
        );
        if (currentWindowEntries.length <= 0) {
          return replayableOverdueEntries;
        }

        const awaitingEntries = currentWindowEntries.filter(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting,
        );
        if (persistProjection && awaitingEntries.length > 0) {
          const mempoolEntries = yield* Effect.forEach(
            awaitingEntries,
            DepositsDB.toMempoolLedgerEntry,
          );
          yield* MempoolLedgerDB.reconcileDepositEntries(mempoolEntries);
          yield* DepositsDB.markAwaitingAsProjected(
            awaitingEntries.map((entry) => entry[DepositsDB.Columns.ID]),
          );
        }

        const normalizedCurrentWindowEntries = currentWindowEntries.map(
          (entry) =>
            entry[DepositsDB.Columns.STATUS] === DepositsDB.Status.Awaiting
              ? {
                  ...entry,
                  [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
                }
              : entry,
        );
        return [...replayableOverdueEntries, ...normalizedCurrentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      DepositsDB.tableName,
      "Failed to resolve deposits for the current block window",
    ),
  );

export const resolveIncludedWithdrawalEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
}): Effect.Effect<readonly WithdrawalsDB.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* WithdrawalsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          );
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.STATUS] ===
            WithdrawalsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message:
                "Refusing to build a block because one or more withdrawals due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) => entry[WithdrawalsDB.Columns.ID].toString("hex"))
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.STATUS] !==
            WithdrawalsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected withdrawal UTxO(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[WithdrawalsDB.Columns.INCLUSION_TIME].getTime(),
        );

        return [...replayableOverdueEntries, ...currentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      WithdrawalsDB.tableName,
      "Failed to resolve withdrawals for the current block window",
    ),
  );

export const resolveIncludedForcedTransactionEntriesForWindow = ({
  currentBlockStartTime,
  effectiveEndTime,
  persistProjection = true,
}: {
  readonly currentBlockStartTime: Date;
  readonly effectiveEndTime: Date;
  readonly persistProjection?: boolean;
}): Effect.Effect<
  readonly ForcedTransactionsDB.Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const pendingEntries =
          yield* ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          );
        if (pendingEntries.length <= 0) {
          return [];
        }

        const overdueEntries = pendingEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime() <=
            currentBlockStartTime.getTime(),
        );
        const skippedAwaitingEntries = overdueEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] ===
            ForcedTransactionsDB.Status.Awaiting,
        );
        if (skippedAwaitingEntries.length > 0) {
          return yield* Effect.fail(
            new DatabaseError({
              table: ForcedTransactionsDB.tableName,
              message:
                "Refusing to build a block because one or more tx-order events due for an earlier block were never assigned to a header",
              cause: skippedAwaitingEntries
                .map((entry) =>
                  entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString(
                    "hex",
                  ),
                )
                .join(","),
            }),
          );
        }

        const replayableOverdueEntries = overdueEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] !==
            ForcedTransactionsDB.Status.Awaiting,
        );
        if (replayableOverdueEntries.length > 0) {
          yield* Effect.logWarning(
            `Re-including ${replayableOverdueEntries.length} previously projected tx-order event(s) whose prior header assignment was abandoned before confirmation.`,
          );
        }

        const currentWindowEntries = pendingEntries.filter(
          (entry) =>
            currentBlockStartTime.getTime() <
            entry[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime(),
        );
        if (currentWindowEntries.length <= 0) {
          return replayableOverdueEntries;
        }

        const awaitingEntries = currentWindowEntries.filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] ===
            ForcedTransactionsDB.Status.Awaiting,
        );
        if (persistProjection && awaitingEntries.length > 0) {
          yield* ForcedTransactionsDB.markAwaitingAsProjected(
            awaitingEntries.map(
              (entry) => entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
            ),
          );
        }

        const normalizedCurrentWindowEntries = currentWindowEntries.map(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.STATUS] ===
            ForcedTransactionsDB.Status.Awaiting
              ? {
                  ...entry,
                  [ForcedTransactionsDB.Columns.STATUS]:
                    ForcedTransactionsDB.Status.Projected,
                }
              : entry,
        );
        return [...replayableOverdueEntries, ...normalizedCurrentWindowEntries];
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      ForcedTransactionsDB.tableName,
      "Failed to resolve forced transactions for the current block window",
    ),
  );

const logCommitMpfPhaseTiming = (
  phase: string,
  startedAtMs: number,
  counts: Record<string, number>,
): Effect.Effect<void, never> => {
  const countSummary = Object.entries(counts)
    .map(([key, value]) => `${key}=${value.toString()}`)
    .join(",");
  const suffix = countSummary.length > 0 ? `,${countSummary}` : "";
  return Effect.logInfo(
    `🔹 Commit MPF phase ${phase} completed duration_ms=${Math.max(
      0,
      Date.now() - startedAtMs,
    ).toString()}${suffix}`,
  );
};

/**
 * The operator's recorded verdict for a Phase A/B rejection, as the #640
 * forced leaf carries it. The node's classifier resolves faults only to the
 * 19 descriptor-level `RejectCode`s, so each bucket names the corresponding
 * `RejectionReasonV1` arm at subject ordinal 0 — the coordinates are refined
 * where the classifier learns to report them.
 *
 * `E_NATIVE_SCRIPT_INVALID` is the one code whose arm depends on the phase:
 * Phase A emits it only from the witness-set native scan
 * (`WitnessNativeScriptFalse`), Phase B only from execution natives
 * (`ExecutionNativeScriptFalse`) — both arms bridge back to exactly this
 * code, so the split loses nothing and never claims a CEK failure
 * (`PlutusExecutionFailed`) for a native-script refusal.
 */
const forcedVerdictForRejection = (
  code: RejectCode,
  phase: "phaseA" | "phaseB",
): SDK.OperatorVerdictV1 => {
  // Rejection codes are grouped into the protocol reason families below.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (code) {
    case RejectCodes.InputNotFound:
      return {
        ForcedTxInvalid: {
          reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
        },
      };
    case RejectCodes.InvalidSignature:
    case RejectCodes.MissingRequiredWitness:
      return {
        ForcedTxInvalid: {
          reason: { AddressWitnessSignatureInvalid: { witness_index: 0n } },
        },
      };
    case RejectCodes.NativeScriptInvalid:
      return phase === "phaseA"
        ? {
            ForcedTxInvalid: {
              reason: { WitnessNativeScriptFalse: { script_index: 0n } },
            },
          }
        : {
            ForcedTxInvalid: {
              reason: { ExecutionNativeScriptFalse: { execution_index: 0n } },
            },
          };
    case RejectCodes.PlutusScriptInvalid:
    case RejectCodes.PlutusEvaluationUnavailable:
      return {
        ForcedTxInvalid: {
          reason: { PlutusExecutionFailed: { execution_index: 0n } },
        },
      };
    case RejectCodes.MinFee:
      return { ForcedTxInvalid: { reason: "FeeBelowMinimum" } };
    default:
      return { ForcedTxInvalid: { reason: "ValueNotPreserved" } };
  }
};

export type ClassifiedForcedTransactionV1 = {
  readonly entry: ForcedTransactionsDB.Entry;
  /** Byte-exact source effect shared with independent replay consumers. */
  readonly transitionEffect: CanonicalTransitionEffectV1;
  /** Consensus MPF operations; insert values are canonical descriptors. */
  readonly ledgerOps: readonly MpfBatchOp[];
  /** Full-output operations used only for Phase B state and DA material. */
  readonly rawLedgerOps: readonly MpfBatchOp[];
  readonly ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  readonly ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[];
  readonly rejectionCode: RejectCode | null;
  readonly programMaterialSidecarCbor: Buffer;
};

export type ForcedProgramMaterialSidecarResolverV1<R> = (
  envelopes: readonly MidgardCekProgramEnvelopeV1[],
) => Effect.Effect<Buffer, DatabaseError, R>;

const applyValidationLedgerMutations = async (
  trie: Trie,
  operations: readonly MpfBatchOp[],
): Promise<readonly ValidationMachineLedgerMutationStep[]> => {
  const steps: ValidationMachineLedgerMutationStep[] = [];
  for (const operation of operations) {
    steps.push(
      await applyValidationMachineLedgerMutationStepV1(trie, operation),
    );
  }
  return steps;
};

const validationLedgerWitnesses = (
  state: ReadonlyMap<string, Buffer>,
  outRefHexes: readonly string[],
): readonly ValidationMachineLedgerEntry[] =>
  [...new Set(outRefHexes)].sort().flatMap((outRefHex) => {
    const output = state.get(outRefHex);
    return output === undefined
      ? []
      : [
          {
            outRef: Buffer.from(outRefHex, "hex"),
            output: Buffer.from(output),
          },
        ];
  });

const programMaterialSidecarForEnvelopes = (
  envelopes: readonly MidgardCekProgramEnvelopeV1[],
): Effect.Effect<Buffer, DatabaseError, Database> =>
  CekProgramMaterialDB.retrieveVerifiedBundles(envelopes).pipe(
    Effect.map((entries) => encodeMidgardCekProgramMaterialSidecarV1(entries)),
  );

export const classifyForcedTransactionsV1 = <R>({
  entries,
  initialState,
  effectiveEndTime,
  consensusProfile,
  validation,
  resolveProgramMaterialSidecar,
}: {
  readonly entries: readonly ForcedTransactionsDB.Entry[];
  readonly initialState: Map<string, Buffer>;
  readonly effectiveEndTime: Date;
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly validation: NonNullable<ProcessMpfsConfig["forcedValidation"]>;
  readonly resolveProgramMaterialSidecar: ForcedProgramMaterialSidecarResolverV1<R>;
}): Effect.Effect<readonly ClassifiedForcedTransactionV1[], DatabaseError, R> =>
  Effect.gen(function* () {
    const state = new Map(
      [...initialState.entries()].map(([key, value]) => [
        key,
        Buffer.from(value),
      ]),
    );
    const mutationTrie = yield* Effect.tryPromise({
      try: () =>
        Trie.fromList(
          [...state.entries()].map(([key, value]) =>
            ledgerOutputToInsertBatchOpV1({
              outRef: Buffer.from(key, "hex"),
              outputCbor: value,
            }),
          ),
          new Store(undefined),
        ),
      catch: (cause) =>
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "Failed to construct the forced-transaction validation mutation trie",
          cause,
        }),
    });
    const classified: ClassifiedForcedTransactionV1[] = [];
    let arrivalSeq = 0n;
    for (const entry of entries) {
      const nativeTxCbor = entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR];
      const transactionCommitment =
        entry[ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT];
      const profileId =
        entry[ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID];
      if (
        profileId !== consensusProfile.profileId ||
        nativeTxCbor == null ||
        transactionCommitment == null
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message:
              "V1 block contains a forced transaction without exact V1 source material",
            cause: `tx_order_id=${entry[
              ForcedTransactionsDB.Columns.TX_ORDER_ID
            ].toString("hex")},profile=${profileId ?? "missing"}`,
          }),
        );
      }
      const txId = entry[ForcedTransactionsDB.Columns.TX_ID];
      const canonicalTx = yield* Effect.try({
        try: () => decodeMidgardNativeTxFullV1FromCanonicalCbor(nativeTxCbor),
        catch: (cause) =>
          new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message:
              "Forced transaction canonical bytes cannot be decoded while resolving CEK program material",
            cause,
          }),
      });
      // Malformed attached script/output bytes remain a deterministic Phase A
      // rejection. No material can be authenticated for a non-envelope.
      const attachedEnvelopes = (() => {
        try {
          return collectMidgardV1AttachedProgramEnvelopes(canonicalTx);
        } catch {
          return Object.freeze([]) as readonly MidgardCekProgramEnvelopeV1[];
        }
      })();
      let programMaterialSidecarCbor =
        yield* resolveProgramMaterialSidecar(attachedEnvelopes);
      const phaseA = yield* runPhaseAValidation(
        [
          {
            txId,
            txCbor: nativeTxCbor,
            arrivalSeq,
            createdAt: entry[ForcedTransactionsDB.Columns.INCLUSION_TIME],
            programMaterialSidecarCbor,
          },
        ],
        {
          expectedNetworkId: validation.expectedNetworkId,
          minFeeA: validation.minFeeA,
          minFeeB: validation.minFeeB,
          concurrency: 1,
          strictnessProfile: "phase1_midgard",
          consensusProfile,
        },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new DatabaseError({
              table: ForcedTransactionsDB.tableName,
              message: "Forced transaction Phase A evaluation failed",
              cause,
            }),
        ),
      );
      arrivalSeq += 1n;

      let verdict: SDK.OperatorVerdictV1;
      let ledgerOps: readonly MpfBatchOp[] = [];
      let rawLedgerOps: readonly MpfBatchOp[] = [];
      let transitionEffect = buildCanonicalTransitionEffectV1([]);
      let ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[] = [];
      let ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[] =
        [];
      let rejectionCode: RejectCode | null = null;
      if (phaseA.rejected.length > 0) {
        rejectionCode = phaseA.rejected[0]!.code;
        verdict = forcedVerdictForRejection(rejectionCode, "phaseA");
      } else {
        let acceptedCandidate = phaseA.accepted[0]!;
        if (
          acceptedCandidate.graph.referenceOutRefHexes.every((outRef) =>
            state.has(outRef),
          )
        ) {
          // A malformed referenced ledger output is classified by Phase B.
          const referencedEnvelopes = (() => {
            try {
              return collectMidgardV1ReferencedProgramEnvelopes(
                canonicalTx,
                state,
              );
            } catch {
              return Object.freeze(
                [],
              ) as readonly MidgardCekProgramEnvelopeV1[];
            }
          })();
          if (referencedEnvelopes.length > 0) {
            programMaterialSidecarCbor = yield* resolveProgramMaterialSidecar([
              ...attachedEnvelopes,
              ...referencedEnvelopes,
            ]);
            acceptedCandidate = {
              ...acceptedCandidate,
              submission: {
                ...acceptedCandidate.submission,
                programMaterialSidecarCbor,
              },
            };
          }
        }
        ledgerWitnessEntries = validationLedgerWitnesses(state, [
          ...acceptedCandidate.graph.spentOutRefHexes,
          ...acceptedCandidate.graph.referenceOutRefHexes,
        ]);
        const phaseB = yield* runPhaseBValidationWithPatch(
          [acceptedCandidate],
          state,
          {
            nowCardanoSlotNo: validation.slotForUnixTime(
              effectiveEndTime.getTime(),
            ),
            bucketConcurrency: validation.bucketConcurrency,
            enforceScriptBudget: true,
          },
        ).pipe(
          Effect.mapError(
            (cause) =>
              new DatabaseError({
                table: ForcedTransactionsDB.tableName,
                message: "Forced transaction Phase B evaluation failed",
                cause,
              }),
          ),
        );
        if (phaseB.rejected.length > 0) {
          rejectionCode = phaseB.rejected[0]!.code;
          verdict = forcedVerdictForRejection(rejectionCode, "phaseB");
        } else {
          verdict = "ForcedTxValid";
          transitionEffect = canonicalTransitionEffectFromStatePatchV1(
            phaseB.statePatch,
          );
          rawLedgerOps = transitionEffectToRawLedgerOpsV1(transitionEffect);
          ledgerOps = transitionEffectToLedgerOpsV1(transitionEffect);
          ledgerMutationSteps = yield* Effect.tryPromise({
            try: () => applyValidationLedgerMutations(mutationTrie, ledgerOps),
            catch: (cause) =>
              new DatabaseError({
                table: ForcedTransactionsDB.tableName,
                message:
                  "Failed to derive forced-transaction ledger mutation roots",
                cause,
              }),
          });
          applyUTxOStatePatch(state, phaseB.statePatch);
        }
      }
      const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor,
        verdict,
        consensusProfile,
      });
      // The row's tx_compact / transaction_commitment columns are the
      // SUBMITTED identity written at ingest (admission requires TxIsValid),
      // while `encoded` carries the operator-adjudicated leaf whose validity
      // scalar is stamped from the verdict. Identity is therefore checked
      // against a fresh derivation from the canonical bytes; `tx_id` hashes
      // the body only and is invariant under adjudication.
      const submittedSource = yield* Effect.try({
        try: () => deriveMidgardNativeTxProofSourceV1(canonicalTx),
        catch: (cause) =>
          new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message:
              "Forced transaction canonical bytes cannot derive a submitted proof source",
            cause,
          }),
      });
      if (
        !encoded.txId.equals(txId) ||
        !computeMidgardNativeTxProofCommitmentV1(submittedSource).equals(
          transactionCommitment,
        ) ||
        !submittedSource.compactCbor.equals(
          entry[ForcedTransactionsDB.Columns.TX_COMPACT],
        )
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: ForcedTransactionsDB.tableName,
            message:
              "Forced transaction persisted identity does not match its exact canonical V1 bytes",
            cause: `tx_order_id=${entry[
              ForcedTransactionsDB.Columns.TX_ORDER_ID
            ].toString("hex")}`,
          }),
        );
      }
      classified.push({
        entry: {
          ...entry,
          [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]:
            ForcedTransactionsDB.midgardTxValidityOfVerdictV1(verdict),
          [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
          [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
            programMaterialSidecarCbor,
          [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
            sha256(programMaterialSidecarCbor),
        },
        transitionEffect,
        ledgerOps,
        rawLedgerOps,
        ledgerWitnessEntries,
        ledgerMutationSteps,
        rejectionCode,
        programMaterialSidecarCbor,
      });
    }
    return classified;
  });

export const processMpfs = (
  ledgerMpf: MidgardMpf | undefined,
  transactionsMpf: MidgardMpf,
  mempoolTxs: readonly Tx.EntryWithTimeStamp[],
  config?: ProcessMpfsConfig,
): Effect.Effect<
  {
    utxoRoot: string;
    rawTxRoot: string;
    txRoot: string;
    transitionTraceRoot: string;
    eventToStepRoot: string;
    validationTracesRoot: string;
    transitionTraceMembers: readonly RetainedTransitionTraceMember[];
    eventToStepMembers: readonly RetainedEventToStepMember[];
    validationTraceMembers: readonly RetainedValidationTraceMember[];
    transitionStepCount: number;
    validationTraceCount: number;
    totalEventCount: number;
    utxoPayloadEntries: readonly UtxoPayloadEntry[];
    ledgerDelta: LedgerDelta;
    utxoPayloadAggregate: UtxoPayloadSizeAggregate;
    mempoolTxHashes: Buffer[];
    processedMempoolTxs: readonly Tx.EntryWithTimeStamp[];
    sizeOfProcessedTxs: number;
    rejectedMempoolTxsCount: number;
    rejectedMempoolTxHashes: readonly Buffer[];
    rejectionEntries: readonly TxRejectionsDB.EntryNoTimestamp[];
    includedDepositEntriesCount: number;
    includedDepositEntries: readonly DepositsDB.Entry[];
    includedDepositEventIds: readonly Buffer[];
    includedForcedTransactionEntriesCount: number;
    includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
    includedForcedTransactionEventIds: readonly Buffer[];
    includedWithdrawalEntriesCount: number;
    includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
    includedWithdrawalEventIds: readonly Buffer[];
    transitionTraceBuild: TransitionTraceBuildResult;
    nativeMpfReplay?: NativeMpfReplayBuild;
    nativeMpfHandle?: NativeMpfGenerationHandle;
  },
  MpfError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const nativeMpf = config?.nativeMpf;
    if ((ledgerMpf === undefined) === (nativeMpf === undefined)) {
      return yield* Effect.fail(
        MpfError.rootBuild(
          "ledger engine selection",
          new Error(
            "Exactly one of a local ledger MPF or Architecture G build context must be supplied",
          ),
        ),
      );
    }
    const processedMempoolTxs: Tx.EntryWithTimeStamp[] = [];
    const rejectedTxHashes: Buffer[] = [];
    const rejectionEntries: TxRejectionsDB.EntryNoTimestamp[] = [];
    const mempoolTxHashes: Buffer[] = [];
    const transactionOps: MpfBatchOp[] = [];
    const transactionSourceOps: MpfInsertBatchOp[] = [];
    const decodedMempoolTxs: DecodedMempoolTxForCommit[] = [];
    let txDeltaCacheHitCount = 0;
    let txDeltaFallbackDecodedCount = 0;
    let sizeOfProcessedTxs = 0;
    const txDeltaResolutionStartedAtMs = Date.now();
    const txDeltasByTxHash = yield* MempoolTxDeltasDB.retrieveByTxIds(
      mempoolTxs.map((entry) => entry[Tx.Columns.TX_ID]),
    );
    yield* Effect.logInfo("🔹 Going through mempool txs and finding roots...");
    yield* Effect.forEach(mempoolTxs, (entry: Tx.EntryWithTimeStamp) =>
      Effect.gen(function* () {
        const txHash = entry[Tx.Columns.TX_ID];
        const txCbor = entry[Tx.Columns.TX];
        const txHashHex = txHash.toString("hex");
        const existingDelta = txDeltasByTxHash.get(txHashHex);
        const resolved = yield* resolveTxDeltaForCommit(
          entry,
          existingDelta,
        ).pipe(Effect.withSpan("resolveTxDeltaForCommit"));
        if (resolved._tag === "Rejected") {
          rejectedTxHashes.push(Buffer.from(txHash));
          rejectionEntries.push(resolved.rejection);
          yield* Effect.logWarning(
            `Skipping malformed mempool tx ${txHashHex}: ${resolved.rejection[TxRejectionsDB.Columns.REJECT_DETAIL]}`,
          );
          return;
        }
        if (existingDelta === undefined) {
          txDeltaFallbackDecodedCount += 1;
        } else {
          txDeltaCacheHitCount += 1;
        }
        const { spent, produced } = resolved;
        decodedMempoolTxs.push({
          entry,
          txHash,
          txCbor,
          spent,
          produced,
        });
      }),
    );
    yield* Metric.incrementBy(
      commitTxDeltaCacheHitCounter,
      BigInt(txDeltaCacheHitCount),
    );
    yield* Metric.incrementBy(
      commitTxDeltaFallbackDecodedCounter,
      BigInt(txDeltaFallbackDecodedCount),
    );
    yield* logCommitMpfPhaseTiming(
      "tx_delta_resolution",
      txDeltaResolutionStartedAtMs,
      {
        candidate_tx_count: mempoolTxs.length,
        decoded_tx_count: decodedMempoolTxs.length,
        cache_hit_tx_count: txDeltaCacheHitCount,
        fallback_decoded_tx_count: txDeltaFallbackDecodedCount,
        rejected_tx_count: rejectedTxHashes.length,
      },
    );

    const effectiveEndTime = establishEffectiveEndTimeFromDecodedMempool(
      decodedMempoolTxs,
      config?.processedOnlyEndTime,
      config?.depositOnlyEndTime,
    );

    if (
      effectiveEndTime !== undefined &&
      config?.depositVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() > config.depositVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: DepositsDB.tableName,
          message:
            "Refusing to build a block because deposit ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},deposit_visibility_barrier_time=${config.depositVisibilityBarrierTime.toISOString()}`,
        }),
      );
    }

    if (
      effectiveEndTime !== undefined &&
      config?.withdrawalVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() >
        config.withdrawalVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message:
            "Refusing to build a block because withdrawal ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},withdrawal_visibility_barrier_time=${config.withdrawalVisibilityBarrierTime.toISOString()}`,
        }),
      );
    }

    if (
      effectiveEndTime !== undefined &&
      config?.txOrderVisibilityBarrierTime !== undefined &&
      effectiveEndTime.getTime() > config.txOrderVisibilityBarrierTime.getTime()
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "Refusing to build a block because tx-order ingestion is not confirmed up to the selected block end time",
          cause: `effective_end_time=${effectiveEndTime.toISOString()},tx_order_visibility_barrier_time=${config.txOrderVisibilityBarrierTime.toISOString()}`,
        }),
      );
    }

    let includedDepositEntries: readonly DepositsDB.Entry[] = [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedDepositEntries = yield* resolveIncludedDepositEntriesForWindow({
        currentBlockStartTime: config.currentBlockStartTime,
        effectiveEndTime,
        persistProjection: config.deferDatabaseWrites !== true,
      });
      includedDepositEntries = includedDepositEntries.filter(
        (entry) =>
          !config.excludedDepositEventIds?.has(
            entry[DepositsDB.Columns.ID].toString("hex"),
          ),
      );
    }
    const includedDepositEntriesCount = includedDepositEntries.length;
    const includedDepositEventIds = includedDepositEntries.map((entry) =>
      Buffer.from(entry[DepositsDB.Columns.ID]),
    );
    const depositLedgerEntries = yield* Effect.forEach(
      includedDepositEntries,
      DepositsDB.toLedgerEntry,
    );
    const sameBlockDepositOutputsByOutRef = new Map(
      depositLedgerEntries.map((entry) => [
        entry[Ledger.Columns.OUTREF].toString("hex"),
        Buffer.from(entry[Ledger.Columns.OUTPUT]),
      ]),
    );
    const rawInsertedLedgerOutputsByOutRef = new Map(
      sameBlockDepositOutputsByOutRef,
    );

    let includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[] =
      [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedForcedTransactionEntries =
        yield* resolveIncludedForcedTransactionEntriesForWindow({
          currentBlockStartTime: config.currentBlockStartTime,
          effectiveEndTime,
          persistProjection: config.deferDatabaseWrites !== true,
        });
      includedForcedTransactionEntries =
        includedForcedTransactionEntries.filter(
          (entry) =>
            !config.excludedForcedTransactionEventIds?.has(
              entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
            ),
        );
    }
    const includedForcedTransactionEntriesCount =
      includedForcedTransactionEntries.length;
    const includedForcedTransactionEventIds =
      includedForcedTransactionEntries.map((entry) =>
        Buffer.from(entry[ForcedTransactionsDB.Columns.TX_ORDER_ID]),
      );
    const shouldCheckPayloadRoot =
      (config?.payloadRootCheck ?? "every_block") === "every_block";
    const initialLedgerEntries =
      config?.initialLedgerEntries ??
      (shouldCheckPayloadRoot ? yield* ConfirmedLedgerDB.retrieve : []);
    const selectedLedgerOutputs =
      yield* indexSelectedLedgerOutputs(initialLedgerEntries);

    let includedWithdrawalEntries: readonly WithdrawalsDB.Entry[] = [];
    let classifiedWithdrawals: readonly ClassifiedWithdrawal[] = [];
    if (
      config?.currentBlockStartTime !== undefined &&
      effectiveEndTime !== undefined
    ) {
      includedWithdrawalEntries =
        yield* resolveIncludedWithdrawalEntriesForWindow({
          currentBlockStartTime: config.currentBlockStartTime,
          effectiveEndTime,
        });
      includedWithdrawalEntries = includedWithdrawalEntries.filter(
        (entry) =>
          !config.excludedWithdrawalEventIds?.has(
            entry[WithdrawalsDB.Columns.ID].toString("hex"),
          ),
      );

      const seenWithdrawalTarget = new Map<string, Buffer>();
      const mutableClassifiedWithdrawals: ClassifiedWithdrawal[] = [];
      for (const entry of includedWithdrawalEntries) {
        const ledgerOutRef = yield* WithdrawalsDB.toLedgerOutRef(entry);
        const ledgerOutRefHex = ledgerOutRef.toString("hex");
        const priorWithdrawalEventId =
          seenWithdrawalTarget.get(ledgerOutRefHex);
        if (priorWithdrawalEventId !== undefined) {
          return yield* Effect.fail(
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message:
                "Refusing to build a block because multiple withdrawals target the same L2 outref in one candidate window",
              cause: `l2_outref=${ledgerOutRefHex},first_event_id=${priorWithdrawalEventId.toString(
                "hex",
              )},duplicate_event_id=${entry[WithdrawalsDB.Columns.ID].toString(
                "hex",
              )}`,
            }),
          );
        }

        const selectedLedgerOutput = selectedLedgerOutputs.get(ledgerOutRefHex);
        const rawLedgerOutput =
          selectedLedgerOutput === undefined
            ? Option.none<Buffer>()
            : Option.some(Buffer.from(selectedLedgerOutput));
        const classifiedWithdrawal = yield* classifyWithdrawal({
          entry,
          ledgerOutRef,
          ledgerOutput: rawLedgerOutput,
        });
        mutableClassifiedWithdrawals.push(classifiedWithdrawal);
        seenWithdrawalTarget.set(
          ledgerOutRefHex,
          entry[WithdrawalsDB.Columns.ID],
        );
      }
      classifiedWithdrawals = mutableClassifiedWithdrawals;

      if (config.deferDatabaseWrites !== true) {
        yield* WithdrawalsDB.setSettlementInfoForEventIds(
          classifiedWithdrawals.map((classified) => ({
            eventId: classified.entry[WithdrawalsDB.Columns.ID],
            settlementEventInfo: classified.settlementEventInfo,
            validity: classified.validity,
            validityDetail: classified.validityDetail,
          })),
        );
        yield* WithdrawalsDB.markAwaitingAsProjected(
          classifiedWithdrawals.map(
            (classified) => classified.entry[WithdrawalsDB.Columns.ID],
          ),
        );
      }

      includedWithdrawalEntries = classifiedWithdrawals.map((classified) => ({
        ...classified.entry,
        [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]:
          classified.settlementEventInfo,
        [WithdrawalsDB.Columns.VALIDITY]: classified.validity,
        [WithdrawalsDB.Columns.VALIDITY_DETAIL]: classified.validityDetail,
        [WithdrawalsDB.Columns.STATUS]:
          classified.entry[WithdrawalsDB.Columns.STATUS] ===
          WithdrawalsDB.Status.Awaiting
            ? WithdrawalsDB.Status.Projected
            : classified.entry[WithdrawalsDB.Columns.STATUS],
      }));
    }

    const validWithdrawalClassifications = classifiedWithdrawals.filter(
      (classified) => classified.shouldDeleteLedgerUtxo,
    );
    const withdrawnOutRefHexes = new Set(
      validWithdrawalClassifications.map((classified) =>
        classified.ledgerOutRef.toString("hex"),
      ),
    );
    const orderedDecodedMempoolTxs =
      yield* orderDecodedMempoolTxsForLedgerApplication(decodedMempoolTxs);

    const consensusProfile =
      config?.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE_V1;
    if (!isMidgardConsensusProfileV1(consensusProfile)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message: "Block construction requires the canonical V1 profile",
          cause: "non-v1 consensus profile",
        }),
      );
    }
    if (
      (includedForcedTransactionEntries.length > 0 ||
        orderedDecodedMempoolTxs.length > 0) &&
      (config?.forcedValidation === undefined || effectiveEndTime === undefined)
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "V1 transactions require an exact block-time validation context",
          cause: `forced_count=${includedForcedTransactionEntries.length.toString()},normal_count=${orderedDecodedMempoolTxs.length.toString()},effective_end_time=${effectiveEndTime?.toISOString() ?? "missing"}`,
        }),
      );
    }
    const forcedPreState = new Map(
      initialLedgerEntries.map((entry) => [
        entry[Ledger.Columns.OUTREF].toString("hex"),
        Buffer.from(entry[Ledger.Columns.OUTPUT]),
      ]),
    );
    for (const classifiedWithdrawal of validWithdrawalClassifications) {
      forcedPreState.delete(classifiedWithdrawal.ledgerOutRef.toString("hex"));
    }
    const classifiedForcedTransactionsV1: readonly ClassifiedForcedTransactionV1[] =
      includedForcedTransactionEntries.length === 0
        ? []
        : yield* classifyForcedTransactionsV1({
            entries: includedForcedTransactionEntries,
            initialState: forcedPreState,
            effectiveEndTime: effectiveEndTime!,
            consensusProfile,
            validation: config!.forcedValidation!,
            resolveProgramMaterialSidecar: programMaterialSidecarForEnvelopes,
          });
    includedForcedTransactionEntries = classifiedForcedTransactionsV1.map(
      ({ entry }) => entry,
    );
    for (const classified of classifiedForcedTransactionsV1) {
      for (const operation of classified.rawLedgerOps) {
        if (operation.type === "insert") {
          rawInsertedLedgerOutputsByOutRef.set(
            operation.key.toString("hex"),
            Buffer.from(operation.value),
          );
        }
      }
    }
    if (
      config?.deferDatabaseWrites !== true &&
      classifiedForcedTransactionsV1.length > 0
    ) {
      yield* ForcedTransactionsDB.setProofClassifications(
        classifiedForcedTransactionsV1.map(({ entry }) => ({
          txOrderId: entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
          operatorValidity:
            entry[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY],
          forcedInclusionValue:
            entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
          programMaterialSidecarCbor:
            entry[
              ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR
            ],
        })),
      );
    }
    const forcedLedgerOpsByEventId = new Map(
      classifiedForcedTransactionsV1.map(({ entry, ledgerOps }) => [
        entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
        ledgerOps,
      ]),
    );
    const forcedTransitionEffectsByEventId = new Map(
      classifiedForcedTransactionsV1.map(({ entry, transitionEffect }) => [
        entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
        transitionEffect,
      ]),
    );
    const forcedSpentOutRefHexes = new Set(
      [...forcedLedgerOpsByEventId.values()].flatMap((ops) =>
        ops.flatMap((op) =>
          op.type === "delete" ? [op.key.toString("hex")] : [],
        ),
      ),
    );
    const proofNormalLedgerOpsByTxId = new Map<string, readonly MpfBatchOp[]>();
    const proofNormalLedgerWitnessesByTxId = new Map<
      string,
      readonly ValidationMachineLedgerEntry[]
    >();
    const proofNormalLedgerMutationsByTxId = new Map<
      string,
      readonly ValidationMachineLedgerMutationStep[]
    >();
    const proofNormalProgramMaterialByTxId = new Map<string, Buffer>();

    yield* Effect.forEach(orderedDecodedMempoolTxs, (decoded) =>
      Effect.gen(function* () {
        const txHashHex = decoded.txHash.toString("hex");
        const withdrawnOutRef = decoded.spent.find((outRef) =>
          withdrawnOutRefHexes.has(outRef.toString("hex")),
        );
        if (withdrawnOutRef !== undefined) {
          rejectedTxHashes.push(Buffer.from(decoded.txHash));
          rejectionEntries.push({
            [TxRejectionsDB.Columns.TX_ID]: Buffer.from(decoded.txHash),
            [TxRejectionsDB.Columns.REJECT_CODE]:
              COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
            [TxRejectionsDB.Columns.REJECT_DETAIL]:
              `Transaction spends L2 outref ${withdrawnOutRef.toString(
                "hex",
              )}, which is consumed by a valid withdrawal in the same block window`,
          });
          yield* Effect.logWarning(
            `Skipping mempool tx ${txHashHex}: it spends an outref consumed by a due withdrawal event.`,
          );
          return;
        }
        const forcedSpentOutRef = decoded.spent.find((outRef) =>
          forcedSpentOutRefHexes.has(outRef.toString("hex")),
        );
        if (forcedSpentOutRef !== undefined) {
          rejectedTxHashes.push(Buffer.from(decoded.txHash));
          rejectionEntries.push({
            [TxRejectionsDB.Columns.TX_ID]: Buffer.from(decoded.txHash),
            [TxRejectionsDB.Columns.REJECT_CODE]:
              COMMIT_REJECT_CODE_FORCED_TRANSACTION_INPUT,
            [TxRejectionsDB.Columns.REJECT_DETAIL]:
              `Transaction spends L2 outref ${forcedSpentOutRef.toString(
                "hex",
              )}, which is consumed by a valid forced transaction earlier in the same block`,
          });
          yield* Effect.logWarning(
            `Skipping mempool tx ${txHashHex}: it spends an outref consumed by a valid forced transaction.`,
          );
          return;
        }
        const sameBlockDepositInput = decoded.spent.find((outRef) =>
          sameBlockDepositOutputsByOutRef.has(outRef.toString("hex")),
        );
        if (sameBlockDepositInput !== undefined) {
          rejectedTxHashes.push(Buffer.from(decoded.txHash));
          rejectionEntries.push({
            [TxRejectionsDB.Columns.TX_ID]: Buffer.from(decoded.txHash),
            [TxRejectionsDB.Columns.REJECT_CODE]:
              COMMIT_REJECT_CODE_SAME_BLOCK_DEPOSIT_INPUT,
            [TxRejectionsDB.Columns.REJECT_DETAIL]:
              `Transaction spends L2 outref ${sameBlockDepositInput.toString(
                "hex",
              )}, which is produced by a deposit that executes later in the same block window`,
          });
          yield* Effect.logWarning(
            `Skipping mempool tx ${txHashHex}: it spends an outref produced by a due deposit event that executes after L2 transactions.`,
          );
          return;
        }

        mempoolTxHashes.push(decoded.txHash);
        processedMempoolTxs.push(decoded.entry);
        sizeOfProcessedTxs += decoded.txCbor.length;
        const transactionInsertOp = {
          type: "insert",
          key: decoded.txHash,
          value: encodeTransactionRootValue(
            decoded.txCbor,
            config?.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE_V1,
          ),
        } as const satisfies MpfInsertBatchOp;
        transactionOps.push(transactionInsertOp);
        transactionSourceOps.push(transactionInsertOp);
      }),
    );

    if (processedMempoolTxs.length > 0) {
      const validation = config!.forcedValidation!;
      const durableProgramMaterial =
        yield* TxAdmissionsDB.retrieveProgramMaterialSidecars(
          processedMempoolTxs.map((entry) => entry[Tx.Columns.TX_ID]),
        );
      for (const material of durableProgramMaterial) {
        proofNormalProgramMaterialByTxId.set(
          material.txId.toString("hex"),
          Buffer.from(material.sidecarCbor),
        );
      }
      if (
        proofNormalProgramMaterialByTxId.size !== processedMempoolTxs.length ||
        processedMempoolTxs.some(
          (entry) =>
            !proofNormalProgramMaterialByTxId.has(
              entry[Tx.Columns.TX_ID].toString("hex"),
            ),
        )
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: TxAdmissionsDB.payloadTableName,
            message:
              "V1 validation requires one durable canonical CEK material sidecar per normal transaction",
            cause: `transactions=${processedMempoolTxs.length.toString()},sidecars=${proofNormalProgramMaterialByTxId.size.toString()}`,
          }),
        );
      }
      const proofPreState = new Map(
        initialLedgerEntries.map((entry) => [
          entry[Ledger.Columns.OUTREF].toString("hex"),
          Buffer.from(entry[Ledger.Columns.OUTPUT]),
        ]),
      );
      for (const classifiedWithdrawal of validWithdrawalClassifications) {
        proofPreState.delete(classifiedWithdrawal.ledgerOutRef.toString("hex"));
      }
      for (const classified of classifiedForcedTransactionsV1) {
        for (const op of classified.rawLedgerOps) {
          if (op.type === "delete") {
            proofPreState.delete(op.key.toString("hex"));
          } else {
            proofPreState.set(op.key.toString("hex"), Buffer.from(op.value));
          }
        }
      }

      const proofPhaseA = yield* runPhaseAValidation(
        processedMempoolTxs.map((entry, index) => ({
          txId: entry[Tx.Columns.TX_ID],
          txCbor: entry[Tx.Columns.TX],
          arrivalSeq: BigInt(index),
          createdAt: entry[Tx.Columns.TIMESTAMPTZ],
          programMaterialSidecarCbor: proofNormalProgramMaterialByTxId.get(
            entry[Tx.Columns.TX_ID].toString("hex"),
          )!,
        })),
        {
          expectedNetworkId: validation.expectedNetworkId,
          minFeeA: validation.minFeeA,
          minFeeB: validation.minFeeB,
          concurrency: 1,
          strictnessProfile: "phase1_midgard",
          consensusProfile,
        },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new DatabaseError({
              table: MempoolDB.tableName,
              message: "V1 normal transaction Phase A failed",
              cause,
            }),
        ),
      );
      const proofPhaseB = yield* runPhaseBValidationWithPatch(
        proofPhaseA.accepted,
        proofPreState,
        {
          nowCardanoSlotNo: validation.slotForUnixTime(
            effectiveEndTime!.getTime(),
          ),
          bucketConcurrency: validation.bucketConcurrency,
          enforceScriptBudget: true,
        },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new DatabaseError({
              table: MempoolDB.tableName,
              message: "V1 normal transaction Phase B failed",
              cause,
            }),
        ),
      );
      const proofRejected = [...proofPhaseA.rejected, ...proofPhaseB.rejected];
      for (const rejected of proofRejected) {
        rejectedTxHashes.push(Buffer.from(rejected.txId));
        rejectionEntries.push({
          [TxRejectionsDB.Columns.TX_ID]: Buffer.from(rejected.txId),
          [TxRejectionsDB.Columns.REJECT_CODE]: rejected.code,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: rejected.detail,
        });
      }

      const acceptedByTxId = new Map(
        proofPhaseB.accepted.map((accepted) => [
          accepted.ledgerTx.txId.toString("hex"),
          accepted,
        ]),
      );
      const proofNormalReplayState = new Map(
        [...proofPreState.entries()].map(([outRef, output]) => [
          outRef,
          Buffer.from(output),
        ]),
      );
      const proofNormalMutationTrie = yield* Effect.tryPromise({
        try: () =>
          Trie.fromList(
            [...proofNormalReplayState.entries()].map(([key, value]) =>
              ledgerOutputToInsertBatchOpV1({
                outRef: Buffer.from(key, "hex"),
                outputCbor: value,
              }),
            ),
            new Store(undefined),
          ),
        catch: (cause) =>
          new DatabaseError({
            table: MempoolDB.tableName,
            message:
              "Failed to construct the normal-transaction validation mutation trie",
            cause,
          }),
      });
      processedMempoolTxs.length = 0;
      mempoolTxHashes.length = 0;
      transactionOps.length = 0;
      transactionSourceOps.length = 0;
      sizeOfProcessedTxs = 0;
      for (const decoded of orderedDecodedMempoolTxs) {
        const txIdHex = decoded.txHash.toString("hex");
        const accepted = acceptedByTxId.get(txIdHex);
        if (accepted === undefined) continue;
        const ledgerOps: readonly MpfBatchOp[] = [
          ...accepted.graph.spentOutRefHexes.map((outRef) => ({
            type: "delete" as const,
            key: Buffer.from(outRef, "hex"),
          })),
          ...accepted.graph.produced.map((produced) =>
            ledgerOutputToInsertBatchOpV1({
              outRef: produced[Ledger.Columns.OUTREF],
              outputCbor: produced[Ledger.Columns.OUTPUT],
            }),
          ),
        ];
        proofNormalLedgerOpsByTxId.set(txIdHex, ledgerOps);
        proofNormalLedgerWitnessesByTxId.set(
          txIdHex,
          validationLedgerWitnesses(proofNormalReplayState, [
            ...accepted.graph.spentOutRefHexes,
            ...accepted.graph.referenceOutRefHexes,
          ]),
        );
        proofNormalLedgerMutationsByTxId.set(
          txIdHex,
          yield* Effect.tryPromise({
            try: () =>
              applyValidationLedgerMutations(
                proofNormalMutationTrie,
                ledgerOps,
              ),
            catch: (cause) =>
              new DatabaseError({
                table: MempoolDB.tableName,
                message:
                  "Failed to derive normal-transaction ledger mutation roots",
                cause,
              }),
          }),
        );
        for (const outRef of accepted.graph.spentOutRefHexes) {
          proofNormalReplayState.delete(outRef);
        }
        for (const produced of accepted.graph.produced) {
          rawInsertedLedgerOutputsByOutRef.set(
            produced[Ledger.Columns.OUTREF].toString("hex"),
            Buffer.from(produced[Ledger.Columns.OUTPUT]),
          );
          proofNormalReplayState.set(
            produced[Ledger.Columns.OUTREF].toString("hex"),
            Buffer.from(produced[Ledger.Columns.OUTPUT]),
          );
        }
        processedMempoolTxs.push(decoded.entry);
        mempoolTxHashes.push(Buffer.from(decoded.txHash));
        sizeOfProcessedTxs += decoded.txCbor.length;
        const transactionInsertOp = {
          type: "insert",
          key: Buffer.from(decoded.txHash),
          value: encodeTransactionRootValue(decoded.txCbor, consensusProfile),
        } as const satisfies MpfInsertBatchOp;
        transactionOps.push(transactionInsertOp);
        transactionSourceOps.push(transactionInsertOp);
      }
    }

    if (depositLedgerEntries.length > 0) {
      yield* Effect.logInfo(
        `🔹 Including ${depositLedgerEntries.length} projected deposit UTxO(s) in the deposit phase.`,
      );
    }

    if (validWithdrawalClassifications.length > 0) {
      yield* Effect.logInfo(
        `🔹 Including ${validWithdrawalClassifications.length} valid withdrawal event(s) in the withdrawal phase.`,
      );
    }

    if (rejectedTxHashes.length > 0 && config?.deferDatabaseWrites !== true) {
      yield* Effect.logWarning(
        `Dropping ${rejectedTxHashes.length} transaction(s) from MempoolDB`,
      );
      yield* persistCommitStageRejectedTransactions({
        rejectedTxHashes,
        rejectionEntries,
      });
    }

    const transactionRootBeforeApply = yield* transactionsMpf.root();
    const ledgerRootBeforeApply =
      ledgerMpf === undefined
        ? Buffer.from(nativeMpf!.handle.baseRoot, "hex")
        : yield* ledgerMpf.root();
    const ledgerRootBeforeApplyHex = ledgerRootBeforeApply.toString("hex");
    const selectedBaseUtxoRoot =
      config?.selectedBaseUtxoRoot ??
      (shouldCheckPayloadRoot
        ? yield* computeUtxoPayloadRoot(
            materializeUtxoPayloadEntries(
              initialLedgerEntries,
              [],
              rawInsertedLedgerOutputsByOutRef,
            ),
          )
        : ledgerRootBeforeApplyHex);
    if (selectedBaseUtxoRoot !== ledgerRootBeforeApplyHex) {
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolLedgerDB.tableName,
          message:
            "Refusing to build a block because the transition trace base UTxO snapshot root does not match the ledger MPF root",
          cause: `selected_base_utxos_root=${selectedBaseUtxoRoot},ledger_mpf_root=${ledgerRootBeforeApplyHex}`,
        }),
      );
    }
    const withdrawalSourceEvents = yield* Effect.forEach(
      includedWithdrawalEntries,
      (entry) =>
        Effect.gen(function* () {
          const eventKey = yield* withdrawalTraceEventKey(entry);
          const valid = entry[WithdrawalsDB.Columns.VALIDITY];
          const effect = canonicalCommittedWithdrawalTransitionEffectV1({
            committedValid: valid === WithdrawalsDB.Validity.WithdrawalIsValid,
            outRefCbor: yield* WithdrawalsDB.toLedgerOutRef(entry),
          });
          return {
            eventKey,
            phase: "Withdrawal" as const,
            ledgerOps: transitionEffectToLedgerOpsV1(effect),
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const forcedTransactionSourceEvents = yield* Effect.forEach(
      includedForcedTransactionEntries,
      (entry) =>
        Effect.gen(function* () {
          const ledgerOps = forcedLedgerOpsByEventId.get(
            entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
          );
          const effect = forcedTransitionEffectsByEventId.get(
            entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
          );
          if (
            entry[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY] ===
              "TxIsValid" &&
            ledgerOps === undefined
          ) {
            return yield* Effect.fail(
              new DatabaseError({
                table: ForcedTransactionsDB.tableName,
                message:
                  "Refusing to build a transition trace for an effectful forced transaction before forced transaction ledger deltas are available",
                cause: `tx_order_id=${entry[
                  ForcedTransactionsDB.Columns.TX_ORDER_ID
                ].toString("hex")}`,
              }),
            );
          }
          return {
            eventKey: yield* forcedTransactionTraceEventKey(entry),
            phase: "ForcedTransaction" as const,
            ledgerOps:
              effect === undefined
                ? (ledgerOps ?? [])
                : transitionEffectToLedgerOpsV1(effect),
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const decodedByTxHash = new Map(
      orderedDecodedMempoolTxs.map((decoded) => [
        decoded.txHash.toString("hex"),
        decoded,
      ]),
    );
    const l2TransactionSourceEvents = yield* Effect.forEach(
      processedMempoolTxs,
      (entry, index) =>
        Effect.gen(function* () {
          const txHash = entry[Tx.Columns.TX_ID];
          const decoded = decodedByTxHash.get(txHash.toString("hex"));
          if (decoded === undefined) {
            return yield* Effect.fail(
              new DatabaseError({
                table: MempoolDB.tableName,
                message:
                  "Refusing to build a transition trace because an included transaction is missing decoded ledger deltas",
                cause: `source_index=${index.toString()},tx_id=${txHash.toString(
                  "hex",
                )}`,
              }),
            );
          }
          const proofLedgerOps = proofNormalLedgerOpsByTxId.get(
            txHash.toString("hex"),
          );
          if (proofLedgerOps === undefined) {
            return yield* Effect.fail(
              new DatabaseError({
                table: MempoolDB.tableName,
                message:
                  "Refusing to build a V1 transition trace because an included transaction is missing its sequentially validated ledger delta",
                cause: `source_index=${index.toString()},tx_id=${txHash.toString(
                  "hex",
                )}`,
              }),
            );
          }
          return {
            eventKey: l2TransactionTraceEventKey(decoded.txHash),
            phase: "L2Transaction" as const,
            ledgerOps: proofLedgerOps,
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const depositSourceEvents = yield* Effect.forEach(
      includedDepositEntries,
      (entry) =>
        Effect.gen(function* () {
          const ledgerEntry = yield* DepositsDB.toLedgerEntry(entry);
          const effect = canonicalDepositTransitionEffectV1({
            outRefCbor: ledgerEntry[Ledger.Columns.OUTREF],
            outputCbor: ledgerEntry[Ledger.Columns.OUTPUT],
          });
          return {
            eventKey: yield* depositTraceEventKey(entry),
            phase: "Deposit" as const,
            ledgerOps: transitionEffectToLedgerOpsV1(effect),
          } satisfies TransitionTraceSourceEvent;
        }),
    );
    const sourceEvents = [
      ...withdrawalSourceEvents,
      ...forcedTransactionSourceEvents,
      ...l2TransactionSourceEvents,
      ...depositSourceEvents,
    ];
    const transitionLedgerOps = sourceEvents.flatMap((event) =>
      event.ledgerOps.map((op) => ({ ...op })),
    );
    const baseUtxoPayloadAggregate =
      config?.baseUtxoPayloadAggregate ??
      ledgerPayloadAggregateFromEntries(initialLedgerEntries);
    const utxoPayloadAggregate =
      yield* applyLedgerOpsToUtxoPayloadAggregateFromFullValues(
        baseUtxoPayloadAggregate,
        transitionLedgerOps,
        new Map(
          initialLedgerEntries.map((entry) => [
            entry[Ledger.Columns.OUTREF].toString("hex"),
            Buffer.from(entry[Ledger.Columns.OUTPUT]),
          ]),
        ),
        rawInsertedLedgerOutputsByOutRef,
      );
    const txRootFiber = yield* buildTransactionsSourceRoot(
      transactionSourceOps,
      SDK.ROOT_DOMAINS.transactionsV1,
    ).pipe(Effect.fork);
    const architectureGTransactionMpfFiber =
      ledgerMpf === undefined
        ? yield* Effect.gen(function* () {
            const startedAtMs = Date.now();
            yield* transactionsMpf.applyBatch(transactionOps).pipe(
              Effect.catchAll((error) =>
                transactionsMpf.resetToRoot(transactionRootBeforeApply).pipe(
                  Effect.catchAll(() => Effect.void),
                  Effect.flatMap(() => Effect.fail(error)),
                ),
              ),
            );
            return { durationMs: Date.now() - startedAtMs };
          }).pipe(Effect.fork)
        : undefined;
    const transitionTraceStartedAtMs = Date.now();
    const transitionTraceBuild = yield* ledgerMpf === undefined
      ? buildNativeTransitionTraceResult({
          nativeMpf: nativeMpf!,
          sourceEvents,
          withdrawalCount: includedWithdrawalEntries.length,
          forcedTransactionCount: includedForcedTransactionEntries.length,
          l2TransactionCount: processedMempoolTxs.length,
          depositCount: includedDepositEntries.length,
        }).pipe(
          Effect.catchAll((error) =>
            Effect.gen(function* () {
              if (architectureGTransactionMpfFiber !== undefined) {
                yield* Fiber.interrupt(architectureGTransactionMpfFiber);
              }
              yield* transactionsMpf
                .resetToRoot(transactionRootBeforeApply)
                .pipe(Effect.catchAll(() => Effect.void));
              return yield* Effect.fail(error);
            }),
          ),
        )
      : buildTransitionTraceResult({
          ledgerMpf,
          sourceEvents,
          withdrawalCount: includedWithdrawalEntries.length,
          forcedTransactionCount: includedForcedTransactionEntries.length,
          l2TransactionCount: processedMempoolTxs.length,
          depositCount: includedDepositEntries.length,
        }).pipe(
          Effect.catchAll((error) =>
            ledgerMpf
              .resetToRoot(ledgerRootBeforeApply)
              .pipe(Effect.flatMap(() => Effect.fail(error))),
          ),
        );
    yield* logCommitMpfPhaseTiming(
      "transition_trace_build",
      transitionTraceStartedAtMs,
      {
        base_entry_count: initialLedgerEntries.length,
        source_event_count: sourceEvents.length,
        ledger_op_count: transitionLedgerOps.length,
        prefetch_ms: transitionTraceBuild.pathHydration.prefetchMs,
        prefetch_unique_paths: transitionTraceBuild.pathHydration.uniquePaths,
        prefetch_nodes_requested:
          transitionTraceBuild.pathHydration.nodesRequested,
        prefetch_hydration_hits:
          transitionTraceBuild.pathHydration.hydrationHits,
        prefetch_hydration_misses:
          transitionTraceBuild.pathHydration.hydrationMisses,
        prefetch_max_in_flight: transitionTraceBuild.pathHydration.maxInFlight,
        prefetch_max_batch_keys:
          transitionTraceBuild.pathHydration.maxBatchKeys,
        prefetch_retained_bytes_estimate:
          transitionTraceBuild.pathHydration.retainedBytesEstimate,
        hydration_chunk_count: transitionTraceBuild.pathHydration.chunkCount,
        arena_checkpoint_ms: transitionTraceBuild.pathHydration.checkpointMs,
        arena_authentication_ms:
          transitionTraceBuild.pathHydration.authenticationMs,
        arena_materialize_ms: transitionTraceBuild.pathHydration.materializeMs,
        arena_collapse_ms: transitionTraceBuild.pathHydration.collapseMs,
        arena_checkpoint_serialized_nodes:
          transitionTraceBuild.pathHydration.checkpointSerializedNodes,
        arena_checkpoint_serialized_bytes:
          transitionTraceBuild.pathHydration.checkpointSerializedBytes,
        arena_verified_upper_nodes:
          transitionTraceBuild.pathHydration.verifiedUpperNodes,
        arena_retained_upper_nodes:
          transitionTraceBuild.pathHydration.retainedUpperNodes,
        arena_collapsed_nodes:
          transitionTraceBuild.pathHydration.collapsedNodes,
        arena_peak_decoded_nodes:
          transitionTraceBuild.pathHydration.peakDecodedNodes,
      },
    );
    const utxoPayloadEntries = shouldCheckPayloadRoot
      ? materializeUtxoPayloadEntries(
          initialLedgerEntries,
          transitionLedgerOps,
          rawInsertedLedgerOutputsByOutRef,
        )
      : [];
    const transactionMpfApplyStartedAtMs = Date.now();
    const transactionMpfApplyDurationMs =
      architectureGTransactionMpfFiber === undefined
        ? yield* transactionsMpf.applyBatch(transactionOps).pipe(
            Effect.catchAll((error) =>
              Effect.gen(function* () {
                yield* transactionsMpf
                  .resetToRoot(transactionRootBeforeApply)
                  .pipe(Effect.catchAll(() => Effect.void));
                yield* ledgerMpf!
                  .resetToRoot(ledgerRootBeforeApply)
                  .pipe(Effect.catchAll(() => Effect.void));
                return yield* Effect.fail(error);
              }),
            ),
            Effect.map(() => Date.now() - transactionMpfApplyStartedAtMs),
          )
        : (yield* Fiber.join(architectureGTransactionMpfFiber)).durationMs;
    yield* logCommitMpfPhaseTiming(
      "transaction_mpf_apply",
      Date.now() - transactionMpfApplyDurationMs,
      {
        transaction_op_count: transactionOps.length,
        overlapped_with_transition_trace:
          architectureGTransactionMpfFiber === undefined ? 0 : 1,
      },
    );

    const rawTxRoot = yield* transactionsMpf.rootHex();
    const txRoot = yield* Fiber.join(txRootFiber);
    const utxoRoot =
      ledgerMpf === undefined
        ? nativeMpf!.candidateRoot!
        : yield* ledgerMpf.rootHex();
    if (shouldCheckPayloadRoot) {
      const payloadRootCheckStartedAtMs = Date.now();
      const payloadUtxoRoot = yield* computeUtxoPayloadRoot(utxoPayloadEntries);
      yield* logCommitMpfPhaseTiming(
        "payload_root_check",
        payloadRootCheckStartedAtMs,
        {
          payload_entry_count: utxoPayloadEntries.length,
        },
      );
      if (payloadUtxoRoot !== utxoRoot) {
        return yield* Effect.fail(
          new DatabaseError({
            table: MempoolLedgerDB.tableName,
            message:
              "Refusing to build a block because the DA payload UTxO snapshot root does not match the computed ledger MPF root",
            cause: `payload_utxos_root=${payloadUtxoRoot},computed_utxos_root=${utxoRoot}`,
          }),
        );
      }
    }
    if (transitionTraceBuild.finalUtxosRoot !== utxoRoot) {
      return yield* Effect.fail(
        new DatabaseError({
          table: MempoolLedgerDB.tableName,
          message:
            "Refusing to build a block because the transition trace final UTxO root does not match the computed ledger MPF root",
          cause: `trace_final_utxos_root=${transitionTraceBuild.finalUtxosRoot},computed_utxos_root=${utxoRoot}`,
        }),
      );
    }

    const validationTraceBuild: ValidationTraceBuildResult = yield* Effect.gen(
      function* () {
        const expectedValidationTraceCount =
          includedForcedTransactionEntries.length + processedMempoolTxs.length;
        if (expectedValidationTraceCount === 0) {
          return {
            validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            validationTraceMembers: [],
            validationTraceCount: 0,
          };
        }
        const validation = config!.forcedValidation!;
        const validationTraceBuilder =
          config?.validationTraceBuilder ??
          buildDeterministicValidationTraceMembers;
        const traceByEventKey = new Map<
          string,
          RetainedTransitionTraceMember
        >();
        for (const member of transitionTraceBuild.transitionTraceMembers) {
          const keyHex = (yield* eventKeyCbor(member.value.event_key)).toString(
            "hex",
          );
          if (traceByEventKey.has(keyHex)) {
            return yield* Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Transition trace contains a duplicate validation-trace event key",
                cause: `event_key_cbor=${keyHex}`,
              }),
            );
          }
          traceByEventKey.set(keyHex, member);
        }
        const forcedByOrderId = new Map(
          classifiedForcedTransactionsV1.map((classified) => [
            classified.entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString(
              "hex",
            ),
            classified,
          ]),
        );
        const forcedValidationInputs = yield* Effect.forEach(
          includedForcedTransactionEntries,
          (entry) =>
            Effect.gen(function* () {
              const eventKey = yield* forcedTransactionTraceEventKey(entry);
              const keyCbor = yield* eventKeyCbor(eventKey);
              const trace = traceByEventKey.get(keyCbor.toString("hex"));
              const classified = forcedByOrderId.get(
                entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
              );
              const canonicalTransactionCbor =
                entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR];
              if (
                trace === undefined ||
                classified === undefined ||
                canonicalTransactionCbor == null
              ) {
                return yield* Effect.fail(
                  new DatabaseError({
                    table: ForcedTransactionsDB.tableName,
                    message:
                      "V1 forced transaction is missing validation-trace source material",
                    cause: `tx_order_id=${entry[
                      ForcedTransactionsDB.Columns.TX_ORDER_ID
                    ].toString("hex")}`,
                  }),
                );
              }
              return {
                eventKey,
                transactionId: Buffer.from(
                  entry[ForcedTransactionsDB.Columns.TX_ID],
                ),
                canonicalTransactionCbor: Buffer.from(canonicalTransactionCbor),
                programMaterialSidecarCbor: Buffer.from(
                  classified.programMaterialSidecarCbor,
                ),
                sourceKind: "forced" as const,
                priorUtxosRoot: trace.value.pre_utxos_root,
                postUtxosRoot: trace.value.post_utxos_root,
                ledgerOps: classified.ledgerOps,
                ledgerWitnessEntries: classified.ledgerWitnessEntries,
                ledgerMutationSteps: classified.ledgerMutationSteps,
                verdict:
                  classified.rejectionCode === null
                    ? ("accepted" as const)
                    : ("rejected" as const),
                rejectionCode: classified.rejectionCode,
              };
            }),
        );
        const normalValidationInputs = yield* Effect.forEach(
          processedMempoolTxs,
          (entry) =>
            Effect.gen(function* () {
              const eventKey = l2TransactionTraceEventKey(
                entry[Tx.Columns.TX_ID],
              );
              const keyCbor = yield* eventKeyCbor(eventKey);
              const trace = traceByEventKey.get(keyCbor.toString("hex"));
              const ledgerOps = proofNormalLedgerOpsByTxId.get(
                entry[Tx.Columns.TX_ID].toString("hex"),
              );
              const ledgerWitnessEntries = proofNormalLedgerWitnessesByTxId.get(
                entry[Tx.Columns.TX_ID].toString("hex"),
              );
              const ledgerMutationSteps = proofNormalLedgerMutationsByTxId.get(
                entry[Tx.Columns.TX_ID].toString("hex"),
              );
              const programMaterialSidecarCbor =
                proofNormalProgramMaterialByTxId.get(
                  entry[Tx.Columns.TX_ID].toString("hex"),
                );
              if (
                trace === undefined ||
                ledgerOps === undefined ||
                ledgerWitnessEntries === undefined ||
                ledgerMutationSteps === undefined ||
                programMaterialSidecarCbor === undefined
              ) {
                return yield* Effect.fail(
                  new DatabaseError({
                    table: MempoolDB.tableName,
                    message:
                      "V1 normal transaction is missing validation-trace source material",
                    cause: `tx_id=${entry[Tx.Columns.TX_ID].toString("hex")}`,
                  }),
                );
              }
              return {
                eventKey,
                transactionId: Buffer.from(entry[Tx.Columns.TX_ID]),
                canonicalTransactionCbor: Buffer.from(entry[Tx.Columns.TX]),
                programMaterialSidecarCbor: Buffer.from(
                  programMaterialSidecarCbor,
                ),
                sourceKind: "normal" as const,
                priorUtxosRoot: trace.value.pre_utxos_root,
                postUtxosRoot: trace.value.post_utxos_root,
                ledgerOps,
                ledgerWitnessEntries,
                ledgerMutationSteps,
                verdict: "accepted" as const,
                rejectionCode: null,
              };
            }),
        );
        const validationInputs = [
          ...forcedValidationInputs,
          ...normalValidationInputs,
        ];
        const validationTraceMembers = yield* validationTraceBuilder({
          consensusProfile,
          blockEndTime: effectiveEndTime!,
          expectedNetworkId: validation.expectedNetworkId,
          minFeeA: validation.minFeeA,
          minFeeB: validation.minFeeB,
          blockSlot: validation.slotForUnixTime(effectiveEndTime!.getTime()),
          transactions: validationInputs,
        });
        yield* validateValidationTraceEventKeySet({
          expectedEventKeys: validationInputs.map(
            (transaction) => transaction.eventKey,
          ),
          transitionEventKeyCbors: new Set(traceByEventKey.keys()),
          members: validationTraceMembers,
        });
        const validationTracesRoot =
          validationTraceMembers.length === 0
            ? SDK.EMPTY_MERKLE_TREE_ROOT
            : yield* countedRootFromEncodedEntries(
                SDK.ROOT_DOMAINS.validationTraces,
                validationTraceMembers.map((member) => ({
                  key: member.keyCbor,
                  value: member.valueCbor,
                })),
              );
        return {
          validationTracesRoot,
          validationTraceMembers,
          validationTraceCount: validationTraceMembers.length,
        };
      },
    );

    yield* Effect.logInfo(
      `🔹 New raw transaction MPF root found: ${rawTxRoot}`,
    );
    yield* Effect.logInfo(`🔹 New transaction source root found: ${txRoot}`);
    yield* Effect.logInfo(`🔹 New UTxO root found: ${utxoRoot}`);
    yield* Effect.logInfo(
      `🔹 New transition trace root found: ${transitionTraceBuild.transitionTraceRoot}`,
    );
    yield* Effect.logInfo(
      `🔹 New event-to-step root found: ${transitionTraceBuild.eventToStepRoot}`,
    );
    yield* Effect.logInfo(
      `🔹 New validation traces root found: ${validationTraceBuild.validationTracesRoot}`,
    );

    const recordCorpusPath = config?.recordCorpusPath?.trim() ?? "";
    if (recordCorpusPath.length > 0) {
      const finalUtxoEntries = materializeUtxoPayloadEntries(
        initialLedgerEntries,
        transitionLedgerOps,
        rawInsertedLedgerOutputsByOutRef,
      );
      const deposits = includedDepositEntries.map((entry) => ({
        key: Buffer.from(entry[DepositsDB.Columns.ID]),
        value: Buffer.from(entry[DepositsDB.Columns.INFO]),
      }));
      const forcedTransactions = includedForcedTransactionEntries.map(
        (entry) => ({
          key: Buffer.from(entry[ForcedTransactionsDB.Columns.TX_ORDER_ID]),
          value: Buffer.from(
            entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
          ),
        }),
      );
      const withdrawals = includedWithdrawalEntries.flatMap((entry) => {
        const value = entry[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
        return value === null
          ? []
          : [
              {
                key: Buffer.from(entry[WithdrawalsDB.Columns.ID]),
                value: Buffer.from(value),
              },
            ];
      });
      const [depositsRoot, withdrawalsRoot, forcedTransactionsRoot] =
        yield* Effect.all(
          [
            countedRootFromEncodedEntries(SDK.ROOT_DOMAINS.deposits, deposits),
            countedRootFromEncodedEntries(
              SDK.ROOT_DOMAINS.withdrawals,
              withdrawals,
            ),
            countedRootFromEncodedEntries(
              SDK.ROOT_DOMAINS.forcedTransactionsV1,
              forcedTransactions,
            ),
          ],
          { concurrency: 1 },
        );
      const encodeEntry = (entry: {
        readonly key: Buffer;
        readonly value: Buffer;
      }): CorpusHexEntry => ({
        key: entry.key.toString("hex"),
        value: entry.value.toString("hex"),
      });
      const encodeOp = (op: MpfBatchOp): CorpusLedgerOp =>
        op.type === "insert"
          ? {
              type: "insert",
              key: op.key.toString("hex"),
              value: op.value.toString("hex"),
            }
          : { type: "delete", key: op.key.toString("hex") };
      const block: MpfReplayCorpusBlock = {
        version: 1,
        label: `commit-${Date.now().toString()}`,
        initialLedgerEntries: initialLedgerEntries.map((entry) =>
          encodeEntry(ledgerEntryToInsertBatchOp(entry)),
        ),
        sourceEvents: yield* Effect.forEach(sourceEvents, (event) =>
          eventKeyCbor(event.eventKey).pipe(
            Effect.map((encoded) => ({
              phase: event.phase,
              eventKeyCbor: encoded.toString("hex"),
              ledgerOps: event.ledgerOps.map(encodeOp),
            })),
          ),
        ),
        transactionOps: transactionSourceOps.map(encodeEntry),
        deposits: deposits.map(encodeEntry),
        withdrawals: withdrawals.map(encodeEntry),
        forcedTransactions: forcedTransactions.map(encodeEntry),
        finalUtxoEntries: finalUtxoEntries.map((entry) =>
          encodeEntry(
            ledgerOutputToInsertBatchOpV1({
              outRef: entry.outref,
              outputCbor: entry.output,
            }),
          ),
        ),
        expected: {
          utxoRoot,
          rawTxRoot,
          txRoot,
          transitionTraceRoot: transitionTraceBuild.transitionTraceRoot,
          eventToStepRoot: transitionTraceBuild.eventToStepRoot,
          depositsRoot,
          withdrawalsRoot,
          forcedTransactionsRoot,
          transitionRoots: transitionTraceBuild.transitionTraceMembers.map(
            (member) => ({
              pre: member.value.pre_utxos_root,
              post: member.value.post_utxos_root,
            }),
          ),
        },
      };
      yield* Effect.try({
        try: () => {
          FS.mkdirSync(dirname(recordCorpusPath), { recursive: true });
          FS.appendFileSync(recordCorpusPath, `${JSON.stringify(block)}\n`);
        },
        catch: (cause) =>
          MpfError.rootBuild("MPF replay corpus record tap", cause),
      });
    }

    const includedWithdrawalEntriesCount = includedWithdrawalEntries.length;
    const includedWithdrawalEventIds = includedWithdrawalEntries.map((entry) =>
      Buffer.from(entry[WithdrawalsDB.Columns.ID]),
    );
    const nativeMpfReplay: NativeMpfReplayBuild | undefined =
      nativeMpf === undefined
        ? undefined
        : {
            schema: 1,
            ownerBinarySha256: Buffer.from(nativeMpf.ownerBinarySha256, "hex"),
            baseRoot: Buffer.from(nativeMpf.handle.baseRoot, "hex"),
            candidateRoot: Buffer.from(nativeMpf.candidateRoot!, "hex"),
            eventLog: Buffer.from(nativeMpf.eventLog!),
            eventLogDigest: Buffer.from(nativeMpf.eventLogDigest!, "hex"),
            eventRoots: Buffer.from(nativeMpf.eventRoots!.join(""), "hex"),
            eventCount: nativeMpf.eventRoots!.length,
          };

    return {
      utxoRoot,
      rawTxRoot,
      txRoot,
      transitionTraceRoot: transitionTraceBuild.transitionTraceRoot,
      eventToStepRoot: transitionTraceBuild.eventToStepRoot,
      validationTracesRoot: validationTraceBuild.validationTracesRoot,
      transitionTraceMembers: transitionTraceBuild.transitionTraceMembers,
      eventToStepMembers: transitionTraceBuild.eventToStepMembers,
      validationTraceMembers: validationTraceBuild.validationTraceMembers,
      transitionStepCount: transitionTraceBuild.transitionStepCount,
      validationTraceCount: validationTraceBuild.validationTraceCount,
      totalEventCount: transitionTraceBuild.totalEventCount,
      utxoPayloadEntries,
      ledgerDelta: collapseLedgerDelta(
        transitionLedgerOps,
        rawInsertedLedgerOutputsByOutRef,
      ),
      utxoPayloadAggregate,
      mempoolTxHashes,
      processedMempoolTxs,
      sizeOfProcessedTxs,
      rejectedMempoolTxsCount: rejectedTxHashes.length,
      rejectedMempoolTxHashes: rejectedTxHashes,
      rejectionEntries,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
      includedForcedTransactionEntriesCount,
      includedForcedTransactionEntries,
      includedForcedTransactionEventIds,
      includedWithdrawalEntriesCount,
      includedWithdrawalEntries,
      includedWithdrawalEventIds,
      transitionTraceBuild,
      nativeMpfReplay,
      nativeMpfHandle: nativeMpf?.handle,
    };
  });

export const withMpfRootTransaction = <A, E, R>(
  mpf: MidgardMpf,
  eff: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | MpfError, R> =>
  Effect.gen(function* () {
    const beforeRoot = yield* mpf.root();
    return yield* eff.pipe(
      Effect.catchAll((e) =>
        Effect.gen(function* () {
          yield* mpf.resetToRoot(beforeRoot);
          return yield* Effect.fail(e);
        }),
      ),
    );
  });

export const withMpfRootTransactions = <A, E, R>(
  mpfs: readonly MidgardMpf[],
  eff: Effect.Effect<A, E, R>,
  shouldPreserveRoots: (value: A) => boolean,
): Effect.Effect<A, E | MpfError, R> =>
  Effect.gen(function* () {
    const beforeRoots = yield* Effect.forEach(mpfs, (mpf) => mpf.root(), {
      concurrency: "unbounded",
    });
    const resetRoots = Effect.forEach(
      mpfs,
      (mpf, index) => mpf.resetToRoot(beforeRoots[index]!),
      {
        discard: true,
        concurrency: "unbounded",
      },
    );

    const result = yield* Effect.either(eff);
    if (result._tag === "Left") {
      yield* resetRoots;
      return yield* Effect.fail(result.left);
    }
    if (!shouldPreserveRoots(result.right)) {
      yield* resetRoots;
    }
    return result.right;
  });

export const withMpfBlockOverlays = <A, E, R>(
  mpfs: readonly MidgardMpf[],
  eff: Effect.Effect<A, E, R>,
  shouldPromote: (value: A) => boolean,
): Effect.Effect<A, E | MpfError, R> =>
  Effect.gen(function* () {
    yield* Effect.forEach(mpfs, (mpf) => mpf.beginBlockOverlay(), {
      discard: true,
      concurrency: "unbounded",
    });
    const discard = Effect.forEach(
      mpfs,
      (mpf) => mpf.discardBlockOverlayIfActive(),
      {
        discard: true,
        concurrency: "unbounded",
      },
    );
    const result = yield* Effect.either(eff);
    if (result._tag === "Left") {
      yield* discard;
      return yield* Effect.fail(result.left);
    }
    if (shouldPromote(result.right)) {
      yield* Effect.forEach(
        mpfs,
        (mpf) =>
          Effect.gen(function* () {
            const root = yield* mpf.root();
            yield* mpf.flushBlockOverlay(root);
          }),
        { discard: true, concurrency: "unbounded" },
      );
    } else {
      yield* discard;
    }
    return result.right;
  });

export type MpfProof = {
  readonly key: Buffer;
  readonly proof: Proof;
  readonly cbor: Buffer;
  readonly json: unknown;
  readonly aiken: string;
};

class MidgardMpfRootViewStore extends Store {
  public readonly retainHydratedChildren: boolean;
  private readonly level?: Level<string, MpfStoredValue>;
  private readonly memory?: Map<string, MpfStoredValue>;
  private readonly persistRootMarker: boolean;
  private readonly engine: MpfEngine;
  private readonly spillThresholdBytes: number;
  private readonly parentStore?: MidgardMpfRootViewStore;
  private readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;
  private currentRoot: Buffer;
  private batchOps: LevelBatchOp[] | undefined;
  private deferredNodePuts: Map<string, MpfSerializableValue> | undefined;
  private deferredNodeDeletes: Set<string> | undefined;
  private deferredMutationRoot: Buffer | undefined;
  private blockDeferredNodePuts: Map<string, MpfSerializableValue> | undefined;
  private blockDeferredNodeDeletes: Set<string> | undefined;
  private blockDeferredNodeEstimates: Map<string, number> | undefined;
  private blockDeferredEstimatedBytes = 0;
  private overlay: Map<string, MpfStoredValue> | undefined;
  private overlayValueBytes = new Map<string, number>();
  private readonly spillingOverlays: Map<string, MpfStoredValue>[] = [];
  private spillChain: Promise<void> = Promise.resolve();
  private spillError: unknown | undefined;
  private pendingSpillBytes = 0;
  private overlayBaseRoot: Buffer | undefined;
  private overlayBytes = 0;
  private readonly readCache = new Map<string, MpfStoredValue>();
  private blockPathCache: Map<string, MpfStoredValue> | undefined;
  private blockPathCacheBytes = 0;
  private blockPathCacheSealed = false;
  private authenticatedNodeObjects = new WeakSet<object>();
  private readonly hydratedNodeSources = new WeakMap<object, object>();
  private liveArenaEnabled = false;
  private readonly transientLiveNodes = new Set<MpfSerializableValue>();
  private readonly transientLiveNodeEstimates = new Map<
    MpfSerializableValue,
    number
  >();
  private readonly transientDirtyNodes = new Set<MpfSerializableValue>();
  private readonly midgardDirtyNodes = new Set<Trie>();
  private transientLiveBytes = 0;
  private transientSnapshotsCaptured = 0;
  private transientCurrentRootEnabled = false;
  private midgardTransientArenaTokenValue: object = {};
  private eventAtomicFinalizations = 0;
  private eventAtomicDirtyNodes = 0;
  private eventAtomicMaxDirtyNodes = 0;
  private arenaLimits: MpfArenaLimits = { ...DEFAULT_MPF_ARENA_LIMITS };
  private levelGets = 0;
  private levelGetManyCalls = 0;
  private levelGetManyMaxKeys = 0;
  private storePuts = 0;
  private storeDels = 0;
  private serialiseCalls = 0;
  private serialiseMs = 0;
  private deferredMaterializedEstimatedBytes = 0;
  private deferredMaterializedActualBytes = 0;
  private deferredLazyReads = 0;
  private deferredLazySerialiseMs = 0;
  private deferredLazySerialisedBytes = 0;
  private arenaCheckpointCalls = 0;
  private arenaCheckpointMs = 0;
  private arenaCheckpointNodes = 0;
  private arenaCheckpointBytes = 0;
  private pathCacheHits = 0;
  private liveArenaPrunedNodes = 0;
  private liveArenaPromotedNodes = 0;
  private liveArenaPromotedBytes = 0;
  private retainedSnapshotAuthentications = 0;
  private retainedSnapshotAuthenticationMs = 0;
  private overlayHits = 0;
  private readCacheHits = 0;
  private levelBatchWrites = 0;
  private bytesFlushed = 0;
  private overlaySpills = 0;
  private levelGetMs = 0;
  private jsonCodecMs = 0;
  private overlaySpillMs = 0;
  private flushMs = 0;
  private openForks = 0;
  private forkClosed = false;
  private invalidatedByChildPromotion = false;
  private promotionReserved = false;
  private poisoned = false;
  private ownsLevelLifecycle: boolean;

  constructor({
    level,
    memory,
    root,
    persistRootMarker,
    engine = "legacy",
    spillThresholdBytes = 512 * 1024 * 1024,
    parentStore,
    parentOverlay,
  }: {
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly root: Buffer;
    readonly persistRootMarker: boolean;
    readonly engine?: MpfEngine;
    readonly spillThresholdBytes?: number;
    readonly parentStore?: MidgardMpfRootViewStore;
    readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;
  }) {
    super(undefined);
    this.level = level;
    this.memory = memory;
    this.currentRoot = Buffer.from(root);
    this.persistRootMarker = persistRootMarker;
    this.engine = engine;
    this.retainHydratedChildren = engine !== "legacy";
    this.spillThresholdBytes = spillThresholdBytes;
    this.parentStore = parentStore;
    this.parentOverlay = parentOverlay;
    this.ownsLevelLifecycle = parentStore === undefined;
    this.parentStore?.registerFork();
  }

  async ready() {
    await this.level?.open();
  }

  async batch(callback: () => Promise<unknown>) {
    if (this.synchronousRetainedWrites) {
      return callback();
    }
    if (this.batchOps !== undefined) {
      throw new Error("MPF store batch already ongoing");
    }
    const rootBefore = Buffer.from(this.currentRoot);
    this.batchOps = [];
    let result: unknown;
    try {
      result = await callback();
    } catch (error) {
      this.currentRoot = rootBefore;
      this.batchOps = undefined;
      throw error;
    }
    const ops = this.batchOps;
    this.batchOps = undefined;
    try {
      if (ops.length > 0) {
        if (this.overlay !== undefined) {
          for (const op of ops) {
            if (op.key === ROOT_KEY) continue;
            if (op.type === "put") {
              this.setOverlayValue(op.key, op.value, op.encodedBytes);
            } else if (this.overlay.has(op.key)) {
              this.deleteOverlayValue(op.key);
            }
          }
          await this.spillIfNeeded();
        } else if (this.level !== undefined) {
          await this.level.batch(ops, JSON_LEVEL_ENCODING_OPTS);
          this.levelBatchWrites += 1;
        } else {
          for (const op of ops) {
            if (op.type === "put") {
              this.memory!.set(op.key, op.value);
            } else {
              this.memory!.delete(op.key);
            }
          }
        }
      }
    } catch (error) {
      this.currentRoot = rootBefore;
      throw error;
    }
    return result;
  }

  async get(key: unknown, deserialise: (...args: unknown[]) => unknown) {
    this.assertUsable();
    if (key === ROOT_KEY) {
      return deserialise(key, this.currentRoot.toString("hex"), this);
    }
    const storageKey = this.storageKey(key);
    let storedValue: MpfReadableValue | undefined;
    const activeMutationValue = this.deferredNodePuts?.get(storageKey);
    const deferredValue = this.blockDeferredNodePuts?.get(storageKey);
    if (activeMutationValue !== undefined) {
      storedValue = activeMutationValue;
      if (this.liveArenaEnabled) {
        this.assertLiveArenaNode(storageKey, activeMutationValue);
      }
    } else if (this.liveArenaEnabled && deferredValue !== undefined) {
      storedValue = deferredValue;
      this.assertLiveArenaNode(storageKey, deferredValue);
    } else if (this.overlay?.has(storageKey)) {
      this.overlayHits += 1;
      storedValue = this.overlay.get(storageKey);
    } else {
      for (
        let index = this.spillingOverlays.length - 1;
        index >= 0;
        index -= 1
      ) {
        const spilling = this.spillingOverlays[index]!;
        if (spilling.has(storageKey)) {
          this.overlayHits += 1;
          storedValue = spilling.get(storageKey);
          break;
        }
      }
    }
    if (storedValue !== undefined) {
      // The active or in-flight overlay supplied the value.
    } else if (this.parentOverlay?.has(storageKey)) {
      storedValue = this.parentOverlay.get(storageKey);
    } else if (this.parentStore !== undefined) {
      storedValue = await this.parentStore.lookupStoredValue(storageKey);
    } else if (this.blockPathCache?.has(storageKey)) {
      this.pathCacheHits += 1;
      storedValue = this.blockPathCache.get(storageKey);
    } else if (this.readCache.has(storageKey)) {
      this.readCacheHits += 1;
      storedValue = this.readCache.get(storageKey);
    } else if (this.liveArenaEnabled && this.blockPathCacheSealed) {
      const durableValue =
        this.level === undefined
          ? this.memory?.get(storageKey)
          : await this.level.get(storageKey, JSON_LEVEL_ENCODING_OPTS);
      throw new Error(
        `Sealed MPF block path cache missed node ${storageKey};durable_source=${durableValue === undefined ? "absent" : "present"}`,
      );
    } else if (this.level === undefined) {
      storedValue = this.memory!.get(storageKey);
    } else {
      const startedAt = performance.now();
      storedValue = await this.level.get(storageKey, JSON_LEVEL_ENCODING_OPTS);
      this.levelGetMs += performance.now() - startedAt;
      this.levelGets += 1;
      if (storedValue !== undefined) {
        this.cacheRead(storageKey, storedValue);
        this.cacheBlockPathNode(storageKey, storedValue);
      }
    }
    const value = applyPendingBatch(storageKey, storedValue, this.batchOps);
    const decoded = await deserialise(key, value, this);
    this.rememberHydratedNodeSource(decoded, value);
    return decoded;
  }

  async getMany(
    keys: readonly unknown[],
    deserialise: (...args: unknown[]) => unknown,
  ) {
    this.assertUsable();
    const storageKeys = keys.map((key) => this.storageKey(key));
    const values = new Array<MpfReadableValue | undefined>(keys.length);
    const unresolvedIndexes: number[] = [];
    const unresolvedStorageKeys: string[] = [];

    for (let index = 0; index < storageKeys.length; index += 1) {
      const storageKey = storageKeys[index]!;
      let storedValue: MpfReadableValue | undefined;
      let resolved = false;
      const activeMutationValue = this.deferredNodePuts?.get(storageKey);
      const deferredValue = this.blockDeferredNodePuts?.get(storageKey);
      if (storageKey === ROOT_KEY) {
        storedValue = this.currentRoot.toString("hex");
        resolved = true;
      } else if (activeMutationValue !== undefined) {
        storedValue = activeMutationValue;
        if (this.liveArenaEnabled) {
          this.assertLiveArenaNode(storageKey, activeMutationValue);
        }
        resolved = true;
      } else if (this.liveArenaEnabled && deferredValue !== undefined) {
        storedValue = deferredValue;
        this.assertLiveArenaNode(storageKey, deferredValue);
        resolved = true;
      } else if (this.overlay?.has(storageKey)) {
        this.overlayHits += 1;
        storedValue = this.overlay.get(storageKey);
        resolved = true;
      } else {
        for (
          let spillIndex = this.spillingOverlays.length - 1;
          spillIndex >= 0;
          spillIndex -= 1
        ) {
          const spilling = this.spillingOverlays[spillIndex]!;
          if (spilling.has(storageKey)) {
            this.overlayHits += 1;
            storedValue = spilling.get(storageKey);
            resolved = true;
            break;
          }
        }
      }
      if (!resolved && this.parentOverlay?.has(storageKey)) {
        storedValue = this.parentOverlay.get(storageKey);
        resolved = true;
      } else if (!resolved && this.parentStore !== undefined) {
        storedValue = await this.parentStore.lookupStoredValue(storageKey);
        resolved = true;
      } else if (!resolved && this.blockPathCache?.has(storageKey)) {
        this.pathCacheHits += 1;
        storedValue = this.blockPathCache.get(storageKey);
        resolved = true;
      } else if (!resolved && this.readCache.has(storageKey)) {
        this.readCacheHits += 1;
        storedValue = this.readCache.get(storageKey);
        resolved = true;
      } else if (!resolved && this.level === undefined) {
        storedValue = this.memory!.get(storageKey);
        resolved = true;
      }
      if (resolved) {
        values[index] = storedValue;
      } else {
        unresolvedIndexes.push(index);
        unresolvedStorageKeys.push(storageKey);
      }
    }

    if (unresolvedStorageKeys.length > 0) {
      if (this.liveArenaEnabled && this.blockPathCacheSealed) {
        const durableValues =
          this.level === undefined
            ? unresolvedStorageKeys.map((key) => this.memory?.get(key))
            : await this.level.getMany(
                unresolvedStorageKeys,
                JSON_LEVEL_ENCODING_OPTS,
              );
        throw new Error(
          `Sealed MPF block path cache missed ${unresolvedStorageKeys.length.toString()} nodes;durable_present=${durableValues.filter((value) => value !== undefined).length.toString()};keys=${unresolvedStorageKeys.join(",")}`,
        );
      }
      const startedAt = performance.now();
      const loaded = await this.level!.getMany(
        unresolvedStorageKeys,
        JSON_LEVEL_ENCODING_OPTS,
      );
      this.levelGetMs += performance.now() - startedAt;
      this.levelGets += unresolvedStorageKeys.length;
      this.levelGetManyCalls += 1;
      this.levelGetManyMaxKeys = Math.max(
        this.levelGetManyMaxKeys,
        unresolvedStorageKeys.length,
      );
      for (let offset = 0; offset < unresolvedIndexes.length; offset += 1) {
        const index = unresolvedIndexes[offset]!;
        const storedValue = loaded[offset];
        values[index] = storedValue;
        if (storedValue !== undefined) {
          this.cacheRead(storageKeys[index]!, storedValue);
          this.cacheBlockPathNode(storageKeys[index]!, storedValue);
        }
      }
    }

    const decoded = await Promise.all(
      values.map((storedValue, index) =>
        deserialise(
          keys[index],
          applyPendingBatch(storageKeys[index]!, storedValue, this.batchOps),
          this,
        ),
      ),
    );
    for (let index = 0; index < decoded.length; index += 1) {
      this.rememberHydratedNodeSource(decoded[index], values[index]);
    }
    return decoded;
  }

  async put(key: unknown, value: MpfSerializableValue) {
    this.assertUsable();
    this.storePuts += 1;
    const storageKey = this.storageKey(key);
    if (
      storageKey !== ROOT_KEY &&
      this.deferredNodePuts !== undefined &&
      this.deferredNodeDeletes !== undefined
    ) {
      this.deferredNodePuts.set(storageKey, value);
      this.deferredNodeDeletes.delete(storageKey);
      return;
    }
    const serialiseStartedAt = performance.now();
    const rawSerialized = value.serialise();
    this.serialiseMs += performance.now() - serialiseStartedAt;
    this.serialiseCalls += 1;
    const serialized =
      storageKey === ROOT_KEY && typeof rawSerialized === "string"
        ? normalizeStoredRootHex(rawSerialized)
        : rawSerialized;
    let encodedBytes: number | undefined;
    if (this.level !== undefined) {
      const jsonCodecStartedAt = performance.now();
      encodedBytes = Buffer.byteLength(JSON.stringify(serialized));
      this.jsonCodecMs += performance.now() - jsonCodecStartedAt;
    }
    if (storageKey === ROOT_KEY) {
      this.currentRoot = Buffer.from(serialized as string, "hex");
      if (!this.persistRootMarker || this.overlay !== undefined) {
        return;
      }
    }
    const op: LevelBatchOp = {
      type: "put",
      key: storageKey,
      value: serialized,
      encodedBytes,
    };
    if (this.batchOps !== undefined) {
      this.batchOps.push(op);
    } else if (this.overlay !== undefined) {
      this.setOverlayValue(op.key, op.value, op.encodedBytes);
      await this.spillIfNeeded();
    } else if (this.level !== undefined) {
      await this.level.put(op.key, op.value, JSON_LEVEL_ENCODING_OPTS);
    } else {
      this.memory!.set(op.key, op.value);
    }
  }

  get synchronousRetainedWrites() {
    return this.retainHydratedChildren && this.deferredNodePuts !== undefined;
  }

  get midgardTransientArenaToken(): object | undefined {
    return this.transientCurrentRootEnabled
      ? this.midgardTransientArenaTokenValue
      : undefined;
  }

  get deferMidgardBranchHashes(): boolean {
    return (
      this.transientCurrentRootEnabled && this.deferredNodePuts !== undefined
    );
  }

  recordMidgardDirtyNode(node: Trie): void {
    if (!this.deferMidgardBranchHashes || node.store !== this) {
      throw new Error("Cannot retain a foreign MPF event mutation node");
    }
    this.midgardDirtyNodes.add(node);
  }

  takeMidgardDirtyNodes(): readonly Trie[] {
    if (!this.deferMidgardBranchHashes) {
      throw new Error("Cannot finalize an inactive MPF event mutation");
    }
    const dirtyNodes = [...this.midgardDirtyNodes];
    this.midgardDirtyNodes.clear();
    this.eventAtomicFinalizations += 1;
    this.eventAtomicDirtyNodes += dirtyNodes.length;
    this.eventAtomicMaxDirtyNodes = Math.max(
      this.eventAtomicMaxDirtyNodes,
      dirtyNodes.length,
    );
    return dirtyNodes;
  }

  putRetainedNode(key: unknown, value: MpfSerializableValue) {
    this.assertUsable();
    if (
      this.deferredNodePuts === undefined ||
      this.deferredNodeDeletes === undefined
    ) {
      throw new Error("Synchronous retained MPF write outside a mutation");
    }
    this.storePuts += 1;
    const storageKey = this.storageKey(key);
    const proofCandidate = value as MpfSerializableValue & {
      readonly consumeMidgardMutationProof?: () => boolean;
    };
    const trustedMutation =
      this.liveArenaEnabled &&
      this.transientCurrentRootEnabled &&
      value instanceof Trie &&
      proofCandidate.consumeMidgardMutationProof === consumeMpfMutationProof &&
      consumeMpfMutationProof.call(value);
    if (trustedMutation) {
      const previousEstimate = this.transientLiveNodeEstimates.get(value) ?? 0;
      const estimate = this.estimateDeferredNodeBytes(storageKey, value);
      this.transientLiveNodes.add(value);
      this.transientLiveNodeEstimates.set(value, estimate);
      this.transientLiveBytes += estimate - previousEstimate;
      this.transientDirtyNodes.add(value);
      this.deferredNodeDeletes.delete(storageKey);
      this.assertLiveArenaCaps();
      return;
    }
    const retainedValue = this.liveArenaEnabled
      ? (
          value as MpfSerializableValue & {
            readonly cloneDetached?: () => MpfSerializableValue;
          }
        ).cloneDetached?.()
      : value;
    if (
      retainedValue === undefined ||
      (this.liveArenaEnabled && retainedValue === value)
    ) {
      throw new Error(
        "Live MPF arena requires an immutable detached node snapshot",
      );
    }
    if (this.liveArenaEnabled) {
      // A content key may already have been authenticated for a different
      // object. Verify every new detached snapshot before it can replace that
      // key; raw cache clones remain memoized by their immutable source object.
      const authenticationStartedAt = performance.now();
      this.authenticateHydratedNodeOnce(retainedValue);
      this.retainedSnapshotAuthenticationMs +=
        performance.now() - authenticationStartedAt;
      this.retainedSnapshotAuthentications += 1;
    }
    this.deferredNodePuts.set(storageKey, retainedValue);
    this.deferredNodeDeletes.delete(storageKey);
  }

  deleteRetainedNode(key: unknown) {
    this.assertUsable();
    if (
      this.deferredNodePuts === undefined ||
      this.deferredNodeDeletes === undefined
    ) {
      throw new Error("Synchronous retained MPF delete outside a mutation");
    }
    this.storeDels += 1;
    if (this.liveArenaEnabled) {
      // Content hashes can be shared by multiple parents. Retain immutable
      // snapshots until the final current-root mark/sweep proves them orphaned.
      return;
    }
    const storageKey = this.storageKey(key);
    this.deferredNodePuts.delete(storageKey);
    this.deferredNodeDeletes.add(storageKey);
  }

  putRetainedRoot(root: Buffer | null | undefined) {
    this.assertUsable();
    this.storePuts += 1;
    const candidate = Buffer.from(root ?? MPF_EMPTY_ROOT);
    this.currentRoot = candidate.equals(MPF_INTERNAL_NULL_ROOT)
      ? Buffer.from(MPF_EMPTY_ROOT)
      : candidate;
  }

  async del(key: unknown) {
    this.assertUsable();
    this.storeDels += 1;
    const storageKey = this.storageKey(key);
    if (
      storageKey !== ROOT_KEY &&
      this.deferredNodePuts !== undefined &&
      this.deferredNodeDeletes !== undefined
    ) {
      if (this.liveArenaEnabled) return;
      this.deferredNodePuts.delete(storageKey);
      this.deferredNodeDeletes.add(storageKey);
      return;
    }
    if (this.overlay !== undefined && storageKey !== ROOT_KEY) {
      if (this.batchOps !== undefined) {
        this.batchOps.push({ type: "del", key: storageKey });
      } else if (this.overlay.has(storageKey)) {
        this.deleteOverlayValue(storageKey);
      }
      return;
    }
    if (storageKey !== ROOT_KEY || !this.persistRootMarker) {
      return;
    }
    const op: LevelBatchOp = { type: "del", key: storageKey };
    if (this.batchOps !== undefined) {
      this.batchOps.push(op);
    } else if (this.level !== undefined) {
      await this.level.del(op.key);
    } else {
      this.memory!.delete(op.key);
    }
  }

  async size() {
    if (this.level !== undefined) {
      return this.level
        .keys()
        .all()
        .then((keys) => keys.length);
    }
    return this.memory!.size;
  }

  root() {
    this.assertUsable();
    return Buffer.from(this.currentRoot);
  }

  setRoot(root: Buffer) {
    this.currentRoot = Buffer.from(root);
  }

  beginOverlay() {
    this.assertUsable();
    if (this.engine === "legacy") {
      throw new Error("Cannot begin an MPF overlay when MPF_ENGINE=legacy");
    }
    if (this.overlay !== undefined) {
      throw new Error("MPF block overlay already active");
    }
    this.overlay = new Map();
    this.overlayValueBytes = new Map();
    this.blockDeferredNodePuts = new Map();
    this.blockDeferredNodeDeletes = new Set();
    this.blockDeferredNodeEstimates = new Map();
    this.blockDeferredEstimatedBytes = 0;
    this.overlayBaseRoot = Buffer.from(this.currentRoot);
    this.overlayBytes = 0;
    this.blockPathCache = undefined;
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.liveArenaEnabled = false;
    this.transientCurrentRootEnabled = false;
    this.clearTransientLiveArena();
    this.transientSnapshotsCaptured = 0;
    this.eventAtomicFinalizations = 0;
    this.eventAtomicDirtyNodes = 0;
    this.eventAtomicMaxDirtyNodes = 0;
    this.arenaLimits = { ...getMpfArenaLimits() };
    this.authenticatedNodeObjects = new WeakSet<object>();
  }

  enableBlockPathArena(transientCurrentRoot = false) {
    this.assertUsable();
    if (this.overlay === undefined) {
      throw new Error("Cannot enable an MPF path arena without an overlay");
    }
    if (this.liveArenaEnabled) {
      throw new Error("MPF block path arena is already active");
    }
    this.liveArenaEnabled = true;
    this.transientCurrentRootEnabled = transientCurrentRoot;
    this.midgardTransientArenaTokenValue = {};
    this.clearTransientLiveArena();
    this.blockPathCache = new Map();
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.arenaLimits = { ...getMpfArenaLimits() };
    for (const [key, value] of this.readCache) {
      this.cacheBlockPathNode(key, value);
    }
  }

  sealBlockPathCache() {
    if (!this.liveArenaEnabled || this.blockPathCache === undefined) {
      throw new Error("Cannot seal an inactive MPF block path cache");
    }
    this.blockPathCacheSealed = true;
  }

  assertEventFlatArenaCaps(nodeCount: number, estimatedBytes: number) {
    this.assertUsable();
    if (
      nodeCount > this.arenaLimits.liveArenaMaxNodes ||
      estimatedBytes > this.arenaLimits.liveArenaMaxBytes
    ) {
      throw new Error(
        `Event-flat arena limit exceeded: nodes=${nodeCount.toString()}/${this.arenaLimits.liveArenaMaxNodes.toString()},bytes=${estimatedBytes.toString()}/${this.arenaLimits.liveArenaMaxBytes.toString()}`,
      );
    }
  }

  assertEventFlatRawCaps(nodeCount: number, estimatedBytes: number) {
    this.assertUsable();
    if (
      nodeCount > this.arenaLimits.pathCacheMaxNodes ||
      estimatedBytes > this.arenaLimits.pathCacheMaxBytes
    ) {
      throw new Error(
        `Event-flat raw path limit exceeded: nodes=${nodeCount.toString()}/${this.arenaLimits.pathCacheMaxNodes.toString()},bytes=${estimatedBytes.toString()}/${this.arenaLimits.pathCacheMaxBytes.toString()}`,
      );
    }
  }

  async loadEventFlatRawRecords(
    hashes: readonly Buffer[],
  ): Promise<readonly (PackedMpfStoredValue | undefined)[]> {
    this.assertUsable();
    const values = (await this.getMany(hashes, (_hash, value) =>
      value === undefined
        ? undefined
        : typeof value === "object" &&
            value !== null &&
            "serialise" in value &&
            typeof value.serialise === "function"
          ? value.serialise()
          : value,
    )) as readonly unknown[];
    return values.map((value) =>
      typeof value === "object" &&
      value !== null &&
      "__kind" in value &&
      (value.__kind === "Leaf" || value.__kind === "Branch")
        ? (value as PackedMpfStoredValue)
        : undefined,
    );
  }

  handoffRawPathsToEventFlat() {
    this.assertUsable();
    if (this.overlay === undefined || this.liveArenaEnabled) {
      throw new Error("Cannot hand off an inactive or mutable raw MPF view");
    }
    if (
      this.deferredNodePuts !== undefined ||
      this.deferredNodeDeletes !== undefined ||
      this.midgardDirtyNodes.size !== 0 ||
      this.transientDirtyNodes.size !== 0 ||
      this.transientLiveNodes.size !== 0 ||
      (this.blockDeferredNodePuts?.size ?? 0) !== 0 ||
      (this.blockDeferredNodeDeletes?.size ?? 0) !== 0
    ) {
      throw new Error("Cannot hand off dirty raw MPF paths to event-flat");
    }
    this.readCache.clear();
  }

  private rememberHydratedNodeSource(
    decoded: unknown,
    source: MpfReadableValue | undefined,
  ) {
    if (
      typeof decoded === "object" &&
      decoded !== null &&
      typeof source === "object" &&
      source !== null
    ) {
      this.hydratedNodeSources.set(decoded, source);
    }
  }

  authenticateHydratedNodeOnce(value: MpfSerializableValue) {
    const hash = (value as MpfSerializableValue & { readonly hash?: Buffer })
      .hash;
    if (hash === undefined) {
      throw new Error("Hydrated MPF node has no content hash");
    }
    const identity =
      this.hydratedNodeSources.get(value as object) ?? (value as object);
    if (this.authenticatedNodeObjects.has(identity)) return;
    const authenticate = (
      value as MpfSerializableValue & {
        readonly assertHydratedNodeHashes?: (maxDepth: number) => unknown;
      }
    ).assertHydratedNodeHashes;
    if (authenticate === undefined) {
      throw new Error("Hydrated MPF node cannot authenticate its hash");
    }
    authenticate.call(value, 0);
    this.authenticatedNodeObjects.add(identity);
  }

  authenticateDirtyLiveNodes(root?: MpfSerializableValue): {
    readonly verifiedNodes: number;
    readonly authenticationMs: number;
  } {
    if (
      !this.liveArenaEnabled ||
      !this.transientCurrentRootEnabled ||
      this.transientDirtyNodes.size === 0
    ) {
      return { verifiedNodes: 0, authenticationMs: 0 };
    }
    const startedAt = performance.now();
    const reachableNodes =
      root === undefined ? undefined : new Set<MpfSerializableValue>();
    const dirtyNodes: MpfSerializableValue[] = [];
    if (root === undefined) {
      dirtyNodes.push(...this.transientDirtyNodes);
    } else {
      const pending = [root];
      while (pending.length > 0) {
        const node = pending.pop()!;
        if (reachableNodes!.has(node)) continue;
        reachableNodes!.add(node);
        if (this.transientDirtyNodes.has(node)) dirtyNodes.push(node);
        const children = (
          node as MpfSerializableValue & {
            readonly children?: readonly unknown[];
          }
        ).children;
        for (const child of children ?? []) {
          if (
            typeof child === "object" &&
            child !== null &&
            "serialise" in child
          ) {
            pending.push(child as MpfSerializableValue);
          }
        }
      }
    }
    let verifiedNodes = 0;
    for (const node of dirtyNodes) {
      const authenticate = (
        node as MpfSerializableValue & {
          readonly assertHydratedNodeHashes?: (maxDepth: number) => unknown;
        }
      ).assertHydratedNodeHashes;
      if (authenticate === undefined) {
        throw new Error("Transient MPF node cannot authenticate its hash");
      }
      authenticate.call(node, 0);
      verifiedNodes += 1;
    }
    if (reachableNodes !== undefined) {
      for (const node of this.transientLiveNodes) {
        if (reachableNodes.has(node)) continue;
        this.transientLiveNodes.delete(node);
        this.transientLiveBytes -=
          this.transientLiveNodeEstimates.get(node) ?? 0;
        this.transientLiveNodeEstimates.delete(node);
        this.liveArenaPrunedNodes += 1;
      }
    }
    this.transientDirtyNodes.clear();
    const authenticationMs = performance.now() - startedAt;
    this.retainedSnapshotAuthentications += verifiedNodes;
    this.retainedSnapshotAuthenticationMs += authenticationMs;
    return { verifiedNodes, authenticationMs };
  }

  captureCurrentLiveTrie(root: MpfSerializableValue): void {
    try {
      this.captureCurrentLiveTrieUnchecked(root);
    } catch (cause) {
      try {
        this.abortDeferredMutation();
        const baseRoot = this.discardOverlay();
        this.currentRoot = Buffer.from(baseRoot);
        this.poisoned = true;
      } catch {
        // Preserve the authentication/capture failure. The durable marker is
        // unchanged and recovery reopens it in a fresh store.
      }
      throw cause;
    }
  }

  private captureCurrentLiveTrieUnchecked(root: MpfSerializableValue): void {
    if (
      !this.liveArenaEnabled ||
      !this.transientCurrentRootEnabled ||
      this.transientLiveNodes.size === 0
    ) {
      return;
    }
    if (
      this.blockDeferredNodePuts === undefined ||
      this.blockDeferredNodeEstimates === undefined
    ) {
      throw new Error("Cannot capture an inactive transient MPF arena");
    }
    const pending = [root];
    const visited = new Set<object>();
    while (pending.length > 0) {
      const node = pending.pop()!;
      if (visited.has(node as object)) continue;
      visited.add(node as object);
      const children = (
        node as MpfSerializableValue & {
          readonly children?: readonly (
            | MpfSerializableValue
            | { readonly hash?: Buffer }
            | undefined
          )[];
        }
      ).children;
      for (const child of children ?? []) {
        if (
          typeof child === "object" &&
          child !== null &&
          "serialise" in child
        ) {
          pending.push(child as MpfSerializableValue);
        }
      }
      if (!this.transientLiveNodes.has(node)) continue;
      const hash = (node as MpfSerializableValue & { readonly hash?: Buffer })
        .hash;
      const clone = (
        node as MpfSerializableValue & {
          readonly cloneDetached?: () => MpfSerializableValue;
        }
      ).cloneDetached?.();
      if (hash === undefined || clone === undefined || clone === node) {
        throw new Error("Cannot capture a mutable transient MPF node");
      }
      const key = this.storageKey(hash);
      this.assertLiveArenaNode(key, clone);
      const priorEstimate = this.blockDeferredNodeEstimates.get(key) ?? 0;
      const estimate = this.estimateDeferredNodeBytes(key, clone);
      this.blockDeferredNodePuts.set(key, clone);
      this.blockDeferredNodeEstimates.set(key, estimate);
      this.blockDeferredEstimatedBytes += estimate - priorEstimate;
      this.transientSnapshotsCaptured += 1;
    }
    this.clearTransientLiveArena();
    this.assertLiveArenaCaps();
  }

  async parkCurrentOverlay(
    root: Buffer,
    trieName: string,
  ): Promise<ParkedMpfOverlayV1> {
    this.assertUsable();
    if (
      this.overlay === undefined ||
      this.overlayBaseRoot === undefined ||
      !root.equals(this.currentRoot)
    ) {
      throw new Error("Cannot park an inactive or mismatched MPF overlay");
    }
    await this.waitForSpills();
    const writesBefore = this.levelBatchWrites;
    if (this.parentStore !== undefined) {
      await this.waitForAncestorSpills();
      this.importReachableParentCandidates(root);
    }
    this.pruneLiveArenaToRoot(root);
    const parkedNodes = new Map<string, MpfStoredValue>();
    const visited = new Set<string>();
    const pending = root.equals(MPF_EMPTY_ROOT) ? [] : [this.storageKey(root)];
    while (pending.length > 0) {
      const key = pending.pop()!;
      if (visited.has(key)) continue;
      visited.add(key);
      const live = this.blockDeferredNodePuts?.get(key);
      const serialized = this.overlay.get(key);
      const candidate = live ?? serialized;
      if (candidate === undefined) {
        // A missing local candidate is an unchanged durable subtree. A new
        // descendant cannot be reachable through an unchanged content hash.
        continue;
      }
      if (live !== undefined) this.assertLiveArenaNode(key, live);
      parkedNodes.set(key, live === undefined ? serialized! : live.serialise());
      for (const childKey of this.candidateChildKeys(candidate)) {
        pending.push(childKey);
      }
    }
    const ordered = [...parkedNodes].sort(([left], [right]) =>
      left.localeCompare(right),
    );
    const nodeHashes = new Uint8Array(ordered.length * 32);
    const encodedValues = ordered.map(([, value]) =>
      Buffer.from(JSON.stringify(value)),
    );
    const nodeValueOffsets = new Uint32Array(ordered.length * 2);
    const nodeValues = new Uint8Array(
      encodedValues.reduce((total, value) => total + value.length, 0),
    );
    let valueOffset = 0;
    for (const [index, [key]] of ordered.entries()) {
      const hash = Buffer.from(key, "hex");
      if (hash.length !== 32) {
        throw new Error(`Cannot park invalid MPF content key ${key}`);
      }
      nodeHashes.set(hash, index * 32);
      const encoded = encodedValues[index]!;
      nodeValues.set(encoded, valueOffset);
      nodeValueOffsets.set([valueOffset, encoded.length], index * 2);
      valueOffset += encoded.length;
    }
    const baseRoot = exactArrayBuffer(this.overlayBaseRoot);
    const candidateRoot = exactArrayBuffer(root);
    const hashesBuffer = exactArrayBuffer(nodeHashes);
    const valuesBuffer = exactArrayBuffer(nodeValues);
    const offsetsBuffer = exactArrayBuffer(
      new Uint8Array(nodeValueOffsets.buffer),
    );
    const digest = parkedOverlayDigest({
      trieName,
      baseRoot,
      candidateRoot,
      nodeCount: ordered.length,
      nodeHashes: hashesBuffer,
      nodeValues: valuesBuffer,
      nodeValueOffsets: offsetsBuffer,
    });
    if (this.levelBatchWrites !== writesBefore) {
      throw new Error("Parking an MPF overlay performed a Level write");
    }
    const artifact: ParkedMpfOverlayV1 = {
      schemaVersion: 1,
      trieName,
      baseRoot,
      candidateRoot,
      closureDigest: exactArrayBuffer(digest),
      nodeCount: ordered.length,
      nodeHashes: hashesBuffer,
      nodeValues: valuesBuffer,
      nodeValueOffsets: offsetsBuffer,
      encodedBytes:
        hashesBuffer.byteLength +
        valuesBuffer.byteLength +
        offsetsBuffer.byteLength,
    };
    this.discardOverlay();
    return artifact;
  }

  async importParkedOverlay(artifact: ParkedMpfOverlayV1): Promise<Buffer> {
    this.assertUsable();
    if (this.overlay === undefined || this.overlayBaseRoot === undefined) {
      throw new Error("Cannot import a parked MPF without an active overlay");
    }
    const baseRoot = Buffer.from(artifact.baseRoot);
    if (!baseRoot.equals(this.overlayBaseRoot)) {
      throw new Error(
        `Parked MPF base mismatch: durable=${this.overlayBaseRoot.toString("hex")},parked=${baseRoot.toString("hex")}`,
      );
    }
    if (
      artifact.schemaVersion !== 1 ||
      !Number.isSafeInteger(artifact.nodeCount) ||
      artifact.nodeCount < 0 ||
      artifact.baseRoot.byteLength !== 32 ||
      artifact.candidateRoot.byteLength !== 32 ||
      artifact.closureDigest.byteLength !== 32 ||
      artifact.nodeHashes.byteLength !== artifact.nodeCount * 32 ||
      artifact.nodeValueOffsets.byteLength !== artifact.nodeCount * 8 ||
      artifact.encodedBytes !==
        artifact.nodeHashes.byteLength +
          artifact.nodeValues.byteLength +
          artifact.nodeValueOffsets.byteLength
    ) {
      throw new Error("Invalid parked MPF artifact shape");
    }
    const expectedDigest = parkedOverlayDigest({
      trieName: artifact.trieName,
      baseRoot: artifact.baseRoot,
      candidateRoot: artifact.candidateRoot,
      nodeCount: artifact.nodeCount,
      nodeHashes: artifact.nodeHashes,
      nodeValues: artifact.nodeValues,
      nodeValueOffsets: artifact.nodeValueOffsets,
    });
    if (!expectedDigest.equals(Buffer.from(artifact.closureDigest))) {
      throw new Error("Parked MPF closure digest mismatch");
    }
    const hashes = new Uint8Array(artifact.nodeHashes);
    const values = new Uint8Array(artifact.nodeValues);
    const offsets = new Uint32Array(artifact.nodeValueOffsets);
    const artifactKeys = new Set<string>();
    const deserialise = (
      Trie as unknown as {
        readonly deserialise: (
          hash: Buffer,
          value: MpfStoredValue,
          store: MidgardMpfRootViewStore,
        ) => Promise<MpfSerializableValue>;
      }
    ).deserialise;
    for (let index = 0; index < artifact.nodeCount; index += 1) {
      const hash = Buffer.from(hashes.subarray(index * 32, (index + 1) * 32));
      const key = hash.toString("hex");
      if (artifactKeys.has(key)) {
        throw new Error(`Parked MPF contains duplicate node ${key}`);
      }
      artifactKeys.add(key);
      const valueOffset = offsets[index * 2]!;
      const valueLength = offsets[index * 2 + 1]!;
      if (valueOffset + valueLength > values.length) {
        throw new Error("Parked MPF node value exceeds its packed arena");
      }
      const value = JSON.parse(
        Buffer.from(
          values.subarray(valueOffset, valueOffset + valueLength),
        ).toString(),
      ) as MpfStoredValue;
      const decoded = await deserialise(hash, value, this);
      this.assertLiveArenaNode(key, decoded);
      this.setOverlayValue(key, value);
    }
    const candidateRoot = Buffer.from(artifact.candidateRoot);
    this.currentRoot = Buffer.from(candidateRoot);
    const reachableArtifactKeys = new Set<string>();
    const visited = new Set<string>();
    const pending = candidateRoot.equals(MPF_EMPTY_ROOT)
      ? []
      : [this.storageKey(candidateRoot)];
    while (pending.length > 0) {
      const key = pending.pop()!;
      if (visited.has(key)) continue;
      visited.add(key);
      const local = this.overlay.get(key);
      if (local !== undefined) {
        reachableArtifactKeys.add(key);
        for (const childKey of this.candidateChildKeys(local)) {
          pending.push(childKey);
        }
        continue;
      }
      if ((await this.lookupStoredValue(key)) === undefined) {
        throw new Error(`Parked MPF closure is missing node ${key}`);
      }
    }
    if (reachableArtifactKeys.size !== artifactKeys.size) {
      throw new Error(
        `Parked MPF contains unreachable nodes: reachable=${reachableArtifactKeys.size.toString()},packed=${artifactKeys.size.toString()}`,
      );
    }
    return candidateRoot;
  }

  private clearTransientLiveArena() {
    this.transientLiveNodes.clear();
    this.transientLiveNodeEstimates.clear();
    this.transientDirtyNodes.clear();
    this.transientLiveBytes = 0;
  }

  beginDeferredMutation() {
    if (!this.retainHydratedChildren || this.overlay === undefined) {
      return false;
    }
    if (this.deferredNodePuts !== undefined) {
      throw new Error("MPF deferred mutation already active");
    }
    if (this.openForks > 0) {
      throw new Error("Cannot mutate an MPF overlay while a fork is active");
    }
    this.deferredNodePuts = new Map();
    this.deferredNodeDeletes = new Set();
    this.deferredMutationRoot = Buffer.from(this.currentRoot);
    this.midgardDirtyNodes.clear();
    return true;
  }

  async commitDeferredMutation() {
    if (
      this.deferredNodePuts === undefined ||
      this.deferredNodeDeletes === undefined
    ) {
      throw new Error("MPF deferred mutation is not active");
    }
    if (
      this.blockDeferredNodePuts === undefined ||
      this.blockDeferredNodeDeletes === undefined ||
      this.blockDeferredNodeEstimates === undefined
    ) {
      throw new Error("MPF block deferred mutation is not active");
    }
    if (!this.liveArenaEnabled) {
      for (const key of this.deferredNodeDeletes) {
        this.blockDeferredNodePuts.delete(key);
        this.blockDeferredNodeDeletes.add(key);
        this.blockDeferredEstimatedBytes -=
          this.blockDeferredNodeEstimates.get(key) ?? 0;
        this.blockDeferredNodeEstimates.delete(key);
      }
    }
    for (const [key, value] of this.deferredNodePuts) {
      this.blockDeferredEstimatedBytes -=
        this.blockDeferredNodeEstimates.get(key) ?? 0;
      const estimate = this.estimateDeferredNodeBytes(key, value);
      this.blockDeferredNodePuts.set(key, value);
      this.blockDeferredNodeEstimates.set(key, estimate);
      this.blockDeferredEstimatedBytes += estimate;
      this.blockDeferredNodeDeletes.delete(key);
    }
    this.deferredNodePuts = undefined;
    this.deferredNodeDeletes = undefined;
    this.deferredMutationRoot = undefined;
    this.midgardDirtyNodes.clear();
    await this.spillIfNeeded();
  }

  abortDeferredMutation() {
    if (this.deferredNodePuts === undefined) return;
    if (this.deferredMutationRoot !== undefined) {
      this.currentRoot = Buffer.from(this.deferredMutationRoot);
    }
    this.deferredNodePuts = undefined;
    this.deferredNodeDeletes = undefined;
    this.deferredMutationRoot = undefined;
    this.midgardDirtyNodes.clear();
  }

  overlayIsActive() {
    return this.overlay !== undefined;
  }

  overlayStartingRoot() {
    return this.overlayBaseRoot === undefined
      ? undefined
      : Buffer.from(this.overlayBaseRoot);
  }

  stageEventFlatCandidate(
    root: Buffer,
    records: ReadonlyMap<string, PackedMpfStoredValue>,
  ) {
    this.assertUsable();
    if (this.overlay === undefined || this.overlayBaseRoot === undefined) {
      throw new Error(
        "Cannot stage an event-flat candidate without an overlay",
      );
    }
    if (this.openForks > 0) {
      throw new Error("Cannot stage an event-flat candidate with open forks");
    }
    this.abortDeferredMutation();
    this.overlay = new Map();
    this.overlayValueBytes = new Map();
    this.overlayBytes = 0;
    this.blockDeferredNodePuts?.clear();
    this.blockDeferredNodeDeletes?.clear();
    this.blockDeferredNodeEstimates?.clear();
    this.blockDeferredEstimatedBytes = 0;
    for (const [key, value] of records) this.setOverlayValue(key, value);
    this.currentRoot = Buffer.from(root);
    this.blockPathCache = undefined;
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.liveArenaEnabled = false;
    this.transientCurrentRootEnabled = false;
    this.clearTransientLiveArena();
  }

  async resetOverlayRoot(root: Buffer) {
    this.assertUsable();
    if (this.overlay === undefined || this.overlayBaseRoot === undefined) {
      throw new Error("Cannot reset an inactive MPF block overlay");
    }
    if (this.openForks > 0) {
      throw new Error("Cannot reset an MPF overlay while a fork is active");
    }
    if (this.deferredNodePuts !== undefined) {
      throw new Error("Cannot reset an MPF overlay during a mutation");
    }
    // A requested root may have been created earlier in this overlay and still
    // be held as a deferred live node. Keep the overlay contents as harmless
    // content-addressed candidates so that root remains readable; only the
    // virtual working root changes. The original overlayBaseRoot continues to
    // define discard/rollback and no durable marker is written here.
    const candidate = Buffer.from(root);
    if (
      this.liveArenaEnabled &&
      !candidate.equals(MPF_EMPTY_ROOT) &&
      (await this.lookupStoredValue(this.storageKey(candidate))) === undefined
    ) {
      throw new Error(
        `Cannot reset live MPF arena to unreadable root ${candidate.toString("hex")}`,
      );
    }
    if (!this.liveArenaEnabled) {
      await this.materializeDeferredNodes();
    }
    this.currentRoot = candidate.equals(MPF_INTERNAL_NULL_ROOT)
      ? Buffer.from(MPF_EMPTY_ROOT)
      : candidate;
    return Buffer.from(this.currentRoot);
  }

  async flushOverlay(root: Buffer) {
    this.assertUsable();
    if (this.overlay === undefined) {
      throw new Error("Cannot flush an inactive MPF block overlay");
    }
    if (this.openForks > 0) {
      throw new Error("Cannot promote an MPF overlay while a fork is active");
    }
    if (!root.equals(this.currentRoot)) {
      throw new Error(
        `Refusing to promote mismatched MPF root: requested=${root.toString("hex")},current=${this.currentRoot.toString("hex")}`,
      );
    }
    const promotionReservations = this.reservePromotionChain();
    try {
      if (this.parentStore !== undefined) {
        await this.waitForAncestorSpills();
        this.importReachableParentCandidates(root);
      }
      if (this.liveArenaEnabled) {
        this.pruneLiveArenaToRoot(root);
      }
      await this.materializeDeferredNodes();
      await this.waitForSpills();
      const ops: LevelBatchOp[] = [...this.overlay].map(([key, value]) => ({
        type: "put" as const,
        key,
        value,
        encodedBytes: this.overlayValueBytes.get(key),
      }));
      if (this.level !== undefined) {
        ops.push({
          type: "put",
          key: ROOT_KEY,
          value: normalizeStoredRootHex(root.toString("hex")),
        });
        const flushStartedAt = performance.now();
        await this.level.batch(ops, JSON_LEVEL_ENCODING_OPTS);
        this.flushMs += performance.now() - flushStartedAt;
        this.levelBatchWrites += 1;
        this.bytesFlushed += ops.reduce(
          (total, op) =>
            total +
            op.key.length +
            (op.type === "put"
              ? (op.encodedBytes ?? Buffer.byteLength(JSON.stringify(op.value)))
              : 0),
          0,
        );
      } else if (this.memory !== undefined) {
        for (const [key, value] of this.overlay) {
          this.memory.set(key, value);
        }
      }
      this.currentRoot = Buffer.from(root);
      this.overlay = undefined;
      this.overlayValueBytes = new Map();
      this.blockDeferredNodePuts = undefined;
      this.blockDeferredNodeDeletes = undefined;
      this.blockDeferredNodeEstimates = undefined;
      this.blockDeferredEstimatedBytes = 0;
      this.blockPathCache = undefined;
      this.blockPathCacheBytes = 0;
      this.blockPathCacheSealed = false;
      this.liveArenaEnabled = false;
      this.transientCurrentRootEnabled = false;
      this.clearTransientLiveArena();
      this.overlayBaseRoot = undefined;
      this.overlayBytes = 0;
      this.readCache.clear();
      if (this.parentStore !== undefined) {
        this.parentStore.transferOwnershipToChild(root);
        this.ownsLevelLifecycle = true;
      }
      this.closeFork();
    } finally {
      this.releasePromotionChain(promotionReservations);
    }
  }

  discardOverlay() {
    this.assertUsable();
    if (this.overlay === undefined || this.overlayBaseRoot === undefined) {
      throw new Error("Cannot discard an inactive MPF block overlay");
    }
    if (this.openForks > 0) {
      throw new Error("Cannot discard an MPF overlay while a fork is active");
    }
    const baseRoot = Buffer.from(this.overlayBaseRoot);
    this.overlay = undefined;
    this.overlayValueBytes = new Map();
    this.blockDeferredNodePuts = undefined;
    this.blockDeferredNodeDeletes = undefined;
    this.blockDeferredNodeEstimates = undefined;
    this.blockDeferredEstimatedBytes = 0;
    this.blockPathCache = undefined;
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.liveArenaEnabled = false;
    this.transientCurrentRootEnabled = false;
    this.clearTransientLiveArena();
    this.overlayBaseRoot = undefined;
    this.overlayBytes = 0;
    this.readCache.clear();
    this.currentRoot = Buffer.from(baseRoot);
    this.closeFork();
    return baseRoot;
  }

  async poisonOverlayAfterMutationFailure() {
    this.abortDeferredMutation();
    await this.waitForSpills();
    const baseRoot = this.discardOverlay();
    this.poisoned = true;
    return baseRoot;
  }

  async spillIfNeeded() {
    if (this.liveArenaEnabled) {
      this.assertLiveArenaCaps();
      return;
    }
    if (
      this.level === undefined ||
      this.overlay === undefined ||
      this.overlayBytes + this.blockDeferredEstimatedBytes <=
        this.spillThresholdBytes
    ) {
      return;
    }
    await this.materializeDeferredNodes();
    const ops: LevelBatchOp[] = [...this.overlay].map(([key, value]) => ({
      type: "put" as const,
      key,
      value,
      encodedBytes: this.overlayValueBytes.get(key),
    }));
    const spillingOverlay = this.overlay;
    const spillBytes = this.overlayBytes;
    this.spillingOverlays.push(spillingOverlay);
    this.pendingSpillBytes += spillBytes;
    this.overlay = new Map();
    this.overlayValueBytes = new Map();
    this.overlayBytes = 0;
    this.overlaySpills += 1;
    this.spillChain = this.spillChain.then(async () => {
      try {
        if (this.spillError === undefined) {
          const spillStartedAt = performance.now();
          await this.level!.batch(ops, JSON_LEVEL_ENCODING_OPTS);
          this.overlaySpillMs += performance.now() - spillStartedAt;
          this.levelBatchWrites += 1;
          this.bytesFlushed += spillBytes;
        }
      } catch (error) {
        this.spillError = error;
      } finally {
        const index = this.spillingOverlays.indexOf(spillingOverlay);
        if (index >= 0) this.spillingOverlays.splice(index, 1);
        this.pendingSpillBytes -= spillBytes;
      }
    });
  }

  async waitForSpills() {
    await this.spillChain;
    if (this.spillError !== undefined) {
      throw this.spillError instanceof Error
        ? this.spillError
        : new Error("MPF spill failed", { cause: this.spillError });
    }
  }

  private async waitForAncestorSpills() {
    if (this.parentStore === undefined) return;
    await this.parentStore.waitForSpills();
    await this.parentStore.waitForAncestorSpills();
  }

  shouldCloseLevel() {
    return this.ownsLevelLifecycle;
  }

  diagnostics(): Omit<MpfStoreDiagnostics, "entries"> {
    return {
      storePuts: this.storePuts,
      storeDels: this.storeDels,
      serialiseCalls: this.serialiseCalls,
      serialiseMs: this.serialiseMs,
      deferredMaterializedEstimatedBytes:
        this.deferredMaterializedEstimatedBytes,
      deferredMaterializedActualBytes: this.deferredMaterializedActualBytes,
      deferredLazyReads: this.deferredLazyReads,
      deferredLazySerialiseMs: this.deferredLazySerialiseMs,
      deferredLazySerialisedBytes: this.deferredLazySerialisedBytes,
      arenaCheckpointCalls: this.arenaCheckpointCalls,
      arenaCheckpointMs: this.arenaCheckpointMs,
      arenaCheckpointNodes: this.arenaCheckpointNodes,
      arenaCheckpointBytes: this.arenaCheckpointBytes,
      pathCacheEntries: this.blockPathCache?.size ?? 0,
      pathCacheBytes: this.blockPathCacheBytes,
      pathCacheHits: this.pathCacheHits,
      liveArenaPrunedNodes: this.liveArenaPrunedNodes,
      liveArenaPromotedNodes: this.liveArenaPromotedNodes,
      liveArenaPromotedBytes: this.liveArenaPromotedBytes,
      retainedSnapshotAuthentications: this.retainedSnapshotAuthentications,
      retainedSnapshotAuthenticationMs: this.retainedSnapshotAuthenticationMs,
      transientLiveNodes: this.transientLiveNodes.size,
      transientLiveBytes: this.transientLiveBytes,
      transientDirtyNodes: this.transientDirtyNodes.size,
      transientSnapshotsCaptured: this.transientSnapshotsCaptured,
      eventAtomicFinalizations: this.eventAtomicFinalizations,
      eventAtomicDirtyNodes: this.eventAtomicDirtyNodes,
      eventAtomicMaxDirtyNodes: this.eventAtomicMaxDirtyNodes,
      levelGets: this.levelGets,
      levelGetManyCalls: this.levelGetManyCalls,
      levelGetManyMaxKeys: this.levelGetManyMaxKeys,
      levelGetMs: this.levelGetMs,
      jsonCodecMs: this.jsonCodecMs,
      overlayHits: this.overlayHits,
      readCacheHits: this.readCacheHits,
      levelBatchWrites: this.levelBatchWrites,
      bytesFlushed: this.bytesFlushed,
      overlayEntries:
        (this.overlay?.size ?? 0) +
        (this.blockDeferredNodePuts?.size ?? 0) +
        this.transientLiveNodes.size +
        this.spillingOverlays.reduce((total, view) => total + view.size, 0),
      overlayBytes:
        this.overlayBytes +
        this.pendingSpillBytes +
        this.blockDeferredEstimatedBytes +
        this.transientLiveBytes,
      overlaySpills: this.overlaySpills,
      overlaySpillMs: this.overlaySpillMs,
      flushMs: this.flushMs,
    };
  }

  async hasStoredNode(root: Buffer): Promise<boolean> {
    const storageKey = root.toString("hex");
    const deferredValue = this.blockDeferredNodePuts?.get(storageKey);
    if (deferredValue !== undefined) {
      const liveHash = (deferredValue as { readonly hash?: Buffer }).hash;
      if (liveHash === undefined || this.storageKey(liveHash) !== storageKey) {
        throw new Error(
          `Deferred MPF root is not content-addressed: expected=${storageKey},actual=${liveHash === undefined ? "undefined" : this.storageKey(liveHash)}`,
        );
      }
      return true;
    }
    if (this.blockDeferredNodeDeletes?.has(storageKey)) return false;
    return (await this.lookupStoredValue(storageKey)) !== undefined;
  }

  async checkpointDeferredNodes(): Promise<{
    readonly serializedNodes: number;
    readonly serializedBytes: number;
    readonly checkpointMs: number;
  }> {
    const serializedNodes = this.blockDeferredNodePuts?.size ?? 0;
    const actualBytesBefore = this.deferredMaterializedActualBytes;
    const startedAt = performance.now();
    await this.materializeDeferredNodes();
    const checkpointMs = performance.now() - startedAt;
    const serializedBytes =
      this.deferredMaterializedActualBytes - actualBytesBefore;
    this.arenaCheckpointCalls += 1;
    this.arenaCheckpointMs += checkpointMs;
    this.arenaCheckpointNodes += serializedNodes;
    this.arenaCheckpointBytes += serializedBytes;
    return { serializedNodes, serializedBytes, checkpointMs };
  }

  recordLiveArenaCheckpoint(checkpointMs: number) {
    this.arenaCheckpointCalls += 1;
    this.arenaCheckpointMs += checkpointMs;
  }

  currentOverlayView(): ReadonlyMap<string, MpfStoredValue> | undefined {
    return this.overlay;
  }

  private setOverlayValue(
    key: string,
    value: MpfStoredValue,
    encodedBytes?: number,
  ) {
    const previous = this.overlay!.get(key);
    if (previous !== undefined) {
      this.overlayBytes -=
        Buffer.byteLength(key) + (this.overlayValueBytes.get(key) ?? 0);
    }
    this.overlay!.set(key, value);
    const valueBytes =
      encodedBytes ??
      (this.level === undefined ? 0 : Buffer.byteLength(JSON.stringify(value)));
    this.overlayValueBytes.set(key, valueBytes);
    this.overlayBytes += Buffer.byteLength(key) + valueBytes;
    this.readCache.delete(key);
  }

  private async materializeDeferredNodes() {
    if (
      this.blockDeferredNodePuts === undefined ||
      this.blockDeferredNodeDeletes === undefined ||
      (this.blockDeferredNodePuts.size === 0 &&
        this.blockDeferredNodeDeletes.size === 0)
    ) {
      return;
    }
    const puts: Extract<LevelBatchOp, { readonly type: "put" }>[] = [];
    let actualBytes = 0;
    for (const [key, value] of this.blockDeferredNodePuts) {
      this.assertLiveArenaNode(key, value);
      const serialiseStartedAt = performance.now();
      const serialized = value.serialise();
      this.serialiseMs += performance.now() - serialiseStartedAt;
      this.serialiseCalls += 1;
      let encodedBytes: number | undefined;
      if (this.level !== undefined) {
        const jsonCodecStartedAt = performance.now();
        encodedBytes = Buffer.byteLength(JSON.stringify(serialized));
        this.jsonCodecMs += performance.now() - jsonCodecStartedAt;
      }
      actualBytes +=
        Buffer.byteLength(key) +
        (encodedBytes ?? Buffer.byteLength(JSON.stringify(serialized)));
      puts.push({ type: "put", key, value: serialized, encodedBytes });
    }
    for (const key of this.blockDeferredNodeDeletes) {
      if (this.overlay!.has(key)) this.deleteOverlayValue(key);
    }
    for (const put of puts) {
      this.setOverlayValue(put.key, put.value, put.encodedBytes);
    }
    this.deferredMaterializedEstimatedBytes += this.blockDeferredEstimatedBytes;
    this.deferredMaterializedActualBytes += actualBytes;
    if (this.liveArenaEnabled) {
      this.liveArenaPromotedNodes += puts.length;
      this.liveArenaPromotedBytes += actualBytes;
    }
    this.blockDeferredNodePuts.clear();
    this.blockDeferredNodeDeletes.clear();
    this.blockDeferredNodeEstimates?.clear();
    this.blockDeferredEstimatedBytes = 0;
  }

  private estimateDeferredNodeBytes(
    key: string,
    value: MpfSerializableValue,
  ): number {
    const node = value as {
      readonly prefix?: string;
      readonly key?: Buffer;
      readonly value?: Buffer;
      readonly children?: readonly unknown[];
    };
    const keyBytes = Buffer.byteLength(key);
    const prefixBytes = Buffer.byteLength(node.prefix ?? "");
    if (Buffer.isBuffer(node.key) && Buffer.isBuffer(node.value)) {
      return (
        keyBytes +
        prefixBytes +
        256 +
        2 * node.key.length +
        2 * node.value.length
      );
    }
    if (Array.isArray(node.children)) {
      return keyBytes + prefixBytes + 2_048;
    }
    return keyBytes + 4_096;
  }

  private deleteOverlayValue(key: string) {
    const previous = this.overlay!.get(key);
    if (previous !== undefined) {
      this.overlayBytes -=
        Buffer.byteLength(key) + (this.overlayValueBytes.get(key) ?? 0);
    }
    this.overlay!.delete(key);
    this.overlayValueBytes.delete(key);
  }

  private cacheRead(key: string, value: MpfStoredValue) {
    this.readCache.delete(key);
    this.readCache.set(key, value);
    if (this.readCache.size > 4_096) {
      const oldest = this.readCache.keys().next().value as string | undefined;
      if (oldest !== undefined) this.readCache.delete(oldest);
    }
  }

  private cacheBlockPathNode(key: string, value: MpfStoredValue) {
    if (!this.liveArenaEnabled || this.blockPathCache === undefined) return;
    if (this.blockPathCache.has(key)) return;
    const bytes = Buffer.byteLength(key) + estimateMpfStoredValueBytes(value);
    const nextNodes = this.blockPathCache.size + 1;
    const nextBytes = this.blockPathCacheBytes + bytes;
    if (
      nextNodes > this.arenaLimits.pathCacheMaxNodes ||
      nextBytes > this.arenaLimits.pathCacheMaxBytes
    ) {
      throw new Error(
        `MPF block path cache limit exceeded: nodes=${nextNodes.toString()}/${this.arenaLimits.pathCacheMaxNodes.toString()},bytes=${nextBytes.toString()}/${this.arenaLimits.pathCacheMaxBytes.toString()}`,
      );
    }
    this.blockPathCache.set(key, value);
    this.blockPathCacheBytes = nextBytes;
  }

  private assertLiveArenaNode(key: string, value: MpfSerializableValue) {
    const node = value as MpfSerializableValue & {
      readonly hash?: Buffer;
      readonly assertHydratedNodeHashes?: (maxDepth: number) => {
        readonly verifiedNodes: number;
      };
    };
    if (node.hash === undefined || this.storageKey(node.hash) !== key) {
      throw new Error(
        `Live MPF arena node is not content-addressed: expected=${key},actual=${node.hash === undefined ? "undefined" : this.storageKey(node.hash)}`,
      );
    }
    this.authenticateHydratedNodeOnce(node);
  }

  private assertLiveArenaCaps() {
    const nodes =
      (this.blockDeferredNodePuts?.size ?? 0) + this.transientLiveNodes.size;
    const bytes = this.blockDeferredEstimatedBytes + this.transientLiveBytes;
    if (
      nodes > this.arenaLimits.liveArenaMaxNodes ||
      bytes > this.arenaLimits.liveArenaMaxBytes
    ) {
      throw new Error(
        `MPF live arena limit exceeded: nodes=${nodes.toString()}/${this.arenaLimits.liveArenaMaxNodes.toString()},bytes=${bytes.toString()}/${this.arenaLimits.liveArenaMaxBytes.toString()}`,
      );
    }
  }

  private candidateChildKeys(value: MpfReadableValue): readonly string[] {
    if (typeof value === "string") return [];
    if ("serialise" in value) {
      const children = (
        value as MpfSerializableValue & {
          readonly children?: readonly (
            | { readonly hash?: Buffer }
            | undefined
          )[];
        }
      ).children;
      return (
        children?.flatMap((child) =>
          child?.hash === undefined ? [] : [this.storageKey(child.hash)],
        ) ?? []
      );
    }
    if (value.__kind !== "Branch" || !Array.isArray(value.children)) return [];
    return value.children.flatMap((child) =>
      typeof child === "string" ? [child] : [],
    );
  }

  private findNonDurableCandidate(key: string): MpfReadableValue | undefined {
    const live = this.blockDeferredNodePuts?.get(key);
    if (live !== undefined) {
      this.assertLiveArenaNode(key, live);
      return live;
    }
    const serialized = this.overlay?.get(key);
    if (serialized !== undefined) return serialized;
    for (let index = this.spillingOverlays.length - 1; index >= 0; index -= 1) {
      const spilling = this.spillingOverlays[index]!;
      const candidate = spilling.get(key);
      if (candidate !== undefined) return candidate;
    }
    return this.parentStore?.findNonDurableCandidate(key);
  }

  private importReachableParentCandidates(root: Buffer) {
    if (this.parentStore === undefined) return;
    const visited = new Set<string>();
    const pending = [this.storageKey(root)];
    while (pending.length > 0) {
      const key = pending.pop()!;
      if (visited.has(key)) continue;
      visited.add(key);
      let candidate: MpfReadableValue | undefined =
        this.blockDeferredNodePuts?.get(key) ?? this.overlay?.get(key);
      if (candidate === undefined) {
        candidate = this.parentStore.findNonDurableCandidate(key);
        if (candidate === undefined) continue;
        if (typeof candidate !== "string" && "serialise" in candidate) {
          const clone = (
            candidate as MpfSerializableValue & {
              readonly cloneDetached?: () => MpfSerializableValue;
            }
          ).cloneDetached?.();
          if (clone === undefined || clone === candidate) {
            throw new Error(
              `Cannot import mutable MPF parent arena node ${key}`,
            );
          }
          const priorEstimate = this.blockDeferredNodeEstimates?.get(key) ?? 0;
          const estimate = this.estimateDeferredNodeBytes(key, clone);
          this.blockDeferredNodePuts!.set(key, clone);
          this.blockDeferredNodeEstimates!.set(key, estimate);
          this.blockDeferredEstimatedBytes += estimate - priorEstimate;
          candidate = clone;
        } else {
          this.setOverlayValue(key, candidate as MpfStoredValue);
        }
      }
      for (const childKey of this.candidateChildKeys(candidate)) {
        pending.push(childKey);
      }
    }
    if (this.liveArenaEnabled) this.assertLiveArenaCaps();
  }

  private pruneLiveArenaToRoot(root: Buffer) {
    if (
      this.blockDeferredNodePuts === undefined ||
      this.blockDeferredNodeEstimates === undefined
    ) {
      return;
    }
    const reachable = new Set<string>();
    const pending = [this.storageKey(root)];
    while (pending.length > 0) {
      const key = pending.pop()!;
      if (reachable.has(key)) continue;
      const value =
        this.blockDeferredNodePuts.get(key) ??
        this.overlay?.get(key) ??
        this.blockPathCache?.get(key);
      if (value === undefined) continue;
      if (typeof value !== "string" && "serialise" in value) {
        this.assertLiveArenaNode(key, value as MpfSerializableValue);
        reachable.add(key);
      }
      for (const childKey of this.candidateChildKeys(value)) {
        pending.push(childKey);
      }
    }
    for (const key of [...this.blockDeferredNodePuts.keys()]) {
      if (reachable.has(key)) continue;
      this.blockDeferredNodePuts.delete(key);
      this.blockDeferredNodeDeletes?.delete(key);
      this.blockDeferredEstimatedBytes -=
        this.blockDeferredNodeEstimates.get(key) ?? 0;
      this.blockDeferredNodeEstimates.delete(key);
      this.liveArenaPrunedNodes += 1;
    }
    this.assertLiveArenaCaps();
  }

  private async lookupStoredValue(
    storageKey: string,
  ): Promise<MpfReadableValue | undefined> {
    const activeMutationValue = this.deferredNodePuts?.get(storageKey);
    if (activeMutationValue !== undefined) {
      if (this.liveArenaEnabled) {
        this.assertLiveArenaNode(storageKey, activeMutationValue);
        return activeMutationValue;
      }
      return activeMutationValue.serialise();
    }
    const deferredValue = this.blockDeferredNodePuts?.get(storageKey);
    if (deferredValue !== undefined) {
      this.assertLiveArenaNode(storageKey, deferredValue);
      if (this.liveArenaEnabled) return deferredValue;
      const startedAt = performance.now();
      const serialized = deferredValue.serialise();
      const elapsedMs = performance.now() - startedAt;
      const serializedBytes = Buffer.byteLength(JSON.stringify(serialized));
      this.deferredLazyReads += 1;
      this.deferredLazySerialiseMs += elapsedMs;
      this.deferredLazySerialisedBytes += serializedBytes;
      this.serialiseCalls += 1;
      this.serialiseMs += elapsedMs;
      return serialized;
    }
    if (this.overlay?.has(storageKey)) {
      return this.overlay.get(storageKey);
    }
    for (let index = this.spillingOverlays.length - 1; index >= 0; index -= 1) {
      const spilling = this.spillingOverlays[index]!;
      if (spilling.has(storageKey)) return spilling.get(storageKey);
    }
    if (this.parentOverlay?.has(storageKey)) {
      return this.parentOverlay.get(storageKey);
    }
    if (this.parentStore !== undefined) {
      return this.parentStore.lookupStoredValue(storageKey);
    }
    if (this.blockPathCache?.has(storageKey)) {
      this.pathCacheHits += 1;
      return this.blockPathCache.get(storageKey);
    }
    if (this.readCache.has(storageKey)) {
      return this.readCache.get(storageKey);
    }
    if (this.level !== undefined) {
      const startedAt = performance.now();
      const value = await this.level.get(storageKey, JSON_LEVEL_ENCODING_OPTS);
      this.levelGetMs += performance.now() - startedAt;
      this.levelGets += 1;
      this.cacheRead(storageKey, value);
      this.cacheBlockPathNode(storageKey, value);
      return value;
    }
    return this.memory?.get(storageKey);
  }

  private registerFork() {
    this.assertUsable();
    if (this.promotionReserved) {
      throw new Error("Cannot fork an MPF overlay while promotion is reserved");
    }
    this.openForks += 1;
  }

  private reservePromotionChain(): readonly MidgardMpfRootViewStore[] {
    const reserved: MidgardMpfRootViewStore[] = [];
    // The traversal intentionally starts at this store and walks its parents.
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    let current: MidgardMpfRootViewStore | undefined = this;
    try {
      while (current !== undefined) {
        if (current.promotionReserved) {
          throw new Error("MPF promotion chain is already reserved");
        }
        if (current !== this && current.openForks !== 1) {
          throw new Error(
            `Cannot promote an MPF fork while an ancestor has ${current.openForks.toString()} open children`,
          );
        }
        current.promotionReserved = true;
        reserved.push(current);
        current = current.parentStore;
      }
      return reserved;
    } catch (cause) {
      this.releasePromotionChain(reserved);
      throw cause;
    }
  }

  private releasePromotionChain(reserved: readonly MidgardMpfRootViewStore[]) {
    for (const store of reserved) store.promotionReserved = false;
  }

  private transferOwnershipToChild(root: Buffer) {
    if (this.openForks !== 1) {
      throw new Error(
        `Cannot transfer MPF ownership without exactly one active child: open_forks=${this.openForks.toString()}`,
      );
    }
    this.parentStore?.transferOwnershipToChild(root);
    this.closeFork();
    this.currentRoot = Buffer.from(root);
    this.overlay = undefined;
    this.overlayValueBytes = new Map();
    this.blockDeferredNodePuts = undefined;
    this.blockDeferredNodeDeletes = undefined;
    this.blockDeferredNodeEstimates = undefined;
    this.blockDeferredEstimatedBytes = 0;
    this.blockPathCache = undefined;
    this.blockPathCacheBytes = 0;
    this.blockPathCacheSealed = false;
    this.liveArenaEnabled = false;
    this.transientCurrentRootEnabled = false;
    this.clearTransientLiveArena();
    this.overlayBaseRoot = undefined;
    this.overlayBytes = 0;
    this.readCache.clear();
    this.invalidatedByChildPromotion = true;
    this.ownsLevelLifecycle = false;
  }

  private assertUsable() {
    if (this.poisoned) {
      throw new Error(
        "MPF overlay is poisoned after a failed mutation and must be reloaded",
      );
    }
    if (this.invalidatedByChildPromotion) {
      throw new Error(
        "MPF handle is stale because durable-root ownership transferred to a promoted child fork",
      );
    }
    if (this.promotionReserved) {
      throw new Error("MPF handle is reserved for promotion");
    }
  }

  private closeFork() {
    if (this.parentStore !== undefined && !this.forkClosed) {
      this.forkClosed = true;
      this.parentStore.openForks -= 1;
    }
  }

  private storageKey(key: unknown): string {
    if (key === null || key === undefined) {
      return MPF_INTERNAL_NULL_ROOT_HEX;
    }
    if (typeof key === "string") {
      return key;
    }
    if (Buffer.isBuffer(key)) {
      return key.toString("hex");
    }
    if (key instanceof Uint8Array) {
      return Buffer.from(key).toString("hex");
    }
    throw new Error(`Unsupported MPF store key type: ${typeof key}`);
  }
}

export class MpfError extends Data.TaggedError(
  "MpfError",
)<SDK.GenericErrorFields> {
  static get(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred getting an entry from ${trie} MPF`,
      cause,
    });
  }

  static insert(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred inserting a new entry in ${trie} MPF`,
      cause,
    });
  }

  static delete(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred deleting an entry from ${trie} MPF`,
      cause,
    });
  }

  static batch(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred during a batch operation on ${trie} MPF`,
      cause,
    });
  }

  static phasRoot(cause: unknown) {
    return new MpfError({
      message: "An error occurred building a Midgard PHAS root or proof",
      cause,
    });
  }

  static rootBuild(rootName: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred building ${rootName} MPF root`,
      cause,
    });
  }

  static create(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred creating ${trie} MPF`,
      cause,
    });
  }

  static close(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred closing ${trie} MPF store`,
      cause,
    });
  }

  static prove(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred proving a key in ${trie} MPF`,
      cause,
    });
  }

  static verify(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred verifying a proof for ${trie} MPF`,
      cause,
    });
  }

  static rootNotSet(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred getting ${trie} MPF root, the root is ${typeof cause}`,
      cause,
    });
  }
}

export class MidgardMpf {
  public readonly trie: Trie;
  public readonly trieName: string;
  private eventFlatArena?: EventFlatMutationArena;
  private readonly store: MidgardMpfRootViewStore;
  private readonly level?: Level<string, MpfStoredValue>;
  private readonly memory?: Map<string, MpfStoredValue>;
  private readonly engine: MpfEngine;
  private readonly spillThresholdBytes: number;
  private readonly parentStore?: MidgardMpfRootViewStore;
  private readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;

  private constructor({
    trie,
    trieName,
    store,
    level,
    memory,
    engine = "legacy",
    spillThresholdBytes = 512 * 1024 * 1024,
    parentStore,
    parentOverlay,
  }: {
    readonly trie: Trie;
    readonly trieName: string;
    readonly store: MidgardMpfRootViewStore;
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly engine?: MpfEngine;
    readonly spillThresholdBytes?: number;
    readonly parentStore?: MidgardMpfRootViewStore;
    readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;
  }) {
    this.trie = trie;
    this.trieName = trieName;
    this.store = store;
    this.level = level;
    this.memory = memory;
    this.engine = engine;
    this.spillThresholdBytes = spillThresholdBytes;
    this.parentStore = parentStore;
    this.parentOverlay = parentOverlay;
  }

  public static create(
    trieName: string,
    levelDBFilePath?: string,
    options: {
      readonly engine?: MpfEngine;
      readonly spillThresholdBytes?: number;
    } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      if (levelDBFilePath === undefined) {
        return yield* MidgardMpf.createScratch(trieName, options);
      }
      const level = new Level<string, MpfStoredValue>(
        levelDBFilePath,
        JSON_LEVEL_ENCODING_OPTS,
      );
      yield* Effect.tryPromise({
        try: () => level.open(),
        catch: (e) => MpfError.create(trieName, e),
      });
      const root = yield* readPersistedRoot(level);
      return yield* MidgardMpf.loadFromLevel({
        trieName,
        level,
        root,
        persistRootMarker: true,
        engine: options.engine ?? "legacy",
        spillThresholdBytes: options.spillThresholdBytes ?? 512 * 1024 * 1024,
      });
    });
  }

  public static createScratch(
    trieName: string,
    options: {
      readonly engine?: MpfEngine;
      readonly spillThresholdBytes?: number;
    } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return MidgardMpf.loadFromRootView({
      trieName,
      root: MPF_EMPTY_ROOT,
      memory: new Map(),
      persistRootMarker: false,
      engine: options.engine ?? "legacy",
      spillThresholdBytes: options.spillThresholdBytes ?? 512 * 1024 * 1024,
    });
  }

  public static createScratchFromList(
    trieName: string,
    entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
    options: { readonly engine?: MpfEngine } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const memory = new Map<string, MpfStoredValue>();
      const store = new MidgardMpfRootViewStore({
        memory,
        root: MPF_EMPTY_ROOT,
        persistRootMarker: false,
        engine: options.engine ?? "legacy",
      });
      const trie = yield* Effect.tryPromise({
        try: () =>
          Trie.fromList(
            entries.map(({ key, value }) => ({
              key: Buffer.from(key),
              value: Buffer.from(value),
            })),
            store,
          ),
        catch: (e) => MpfError.create(trieName, e),
      });
      return new MidgardMpf({
        trie,
        trieName,
        store,
        memory,
        engine: options.engine ?? "legacy",
      });
    });
  }

  /** Builds a deterministic Level-backed fixture with `fromList`, then reopens
   * only its root. This is intentionally benchmark-only: production bootstrap
   * continues to use the audited confirmed-ledger migration path. */
  public static createLevelFromListForBenchmark(
    trieName: string,
    levelDBFilePath: string,
    entries: readonly { readonly key: Buffer; readonly value: Buffer }[],
    options: {
      readonly engine?: MpfEngine;
      readonly spillThresholdBytes?: number;
    } = {},
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const memory = new Map<string, MpfStoredValue>();
      const stagingStore = new MidgardMpfRootViewStore({
        memory,
        root: MPF_EMPTY_ROOT,
        persistRootMarker: false,
        engine: "legacy",
      });
      const trie = yield* Effect.tryPromise({
        try: () =>
          Trie.fromList(
            entries.map(({ key, value }) => ({
              key: Buffer.from(key),
              value: Buffer.from(value),
            })),
            stagingStore,
          ),
        catch: (cause) => MpfError.create(trieName, cause),
      });
      const root = Buffer.from(trie.hash ?? MPF_EMPTY_ROOT);
      const level = new Level<string, MpfStoredValue>(
        levelDBFilePath,
        JSON_LEVEL_ENCODING_OPTS,
      );
      yield* Effect.tryPromise({
        try: async () => {
          await level.open();
          let batch: Extract<LevelBatchOp, { readonly type: "put" }>[] = [];
          for (const [key, value] of memory) {
            batch.push({ type: "put", key, value });
            if (batch.length === 10_000) {
              await level.batch(batch, JSON_LEVEL_ENCODING_OPTS);
              batch = [];
            }
          }
          if (batch.length > 0) {
            await level.batch(batch, JSON_LEVEL_ENCODING_OPTS);
          }
          await level.put(
            ROOT_KEY,
            normalizeStoredRootHex(root.toString("hex")),
            JSON_LEVEL_ENCODING_OPTS,
          );
        },
        catch: (cause) => MpfError.create(trieName, cause),
      });
      return yield* MidgardMpf.loadFromLevel({
        trieName,
        level,
        root,
        persistRootMarker: true,
        engine: options.engine ?? "overlay",
        spillThresholdBytes: options.spillThresholdBytes ?? 512 * 1024 * 1024,
      });
    });
  }

  public static load(
    trieName: string,
    levelDBFilePath: string,
    root: Buffer,
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const level = new Level<string, MpfStoredValue>(
        levelDBFilePath,
        JSON_LEVEL_ENCODING_OPTS,
      );
      yield* Effect.tryPromise({
        try: () => level.open(),
        catch: (e) => MpfError.create(trieName, e),
      });
      return yield* MidgardMpf.loadFromLevel({
        trieName,
        level,
        root,
        persistRootMarker: false,
      });
    });
  }

  private static loadFromLevel({
    trieName,
    level,
    root,
    persistRootMarker,
    engine = "legacy",
    spillThresholdBytes = 512 * 1024 * 1024,
  }: {
    readonly trieName: string;
    readonly level?: Level<string, MpfStoredValue>;
    readonly root: Buffer;
    readonly persistRootMarker: boolean;
    readonly engine?: MpfEngine;
    readonly spillThresholdBytes?: number;
  }): Effect.Effect<MidgardMpf, MpfError> {
    return MidgardMpf.loadFromRootView({
      trieName,
      root,
      level,
      persistRootMarker,
      engine,
      spillThresholdBytes,
    });
  }

  private static loadFromRootView({
    trieName,
    root,
    level,
    memory,
    persistRootMarker,
    engine = "legacy",
    spillThresholdBytes = 512 * 1024 * 1024,
    parentStore,
    parentOverlay,
  }: {
    readonly trieName: string;
    readonly root: Buffer;
    readonly level?: Level<string, MpfStoredValue>;
    readonly memory?: Map<string, MpfStoredValue>;
    readonly persistRootMarker: boolean;
    readonly engine?: MpfEngine;
    readonly spillThresholdBytes?: number;
    readonly parentStore?: MidgardMpfRootViewStore;
    readonly parentOverlay?: ReadonlyMap<string, MpfStoredValue>;
  }): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      const store = new MidgardMpfRootViewStore({
        level,
        memory,
        root,
        persistRootMarker,
        engine,
        spillThresholdBytes,
        parentStore,
        parentOverlay,
      });
      const trie = yield* Effect.tryPromise({
        try: async () =>
          root.equals(MPF_EMPTY_ROOT)
            ? new Trie(store)
            : await Trie.load(store),
        catch: (e) => MpfError.create(trieName, e),
      });
      return new MidgardMpf({
        trie,
        trieName,
        store,
        level,
        memory,
        engine,
        spillThresholdBytes,
        parentStore,
        parentOverlay,
      });
    });
  }

  public root(): Effect.Effect<Buffer, MpfError> {
    return Effect.try({
      try: () => {
        this.store.root();
        return (
          this.eventFlatArena?.rootHash() ??
          Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT)
        );
      },
      catch: (cause) => MpfError.get(this.trieName, cause),
    });
  }

  public rootHex(): Effect.Effect<string, MpfError> {
    return this.root().pipe(Effect.map((root) => root.toString("hex")));
  }

  public persistedRootHex(): Effect.Effect<string, MpfError> {
    return this.level === undefined
      ? this.rootHex()
      : readPersistedRoot(this.level).pipe(
          Effect.map((root) => root.toString("hex")),
        );
  }

  public rootIsEmpty(): Effect.Effect<boolean, MpfError> {
    return this.root().pipe(Effect.map((root) => root.equals(MPF_EMPTY_ROOT)));
  }

  public get(key: Buffer): Effect.Effect<Option.Option<Buffer>, MpfError> {
    if (this.eventFlatArena !== undefined) {
      return Effect.try({
        try: () => this.eventFlatArena!.get(key),
        catch: (cause) => MpfError.get(this.trieName, cause),
      }).pipe(
        Effect.map((value) =>
          value === undefined ? Option.none() : Option.some(value),
        ),
      );
    }
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.get(key),
      catch: (e) => MpfError.get(trieName, e),
    }).pipe(
      Effect.map((value) =>
        value === null || value === undefined
          ? Option.none()
          : Option.some(Buffer.from(value)),
      ),
    );
  }

  public insert(key: Buffer, value: Buffer): Effect.Effect<void, MpfError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.insert(key, value),
      catch: (e) => MpfError.insert(trieName, e),
    });
  }

  public delete(key: Buffer): Effect.Effect<void, MpfError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.delete(key),
      catch: (e) => MpfError.delete(trieName, e),
    });
  }

  public applyBatch(
    ops: readonly MpfBatchOp[],
  ): Effect.Effect<Buffer, MpfError> {
    if (
      this.store.overlayIsActive() &&
      this.engine === "event_flat" &&
      this.eventFlatArena !== undefined
    ) {
      return Effect.gen(this, function* () {
        const mutated = yield* Effect.either(
          Effect.try({
            try: () => this.eventFlatArena!.applyEvent(ops),
            catch: (cause) => MpfError.batch(this.trieName, cause),
          }),
        );
        if (mutated._tag === "Right") {
          this.store.putRetainedRoot(mutated.right);
          this.trie.hash = Buffer.from(mutated.right);
          return mutated.right;
        }
        const poisoned = yield* Effect.either(
          Effect.tryPromise({
            try: () => this.store.poisonOverlayAfterMutationFailure(),
            catch: (cause) => MpfError.batch(this.trieName, cause),
          }),
        );
        this.eventFlatArena = undefined;
        if (poisoned._tag === "Right") {
          this.trie.hash = Buffer.from(poisoned.right);
        }
        return yield* Effect.fail(mutated.left);
      });
    }
    if (this.store.overlayIsActive() && this.engine === "overlay") {
      return Effect.tryPromise({
        try: async () => {
          const deferred = this.store.beginDeferredMutation();
          try {
            for (const op of ops) {
              try {
                if (op.type === "insert") {
                  await this.trie.insert(op.key, op.value);
                } else {
                  await this.trie.delete(op.key);
                }
              } catch (cause) {
                throw op.type === "insert"
                  ? MpfError.insert(
                      this.trieName,
                      new Error(
                        `Failed to insert MPF key ${op.key.toString("hex")}`,
                        { cause },
                      ),
                    )
                  : MpfError.delete(
                      this.trieName,
                      new Error(
                        `Failed to delete MPF key ${op.key.toString("hex")}`,
                        { cause },
                      ),
                    );
              }
            }
            if (this.store.deferMidgardBranchHashes) {
              this.trie.finalizeMidgardEventMutation();
            }
            if (deferred) await this.store.commitDeferredMutation();
            return Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT);
          } catch (error) {
            this.store.abortDeferredMutation();
            try {
              const baseRoot =
                await this.store.poisonOverlayAfterMutationFailure();
              this.trie.hash = Buffer.from(baseRoot);
            } catch {
              // Preserve the original mutation error; recovery will reopen from
              // the unchanged durable marker.
            }
            throw error;
          }
        },
        catch: (cause) =>
          cause instanceof MpfError
            ? cause
            : MpfError.batch(this.trieName, cause),
      });
    }
    return Effect.gen(this, function* () {
      const rootBefore = yield* this.root();
      yield* Effect.gen(this, function* () {
        for (const op of ops) {
          if (op.type === "insert") {
            yield* this.insert(op.key, op.value);
          } else {
            yield* this.delete(op.key);
          }
        }
      }).pipe(
        Effect.catchAll((error) =>
          this.resetToRoot(rootBefore).pipe(
            Effect.flatMap(() => Effect.fail(error)),
          ),
        ),
      );
      const rootAfter = yield* this.root();
      if (!this.store.overlayIsActive()) {
        yield* this.persistRootMarker(rootAfter);
      }
      return rootAfter;
    }).pipe(
      Effect.mapError((cause) =>
        cause instanceof MpfError
          ? cause
          : MpfError.batch(this.trieName, cause),
      ),
    );
  }

  public prove(key: Buffer): Effect.Effect<MpfProof, MpfError> {
    const trieName = this.trieName;
    return Effect.tryPromise({
      try: () => this.trie.prove(key),
      catch: (e) => MpfError.prove(trieName, e),
    }).pipe(
      Effect.map((proof: Proof) => ({
        key: Buffer.from(key),
        proof,
        cbor: proof.toCBOR(),
        json: proof.toJSON(),
        aiken: proof.toAiken(),
      })),
    );
  }

  public verify(
    proof:
      | MpfProof
      | Proof
      | { readonly verify: (includingItem?: boolean) => Buffer },
    includingItem: boolean,
  ): Effect.Effect<Buffer, MpfError> {
    return Effect.try({
      try: () => {
        const proofObject = "proof" in proof ? proof.proof : proof;
        const verifiedRoot = proofObject.verify(includingItem);
        if (verifiedRoot === null || verifiedRoot === undefined) {
          return MPF_EMPTY_ROOT;
        }
        const normalizedRoot = Buffer.from(verifiedRoot);
        return normalizedRoot.equals(Buffer.alloc(32))
          ? MPF_EMPTY_ROOT
          : normalizedRoot;
      },
      catch: (e) => MpfError.verify(this.trieName, e),
    });
  }

  /**
   * Resets the working trie view. While a block overlay is active this is a
   * logical reset only: the overlay's original rollback root remains intact
   * and callers must explicitly flush/promote after their journal or recovery
   * boundary. Non-overlay callers persist the root marker immediately for
   * standalone migration and recovery tooling.
   */
  public resetToRoot(root: Buffer): Effect.Effect<void, MpfError> {
    return Effect.gen(this, function* () {
      if (this.store.overlayIsActive()) {
        const workingRoot = yield* Effect.tryPromise({
          try: () => this.store.resetOverlayRoot(root),
          catch: (e) => MpfError.batch(this.trieName, e),
        });
        const trie = yield* Effect.tryPromise({
          try: async () =>
            workingRoot.equals(MPF_EMPTY_ROOT)
              ? new Trie(this.store)
              : await Trie.load(this.store),
          catch: (e) => MpfError.create(this.trieName, e),
        });
        Object.assign(this, { trie });
        return;
      }
      const reloaded = yield* MidgardMpf.loadFromRootView({
        trieName: this.trieName,
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: this.level !== undefined,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
        parentStore: this.parentStore,
        parentOverlay: this.parentOverlay,
      });
      Object.assign(this, reloaded);
      yield* this.persistRootMarker(root);
    });
  }

  public resetToEmpty(): Effect.Effect<void, MpfError> {
    return this.resetToRoot(MPF_EMPTY_ROOT);
  }

  public close(): Effect.Effect<void, MpfError> {
    return Effect.tryPromise({
      try: async () => {
        await this.store.waitForSpills();
        if (this.store.shouldCloseLevel()) {
          await (this.level?.close() ?? Promise.resolve());
        }
      },
      catch: (e) => MpfError.close(this.trieName, e),
    });
  }

  public diagnostics(): Effect.Effect<MpfStoreDiagnostics, MpfError> {
    return Effect.tryPromise({
      try: async () => ({
        entries: await this.store.size(),
        ...this.store.diagnostics(),
      }),
      catch: (e) => MpfError.get(this.trieName, e),
    });
  }

  public prefetchTouchedPaths(
    touched: readonly (Buffer | MpfBatchOp)[],
    concurrency = 64,
  ): Effect.Effect<MpfPathHydrationDiagnostics, MpfError> {
    if (
      this.level === undefined ||
      !this.store.overlayIsActive() ||
      touched.length === 0
    ) {
      return Effect.succeed({
        prefetchMs: 0,
        uniquePaths: new Set(
          touched.map((item) =>
            Buffer.isBuffer(item)
              ? item.toString("hex")
              : item.key.toString("hex"),
          ),
        ).size,
        nodesRequested: 0,
        hydrationHits: 0,
        hydrationMisses: 0,
        loadedNodes: 0,
        maxInFlight: 0,
        maxBatchKeys: 0,
        maxFrontierPaths: 0,
        retainedBytesEstimate: 0,
        chunkCount: 0,
        checkpointMs: 0,
        authenticationMs: 0,
        materializeMs: 0,
        collapseMs: 0,
        checkpointSerializedNodes: 0,
        checkpointSerializedBytes: 0,
        verifiedUpperNodes: 0,
        retainedUpperNodes: 0,
        collapsedNodes: 0,
        peakDecodedNodes: 0,
      });
    }
    const trie = this.trie as Trie & {
      hydratePaths?: (
        touchedItems: readonly (Buffer | MpfBatchOp)[],
        options: {
          readonly concurrency: number;
          readonly nativeBatchSize: number;
        },
      ) => Promise<
        Omit<
          MpfPathHydrationDiagnostics,
          | "prefetchMs"
          | "chunkCount"
          | "checkpointMs"
          | "authenticationMs"
          | "materializeMs"
          | "collapseMs"
          | "checkpointSerializedNodes"
          | "checkpointSerializedBytes"
          | "verifiedUpperNodes"
          | "retainedUpperNodes"
          | "collapsedNodes"
          | "peakDecodedNodes"
        >
      >;
    };
    return Effect.tryPromise({
      try: async () => {
        if (trie.hydratePaths === undefined) {
          throw new Error(
            "Patched MPF trie does not expose bounded touched-path hydration",
          );
        }
        const levelWritesBefore = this.store.diagnostics().levelBatchWrites;
        const startedAt = performance.now();
        const result = await trie.hydratePaths(touched, {
          concurrency: Math.max(1, Math.min(256, Math.floor(concurrency))),
          nativeBatchSize: 4_096,
        });
        const prefetchMs = performance.now() - startedAt;
        if (this.store.diagnostics().levelBatchWrites !== levelWritesBefore) {
          throw new Error(
            "MPF touched-path hydration performed a durable write",
          );
        }
        return {
          prefetchMs,
          ...result,
          chunkCount: 1,
          checkpointMs: 0,
          authenticationMs: 0,
          materializeMs: 0,
          collapseMs: 0,
          checkpointSerializedNodes: 0,
          checkpointSerializedBytes: 0,
          verifiedUpperNodes: 0,
          retainedUpperNodes: 0,
          collapsedNodes: 0,
          peakDecodedNodes: result.loadedNodes,
        };
      },
      catch: (cause) => MpfError.get(`${this.trieName} touched paths`, cause),
    });
  }

  public primeBlockPathArena(
    touched: readonly (Buffer | MpfBatchOp)[],
    retainDepth = 2,
    collapseDecodedArena = true,
  ): Effect.Effect<
    {
      readonly hydration: MpfPathHydrationDiagnostics;
      readonly authenticationMs: number;
      readonly verifiedNodes: number;
      readonly checkpoint: MpfArenaCheckpointDiagnostics;
    },
    MpfError
  > {
    return Effect.gen(this, function* () {
      if (this.engine === "event_flat") {
        const rawPrimed = yield* Effect.either(
          this.primeEventFlatRawPaths(touched),
        );
        if (rawPrimed._tag === "Right") return rawPrimed.right;
        this.eventFlatArena = undefined;
        if (this.store.overlayIsActive()) {
          yield* Effect.promise(async () => {
            try {
              const baseRoot =
                await this.store.poisonOverlayAfterMutationFailure();
              this.trie.hash = Buffer.from(baseRoot);
            } catch {
              // Preserve the raw hydration failure; restart uses the marker.
            }
          });
        }
        return yield* Effect.fail(rawPrimed.left);
      }
      const primed = yield* Effect.either(
        Effect.gen(this, function* () {
          yield* Effect.try({
            try: () => this.store.enableBlockPathArena(!collapseDecodedArena),
            catch: (cause) => MpfError.get(this.trieName, cause),
          });
          const hydration = yield* this.prefetchTouchedPaths(touched);
          // Each fetched raw node is authenticated once in hydratePaths before
          // attachment. Verify the already-resident root here; chunk reads then
          // reuse the sealed content-hash authentication set.
          const authenticated = yield* this.authenticateDecodedArena(0);
          const checkpoint = yield* this.checkpointAndCollapseDecodedArena(
            retainDepth,
            false,
            collapseDecodedArena,
          );
          yield* Effect.try({
            try: () => this.store.sealBlockPathCache(),
            catch: (cause) => MpfError.get(this.trieName, cause),
          });
          return {
            hydration,
            authenticationMs: authenticated.authenticationMs,
            verifiedNodes: authenticated.verifiedNodes,
            checkpoint,
          };
        }),
      );
      if (primed._tag === "Right") {
        return primed.right;
      }
      this.eventFlatArena = undefined;
      if (this.store.overlayIsActive()) {
        yield* Effect.promise(async () => {
          try {
            const baseRoot =
              await this.store.poisonOverlayAfterMutationFailure();
            this.trie.hash = Buffer.from(baseRoot);
          } catch {
            // Preserve the prime failure; restart reads the durable marker.
          }
        });
      }
      return yield* Effect.fail(primed.left);
    });
  }

  private primeEventFlatRawPaths(
    touched: readonly (Buffer | MpfBatchOp)[],
  ): Effect.Effect<
    {
      readonly hydration: MpfPathHydrationDiagnostics;
      readonly authenticationMs: number;
      readonly verifiedNodes: number;
      readonly checkpoint: MpfArenaCheckpointDiagnostics;
    },
    MpfError
  > {
    return Effect.tryPromise({
      try: async () => {
        await prepareEventFlatDigest();
        const startedAt = performance.now();
        const root = this.store.root();
        const overlayBase = this.store.overlayStartingRoot();
        const trieRoot = Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT);
        if (
          overlayBase === undefined ||
          !root.equals(overlayBase) ||
          !root.equals(trieRoot)
        ) {
          throw new Error(
            `Event-flat raw hydration base mismatch: root=${root.toString("hex")},overlay=${overlayBase?.toString("hex") ?? "absent"},trie=${trieRoot.toString("hex")}`,
          );
        }
        type PathState = {
          readonly id: number;
          readonly path: string;
          readonly deletePath: boolean;
          readonly ix: number;
        };
        type RawNode = {
          readonly hash: Buffer;
          readonly value: PackedMpfStoredValue;
          readonly states: readonly PathState[];
        };
        const uniqueByPath = new Map<
          string,
          { readonly key: Buffer; deletePath: boolean }
        >();
        for (const item of touched) {
          const key = Buffer.isBuffer(item) ? item : item.key;
          const path = eventFlatDigest(key).toString("hex");
          const previous = uniqueByPath.get(path);
          uniqueByPath.set(path, {
            key,
            deletePath:
              (previous?.deletePath ?? false) ||
              (!Buffer.isBuffer(item) && item.type === "delete"),
          });
        }
        const states = [...uniqueByPath].map(
          ([path, state], id): PathState => ({
            id,
            path,
            deletePath: state.deletePath,
            ix: 0,
          }),
        );
        const authenticated = new Map<string, AuthenticatedPackedMpfRecord>();
        let retainedBytesEstimate = 0;
        let authenticationMs = 0;
        const authenticate = (
          hash: Buffer,
          value: PackedMpfStoredValue,
        ): AuthenticatedPackedMpfRecord => {
          const hashHex = hash.toString("hex");
          const existing = authenticated.get(hashHex);
          if (existing !== undefined) return existing;
          const authenticationStartedAt = performance.now();
          const branchMerkleNodes = authenticatePackedMpfRecord(hash, value);
          authenticationMs += performance.now() - authenticationStartedAt;
          const record = { hash, value, branchMerkleNodes };
          authenticated.set(hashHex, record);
          retainedBytesEstimate +=
            Buffer.byteLength(hashHex) + estimateMpfStoredValueBytes(value);
          this.store.assertEventFlatRawCaps(
            authenticated.size,
            retainedBytesEstimate,
          );
          return record;
        };
        let frontier: RawNode[] = [];
        let loadedNodes = 0;
        let nodesRequested = 0;
        let hydrationHits = 0;
        let hydrationMisses = 0;
        let maxInFlight = 0;
        let maxBatchKeys = 0;
        let maxFrontierPaths = states.length;
        const resolved = new Set<number>();
        if (!root.equals(MPF_EMPTY_ROOT)) {
          const [rootValue] = await this.store.loadEventFlatRawRecords([root]);
          if (rootValue === undefined) {
            throw new Error(
              `Event-flat durable base is missing root ${root.toString("hex")}`,
            );
          }
          authenticate(root, rootValue);
          loadedNodes += 1;
          frontier = [{ hash: root, value: rootValue, states }];
        } else {
          for (const state of states) resolved.add(state.id);
        }
        while (frontier.length > 0) {
          const requests: {
            readonly hash: Buffer;
            readonly states: readonly PathState[];
          }[] = [];
          const next: RawNode[] = [];
          for (const node of frontier) {
            if (node.value.__kind === "Leaf") {
              for (const state of node.states) resolved.add(state.id);
              continue;
            }
            const branch = node.value;
            const groups = new Map<number, PathState[]>();
            const deleteTargetChildIndexes = new Set<number>();
            for (const state of node.states) {
              if (!state.path.slice(state.ix).startsWith(branch.prefix)) {
                resolved.add(state.id);
                continue;
              }
              const childIndex = Number.parseInt(
                state.path[state.ix + branch.prefix.length]!,
                16,
              );
              if (state.deletePath) {
                deleteTargetChildIndexes.add(childIndex);
              }
              const group = groups.get(childIndex);
              if (group === undefined) groups.set(childIndex, [state]);
              else group.push(state);
            }
            const nonEmptyChildIndexes = branch.children.flatMap(
              (child, childIndex) => (child == null ? [] : [childIndex]),
            );
            const targetedExistingChildren = [
              ...deleteTargetChildIndexes,
            ].filter(
              (childIndex) => branch.children[childIndex] != null,
            ).length;
            if (
              targetedExistingChildren > 0 &&
              nonEmptyChildIndexes.length - targetedExistingChildren <= 1
            ) {
              for (const siblingIndex of nonEmptyChildIndexes) {
                if (!groups.has(siblingIndex)) groups.set(siblingIndex, []);
              }
            }
            for (const [childIndex, childStates] of groups) {
              const childHashHex = branch.children[childIndex];
              if (childHashHex == null) {
                hydrationMisses += 1;
                for (const state of childStates) resolved.add(state.id);
                continue;
              }
              nodesRequested += 1;
              const nextStates = childStates.map((state) => ({
                ...state,
                ix: state.ix + branch.prefix.length + 1,
              }));
              const known = authenticated.get(childHashHex);
              if (known !== undefined) {
                hydrationHits += 1;
                if (nextStates.length > 0) {
                  next.push({
                    hash: known.hash,
                    value: known.value,
                    states: nextStates,
                  });
                }
                continue;
              }
              requests.push({
                hash: Buffer.from(childHashHex, "hex"),
                states: nextStates,
              });
            }
          }
          const uniqueRequests = new Map<
            string,
            { readonly hash: Buffer; readonly requests: typeof requests }
          >();
          for (const request of requests) {
            const hashHex = request.hash.toString("hex");
            const existing = uniqueRequests.get(hashHex);
            if (existing === undefined) {
              uniqueRequests.set(hashHex, {
                hash: request.hash,
                requests: [request],
              });
            } else {
              existing.requests.push(request);
            }
          }
          const pending = [...uniqueRequests.values()];
          maxInFlight = Math.max(maxInFlight, pending.length);
          for (let offset = 0; offset < pending.length; offset += 4_096) {
            const batch = pending.slice(offset, offset + 4_096);
            maxBatchKeys = Math.max(maxBatchKeys, batch.length);
            const values = await this.store.loadEventFlatRawRecords(
              batch.map((request) => request.hash),
            );
            for (const [index, request] of batch.entries()) {
              const value = values[index];
              if (value === undefined) {
                throw new Error(
                  `Event-flat touched path is missing node ${request.hash.toString("hex")}`,
                );
              }
              const record = authenticate(request.hash, value);
              loadedNodes += 1;
              for (const relation of request.requests) {
                if (relation.states.length > 0) {
                  next.push({
                    hash: record.hash,
                    value: record.value,
                    states: relation.states,
                  });
                }
              }
            }
          }
          maxFrontierPaths = Math.max(
            maxFrontierPaths,
            next.reduce((total, node) => total + node.states.length, 0),
          );
          frontier = next;
        }
        if (resolved.size !== states.length) {
          throw new Error(
            `Event-flat touched-path proof is incomplete: resolved=${resolved.size.toString()},expected=${states.length.toString()}`,
          );
        }
        const arena = EventFlatMutationArena.fromAuthenticatedRecords(root, [
          ...authenticated.values(),
        ]);
        this.store.assertEventFlatArenaCaps(
          arena.nodeCount(),
          arena.estimatedBytes(),
        );
        this.store.handoffRawPathsToEventFlat();
        this.eventFlatArena = arena;
        const elapsedMs = performance.now() - startedAt;
        const checkpoint: MpfArenaCheckpointDiagnostics = {
          checkpointMs: 0,
          authenticationMs: 0,
          materializeMs: 0,
          collapseMs: 0,
          serializedNodes: 0,
          serializedBytes: 0,
          verifiedUpperNodes: 0,
          retainedUpperNodes: 0,
          collapsedNodes: 0,
        };
        return {
          hydration: {
            prefetchMs: Math.max(0, elapsedMs - authenticationMs),
            uniquePaths: states.length,
            nodesRequested,
            hydrationHits,
            hydrationMisses,
            loadedNodes,
            maxInFlight,
            maxBatchKeys,
            maxFrontierPaths,
            retainedBytesEstimate,
            chunkCount: 1,
            checkpointMs: 0,
            authenticationMs: 0,
            materializeMs: 0,
            collapseMs: 0,
            checkpointSerializedNodes: 0,
            checkpointSerializedBytes: 0,
            verifiedUpperNodes: 0,
            retainedUpperNodes: 0,
            collapsedNodes: 0,
            peakDecodedNodes: loadedNodes,
          },
          authenticationMs,
          verifiedNodes: authenticated.size,
          checkpoint,
        };
      },
      catch: (cause) => MpfError.get(`${this.trieName} event-flat raw`, cause),
    });
  }

  public checkpointAndCollapseDecodedArena(
    retainDepth = 2,
    materialize = true,
    collapseDecodedArena = true,
  ): Effect.Effect<MpfArenaCheckpointDiagnostics, MpfError> {
    const trie = this.trie as Trie & {
      assertHydratedNodeHashes?: (maxDepth: number) => {
        readonly verifiedNodes: number;
      };
      collapseHydratedChildren?: (retainedDepth: number) => {
        readonly retainedNodes: number;
        readonly collapsedNodes: number;
      };
    };
    return Effect.tryPromise({
      try: async () => {
        try {
          if (
            trie.assertHydratedNodeHashes === undefined ||
            trie.collapseHydratedChildren === undefined
          ) {
            throw new Error(
              "Patched MPF trie does not expose authenticated arena collapse",
            );
          }
          const boundedRetainDepth = Math.max(
            0,
            Math.min(8, Math.floor(retainDepth)),
          );
          const checkpointStartedAt = performance.now();
          const rootBefore = Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT);
          const writesBefore = this.store.diagnostics().levelBatchWrites;
          const transientVerification = collapseDecodedArena
            ? this.store.authenticateDirtyLiveNodes(
                this.trie as unknown as MpfSerializableValue,
              )
            : { verifiedNodes: 0, authenticationMs: 0 };
          const authenticationStartedAt = performance.now();
          const verification = trie.assertHydratedNodeHashes(
            collapseDecodedArena ? boundedRetainDepth : 0,
          );
          const authenticationMs =
            transientVerification.authenticationMs +
            performance.now() -
            authenticationStartedAt;
          const checkpoint = materialize
            ? await this.store.checkpointDeferredNodes()
            : { serializedNodes: 0, serializedBytes: 0, checkpointMs: 0 };
          const collapseStartedAt = performance.now();
          const collapsed = collapseDecodedArena
            ? trie.collapseHydratedChildren(boundedRetainDepth)
            : { retainedNodes: verification.verifiedNodes, collapsedNodes: 0 };
          const collapseMs = collapseDecodedArena
            ? performance.now() - collapseStartedAt
            : 0;
          const rootAfter = Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT);
          if (!rootAfter.equals(rootBefore)) {
            throw new Error(
              `Decoded-arena collapse changed MPF root: before=${rootBefore.toString("hex")},after=${rootAfter.toString("hex")}`,
            );
          }
          if (this.store.diagnostics().levelBatchWrites !== writesBefore) {
            throw new Error(
              "Decoded-arena checkpoint performed a durable Level write",
            );
          }
          const checkpointMs = performance.now() - checkpointStartedAt;
          if (!materialize) {
            this.store.recordLiveArenaCheckpoint(checkpointMs);
          }
          return {
            checkpointMs,
            authenticationMs,
            materializeMs: checkpoint.checkpointMs,
            collapseMs,
            serializedNodes: checkpoint.serializedNodes,
            serializedBytes: checkpoint.serializedBytes,
            verifiedUpperNodes:
              transientVerification.verifiedNodes + verification.verifiedNodes,
            retainedUpperNodes: collapsed.retainedNodes,
            collapsedNodes: collapsed.collapsedNodes,
          };
        } catch (cause) {
          try {
            const baseRoot =
              await this.store.poisonOverlayAfterMutationFailure();
            this.trie.hash = Buffer.from(baseRoot);
          } catch {
            // Preserve the checkpoint error; recovery reopens from the durable marker.
          }
          throw cause;
        }
      },
      catch: (cause) =>
        MpfError.get(`${this.trieName} decoded arena checkpoint`, cause),
    });
  }

  public authenticateDecodedArena(
    retainDepth = 2,
  ): Effect.Effect<
    { readonly verifiedNodes: number; readonly authenticationMs: number },
    MpfError
  > {
    const trie = this.trie as Trie & {
      assertHydratedNodeHashes?: (maxDepth: number) => {
        readonly verifiedNodes: number;
      };
    };
    return Effect.tryPromise({
      try: async () => {
        try {
          if (trie.assertHydratedNodeHashes === undefined) {
            throw new Error(
              "Patched MPF trie does not expose authenticated arena verification",
            );
          }
          const startedAt = performance.now();
          const verification = trie.assertHydratedNodeHashes(
            Math.max(0, Math.min(64, Math.floor(retainDepth))),
          );
          return {
            verifiedNodes: verification.verifiedNodes,
            authenticationMs: performance.now() - startedAt,
          };
        } catch (cause) {
          try {
            const baseRoot =
              await this.store.poisonOverlayAfterMutationFailure();
            this.trie.hash = Buffer.from(baseRoot);
          } catch {
            // Preserve the authentication error; recovery uses the durable marker.
          }
          throw cause;
        }
      },
      catch: (cause) =>
        MpfError.get(`${this.trieName} decoded arena verification`, cause),
    });
  }

  public beginBlockOverlay(): Effect.Effect<void, MpfError> {
    return Effect.try({
      try: () => {
        this.eventFlatArena = undefined;
        this.store.beginOverlay();
      },
      catch: (e) => MpfError.batch(this.trieName, e),
    });
  }

  public flushBlockOverlay(root: Buffer): Effect.Effect<void, MpfError> {
    return Effect.gen(this, function* () {
      yield* Effect.try({
        try: () => {
          if (this.eventFlatArena !== undefined) {
            if (!this.eventFlatArena.rootHash().equals(root)) {
              throw new Error("Event-flat flush root does not match its arena");
            }
            this.store.stageEventFlatCandidate(
              root,
              this.eventFlatArena.reachableDirtyRecords(),
            );
            return;
          }
          this.store.captureCurrentLiveTrie(
            this.trie as unknown as MpfSerializableValue,
          );
        },
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      yield* Effect.tryPromise({
        try: () => this.store.flushOverlay(root),
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      const reloaded = yield* MidgardMpf.loadFromRootView({
        trieName: this.trieName,
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: this.level !== undefined,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
      });
      Object.assign(this, reloaded);
      this.eventFlatArena = undefined;
    });
  }

  public parkBlockOverlay(): Effect.Effect<ParkedMpfOverlayV1, MpfError> {
    return Effect.gen(this, function* () {
      const root = yield* this.root();
      yield* Effect.try({
        try: () => {
          if (this.eventFlatArena !== undefined) {
            this.store.stageEventFlatCandidate(
              root,
              this.eventFlatArena.reachableDirtyRecords(),
            );
            return;
          }
          this.store.captureCurrentLiveTrie(
            this.trie as unknown as MpfSerializableValue,
          );
        },
        catch: (cause) => MpfError.batch(`${this.trieName}-park`, cause),
      });
      const artifact = yield* Effect.tryPromise({
        try: () => this.store.parkCurrentOverlay(root, this.trieName),
        catch: (cause) => MpfError.batch(`${this.trieName}-park`, cause),
      });
      yield* Effect.tryPromise({
        try: () =>
          this.store.shouldCloseLevel()
            ? (this.level?.close() ?? Promise.resolve())
            : Promise.resolve(),
        catch: (cause) => MpfError.close(`${this.trieName}-park`, cause),
      });
      this.eventFlatArena = undefined;
      return artifact;
    });
  }

  public parkEventFlatOverlayV1(
    shardCount = 4,
  ): Effect.Effect<ParkedEventFlatOverlayV1, MpfError> {
    return Effect.gen(this, function* () {
      if (this.engine !== "event_flat" || this.eventFlatArena === undefined) {
        return yield* Effect.fail(
          MpfError.batch(
            `${this.trieName}-event-flat-park`,
            new Error("Event-flat V1 park requires an active packed arena"),
          ),
        );
      }
      const baseRoot = this.store.overlayStartingRoot();
      if (baseRoot === undefined) {
        return yield* Effect.fail(
          MpfError.batch(
            `${this.trieName}-event-flat-park`,
            new Error("Event-flat V1 park requires an active overlay"),
          ),
        );
      }
      const writesBefore = this.store.diagnostics().levelBatchWrites;
      const frozen = yield* Effect.either(
        Effect.tryPromise({
          try: () =>
            this.eventFlatArena!.freezeParallel({
              trieName: this.trieName,
              baseRoot,
              shardCount,
            }),
          catch: (cause) =>
            MpfError.batch(`${this.trieName}-event-flat-park`, cause),
        }),
      );
      if (frozen._tag === "Left") {
        yield* Effect.tryPromise({
          try: () => this.store.poisonOverlayAfterMutationFailure(),
          catch: (cause) =>
            MpfError.batch(`${this.trieName}-event-flat-park`, cause),
        }).pipe(Effect.catchAll(() => Effect.void));
        return yield* Effect.fail(frozen.left);
      }
      if (this.store.diagnostics().levelBatchWrites !== writesBefore) {
        return yield* Effect.fail(
          MpfError.batch(
            `${this.trieName}-event-flat-park`,
            new Error("Event-flat V1 park performed a Level write"),
          ),
        );
      }
      yield* Effect.try({
        try: () => this.store.discardOverlay(),
        catch: (cause) =>
          MpfError.batch(`${this.trieName}-event-flat-park`, cause),
      });
      yield* Effect.tryPromise({
        try: () =>
          this.store.shouldCloseLevel()
            ? (this.level?.close() ?? Promise.resolve())
            : Promise.resolve(),
        catch: (cause) =>
          MpfError.close(`${this.trieName}-event-flat-park`, cause),
      });
      this.eventFlatArena = undefined;
      return frozen.right;
    });
  }

  public static resumeParkedEventFlatOverlayV1(
    trieName: string,
    levelDBFilePath: string | undefined,
    artifact: ParkedEventFlatOverlayV1,
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      yield* Effect.tryPromise({
        try: () => prepareEventFlatDigest(),
        catch: (cause) =>
          MpfError.create(`${trieName}-event-flat-digest`, cause),
      });
      if (artifact.trieName !== trieName) {
        return yield* Effect.fail(
          MpfError.create(
            trieName,
            new Error(
              `Parked event-flat trie mismatch: expected=${trieName},actual=${artifact.trieName}`,
            ),
          ),
        );
      }
      const resumedView = yield* Effect.try({
        try: () => new ResumedEventFlatOverlayV1(artifact),
        catch: (cause) =>
          MpfError.create(`${trieName}-event-flat-resume`, cause),
      });
      const mpf = yield* MidgardMpf.create(trieName, levelDBFilePath, {
        engine: "overlay",
      });
      const resumed = yield* Effect.either(
        Effect.gen(function* () {
          const durableRoot = yield* mpf.root();
          const parkedBase = Buffer.from(artifact.baseRoot);
          if (!durableRoot.equals(parkedBase)) {
            return yield* Effect.fail(
              MpfError.create(
                trieName,
                new Error(
                  `Parked event-flat durable base changed: durable=${durableRoot.toString("hex")},parked=${parkedBase.toString("hex")}`,
                ),
              ),
            );
          }
          yield* mpf.beginBlockOverlay();
          const candidateRoot = resumedView.rootHash();
          yield* Effect.try({
            try: () =>
              mpf.store.stageEventFlatCandidate(
                candidateRoot,
                resumedView.flushReadyRecords(),
              ),
            catch: (cause) =>
              MpfError.batch(`${trieName}-event-flat-resume`, cause),
          });
          const candidateTrie = yield* Effect.tryPromise({
            try: () =>
              candidateRoot.equals(MPF_EMPTY_ROOT)
                ? Promise.resolve(new Trie(mpf.store))
                : Trie.load(mpf.store),
            catch: (cause) =>
              MpfError.create(`${trieName}-event-flat-resume`, cause),
          });
          Object.assign(mpf, { trie: candidateTrie });
          if (!(yield* mpf.root()).equals(candidateRoot)) {
            return yield* Effect.fail(
              MpfError.create(
                `${trieName}-event-flat-resume`,
                new Error("Parked event-flat candidate root is unreadable"),
              ),
            );
          }
          return mpf;
        }),
      );
      if (resumed._tag === "Right") return resumed.right;
      yield* mpf.close().pipe(Effect.catchAll(() => Effect.void));
      return yield* Effect.fail(resumed.left);
    });
  }

  public static resumeParkedOverlay(
    trieName: string,
    levelDBFilePath: string | undefined,
    artifact: ParkedMpfOverlayV1,
  ): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(function* () {
      if (artifact.trieName !== trieName) {
        return yield* Effect.fail(
          MpfError.create(
            trieName,
            new Error(
              `Parked MPF trie mismatch: expected=${trieName},actual=${artifact.trieName}`,
            ),
          ),
        );
      }
      const mpf = yield* MidgardMpf.create(trieName, levelDBFilePath, {
        engine: "overlay",
      });
      const resumed = yield* Effect.either(
        Effect.gen(function* () {
          const durableRoot = yield* mpf.root();
          const parkedBase = Buffer.from(artifact.baseRoot);
          if (!durableRoot.equals(parkedBase)) {
            return yield* Effect.fail(
              MpfError.create(
                trieName,
                new Error(
                  `Parked MPF durable base changed: durable=${durableRoot.toString("hex")},parked=${parkedBase.toString("hex")}`,
                ),
              ),
            );
          }
          yield* mpf.beginBlockOverlay();
          const candidateRoot = yield* Effect.tryPromise({
            try: () => mpf.store.importParkedOverlay(artifact),
            catch: (cause) => MpfError.batch(`${trieName}-resume`, cause),
          });
          const candidateTrie = yield* Effect.tryPromise({
            try: () =>
              candidateRoot.equals(MPF_EMPTY_ROOT)
                ? Promise.resolve(new Trie(mpf.store))
                : Trie.load(mpf.store),
            catch: (cause) => MpfError.create(`${trieName}-resume`, cause),
          });
          Object.assign(mpf, { trie: candidateTrie });
          if (!(yield* mpf.root()).equals(candidateRoot)) {
            return yield* Effect.fail(
              MpfError.create(
                `${trieName}-resume`,
                new Error("Parked MPF candidate root is unreadable"),
              ),
            );
          }
          return mpf;
        }),
      );
      if (resumed._tag === "Right") return resumed.right;
      yield* mpf.close().pipe(Effect.catchAll(() => Effect.void));
      return yield* Effect.fail(resumed.left);
    });
  }

  public static promoteParkedOverlay(
    trieName: string,
    levelDBFilePath: string | undefined,
    artifact: ParkedMpfOverlayV1,
  ): Effect.Effect<void, MpfError> {
    return Effect.gen(function* () {
      const mpf = yield* MidgardMpf.resumeParkedOverlay(
        trieName,
        levelDBFilePath,
        artifact,
      );
      const promoted = yield* Effect.either(
        mpf.flushBlockOverlay(Buffer.from(artifact.candidateRoot)),
      );
      yield* mpf.close().pipe(Effect.catchAll(() => Effect.void));
      if (promoted._tag === "Left") return yield* Effect.fail(promoted.left);
    });
  }

  public discardBlockOverlay(): Effect.Effect<void, MpfError> {
    return Effect.gen(this, function* () {
      this.eventFlatArena = undefined;
      yield* Effect.tryPromise({
        try: () => this.store.waitForSpills(),
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      const root = yield* Effect.try({
        try: () => this.store.discardOverlay(),
        catch: (e) => MpfError.batch(this.trieName, e),
      });
      if (this.parentStore !== undefined) {
        this.trie.hash = Buffer.from(root);
        return;
      }
      const reloaded = yield* MidgardMpf.loadFromRootView({
        trieName: this.trieName,
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: this.level !== undefined,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
        parentStore: this.parentStore,
        parentOverlay: this.parentOverlay,
      });
      Object.assign(this, reloaded);
    });
  }

  public discardBlockOverlayIfActive(): Effect.Effect<void, MpfError> {
    return this.store.overlayIsActive()
      ? this.discardBlockOverlay()
      : Effect.void;
  }

  public blockOverlayIsActive(): boolean {
    return this.store.overlayIsActive();
  }

  public usesStrictOverlayMutations(): boolean {
    return this.engine !== "legacy" && this.store.overlayIsActive();
  }

  public usesEventFlatEngine(): boolean {
    return this.engine === "event_flat";
  }

  public eventFlatMutationDiagnostics():
    | EventFlatMutationDiagnostics
    | undefined {
    return this.eventFlatArena?.diagnostics();
  }

  public spillIfNeeded(): Effect.Effect<void, MpfError> {
    return Effect.tryPromise({
      try: () => this.store.spillIfNeeded(),
      catch: (e) => MpfError.batch(this.trieName, e),
    });
  }

  public ledgerOverlayHandle(): Effect.Effect<LedgerOverlayHandle, MpfError> {
    return Effect.gen(this, function* () {
      if (!this.store.overlayIsActive()) {
        yield* this.beginBlockOverlay();
      }
      return new MidgardLedgerOverlayHandle(this);
    });
  }

  public forkBlockOverlay(): Effect.Effect<MidgardMpf, MpfError> {
    return Effect.gen(this, function* () {
      yield* Effect.try({
        try: () =>
          this.store.captureCurrentLiveTrie(
            this.trie as unknown as MpfSerializableValue,
          ),
        catch: (e) => MpfError.batch(`${this.trieName}-fork`, e),
      });
      const root = yield* this.root();
      const rootAvailable = yield* Effect.tryPromise({
        try: () => this.store.hasStoredNode(root),
        catch: (e) => MpfError.get(this.trieName, e),
      });
      if (!rootAvailable) {
        return yield* Effect.fail(
          MpfError.create(
            `${this.trieName}-fork`,
            new Error(
              `Current overlay root ${root.toString("hex")} is not readable from its parent store`,
            ),
          ),
        );
      }
      const forkStore = new MidgardMpfRootViewStore({
        level: this.level,
        memory: this.memory,
        root,
        persistRootMarker: false,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
        parentStore: this.store,
        parentOverlay: this.store.currentOverlayView(),
      });
      const forkTrie = Object.assign(
        Object.create(Object.getPrototypeOf(this.trie)) as Trie,
        this.trie,
        {
          hash: Buffer.from(this.trie.hash ?? MPF_EMPTY_ROOT),
          store: forkStore,
        },
      );
      const mutableForkTrie = forkTrie as Trie & {
        children?: Array<{ hash: Buffer } | undefined>;
        key?: Buffer;
        value?: Buffer;
        __midgardMerkleNodes?: unknown;
        __midgardDirtyChild?: unknown;
      };
      if (mutableForkTrie.children !== undefined) {
        mutableForkTrie.children = mutableForkTrie.children.map((child) =>
          child === undefined ? undefined : { hash: Buffer.from(child.hash) },
        );
      }
      if (mutableForkTrie.key !== undefined) {
        mutableForkTrie.key = Buffer.from(mutableForkTrie.key);
      }
      if (mutableForkTrie.value !== undefined) {
        mutableForkTrie.value = Buffer.from(mutableForkTrie.value);
      }
      delete mutableForkTrie.__midgardMerkleNodes;
      delete mutableForkTrie.__midgardDirtyChild;
      const fork = new MidgardMpf({
        trie: forkTrie,
        // A fork is a lifecycle view of the same logical trie. Parked
        // artifacts must retain the durable store identity so a fresh process
        // can resume them after the owning parent closes.
        trieName: this.trieName,
        store: forkStore,
        level: this.level,
        memory: this.memory,
        engine: this.engine,
        spillThresholdBytes: this.spillThresholdBytes,
        parentStore: this.store,
        parentOverlay: this.store.currentOverlayView(),
      });
      yield* fork.beginBlockOverlay();
      return fork;
    });
  }

  private persistRootMarker(root: Buffer): Effect.Effect<void, MpfError> {
    return Effect.tryPromise({
      try: async () => {
        this.store.setRoot(root);
        if (this.level !== undefined) {
          await this.level.put(
            ROOT_KEY,
            normalizeStoredRootHex(root.toString("hex")),
            JSON_LEVEL_ENCODING_OPTS,
          );
        }
      },
      catch: (e) => MpfError.create(this.trieName, e),
    });
  }
}

/**
 * Block-scoped ledger overlay contract consumed by Phase 4 pipelining.
 *
 * G1: after `promote`, the durable root marker and retained working root equal
 * the root returned by `rootHex`. G2: `fork` is O(1): the fork reads through
 * its immutable parent overlay and owns only its subsequent delta. G3: block
 * deltas are plain `MpfBatchOp` values and can cross a worker boundary.
 */
export interface LedgerOverlayHandle {
  rootHex(): Effect.Effect<string, MpfError>;
  fork(): Effect.Effect<LedgerOverlayHandle, MpfError>;
  applyBlockDelta(ops: readonly MpfBatchOp[]): Effect.Effect<string, MpfError>;
  promote(): Effect.Effect<void, MpfError>;
  discard(): Effect.Effect<void, MpfError>;
}

class MidgardLedgerOverlayHandle implements LedgerOverlayHandle {
  constructor(private readonly mpf: MidgardMpf) {}

  rootHex() {
    return this.mpf.rootHex();
  }

  fork(): Effect.Effect<LedgerOverlayHandle, MpfError> {
    return this.mpf
      .forkBlockOverlay()
      .pipe(Effect.map((fork) => new MidgardLedgerOverlayHandle(fork)));
  }

  applyBlockDelta(ops: readonly MpfBatchOp[]) {
    return this.mpf
      .applyBatch(ops)
      .pipe(Effect.map((root) => root.toString("hex")));
  }

  promote() {
    return Effect.gen(this, function* () {
      const root = yield* this.mpf.root();
      yield* this.mpf.flushBlockOverlay(root);
    });
  }

  discard() {
    return this.mpf.discardBlockOverlay();
  }
}

const readPersistedRoot = (
  level: Level<string, MpfStoredValue>,
): Effect.Effect<Buffer, MpfError> =>
  Effect.tryPromise({
    try: async () => {
      const rootHex = await level.get(ROOT_KEY, JSON_LEVEL_ENCODING_OPTS);
      return parseStoredRootHex(rootHex);
    },
    catch: (e) => MpfError.rootNotSet("persisted", e),
  });

export const emptyRootHexProgram: Effect.Effect<string, MpfError> =
  Effect.succeed(MPF_EMPTY_ROOT_HEX);
