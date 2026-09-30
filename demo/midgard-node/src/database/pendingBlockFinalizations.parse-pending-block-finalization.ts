import { Effect } from "effect";

import { sha256 } from "../sha256.js";
import * as DepositsDB from "./deposits.js";
import {
  Columns,
  MemberColumns,
  type MemberRecord,
  PENDING_BLOCK_FINALIZATION_VERSION,
  PendingBlockFinalizationReplayKind,
  type Row,
  UtxoColumns,
} from "./pendingBlockFinalizations.columns.js";
import {
  type LedgerDeltaInput,
  type NativeMpfReplayInput,
  parseLedgerDelta,
  type PendingBlockFinalization,
  type PendingBlockFinalizationMetadata,
} from "./pendingBlockFinalizations.parse-ledger-delta.js";
import {
  parseNativeMpfReplay,
  parsePendingBlockFinalizationMetadata,
} from "./pendingBlockFinalizations.parse-pending-block-finalization-metadata.js";
import { DatabaseError } from "./utils/common.js";
import { exactRecord } from "./utils/exact-record.js";
import * as TxTable from "./utils/tx.js";

export const parsePendingBlockFinalization = (
  value: unknown,
): PendingBlockFinalization => {
  const candidate = exactRecord(
    value,
    ["version", "metadata", "replay"],
    "PendingBlockFinalizationV1",
  );
  if (candidate.version !== PENDING_BLOCK_FINALIZATION_VERSION) {
    throw new Error(
      `PendingBlockFinalizationV1 version must equal ${PENDING_BLOCK_FINALIZATION_VERSION.toString()}`,
    );
  }
  const metadata = parsePendingBlockFinalizationMetadata(candidate.metadata);
  const replayCandidate = exactRecord(
    candidate.replay,
    (candidate.replay as { readonly kind?: unknown })?.kind ===
      PendingBlockFinalizationReplayKind.LedgerDeltaWithNativeMpf
      ? ["kind", "ledgerDelta", "nativeMpfReplay"]
      : ["kind", "ledgerDelta"],
    "PendingBlockFinalizationV1 replay",
  );
  const ledgerDelta = parseLedgerDelta(replayCandidate.ledgerDelta);
  if (replayCandidate.kind === PendingBlockFinalizationReplayKind.LedgerDelta) {
    return {
      version: PENDING_BLOCK_FINALIZATION_VERSION,
      metadata,
      replay: {
        kind: PendingBlockFinalizationReplayKind.LedgerDelta,
        ledgerDelta,
      },
    };
  }
  if (
    replayCandidate.kind !==
    PendingBlockFinalizationReplayKind.LedgerDeltaWithNativeMpf
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 replay kind must be an exact V1 discriminator",
    );
  }
  const nativeMpfReplay = parseNativeMpfReplay(replayCandidate.nativeMpfReplay);
  if (
    nativeMpfReplay.baseRoot.toString("hex") !== metadata.baseRoots.utxosRoot ||
    nativeMpfReplay.candidateRoot.toString("hex") !==
      metadata.expectedRoots.utxosRoot
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 native MPF replay roots do not match metadata",
    );
  }
  return {
    version: PENDING_BLOCK_FINALIZATION_VERSION,
    metadata,
    replay: {
      kind: PendingBlockFinalizationReplayKind.LedgerDeltaWithNativeMpf,
      ledgerDelta,
      nativeMpfReplay,
    },
  };
};

export const decodeNativeMpfReplay = (
  row: Row,
): NativeMpfReplayInput | undefined => {
  const values = [
    row[Columns.MPF_OWNER_SCHEMA],
    row[Columns.MPF_OWNER_BINARY_SHA256],
    row[Columns.MPF_REPLAY_BASE_ROOT],
    row[Columns.MPF_REPLAY_CANDIDATE_ROOT],
    row[Columns.MPF_REPLAY_EVENT_LOG],
    row[Columns.MPF_REPLAY_EVENT_LOG_DIGEST],
    row[Columns.MPF_REPLAY_EVENT_ROOTS],
    row[Columns.MPF_REPLAY_EVENT_COUNT],
  ];
  if (
    row[Columns.REPLAY_KIND] === PendingBlockFinalizationReplayKind.LedgerDelta
  ) {
    if (values.some((value) => value != null)) {
      throw new Error(
        "PendingBlockFinalizationV1 delta-only replay contains native MPF fields",
      );
    }
    return undefined;
  }
  if (
    row[Columns.REPLAY_KIND] !==
    PendingBlockFinalizationReplayKind.LedgerDeltaWithNativeMpf
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 persisted replay kind is unsupported",
    );
  }
  if (values.some((value) => value == null)) {
    throw new Error(
      "PendingBlockFinalizationV1 native MPF replay fields are partially null",
    );
  }
  const replay: NativeMpfReplayInput = {
    schema: row[Columns.MPF_OWNER_SCHEMA] as 1,
    ownerBinarySha256: Buffer.from(row[Columns.MPF_OWNER_BINARY_SHA256]!),
    baseRoot: Buffer.from(row[Columns.MPF_REPLAY_BASE_ROOT]!),
    candidateRoot: Buffer.from(row[Columns.MPF_REPLAY_CANDIDATE_ROOT]!),
    eventLog: Buffer.from(row[Columns.MPF_REPLAY_EVENT_LOG]!),
    eventLogDigest: Buffer.from(row[Columns.MPF_REPLAY_EVENT_LOG_DIGEST]!),
    eventRoots: Buffer.from(row[Columns.MPF_REPLAY_EVENT_ROOTS]!),
    eventCount: row[Columns.MPF_REPLAY_EVENT_COUNT]!,
  };
  return parseNativeMpfReplay(replay);
};

const decodeHexArray = (value: unknown, label: string): readonly Buffer[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must be an array of exact lowercase hex strings`);
  }
  return value.map((item, index) =>
    Buffer.from(
      exactNonEmptyCanonicalHex(item, `${label}[${index.toString()}]`),
      "hex",
    ),
  );
};

const exactNonEmptyCanonicalHex = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.length % 2 !== 0 ||
    !/^[0-9a-f]+$/u.test(value)
  ) {
    throw new Error(`${label} must be non-empty even-length lowercase hex`);
  }
  return value;
};

export const decodeLedgerDelta = (row: Row): LedgerDeltaInput => {
  const parseJson = (value: unknown): unknown =>
    typeof value === "string" ? JSON.parse(value) : value;
  const spent = parseJson(row[Columns.LEDGER_DELTA_SPENT]);
  const produced = parseJson(row[Columns.LEDGER_DELTA_PRODUCED]);
  if (!Array.isArray(produced)) {
    throw new Error("ledger_delta_produced must be an array");
  }
  return parseLedgerDelta({
    spent: decodeHexArray(spent, "ledger_delta_spent"),
    produced: produced.map((entry) => {
      const candidate = exactRecord(
        entry,
        ["outref", "output"],
        "ledger_delta_produced entry",
      );
      return {
        [UtxoColumns.OUTREF]: Buffer.from(
          exactNonEmptyCanonicalHex(
            candidate.outref,
            "ledger_delta_produced.outref",
          ),
          "hex",
        ),
        [UtxoColumns.OUTPUT]: Buffer.from(
          exactNonEmptyCanonicalHex(
            candidate.output,
            "ledger_delta_produced.output",
          ),
          "hex",
        ),
      };
    }),
  });
};

export const pendingBlockFinalizationMetadataFromRow = (
  row: Row,
): PendingBlockFinalizationMetadata => ({
  deploymentMarker: {
    schemaVersion: row[Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION],
    manifestId: row[Columns.DEPLOYMENT_MANIFEST_ID],
  },
  consensusProfileId: row[Columns.CONSENSUS_PROFILE_ID],
  stateQueueLeaseToken: row[Columns.STATE_QUEUE_LEASE_TOKEN],
  baseSnapshotId: row[Columns.BASE_SNAPSHOT_ID],
  baseTailOutRef: row[Columns.BASE_TAIL_OUT_REF],
  baseTailHeaderHash: row[Columns.BASE_TAIL_HEADER_HASH],
  baseTailDatumCbor: row[Columns.BASE_TAIL_DATUM_CBOR],
  baseRoots: {
    utxosRoot: row[Columns.BASE_UTXOS_ROOT],
    forcedTransactionsRoot: row[Columns.BASE_FORCED_TRANSACTIONS_ROOT],
    transactionsRoot: row[Columns.BASE_TRANSACTIONS_ROOT],
    depositsRoot: row[Columns.BASE_DEPOSITS_ROOT],
    withdrawalsRoot: row[Columns.BASE_WITHDRAWALS_ROOT],
  },
  blockStartTime: row[Columns.BLOCK_START_TIME],
  expectedRoots: {
    utxosRoot: row[Columns.EXPECTED_UTXOS_ROOT],
    forcedTransactionsRoot: row[Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT],
    transactionsRoot: row[Columns.EXPECTED_TRANSACTIONS_ROOT],
    depositsRoot: row[Columns.EXPECTED_DEPOSITS_ROOT],
    withdrawalsRoot: row[Columns.EXPECTED_WITHDRAWALS_ROOT],
    transitionTraceRoot: row[Columns.EXPECTED_TRANSITION_TRACE_ROOT],
    eventToStepRoot: row[Columns.EXPECTED_EVENT_TO_STEP_ROOT],
    validationTracesRoot: row[Columns.EXPECTED_VALIDATION_TRACES_ROOT],
  },
  expectedCounts: {
    withdrawalCount: row[Columns.EXPECTED_WITHDRAWAL_COUNT],
    forcedTransactionCount: row[Columns.EXPECTED_FORCED_TRANSACTION_COUNT],
    l2TransactionCount: row[Columns.EXPECTED_L2_TRANSACTION_COUNT],
    depositCount: row[Columns.EXPECTED_DEPOSIT_COUNT],
    totalEventCount: row[Columns.EXPECTED_TOTAL_EVENT_COUNT],
    transitionStepCount: row[Columns.EXPECTED_TRANSITION_STEP_COUNT],
    validationTraceCount: row[Columns.EXPECTED_VALIDATION_TRACE_COUNT],
  },
});

export const assertSameIdSet = (
  table: string,
  label: string,
  expected: readonly Buffer[],
  actual: readonly Buffer[],
): Effect.Effect<void, DatabaseError> =>
  Effect.gen(function* () {
    const expectedSet = new Set(expected.map((value) => value.toString("hex")));
    const actualSet = new Set(actual.map((value) => value.toString("hex")));
    if (
      expectedSet.size !== actualSet.size ||
      [...expectedSet].some((hex) => !actualSet.has(hex))
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table,
          message: `Refusing to prepare pending journal because ${label} ids do not match the provided payload entries`,
          cause: `expected=[${[...expectedSet].join(",")}],actual=[${[
            ...actualSet,
          ].join(",")}]`,
        }),
      );
    }
  });

export const txMemberEntry = (
  headerHash: Buffer,
  entry: TxTable.EntryWithTimeStamp,
  ordinal: number,
  sourceTable: string,
  programMaterialSidecarCbor?: Buffer,
): MemberRecord => {
  const payload = Buffer.from(entry[TxTable.Columns.TX]);
  const memberId = Buffer.from(entry[TxTable.Columns.TX_ID]);
  return {
    [MemberColumns.HEADER_HASH]: headerHash,
    [MemberColumns.MEMBER_ID]: memberId,
    [MemberColumns.ORDINAL]: ordinal,
    [MemberColumns.PAYLOAD_CBOR]: payload,
    [MemberColumns.PAYLOAD_SHA256]: sha256(payload),
    ...(programMaterialSidecarCbor === undefined
      ? {}
      : {
          [MemberColumns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]: Buffer.from(
            programMaterialSidecarCbor,
          ),
          [MemberColumns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]: sha256(
            programMaterialSidecarCbor,
          ),
        }),
    [MemberColumns.SOURCE_TABLE]: sourceTable,
    [MemberColumns.SOURCE_ID]: memberId,
    [MemberColumns.SOURCE_TIMESTAMP]: entry[TxTable.Columns.TIMESTAMPTZ],
  };
};

export const depositMemberEntry = (
  headerHash: Buffer,
  entry: DepositsDB.Entry,
  ordinal: number,
): MemberRecord => {
  const payload = Buffer.from(entry[DepositsDB.Columns.INFO]);
  const memberId = Buffer.from(entry[DepositsDB.Columns.ID]);
  return {
    [MemberColumns.HEADER_HASH]: headerHash,
    [MemberColumns.MEMBER_ID]: memberId,
    [MemberColumns.ORDINAL]: ordinal,
    [MemberColumns.PAYLOAD_CBOR]: payload,
    [MemberColumns.PAYLOAD_SHA256]: sha256(payload),
    [MemberColumns.SOURCE_TABLE]: DepositsDB.tableName,
    [MemberColumns.SOURCE_ID]: memberId,
    [MemberColumns.SOURCE_TIMESTAMP]: entry[DepositsDB.Columns.INCLUSION_TIME],
  };
};
