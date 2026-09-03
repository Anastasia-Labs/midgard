import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import * as Ledger from "../src/database/utils/ledger.js";
import {
  materializeConfirmedLedgerDeltaChainV1,
  pendingUtxoMemberToConfirmedLedgerEntry,
} from "../src/transactions/state-queue/confirmed-ledger-snapshot.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/workers/utils/mpf.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

const VALID_ADDRESS =
  "addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs";
const EMPTY_ROOT = SDK.EMPTY_MERKLE_TREE_ROOT;
const BLOCK_START = new Date("2026-06-19T00:00:00.000Z");
const BLOCK_END = new Date("2026-06-19T00:00:10.000Z");

const txOutRefCbor = (txHashByte: string, outputIndex: bigint) =>
  makeOutRefCbor(txHashByte.repeat(32), outputIndex);

const utxo = (
  txHashByte: string,
  outputIndex: bigint,
  lovelace: bigint,
): PendingBlockFinalizationsDB.UtxoInput => ({
  [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]: txOutRefCbor(
    txHashByte,
    outputIndex,
  ),
  [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]: Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(VALID_ADDRESS),
      CML.Value.from_coin(lovelace),
    ).to_cbor_bytes(),
  ),
});

const ledgerEntry = (
  member: PendingBlockFinalizationsDB.UtxoInput,
): Promise<Ledger.Entry> =>
  Effect.runPromise(pendingUtxoMemberToConfirmedLedgerEntry(member));

const ledgerRoot = (entries: readonly Ledger.Entry[]): Promise<string> =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries(entries));

const record = ({
  headerByte,
  baseHeaderByte,
  baseRoot,
  expectedRoot,
  spent = [],
  produced = [],
}: {
  readonly headerByte: number;
  readonly baseHeaderByte: number;
  readonly baseRoot: string;
  readonly expectedRoot: string;
  readonly spent?: readonly Buffer[];
  readonly produced?: readonly PendingBlockFinalizationsDB.UtxoInput[];
}): PendingBlockFinalizationsDB.Record => ({
  [PendingBlockFinalizationsDB.Columns.HEADER_HASH]: Buffer.alloc(
    28,
    headerByte,
  ),
  [PendingBlockFinalizationsDB.Columns.HEADER_CBOR]: Buffer.from(
    "d87980",
    "hex",
  ),
  [PendingBlockFinalizationsDB.Columns.FORMAT_VERSION]:
    PendingBlockFinalizationsDB.PENDING_BLOCK_FINALIZATION_V1_VERSION,
  [PendingBlockFinalizationsDB.Columns.REPLAY_KIND]:
    PendingBlockFinalizationsDB.PendingBlockFinalizationReplayKindV1
      .LedgerDelta,
  [PendingBlockFinalizationsDB.Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION]:
    "midgard-deployment-marker-v1",
  [PendingBlockFinalizationsDB.Columns.DEPLOYMENT_MANIFEST_ID]: "de".repeat(32),
  [PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID]:
    "midgard-consensus-v1",
  [PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]: Buffer.alloc(32, 2),
  [PendingBlockFinalizationsDB.Columns.STATE_QUEUE_LEASE_TOKEN]: "lease",
  [PendingBlockFinalizationsDB.Columns.BASE_SNAPSHOT_ID]: "snapshot",
  [PendingBlockFinalizationsDB.Columns.BASE_TAIL_OUT_REF]:
    `${baseHeaderByte.toString(16).padStart(2, "0").repeat(32)}#0`,
  [PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH]: Buffer.alloc(
    28,
    baseHeaderByte,
  ),
  [PendingBlockFinalizationsDB.Columns.BASE_TAIL_DATUM_CBOR]: "d87980",
  [PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT]: baseRoot,
  [PendingBlockFinalizationsDB.Columns.BASE_FORCED_TRANSACTIONS_ROOT]:
    EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.BASE_TRANSACTIONS_ROOT]: EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.BASE_DEPOSITS_ROOT]: EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.BASE_WITHDRAWALS_ROOT]: EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME]: BLOCK_START,
  [PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME]: BLOCK_END,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]: expectedRoot,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT]:
    EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT]: EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT]: EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT]: EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_TRACE_ROOT]:
    EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_EVENT_TO_STEP_ROOT]: EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACES_ROOT]:
    EMPTY_ROOT,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWAL_COUNT]: 0n,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTION_COUNT]: 0n,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_L2_TRANSACTION_COUNT]: 0n,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSIT_COUNT]: 0n,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT]: 0n,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_STEP_COUNT]: 0n,
  [PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT]: 0n,
  [PendingBlockFinalizationsDB.Columns.LEDGER_DELTA_SPENT]: spent.map((item) =>
    item.toString("hex"),
  ),
  [PendingBlockFinalizationsDB.Columns.LEDGER_DELTA_PRODUCED]: produced.map(
    (item) => ({
      outref:
        item[PendingBlockFinalizationsDB.UtxoColumns.OUTREF].toString("hex"),
      output:
        item[PendingBlockFinalizationsDB.UtxoColumns.OUTPUT].toString("hex"),
    }),
  ),
  [PendingBlockFinalizationsDB.Columns.STATUS]:
    PendingBlockFinalizationsDB.Status.Finalized,
  [PendingBlockFinalizationsDB.Columns.OBSERVED_CONFIRMED_AT_MS]: 1n,
  [PendingBlockFinalizationsDB.Columns.CREATED_AT]: BLOCK_START,
  [PendingBlockFinalizationsDB.Columns.UPDATED_AT]: BLOCK_END,
  depositEventIds: [],
  forcedTransactionEventIds: [],
  withdrawalEventIds: [],
  mempoolTxIds: [],
  depositMembers: [],
  forcedTransactionMembers: [],
  withdrawalMembers: [],
  txMembers: [],
  transitionTraceMembers: [],
  eventToStepMembers: [],
  validationTraceMembers: [],
  validationTraceWitnessMembers: [],
  ledgerDelta: { spent, produced },
});

describe("confirmed ledger finalized-journal delta-chain recovery", () => {
  it("materializes a complete ordered chain from the exact confirmed base", async () => {
    const a = utxo("41", 0n, 2_000_000n);
    const b = utxo("42", 1n, 3_000_000n);
    const c = utxo("43", 2n, 4_000_000n);
    const [aEntry, bEntry, cEntry] = await Promise.all([
      ledgerEntry(a),
      ledgerEntry(b),
      ledgerEntry(c),
    ]);
    const [aRoot, bRoot, cRoot] = await Promise.all([
      ledgerRoot([aEntry]),
      ledgerRoot([bEntry]),
      ledgerRoot([cEntry]),
    ]);
    const parent = record({
      headerByte: 2,
      baseHeaderByte: 1,
      baseRoot: aRoot,
      expectedRoot: bRoot,
      spent: [a[PendingBlockFinalizationsDB.UtxoColumns.OUTREF]],
      produced: [b],
    });
    const child = record({
      headerByte: 3,
      baseHeaderByte: 2,
      baseRoot: bRoot,
      expectedRoot: cRoot,
      spent: [b[PendingBlockFinalizationsDB.UtxoColumns.OUTREF]],
      produced: [c],
    });

    const snapshot = await Effect.runPromise(
      materializeConfirmedLedgerDeltaChainV1({
        record: child,
        confirmedEntries: [aEntry],
        retrieveParent: (headerHash) =>
          Effect.succeed(
            headerHash.equals(
              parent[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
            )
              ? Option.some(parent)
              : Option.none(),
          ),
      }),
    );

    expect(snapshot.baseRoot).toBe(aRoot);
    expect(snapshot.root).toBe(cRoot);
    expect(snapshot.deltaChain).toHaveLength(2);
    expect(snapshot.entries).toHaveLength(1);
    expect(snapshot.entries[0]?.[Ledger.Columns.TX_ID].toString("hex")).toBe(
      "43".repeat(32),
    );
    expect(snapshot.entries[0]?.[Ledger.Columns.ADDRESS]).toBe(VALID_ADDRESS);
  });

  it("rejects an incomplete chain whose authenticated parent is missing", async () => {
    const current = await ledgerEntry(utxo("51", 0n, 2_000_000n));
    const expected = await ledgerEntry(utxo("52", 0n, 3_000_000n));
    const candidate = record({
      headerByte: 5,
      baseHeaderByte: 4,
      baseRoot: await ledgerRoot([expected]),
      expectedRoot: await ledgerRoot([expected]),
    });

    await expect(
      Effect.runPromise(
        materializeConfirmedLedgerDeltaChainV1({
          record: candidate,
          confirmedEntries: [current],
          retrieveParent: () => Effect.succeed(Option.none()),
        }),
      ),
    ).rejects.toThrow(/parent journal is missing/u);
  });

  it("rejects a cycle before reusing a journal delta", async () => {
    const current = await ledgerEntry(utxo("61", 0n, 2_000_000n));
    const candidate = record({
      headerByte: 6,
      baseHeaderByte: 6,
      baseRoot: await ledgerRoot([
        await ledgerEntry(utxo("62", 0n, 3_000_000n)),
      ]),
      expectedRoot: EMPTY_ROOT,
    });

    await expect(
      Effect.runPromise(
        materializeConfirmedLedgerDeltaChainV1({
          record: candidate,
          confirmedEntries: [current],
          retrieveParent: () => Effect.succeed(Option.some(candidate)),
        }),
      ),
    ).rejects.toThrow(/contains a cycle/u);
  });

  it("rejects a journal substituted for the requested parent identity", async () => {
    const current = await ledgerEntry(utxo("71", 0n, 2_000_000n));
    const expectedBase = await ledgerEntry(utxo("72", 0n, 3_000_000n));
    const child = record({
      headerByte: 7,
      baseHeaderByte: 8,
      baseRoot: await ledgerRoot([expectedBase]),
      expectedRoot: EMPTY_ROOT,
    });
    const substituted = record({
      headerByte: 9,
      baseHeaderByte: 1,
      baseRoot: await ledgerRoot([current]),
      expectedRoot: await ledgerRoot([expectedBase]),
      spent: [current[Ledger.Columns.OUTREF]],
      produced: [
        {
          [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]:
            expectedBase[Ledger.Columns.OUTREF],
          [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]:
            expectedBase[Ledger.Columns.OUTPUT],
        },
      ],
    });

    await expect(
      Effect.runPromise(
        materializeConfirmedLedgerDeltaChainV1({
          record: child,
          confirmedEntries: [current],
          retrieveParent: () => Effect.succeed(Option.some(substituted)),
        }),
      ),
    ).rejects.toThrow(/substituted journal/u);
  });

  it("rejects a produced UTxO substituted over an existing unspent outref", async () => {
    const currentMember = utxo("81", 0n, 2_000_000n);
    const replacementMember = {
      ...utxo("82", 0n, 3_000_000n),
      [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]:
        currentMember[PendingBlockFinalizationsDB.UtxoColumns.OUTREF],
    };
    const current = await ledgerEntry(currentMember);
    const replacement = await ledgerEntry(replacementMember);
    const candidate = record({
      headerByte: 8,
      baseHeaderByte: 7,
      baseRoot: await ledgerRoot([current]),
      expectedRoot: await ledgerRoot([replacement]),
      produced: [replacementMember],
    });

    await expect(
      Effect.runPromise(
        materializeConfirmedLedgerDeltaChainV1({
          record: candidate,
          confirmedEntries: [current],
          retrieveParent: () => Effect.succeed(Option.none()),
        }),
      ),
    ).rejects.toThrow(/invalid for its authenticated base/u);
  });
});
