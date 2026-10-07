import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  type CheckId,
  evaluateStateReconciliation,
  type JournalSummary,
  type L1StateView,
  type NativeRootObservation,
  type SqlDepositRow,
  type SqlStateSnapshot,
  STATE_RECONCILIATION_CHECK_IDS,
  type StateReconciliationInput,
} from "../src/commands/state-reconciliation.js";

/**
 * Pure evaluator coverage: one consistent world where every check passes, and
 * per check a single introduced inconsistency that fails that check alone.
 */

export const h28 = (byte: string) => byte.repeat(28);

export const h32 = (byte: string) => byte.repeat(32);

export const GENESIS = SDK.GENESIS_HEADER_HASH;

export const CONFIRMED = h28("c1");

export const TIP = h28("a1");

export const REMOVED = h28("de");

const R0 = h32("10");

export const R1 = h32("11");

export const ROOTS = {
  deposits: h32("d0"),
  withdrawals: h32("e0"),
  forcedTransactions: h32("f0"),
  transactions: h32("70"),
};

export const CONFIRMED_ROOTS = {
  deposits: h32("d1"),
  withdrawals: h32("e1"),
  forcedTransactions: h32("f1"),
  transactions: h32("71"),
};

const DIGEST = h32("99");

/** A da_attestation_timeout_ms unlike every deployment profile's, so a
 * hardcoded profile value cannot pass. */
export const DA_TIMEOUT = 60_000;

export const NOW = 10_000;

export const OUT_A = "aa01";

const OUT_B = "bb01";

export const OUT_C = "cc01";

export const OUT_P = "dd01";

const address = {
  paymentCredential: { PublicKeyCredential: [h28("01")] },
  stakeCredential: null,
};

const addressCbor = LucidData.to(address as never, SDK.AddressData as never);

const datumCbor = LucidData.to("NoDatum" as never, SDK.CardanoDatum as never);

const l2Value = new Map([["", new Map([["", 5_000_000n]])]]);

const l2ValueCbor = LucidData.to(l2Value as never, SDK.Value as never);

export const depositPayload = {
  eventId: "d8799f58200101",
  info: "d87980",
  inclusionTimeMs: 1_000,
  ledgerTxId: h32("02"),
  ledgerOutput: "b0b0",
  ledgerAddress: "addr_test1",
};

const withdrawalPayload = (eventId: string, assetName: string) => ({
  eventId,
  rawEventInfo: "d87980",
  inclusionTimeMs: 2_000,
  l1TxHash: h32("03"),
  l1OutputIndex: 0,
  assetName,
  l2Outref: "d87980",
  l2Owner: h28("04"),
  l2Value: l2ValueCbor,
  l1Address: addressCbor,
  l1Datum: datumCbor,
  refundAddress: addressCbor,
  refundDatum: datumCbor,
});

export const journal = (
  overrides: Partial<JournalSummary>,
): JournalSummary => ({
  headerHash: TIP,
  status: "finalized",
  baseTailHeaderHash: CONFIRMED,
  baseUtxosRoot: R0,
  expected: { utxos: R1, ...ROOTS },
  correctionTransitionDigest: null,
  submittedTxHash: h32("05"),
  endTimeMs: 3_000,
  ...overrides,
});

const consistentL1 = (): L1StateView => ({
  confirmed: {
    outRef: `${h32("0c")}#0`,
    headerHash: CONFIRMED,
    utxoRoot: R0,
    endTimeMs: 500,
  },
  unmerged: [
    {
      outRef: `${h32("0a")}#0`,
      headerHash: TIP,
      recomputedHeaderHash: TIP,
      prevHeaderHash: CONFIRMED,
      endTimeMs: 3_000,
      roots: { utxos: R1, ...ROOTS },
      daStatus: "Attested",
      decodeError: null,
    },
  ],
  deposits: [
    { outRef: `${h32("0d")}#0`, payload: depositPayload, decodeError: null },
  ],
  withdrawals: [
    {
      outRef: `${h32("0e")}#0`,
      payload: withdrawalPayload("d8799f0201", "a1"),
      decodeError: null,
    },
  ],
  payouts: [
    {
      outRef: `${h32("0f")}#0`,
      tokens: [{ assetName: "a0", quantity: "1" }],
      l2Value: { lovelace: 5_000_000n },
      l1AddressCbor: addressCbor,
      l1DatumCbor: datumCbor,
      decodeError: null,
    },
  ],
  settlements: [
    {
      outRef: `${h32("5e")}#0`,
      tokens: [{ assetName: CONFIRMED, quantity: "1" }],
      roots: CONFIRMED_ROOTS,
      decodeError: null,
    },
  ],
});

export const tipEntries = new Map([
  ["0a", OUT_A],
  ["0b", OUT_B],
]);

const consistentSql = (): SqlStateSnapshot => ({
  confirmedRoot: R0,
  confirmedRootError: null,
  confirmedEntryCount: 1,
  journals: [
    journal({
      headerHash: CONFIRMED,
      baseTailHeaderHash: GENESIS,
      baseUtxosRoot: h32("00"),
      expected: { utxos: R0, ...CONFIRMED_ROOTS },
      endTimeMs: 500,
    }),
    journal({}),
    journal({
      headerHash: REMOVED,
      status: "abandoned",
      correctionTransitionDigest: DIGEST,
    }),
  ],
  activeHeaderHashes: [],
  finalizedTip: {
    kind: "materialized",
    point: {
      label: `committed tip ${TIP}`,
      headerHash: TIP,
      root: R1,
      entries: tipEntries,
      chainHeaderHashes: [TIP],
    },
  },
  activeTip: null,
  deposits: [
    {
      payload: depositPayload,
      status: "projected",
      projectedHeaderHash: TIP,
      ledgerOutref: "0b",
    },
  ],
  withdrawals: [
    {
      payload: withdrawalPayload("d8799f0201", "a1"),
      status: "finalized",
      validity: "WithdrawalIsValid",
      projectedHeaderHash: TIP,
    },
    {
      payload: withdrawalPayload("d8799f0200", "a0"),
      status: "finalized",
      validity: "WithdrawalIsValid",
      projectedHeaderHash: CONFIRMED,
    },
  ],
  // Tip ledger {0a, 0b}; a pending transaction spends 0a and produces 0c.
  mempoolLedger: [
    { outref: "0b", output: OUT_B, sourceEventId: null },
    { outref: "0c", output: OUT_C, sourceEventId: null },
  ],
  pendingTxs: [
    {
      txId: h32("77"),
      source: "mempool",
      delta: { spent: ["0a"], produced: [{ outref: "0c", output: OUT_C }] },
      rejectDetail: null,
    },
  ],
  blockHeaderHashes: [TIP],
  observer: {
    kind: "present",
    admitted: [
      {
        transactionHash: h32("78"),
        transitionKind: "timeout_correction",
        removedHeaderHashes: [REMOVED],
        transitionDigest: DIGEST,
      },
    ],
    pendingCount: 0,
  },
});

const consistentNative = (): NativeRootObservation => ({
  kind: "observed",
  root: R1,
  source: "leveldb-copy",
});

export type World = {
  l1: L1StateView;
  sql: SqlStateSnapshot;
  native: NativeRootObservation;
};

export const input = (
  mutate: (world: World) => Partial<World> = () => ({}),
  allowInFlight = false,
): StateReconciliationInput => {
  const world = {
    l1: consistentL1(),
    sql: consistentSql(),
    native: consistentNative(),
  };
  const next = { ...world, ...mutate(world) };
  return {
    l1: { kind: "observed", view: next.l1 },
    sql: next.sql,
    native: next.native,
    allowInFlight,
    nowMs: NOW,
    daAttestationTimeoutMs: DA_TIMEOUT,
  };
};

/** A SQL deposit no block holds yet, whose ledger entry is `0d -> OUT_P`. */
export const unassignedDeposit = (
  eventId: string,
  status: string,
  inclusionTimeMs = 1_000,
): SqlDepositRow => ({
  payload: { ...depositPayload, eventId, inclusionTimeMs, ledgerOutput: OUT_P },
  status,
  projectedHeaderHash: null,
  ledgerOutref: "0d",
});

export const orderOf = (
  row: SqlDepositRow,
): L1StateView["deposits"][number] => ({
  outRef: `${h32("1d")}#0`,
  payload: row.payload,
  decodeError: null,
});

export const cacheRowOf = (row: SqlDepositRow) => ({
  outref: row.ledgerOutref!,
  output: row.payload.ledgerOutput,
  sourceEventId: row.payload.eventId,
});

export const statuses = (
  report: ReturnType<typeof evaluateStateReconciliation>,
) => Object.fromEntries(report.checks.map((c) => [c.id, c.status]));

export const expectOnlyFailure = (
  report: ReturnType<typeof evaluateStateReconciliation>,
  failing: CheckId,
  reasonFragment: string,
) => {
  const byId = statuses(report);
  for (const id of STATE_RECONCILIATION_CHECK_IDS) {
    expect(
      byId[id],
      `${id}: ${report.checks.find((c) => c.id === id)?.reason ?? ""}`,
    ).toBe(id === failing ? "FAIL" : "PASS");
  }
  expect(report.ok).toBe(false);
  expect(report.exitCode).toBe(1);
  expect(report.checks.find((c) => c.id === failing)?.reason).toContain(
    reasonFragment,
  );
};
