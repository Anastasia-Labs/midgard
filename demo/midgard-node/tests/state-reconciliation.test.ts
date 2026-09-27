import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type CheckId,
  evaluateStateReconciliation,
  formatStateReconciliationReport,
  type JournalSummary,
  type L1StateView,
  type NativeRootObservation,
  readNativeRootFromLevelCopy,
  redactSensitive,
  type SqlDepositRow,
  type SqlStateSnapshot,
  STATE_RECONCILIATION_CHECK_IDS,
  type StateReconciliationInput,
} from "../src/commands/state-reconciliation.js";

/**
 * Pure evaluator coverage: one consistent world where every check passes, and
 * per check a single introduced inconsistency that fails that check alone.
 */

const h28 = (byte: string) => byte.repeat(28);
const h32 = (byte: string) => byte.repeat(32);

const GENESIS = SDK.GENESIS_HEADER_HASH;
const CONFIRMED = h28("c1");
const TIP = h28("a1");
const REMOVED = h28("de");
const R0 = h32("10");
const R1 = h32("11");
const ROOTS = {
  deposits: h32("d0"),
  withdrawals: h32("e0"),
  forcedTransactions: h32("f0"),
  transactions: h32("70"),
};
const CONFIRMED_ROOTS = {
  deposits: h32("d1"),
  withdrawals: h32("e1"),
  forcedTransactions: h32("f1"),
  transactions: h32("71"),
};
const DIGEST = h32("99");
/** A da_attestation_timeout_ms unlike every deployment profile's, so a
 * hardcoded profile value cannot pass. */
const DA_TIMEOUT = 60_000;
const NOW = 10_000;

const OUT_A = "aa01";
const OUT_B = "bb01";
const OUT_C = "cc01";
const OUT_P = "dd01";

const address = {
  paymentCredential: { PublicKeyCredential: [h28("01")] },
  stakeCredential: null,
};
const addressCbor = LucidData.to(address as never, SDK.AddressData as never);
const datumCbor = LucidData.to("NoDatum" as never, SDK.CardanoDatum as never);
const l2Value = new Map([["", new Map([["", 5_000_000n]])]]);
const l2ValueCbor = LucidData.to(l2Value as never, SDK.Value as never);

const depositPayload = {
  eventId: "d8799f58200101",
  info: "d87980",
  inclusionTimeMs: 1_000,
  l1TxHash: h32("01"),
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

const journal = (overrides: Partial<JournalSummary>): JournalSummary => ({
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

const tipEntries = new Map([
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
  foreign: [],
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

type World = {
  l1: L1StateView;
  sql: SqlStateSnapshot;
  native: NativeRootObservation;
};

const input = (
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
const unassignedDeposit = (
  eventId: string,
  status: string,
  inclusionTimeMs = 1_000,
): SqlDepositRow => ({
  payload: { ...depositPayload, eventId, inclusionTimeMs, ledgerOutput: OUT_P },
  status,
  projectedHeaderHash: null,
  ledgerOutref: "0d",
});

const orderOf = (row: SqlDepositRow): L1StateView["deposits"][number] => ({
  outRef: `${h32("1d")}#0`,
  payload: row.payload,
  decodeError: null,
});

const cacheRowOf = (row: SqlDepositRow) => ({
  outref: row.ledgerOutref!,
  output: row.payload.ledgerOutput,
  sourceEventId: row.payload.eventId,
});

const statuses = (report: ReturnType<typeof evaluateStateReconciliation>) =>
  Object.fromEntries(report.checks.map((c) => [c.id, c.status]));

const expectOnlyFailure = (
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

describe("state reconciliation evaluator", () => {
  it("passes every check on a consistent world", () => {
    const report = evaluateStateReconciliation(input());
    for (const check of report.checks) {
      expect(check.status, `${check.id}: ${check.reason}`).toBe("PASS");
      expect(check.reason.length).toBeGreaterThan(0);
      expect(check.compares.length).toBeGreaterThan(0);
    }
    expect(report.checks.map((c) => c.id)).toEqual([
      ...STATE_RECONCILIATION_CHECK_IDS,
    ]);
    expect(report.ok).toBe(true);
    expect(report.exitCode).toBe(0);
    expect(formatStateReconciliationReport(report)).toContain(
      "10 PASS, 0 FAIL, 0 SKIPPED",
    );
  });

  it("confirmed-root fails alone when SQL's confirmed ledger root differs from L1", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({ sql: { ...sql, confirmedRoot: h32("bd") } })),
    );
    expectOnlyFailure(report, "confirmed-root", "SQL confirmed_ledger root");
  });

  it("confirmed-root fails when SQL's confirmed ledger cannot be encoded, and dependent ledger points skip", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          confirmedRoot: "<unencodable>",
          confirmedRootError: "output is not a transaction output",
          finalizedTip: {
            kind: "failed",
            label: "committed tip",
            headerHash: TIP,
            reason: "SQL confirmed_ledger cannot be encoded",
            parentMissing: false,
          },
        },
      })),
    );
    const byId = statuses(report);
    expect(byId["confirmed-root"]).toBe("FAIL");
    expect(byId["native-root"]).toBe("SKIPPED");
    expect(byId["ledger-cache"]).toBe("SKIPPED");
    for (const id of [
      "state-queue-journal",
      "state-queue-tail-root",
      "deposits",
      "withdrawals",
      "payouts",
      "settlements",
    ] as const) {
      expect(byId[id]).toBe("PASS");
    }
  });

  it("native-root fails alone when the recomputed committed tip differs from the native root", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          finalizedTip: {
            kind: "materialized",
            point: {
              label: `committed tip ${TIP}`,
              headerHash: TIP,
              root: h32("bd"),
              entries: tipEntries,
              chainHeaderHashes: [TIP],
            },
          },
        },
      })),
    );
    expectOnlyFailure(
      report,
      "native-root",
      "matches no recomputed ledger point",
    );
  });

  it("native-root fails when the journal delta chain does not reproduce its expected root", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          activeHeaderHashes: [h28("a2")],
          activeTip: {
            kind: "failed",
            label: "active journal",
            headerHash: h28("a2"),
            reason: "materialized root differs from expected_utxos_root",
            parentMissing: false,
          },
        },
      })),
    );
    expect(statuses(report)["native-root"]).toBe("FAIL");
  });

  it("native-root is SKIPPED with a reason when the native root is unavailable or the chain reaches a foreign parent", () => {
    const unavailable = evaluateStateReconciliation(
      input(() => ({
        native: { kind: "unavailable", reason: "no MPF LevelDB exists" },
      })),
    );
    expect(statuses(unavailable)["native-root"]).toBe("SKIPPED");
    expect(statuses(unavailable)["state-queue-tail-root"]).toBe("SKIPPED");
    expect(unavailable.ok).toBe(true);
    const foreignParent = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          finalizedTip: {
            kind: "failed",
            label: "committed tip",
            headerHash: TIP,
            reason: "parent journal is missing for header",
            parentMissing: true,
          },
        },
      })),
    );
    expect(statuses(foreignParent)["native-root"]).toBe("SKIPPED");
    expect(statuses(foreignParent)["ledger-cache"]).toBe("SKIPPED");
  });

  it("native-root fails, and state-queue-tail-root skips, when the node reports its native owner unhealthy", () => {
    const reason = "node reports its native MPF owner unhealthy (Error: gone)";
    const report = evaluateStateReconciliation(
      input(() => ({ native: { kind: "unhealthy", reason } })),
    );
    const byId = statuses(report);
    for (const id of STATE_RECONCILIATION_CHECK_IDS) {
      expect(byId[id]).toBe(
        id === "native-root"
          ? "FAIL"
          : id === "state-queue-tail-root"
            ? "SKIPPED"
            : "PASS",
      );
    }
    expect(report.exitCode).toBe(1);
    expect(report.checks.find((c) => c.id === "native-root")?.failures).toEqual(
      [reason],
    );
    expect(report.snapshot.nativeRoot).toBe(`unhealthy: ${reason}`);
  });

  it("native-root passes with a note when the native root is at the active journal", () => {
    const active = h28("a2");
    const mutate = ({ sql }: World): Partial<World> => ({
      native: { kind: "observed", root: h32("12"), source: "node-readiness" },
      sql: {
        ...sql,
        journals: [
          ...sql.journals,
          journal({
            headerHash: active,
            status: "pending_submission",
            baseTailHeaderHash: TIP,
            baseUtxosRoot: R1,
            expected: { utxos: h32("12"), ...ROOTS },
            submittedTxHash: null,
          }),
        ],
        activeHeaderHashes: [active],
        activeTip: {
          kind: "materialized",
          point: {
            label: `active journal ${active}`,
            headerHash: active,
            root: h32("12"),
            entries: tipEntries,
            chainHeaderHashes: [TIP, active],
          },
        },
      },
    });
    const report = evaluateStateReconciliation(input(mutate));
    expect(statuses(report)["native-root"]).toBe("PASS");
    // The L1 tail is still the committed tip: unprovable, so strict FAIL.
    expect(statuses(report)["state-queue-tail-root"]).toBe("FAIL");
    expect(
      report.checks.find((c) => c.id === "state-queue-tail-root")?.inFlight,
    ).toHaveLength(1);
    const accepted = evaluateStateReconciliation(input(mutate, true));
    expect(accepted.ok).toBe(true);
    expect(statuses(accepted)["state-queue-tail-root"]).toBe("PASS");
  });

  it("state-queue-journal fails alone when an on-chain header has no journal", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          journals: sql.journals.filter((j) => j.headerHash !== TIP),
        },
      })),
    );
    expectOnlyFailure(report, "state-queue-journal", "no journal");
  });

  it("state-queue-journal fails alone when a removed header's journal is not marked with the admitted transition", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          journals: sql.journals.map((j) =>
            j.headerHash === REMOVED
              ? { ...j, correctionTransitionDigest: h32("98") }
              : j,
          ),
        },
      })),
    );
    expectOnlyFailure(report, "state-queue-journal", "correction digest");
  });

  it("state-queue-journal fails alone when a finalized journal is neither on L1 nor merged", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          journals: [
            ...sql.journals,
            journal({ headerHash: h28("0f"), baseTailHeaderHash: TIP }),
          ],
        },
      })),
    );
    expectOnlyFailure(
      report,
      "state-queue-journal",
      "neither on the L1 queue nor merged",
    );
  });

  it("state-queue-journal accepts an older finalized journal beyond an incomplete merged walk only when it ends before the confirmed state", () => {
    const beyondGap = (endTimeMs: number) =>
      evaluateStateReconciliation(
        input(({ sql }) => ({
          sql: {
            ...sql,
            journals: [
              ...sql.journals.map((j) =>
                j.headerHash === CONFIRMED
                  ? { ...j, baseTailHeaderHash: h28("f7") }
                  : j,
              ),
              journal({
                headerHash: h28("01"),
                baseTailHeaderHash: GENESIS,
                endTimeMs,
              }),
            ],
          },
        })),
      );
    const older = beyondGap(100);
    expect(statuses(older)["state-queue-journal"]).toBe("PASS");
    expect(
      older.checks
        .find((c) => c.id === "state-queue-journal")
        ?.notes.join("\n"),
    ).toContain("treated as merged");
    expectOnlyFailure(
      beyondGap(9_000),
      "state-queue-journal",
      "neither on the L1 queue nor merged",
    );
  });

  it("state-queue-journal treats an unreincluded admitted removal as in-flight", () => {
    const mutate = ({ sql }: World): Partial<World> => ({
      sql: {
        ...sql,
        journals: sql.journals.map((j) =>
          j.headerHash === REMOVED
            ? { ...j, status: "submitted_unconfirmed" }
            : j,
        ),
        activeHeaderHashes: [REMOVED],
      },
    });
    const strict = evaluateStateReconciliation(input(mutate));
    expect(statuses(strict)["state-queue-journal"]).toBe("FAIL");
    const accepted = evaluateStateReconciliation(input(mutate, true));
    expect(statuses(accepted)["state-queue-journal"]).toBe("PASS");
    expect(
      accepted.checks.find((c) => c.id === "state-queue-journal")?.reason,
    ).toContain("accepted");
  });

  it("passes a signed-intent journal abandoned for a replacement whose header never landed", () => {
    // The replaced intent shares its base with the landed replacement (TIP),
    // carries the replacement digest rather than an observed correction's,
    // and no admitted transition names it: nothing on L1 ever held it.
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          journals: [
            ...sql.journals,
            journal({
              headerHash: h28("3b"),
              status: "abandoned",
              correctionTransitionDigest: h32("3d"),
              submittedTxHash: h32("3e"),
            }),
          ],
        },
      })),
    );
    for (const check of report.checks) {
      expect(check.status, `${check.id}: ${check.reason}`).toBe("PASS");
    }
  });

  it("state-queue-tail-root fails alone when the L1 tail root differs from the native root", () => {
    // Native root and the recomputed tip agree with each other but not with
    // the L1 tail (whose roots still equal the journal's expected roots).
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        native: { kind: "observed", root: h32("bd"), source: "leveldb-copy" },
        sql: {
          ...sql,
          finalizedTip: {
            kind: "materialized",
            point: {
              label: `committed tip ${TIP}`,
              headerHash: TIP,
              root: h32("bd"),
              entries: tipEntries,
              chainHeaderHashes: [TIP],
            },
          },
        },
      })),
    );
    expectOnlyFailure(report, "state-queue-tail-root", "native root");
  });

  describe("da-attestation", () => {
    // The tip header ends at 3_000, so its deadline is 3_000 + DA_TIMEOUT.
    const DEADLINE = 3_000 + DA_TIMEOUT;
    const tipWith = (
      daStatus: SDK.DaAvailabilityStateQueueStatusKind,
      nowMs: number,
      allowInFlight = false,
    ): StateReconciliationInput => ({
      ...input(
        ({ l1 }) => ({
          l1: {
            ...l1,
            unmerged: l1.unmerged.map((h) => ({ ...h, daStatus })),
          },
        }),
        allowInFlight,
      ),
      nowMs,
    });
    const daCheck = (report: ReturnType<typeof evaluateStateReconciliation>) =>
      report.checks.find((c) => c.id === "da-attestation")!;

    it("fails alone when an unmerged header is Unattested past its deadline, naming it, the deadline and the lateness", () => {
      const report = evaluateStateReconciliation(
        tipWith("Unattested", DEADLINE + 5_000),
      );
      expectOnlyFailure(
        report,
        "da-attestation",
        `header ${TIP} (${h32("0a")}#0) is Unattested past its DA-attestation deadline ${new Date(DEADLINE).toISOString()} (${DEADLINE.toString()} ms) by 5000 ms`,
      );
      // A missed deadline is an inconsistency, not an in-flight state.
      expect(
        daCheck(
          evaluateStateReconciliation(
            tipWith("Unattested", DEADLINE + 5_000, true),
          ),
        ).status,
      ).toBe("FAIL");
    });

    it.each([
      ["well before", NOW, `in ${(DEADLINE - NOW).toString()} ms`],
      ["exactly at", DEADLINE, "in 0 ms"],
    ])(
      "passes with a note when an Unattested header is %s its deadline",
      (_label, nowMs, remaining) => {
        const report = evaluateStateReconciliation(
          tipWith("Unattested", nowMs),
        );
        expect(report.ok).toBe(true);
        const check = daCheck(report);
        expect(check.status).toBe("PASS");
        expect(check.inFlight).toEqual([]);
        expect(check.notes).toEqual([
          `header ${TIP} (${h32("0a")}#0) awaits DA attestation; deadline ${new Date(DEADLINE).toISOString()} (${DEADLINE.toString()} ms) ${remaining}`,
        ]);
      },
    );

    it.each(["Attested", "Challenged", "Published"] as const)(
      "passes silently for a %s header past the deadline",
      (daStatus) => {
        const report = evaluateStateReconciliation(
          tipWith(daStatus, DEADLINE + 5_000),
        );
        expect(report.ok).toBe(true);
        const check = daCheck(report);
        expect(check.status).toBe("PASS");
        expect(check.notes).toEqual([]);
      },
    );

    it("passes with a note, and leaves the failure to state-queue-journal, when a header datum is undecodable", () => {
      const report = evaluateStateReconciliation({
        ...input(({ l1 }) => ({
          l1: {
            ...l1,
            unmerged: l1.unmerged.map((h) => ({
              ...h,
              daStatus: null,
              endTimeMs: null,
              decodeError: "header datum is not a state-queue node",
            })),
          },
        })),
        nowMs: DEADLINE + 5_000,
      });
      const check = daCheck(report);
      expect(check.status).toBe("PASS");
      expect(check.notes).toEqual([
        `header ${TIP} (${h32("0a")}#0): DA status unknown, datum undecodable (state-queue-journal reports it)`,
      ]);
    });
  });

  it("deposits fails alone when an L1 deposit is unknown to SQL", () => {
    const report = evaluateStateReconciliation(
      input(({ l1 }) => ({
        l1: {
          ...l1,
          deposits: [
            ...l1.deposits,
            {
              outRef: `${h32("1d")}#0`,
              payload: { ...depositPayload, eventId: "d8799f58200102" },
              decodeError: null,
            },
          ],
        },
      })),
    );
    expectOnlyFailure(report, "deposits", "unknown to SQL");
  });

  it("deposits fails alone on a payload mismatch or an awaiting row assigned to a header", () => {
    const payload = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          deposits: sql.deposits.map((d) => ({
            ...d,
            payload: { ...d.payload, ledgerAddress: "addr_test_other" },
          })),
        },
      })),
    );
    expectOnlyFailure(payload, "deposits", "ledgerAddress");
    const status = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          deposits: sql.deposits.map((d) => ({ ...d, status: "awaiting" })),
        },
      })),
    );
    expectOnlyFailure(status, "deposits", "status is awaiting");
  });

  it("deposits treats an unindexed order newer than every committed block as in-flight", () => {
    const report = evaluateStateReconciliation(
      input(
        ({ l1 }) => ({
          l1: {
            ...l1,
            deposits: [
              ...l1.deposits,
              {
                outRef: `${h32("1d")}#0`,
                payload: {
                  ...depositPayload,
                  eventId: "d8799f58200103",
                  inclusionTimeMs: 9_000,
                },
                decodeError: null,
              },
            ],
          },
        }),
        true,
      ),
    );
    expect(statuses(report).deposits).toBe("PASS");
    expect(
      report.checks.find((c) => c.id === "deposits")?.inFlight,
    ).toHaveLength(1);
  });

  it("deposits fails alone when an unmerged SQL deposit's L1 order is gone, in-flight only when newer than every committed block, and not for a merged deposit", () => {
    const orphan = unassignedDeposit("e2", "projected");
    const unassigned = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          deposits: [...sql.deposits, orphan],
          mempoolLedger: [...sql.mempoolLedger, cacheRowOf(orphan)],
        },
      })),
    );
    expectOnlyFailure(
      unassigned,
      "deposits",
      "SQL deposit e2 is not among the L1 deposit orders",
    );
    const assigned = evaluateStateReconciliation(
      input(({ l1 }) => ({ l1: { ...l1, deposits: [] } })),
    );
    expectOnlyFailure(
      assigned,
      "deposits",
      `SQL deposit ${depositPayload.eventId} is not among the L1 deposit orders`,
    );
    const fresh = unassignedDeposit("e3", "awaiting", 9_000);
    const mutateFresh = ({ sql }: World): Partial<World> => ({
      sql: { ...sql, deposits: [...sql.deposits, fresh] },
    });
    const strict = evaluateStateReconciliation(input(mutateFresh));
    expect(statuses(strict).deposits).toBe("FAIL");
    const deposits = strict.checks.find((c) => c.id === "deposits");
    expect(deposits?.failures).toEqual([]);
    expect(deposits?.inFlight).toEqual([
      "SQL deposit e3 is not among the L1 deposit orders; its inclusion time is after every committed block",
    ]);
    const accepted = evaluateStateReconciliation(input(mutateFresh, true));
    expect(accepted.ok).toBe(true);
    // The settlement a merge creates is what spends the order, so a deposit
    // of a merged header is not required to keep one.
    const absorbed = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          deposits: [
            ...sql.deposits,
            {
              ...unassignedDeposit("e6", "consumed"),
              projectedHeaderHash: CONFIRMED,
            },
          ],
        },
      })),
    );
    for (const check of absorbed.checks) {
      expect(check.status, `${check.id}: ${check.reason}`).toBe("PASS");
    }
  });

  it("ledger-cache fails alone when a cached deposit row is not projected", () => {
    const awaiting = unassignedDeposit("e4", "awaiting");
    const report = evaluateStateReconciliation(
      input(({ l1, sql }) => ({
        l1: { ...l1, deposits: [...l1.deposits, orderOf(awaiting)] },
        sql: {
          ...sql,
          deposits: [...sql.deposits, awaiting],
          mempoolLedger: [...sql.mempoolLedger, cacheRowOf(awaiting)],
        },
      })),
    );
    expectOnlyFailure(report, "ledger-cache", "unexpected outref (first 0d)");
  });

  it("ledger-cache fails alone when a projected deposit no block holds is missing from the cache, unless a pending transaction spent it, and expects no other deposit", () => {
    const projected = unassignedDeposit("e5", "projected");
    const missing = evaluateStateReconciliation(
      input(({ l1, sql }) => ({
        l1: { ...l1, deposits: [...l1.deposits, orderOf(projected)] },
        sql: { ...sql, deposits: [...sql.deposits, projected] },
      })),
    );
    expectOnlyFailure(missing, "ledger-cache", "missing outref (first 0d)");
    const spent = evaluateStateReconciliation(
      input(({ l1, sql }) => ({
        l1: { ...l1, deposits: [...l1.deposits, orderOf(projected)] },
        sql: {
          ...sql,
          deposits: [...sql.deposits, projected],
          pendingTxs: [
            ...sql.pendingTxs,
            {
              txId: h32("7a"),
              source: "processed_mempool",
              delta: {
                spent: ["0d"],
                produced: [{ outref: "0e", output: OUT_C }],
              },
              rejectDetail: null,
            },
          ],
          mempoolLedger: [
            ...sql.mempoolLedger,
            { outref: "0e", output: OUT_C, sourceEventId: null },
          ],
        },
      })),
    );
    for (const check of spent.checks) {
      expect(check.status, `${check.id}: ${check.reason}`).toBe("PASS");
    }
    // Only a projected deposit no block holds is owed a cache entry: a spend
    // marks it consumed, an awaiting one is not projected yet, and the
    // recomputed ledger point, not the deposit row, accounts for one a block
    // of that point holds.
    const notOwed = [
      unassignedDeposit("e7", "consumed"),
      unassignedDeposit("e8", "awaiting"),
      { ...unassignedDeposit("e9", "projected"), projectedHeaderHash: TIP },
    ];
    for (const deposit of notOwed) {
      const report = evaluateStateReconciliation(
        input(({ l1, sql }) => ({
          l1: { ...l1, deposits: [...l1.deposits, orderOf(deposit)] },
          sql: { ...sql, deposits: [...sql.deposits, deposit] },
        })),
      );
      for (const check of report.checks) {
        expect(
          check.status,
          `${deposit.payload.eventId} ${check.id}: ${check.reason}`,
        ).toBe("PASS");
      }
    }
  });

  it("withdrawals fails alone when the SQL status contradicts a finalized journal", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          withdrawals: sql.withdrawals.map((w) =>
            w.projectedHeaderHash === TIP ? { ...w, status: "projected" } : w,
          ),
        },
      })),
    );
    expectOnlyFailure(report, "withdrawals", "expected finalized");
  });

  it("withdrawals fails alone when a SQL header assignment points nowhere", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          withdrawals: sql.withdrawals.map((w) =>
            w.projectedHeaderHash === TIP
              ? { ...w, projectedHeaderHash: h28("0e") }
              : w,
          ),
        },
      })),
    );
    expectOnlyFailure(report, "withdrawals", "not on the L1 queue, not merged");
  });

  it("payouts fails alone when the payout amount differs from the SQL withdrawal", () => {
    const report = evaluateStateReconciliation(
      input(({ l1 }) => ({
        l1: {
          ...l1,
          payouts: l1.payouts.map((p) => ({
            ...p,
            l2Value: { lovelace: 4_999_999n },
          })),
        },
      })),
    );
    expectOnlyFailure(report, "payouts", "l2_value");
  });

  it("payouts fails alone when the payout's withdrawal is not finalized-valid", () => {
    const report = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          withdrawals: sql.withdrawals.map((w) =>
            w.payload.assetName === "a0"
              ? { ...w, validity: "WithdrawalIsInvalid" }
              : w,
          ),
        },
      })),
    );
    expectOnlyFailure(report, "payouts", "WithdrawalIsValid");
  });

  it("settlements fails alone when a settlement's roots differ from the journal", () => {
    const report = evaluateStateReconciliation(
      input(({ l1 }) => ({
        l1: {
          ...l1,
          settlements: l1.settlements.map((s) => ({
            ...s,
            roots: { ...CONFIRMED_ROOTS, withdrawals: h32("bd") },
          })),
        },
      })),
    );
    expectOnlyFailure(report, "settlements", "withdrawals root differs");
  });

  it("settlements fails alone when a settlement names an unmerged header", () => {
    const report = evaluateStateReconciliation(
      input(({ l1 }) => ({
        l1: {
          ...l1,
          settlements: [
            ...l1.settlements,
            {
              outRef: `${h32("5f")}#0`,
              tokens: [{ assetName: h28("0b"), quantity: "1" }],
              roots: ROOTS,
              decodeError: null,
            },
          ],
        },
      })),
    );
    expectOnlyFailure(report, "settlements", "not on the merged chain");
  });

  it("ledger-cache fails alone on a missing, extra or altered cache row", () => {
    const missing = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          mempoolLedger: sql.mempoolLedger.filter((r) => r.outref !== "0c"),
        },
      })),
    );
    expectOnlyFailure(missing, "ledger-cache", "missing outref");
    const altered = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          mempoolLedger: sql.mempoolLedger.map((r) =>
            r.outref === "0b" ? { ...r, output: "ffff" } : r,
          ),
        },
      })),
    );
    expectOnlyFailure(altered, "ledger-cache", "mismatched output");
    const extra = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          mempoolLedger: [
            ...sql.mempoolLedger,
            { outref: "0a", output: OUT_A, sourceEventId: null },
          ],
        },
      })),
    );
    expectOnlyFailure(extra, "ledger-cache", "unexpected outref");
  });

  it("ledger-cache accepts a projected deposit row not yet in a block and skips on an undecodable pending tx", () => {
    const projected = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          deposits: [
            ...sql.deposits,
            {
              payload: {
                ...depositPayload,
                eventId: "e1",
                ledgerOutput: OUT_P,
              },
              status: "projected",
              projectedHeaderHash: null,
              ledgerOutref: "0d",
            },
          ],
          mempoolLedger: [
            ...sql.mempoolLedger,
            { outref: "0d", output: OUT_P, sourceEventId: "e1" },
          ],
        },
      })),
    );
    expect(statuses(projected)["ledger-cache"]).toBe("PASS");
    const rejected = evaluateStateReconciliation(
      input(({ sql }) => ({
        sql: {
          ...sql,
          pendingTxs: [
            ...sql.pendingTxs,
            {
              txId: h32("79"),
              source: "mempool",
              delta: null,
              rejectDetail: "bad cbor",
            },
          ],
        },
      })),
    );
    expect(statuses(rejected)["ledger-cache"]).toBe("SKIPPED");
    expect(
      rejected.checks.find((c) => c.id === "ledger-cache")?.reason,
    ).toContain("bad cbor");
  });

  it("skips every L1 comparison with the reason when L1 is unavailable", () => {
    const report = evaluateStateReconciliation({
      ...input(),
      l1: {
        kind: "unavailable",
        reason: "L1 state changed during each of 3 snapshot attempts",
      },
    });
    const byId = statuses(report);
    for (const id of [
      "confirmed-root",
      "state-queue-journal",
      "state-queue-tail-root",
      "deposits",
      "withdrawals",
      "payouts",
      "settlements",
      "da-attestation",
    ] as const) {
      expect(byId[id]).toBe("SKIPPED");
      expect(report.checks.find((c) => c.id === id)?.reason).toContain(
        "L1 state changed",
      );
    }
    expect(byId["native-root"]).toBe("PASS");
    expect(byId["ledger-cache"]).toBe("PASS");
    expect(report.exitCode).toBe(0);
  });

  it("redacts URLs and passwords from error text", () => {
    expect(
      redactSensitive(
        "connect failed postgres://user:secret@db:5432/midgard and https://key@example/x password=hunter2",
      ),
    ).toBe(
      "connect failed <redacted-url> and <redacted-url> password=<redacted>",
    );
  });

  it("reports a missing native LevelDB as unavailable without creating it", async () => {
    const observation = await readNativeRootFromLevelCopy(
      "/nonexistent/midgard-state-reconcile-test",
    );
    expect(observation.kind).toBe("unavailable");
  });
});
