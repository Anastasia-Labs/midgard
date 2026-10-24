import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as LedgerUtils from "../src/database/utils/ledger.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { ledgerEntryToInsertBatchOp } from "../src/mpf/ledger-delta.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { rebind } from "./foreign-block-import.fixture.js";
import {
  BASE,
  build,
  hex,
  order,
  payloadOf,
  replay,
  run,
  U,
} from "./process-mpfs-withdrawal.fixture.js";

/**
 * Two due withdrawal orders naming one L2 outref in one block window. Anyone
 * can submit both, so the block must still build: each withdrawal is
 * classified against the ledger the earlier payable ones leave, and foreign
 * replay must derive the same bytes.
 */

// The native owner is replaced by the in-process trace builder over the same
// base ledger; the withdrawal classification and trace sources stay real.
const base = vi.hoisted(() => ({
  entries: [] as { readonly key: Buffer; readonly value: Buffer }[],
}));
vi.mock("../src/mpf/transition-trace.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/mpf/transition-trace.js")
  >("../src/mpf/transition-trace.js");
  const { MidgardMpf: Mpf } = await vi.importActual<
    typeof import("../src/mpf/store.midgard-mpf.js")
  >("../src/mpf/store.midgard-mpf.js");
  return {
    ...actual,
    buildNativeTransitionTraceResult: ({
      nativeMpf,
      ...rest
    }: Parameters<typeof actual.buildNativeTransitionTraceResult>[0]) =>
      Effect.gen(function* () {
        const ledgerMpf = yield* Mpf.createScratchFromList(
          "duplicate-withdrawal-ledger",
          base.entries,
        );
        const result = yield* actual.buildTransitionTraceResult({
          ledgerMpf,
          ...rest,
        });
        nativeMpf.candidateRoot = result.finalUtxosRoot;
        nativeMpf.eventLog = Buffer.alloc(0);
        nativeMpf.eventLogDigest = "00".repeat(32);
        nativeMpf.eventRoots = [];
        return result;
      }),
  };
});

describe("duplicate-outref withdrawals in one block window", () => {
  it("builds the block, drains the outref once and replays to the same bytes", async () => {
    const orders = [await order(0x01), await order(0x02)];
    base.entries = BASE.map(ledgerEntryToInsertBatchOp);
    const baseRoot = await Effect.runPromise(
      computeLedgerMpfRootFromLedgerEntries(BASE),
    );
    const { produced, rows } = await run(build(orders, baseRoot));

    // The first order drains U; the second finds it gone.
    expect(
      produced.includedWithdrawalEntries.map((row) => [
        hex(row[WithdrawalsDB.Columns.ID]),
        row[WithdrawalsDB.Columns.VALIDITY],
      ]),
    ).toEqual([
      [hex(orders[0]![WithdrawalsDB.Columns.ID]), "WithdrawalIsValid"],
      [hex(orders[1]![WithdrawalsDB.Columns.ID]), "NonExistentWithdrawalUtxo"],
    ]);
    expect(rows.map((row) => [row.validity, row.status])).toEqual([
      ["WithdrawalIsValid", WithdrawalsDB.Status.Projected],
      ["NonExistentWithdrawalUtxo", WithdrawalsDB.Status.Projected],
    ]);
    expect(rows.map((row) => hex(row.settlement_event_info as Buffer))).toEqual(
      produced.includedWithdrawalEntries.map((row) =>
        hex(row[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]!),
      ),
    );
    // Exactly one delete, of U; step 1 removes it and step 2 is a no-op.
    expect(produced.ledgerDelta.spent.map(hex)).toEqual([
      hex(U[LedgerUtils.Columns.OUTREF]),
    ]);
    expect(produced.ledgerDelta.produced).toEqual([]);
    const [first, second] = produced.transitionTraceMembers.map(
      (member) => member.value,
    );
    expect(produced.transitionTraceMembers).toHaveLength(2);
    expect(first!.pre_utxos_root).toBe(baseRoot);
    expect(first!.post_utxos_root).not.toBe(first!.pre_utxos_root);
    expect(second!.pre_utxos_root).toBe(first!.post_utxos_root);
    expect(second!.post_utxos_root).toBe(second!.pre_utxos_root);
    expect(second!.post_utxos_root).toBe(produced.utxoRoot);

    // Foreign replay derives the same verdicts, transitions and root.
    const honest = await payloadOf(produced, baseRoot);
    expect(honest.block_body.header.utxosRoot).toBe(produced.utxoRoot);
    const accepted = await replay(honest, orders, baseRoot);
    expect(accepted.result._tag).toBe("Right");
    expect(
      accepted.memberships.map(({ id, validity }) => [id, validity]),
    ).toEqual([
      [hex(orders[0]![WithdrawalsDB.Columns.ID]), "WithdrawalIsValid"],
      [hex(orders[1]![WithdrawalsDB.Columns.ID]), "NonExistentWithdrawalUtxo"],
    ]);

    // Tagging the duplicate payable instead is refused by the same replay.
    const info = Data.from(
      hex(orders[1]![WithdrawalsDB.Columns.RAW_EVENT_INFO]),
      SDK.WithdrawalInfo,
    ) as SDK.WithdrawalInfo;
    const payableDuplicate = SDK.committedWithdrawalValueBytes({
      ...info,
      validity: "WithdrawalIsValid",
    });
    const tampered = await rebind({
      ...honest,
      block_body: {
        ...honest.block_body,
        withdrawals: honest.block_body.withdrawals.map(([key, value]) => [
          key,
          key === hex(orders[1]![WithdrawalsDB.Columns.ID])
            ? payableDuplicate
            : value,
        ]),
      },
    });
    const refused = await replay(tampered, orders, baseRoot);
    expect(refused.result).toMatchObject({
      _tag: "Left",
      left: {
        kind: "invalid",
        detail: "a withdrawal's verdict differs from its replay",
      },
    });
  }, 120_000);
});
