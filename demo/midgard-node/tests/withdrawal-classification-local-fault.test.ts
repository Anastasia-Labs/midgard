import "./utils.js";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  createReplayedLedger,
  isReplayLocalFault,
  ledgerEntriesRoot,
  type ReplayedLedgerRefusal,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CANONICAL_MAX_DEPTHS,
  deepPlutusDataCbor,
  plutusDataNodeCount,
} from "@al-ft/midgard-test-support/deep-plutus-data";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import * as LedgerUtils from "../src/database/utils/ledger.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { foreignReplayMaterial } from "../src/landed-blocks/replay-foreign.js";
import { heldBuildStage } from "../src/mpf/commit-rejection.evict-uncommittable-normal-txs.js";
import { ledgerEntryToInsertBatchOp } from "../src/mpf/ledger-delta.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { WITHDRAWAL_CLASSIFICATION_STAGE } from "../src/workers/utils/mpf/withdrawal-classification.js";
import { rebind } from "./foreign-block-import.fixture.js";
import {
  at,
  BASE,
  build,
  hex,
  order,
  payloadOf,
  privateKey,
  replay,
  run,
  U,
} from "./process-mpfs-withdrawal.fixture.js";

/**
 * The withdrawal classifier failing at runtime (as when its signature
 * library faults) on input it handles: own-block production holds the
 * build, foreign replay holds the block as a local fault, and neither
 * reaches a verdict until the fault clears, however deep the info nests.
 * An info that is malformed, or whose fields do not read, keeps the
 * block's own verdict.
 */

const base = vi.hoisted(() => ({
  entries: [] as { readonly key: Buffer; readonly value: Buffer }[],
}));
const fault = vi.hoisted(() => ({ on: false }));

vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@al-ft/midgard-sdk")>();
  const { Effect: E } = await import("effect");
  return {
    ...actual,
    classifyWithdrawalFromLedger: (
      input: Parameters<typeof actual.classifyWithdrawalFromLedger>[0],
    ) =>
      fault.on
        ? E.sync(() => {
            throw new Error("signature runtime fault");
          })
        : actual.classifyWithdrawalFromLedger(input),
  };
});

// The native owner is replaced by the in-process trace builder over the same
// base ledger.
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
          "classification-fault-ledger",
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

const baseRoot = () => {
  base.entries = BASE.map(ledgerEntryToInsertBatchOp);
  return Effect.runPromise(computeLedgerMpfRootFromLedgerEntries(BASE));
};

/** An honest one-withdrawal block, built with the classifier working. */
const honestBlock = async (orders: readonly WithdrawalsDB.Entry[]) => {
  fault.on = false;
  const root = await baseRoot();
  const { produced } = await run(build(orders, root));
  return { root, produced, payload: await payloadOf(produced, root) };
};

const withdrawalRows = () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        status: string;
        validity: string | null;
        settlement_event_info: Buffer | null;
      }>`SELECT status, validity, settlement_event_info FROM withdrawal_utxos ORDER BY event_id`;
    }),
  );

/** `order`'s event info with its L1 datum the inline datum `datum`. */
const withDatum = (row: WithdrawalsDB.Entry, datum: string): Buffer => {
  const marker = "c0ffee".repeat(4);
  const info = Data.from(
    hex(row[WithdrawalsDB.Columns.RAW_EVENT_INFO]),
    SDK.WithdrawalInfo,
  ) as SDK.WithdrawalInfo;
  const encoded = Data.to(
    {
      ...info,
      body: { ...info.body, l1_datum: { InlineDatum: { data: marker } } },
    },
    SDK.WithdrawalInfo,
  );
  const placeholder = `4c${marker}`;
  if (encoded.split(placeholder).length !== 2)
    throw new Error("the datum placeholder is not unique");
  return Buffer.from(encoded.replace(placeholder, datum), "hex");
};

/** `row` with its L1 datum `depth` containers of `shape` deep. */
const deepRow = (
  row: WithdrawalsDB.Entry,
  depth: number,
  shape: "list" | "constr" = "list",
): WithdrawalsDB.Entry => ({
  ...row,
  raw_event_info: withDatum(row, deepPlutusDataCbor({ shape, depth })),
});

const input = (row: WithdrawalsDB.Entry) => ({
  l2Owner: hex(row[WithdrawalsDB.Columns.L2_OWNER]),
  l2ValueCbor: hex(row[WithdrawalsDB.Columns.L2_VALUE]),
  eventInfoCbor: hex(row[WithdrawalsDB.Columns.RAW_EVENT_INFO]),
  ledgerOutRef: U[LedgerUtils.Columns.OUTREF],
  ledgerOutput: U[LedgerUtils.Columns.OUTPUT],
});

describe("a withdrawal classifier runtime fault", { timeout: 120_000 }, () => {
  // A failed assertion leaves no fault on for the next row.
  afterEach(() => {
    fault.on = false;
  });

  it("holds own-block production without classifying, then builds once it clears", async () => {
    const orders = [await order(0x01)];
    const root = await baseRoot();
    const alarms: [string, string][] = [];
    const onAlarm = (alarm: "held" | "breaker", stage: string) =>
      Effect.sync(() => {
        alarms.push([alarm, stage]);
      });

    fault.on = true;
    const held = await run(Effect.either(build(orders, root, onAlarm)));
    expect(held._tag).toBe("Left");
    if (held._tag !== "Left") return;
    expect(heldBuildStage(held.left)).toBe(WITHDRAWAL_CLASSIFICATION_STAGE);
    expect(alarms).toEqual([["held", WITHDRAWAL_CLASSIFICATION_STAGE]]);
    // Nothing about the withdrawal was recorded or projected.
    expect(await withdrawalRows()).toEqual([
      {
        status: WithdrawalsDB.Status.Awaiting,
        validity: null,
        settlement_event_info: null,
      },
    ]);

    fault.on = false;
    const { produced, rows } = await run(build(orders, root, onAlarm));
    expect(
      produced.includedWithdrawalEntries.map(
        (row) => row[WithdrawalsDB.Columns.VALIDITY],
      ),
    ).toEqual(["WithdrawalIsValid"]);
    expect(rows.map((row) => [row.validity, row.status])).toEqual([
      ["WithdrawalIsValid", WithdrawalsDB.Status.Projected],
    ]);
    expect(alarms).toHaveLength(1);
  });

  it("holds an honest foreign block as a local fault, then imports it once it clears", async () => {
    const orders = [await order(0x02)];
    const { root, payload } = await honestBlock(orders);

    fault.on = true;
    const faulted = await replay(payload, orders, root);
    expect(faulted.result).toMatchObject({
      _tag: "Left",
      left: { reason: "local_fault" },
    });
    expect(faulted.memberships).toEqual([]);

    fault.on = false;
    const imported = await replay(payload, orders, root);
    expect(imported.result._tag).toBe("Right");
    expect(imported.memberships.map(({ validity }) => validity)).toEqual([
      "WithdrawalIsValid",
    ]);
  });

  it("raises the fault from the replayed ledger without refusing the block", async () => {
    const orders = [await order(0x03)];
    const { root, payload } = await honestBlock(orders);
    const header = payload.block_body.header;
    const known = new Map(
      orders.map((row) => [
        hex(row[WithdrawalsDB.Columns.ID]),
        {
          l2Outref: hex(row[WithdrawalsDB.Columns.L2_OUTREF]),
          l2Owner: hex(row[WithdrawalsDB.Columns.L2_OWNER]),
          l2Value: hex(row[WithdrawalsDB.Columns.L2_VALUE]),
          info: hex(row[WithdrawalsDB.Columns.RAW_EVENT_INFO]),
        },
      ]),
    );
    const refusals: ReplayedLedgerRefusal[] = [];
    const ledger = createReplayedLedger({
      checkpoints: {
        read: async () => ({
          headerHash: header.prevHeaderHash,
          entries: BASE.map((row) => ({
            outRef: row[LedgerUtils.Columns.OUTREF],
            output: row[LedgerUtils.Columns.OUTPUT],
          })),
        }),
        write: async () => undefined,
      },
      onRefusal: (refusal) => refusals.push(refusal),
    });
    expect(
      await ledger.restore({
        headerHash: header.prevHeaderHash,
        utxosRoot: root,
      }),
    ).toBe("restored");
    const ledgerAt = ledger.ledgerAt({
      payloadAt: async (headerHash) =>
        headerHash === payload.block_body.header_hash ? payload : undefined,
      materialFor: async () => {
        const material = foreignReplayMaterial(new Map(), known, new Map(), []);
        return {
          blockTimeMs: at(10).getTime(),
          replayUserEvent: material.replayUserEvent,
          verifyForcedSource: material.verifyForcedSource,
        };
      },
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    });

    fault.on = true;
    const raised = await ledgerAt(payload.block_body.header_hash).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(isReplayLocalFault(raised)).toBe(true);
    expect(refusals).toEqual([]);

    fault.on = false;
    const entries = await ledgerAt(payload.block_body.header_hash);
    expect(entries).toBeDefined();
    expect(await ledgerEntriesRoot(entries!)).toBe(header.utxosRoot);
    expect(refusals).toEqual([]);
  });

  it("still refuses a dishonest block over a malformed signature", async () => {
    const malformed: SDK.WithdrawalInfo["signature"] = [
      Buffer.from(privateKey.to_public().to_raw_bytes()).toString("hex"),
      "00".repeat(10),
    ];
    const orders = [await order(0x04, malformed)];
    const { root, produced, payload } = await honestBlock(orders);
    expect(
      produced.includedWithdrawalEntries.map(
        (row) => row[WithdrawalsDB.Columns.VALIDITY],
      ),
    ).toEqual(["IncorrectWithdrawalSignature"]);
    expect((await replay(payload, orders, root)).result._tag).toBe("Right");

    const info = Data.from(
      hex(orders[0]![WithdrawalsDB.Columns.RAW_EVENT_INFO]),
      SDK.WithdrawalInfo,
    ) as SDK.WithdrawalInfo;
    const tampered = await rebind({
      ...payload,
      block_body: {
        ...payload.block_body,
        withdrawals: payload.block_body.withdrawals.map(([key]) => [
          key,
          SDK.committedWithdrawalValueBytes({
            ...info,
            validity: "WithdrawalIsValid",
          }),
        ]),
      },
    });
    expect((await replay(tampered, orders, root)).result).toMatchObject({
      _tag: "Left",
      left: {
        kind: "invalid",
        detail: "a withdrawal's verdict differs from its replay",
      },
    });
  });

  it("classifies a withdrawal whose datum is nested 20,000 deep, and names a fault on it only when the classifier throws", async () => {
    const honest = await order(0x05);
    const deep = deepRow(honest, 20_000);

    // The Data parser reads the whole info: the classifier's verdict is on
    // the signature, which no longer covers the body.
    const classified = await Effect.runPromise(
      Effect.either(SDK.classifyWithdrawalFromLedger(input(deep))),
    );
    expect(classified).toMatchObject({
      _tag: "Right",
      right: { validity: "IncorrectWithdrawalSignature" },
    });

    // A block whose verdict on the row is another refuses as invalid.
    const { root, payload } = await honestBlock([honest]);
    expect((await replay(payload, [deep], root)).result).toMatchObject({
      _tag: "Left",
      left: {
        kind: "invalid",
        detail: "a withdrawal's verdict differs from its replay",
      },
    });

    // Own-block production and foreign replay reach the same verdict.
    const own = await honestBlock([deep]);
    expect(
      own.produced.includedWithdrawalEntries.map(
        (row) => row[WithdrawalsDB.Columns.VALIDITY],
      ),
    ).toEqual(["IncorrectWithdrawalSignature"]);
    const imported = await replay(own.payload, [deep], own.root);
    expect(imported.result._tag).toBe("Right");
    expect(imported.memberships.map(({ validity }) => validity)).toEqual([
      "IncorrectWithdrawalSignature",
    ]);

    // A throw on an info that reads is this node's own fault.
    fault.on = true;
    expect((await replay(payload, [deep], root)).result).toMatchObject({
      _tag: "Left",
      left: { reason: "local_fault" },
    });
    fault.on = false;
  });

  it("holds a block over an info of 1,025 nodes as a local fault, then imports it once the fault clears", async () => {
    const honest = await order(0x06);
    const shallow = plutusDataNodeCount(hex(withDatum(honest, "00")));
    const row = deepRow(honest, 1_025 - shallow);
    expect(
      plutusDataNodeCount(hex(row[WithdrawalsDB.Columns.RAW_EVENT_INFO])),
    ).toBe(1_025);
    const { root, payload } = await honestBlock([row]);

    fault.on = true;
    const held = await run(Effect.either(build([row], root)));
    expect(held._tag === "Left" && heldBuildStage(held.left)).toBe(
      WITHDRAWAL_CLASSIFICATION_STAGE,
    );
    expect((await replay(payload, [row], root)).result).toMatchObject({
      _tag: "Left",
      left: { reason: "local_fault" },
    });

    fault.on = false;
    const imported = await replay(payload, [row], root);
    expect(imported.result._tag).toBe("Right");
    expect(imported.memberships.map(({ validity }) => validity)).toEqual([
      "IncorrectWithdrawalSignature",
    ]);
  });

  it.each([
    { name: "a malformed info", info: (cbor: string) => cbor.slice(0, -2) },
    { name: "an info whose fields do not read", info: () => "d87980" },
  ])(
    "keeps the input's verdict on $name, whether or not the classifier throws",
    async ({ info }) => {
      const honest = await order(0x07);
      const { root, payload } = await honestBlock([honest]);
      const row: WithdrawalsDB.Entry = {
        ...honest,
        raw_event_info: Buffer.from(
          info(hex(honest[WithdrawalsDB.Columns.RAW_EVENT_INFO])),
          "hex",
        ),
      };
      expect((await replay(payload, [row], root)).result).toMatchObject({
        _tag: "Left",
        left: { _tag: "WithdrawalValidationError" },
      });
      fault.on = true;
      expect((await replay(payload, [row], root)).result).toMatchObject({
        _tag: "Left",
        left: { reason: "invalid" },
      });
      fault.on = false;
    },
  );

  it.each(CANONICAL_MAX_DEPTHS)(
    "classifies an info whose $shape datum nests $depth deep without throwing",
    async ({ shape, depth }) => {
      const row = deepRow(await order(0x08), depth, shape);
      expect(
        await Effect.runPromise(
          Effect.either(SDK.classifyWithdrawalFromLedger(input(row))),
        ),
      ).toMatchObject({
        _tag: "Right",
        right: { validity: "IncorrectWithdrawalSignature" },
      });
    },
  );
});
