import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import { isReplayLocalFault } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";
import { describe, expect, it, vi } from "vitest";

import { watcherReplayMaterial } from "../../src/l1-follower/replay-material.js";
import type { WatcherUserEvent } from "../../src/verification/user-event.js";

/**
 * The withdrawal classifier failing at runtime on an event it handles is
 * this watcher's own fault: the replay raises it as such, so no claim is
 * made on the block. An event the Data parser cannot read is not.
 */

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

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const LOVELACE = 7_000_000n;
const outRefKey = encodeMidgardSpendInputItem({
  txId: Buffer.alloc(32, 0xaa),
  outputIndex: 0,
}).toString("hex");
const ledger = new Map([
  [
    outRefKey,
    encodeMidgardTxOutput({
      address: midgardAddressFromText(wallet.address),
      value: { lovelace: LOVELACE, assets: new Map() },
    }),
  ],
]);
const eventId = Data.to(
  { transactionId: "01".repeat(32), outputIndex: 0n },
  SDK.OutputReference,
);
const MARKER = "c0ffee".repeat(4);

/** A signed withdrawal of the ledger's one output; with `depth`, its L1
 * datum is an inline datum nested `depth` lists deep. */
const withdrawalEvent = async (depth?: number): Promise<WatcherUserEvent> => {
  const value: SDK.Value = new Map([["", new Map([["", LOVELACE]])]]);
  const body: SDK.WithdrawalBody = {
    l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
    l2_owner: privateKey.to_public().hash().to_hex(),
    l2_value: value,
    l1_address: await Effect.runPromise(
      SDK.addressDataFromBech32(wallet.address),
    ),
    l1_datum:
      depth === undefined ? "NoDatum" : { InlineDatum: { data: MARKER } },
  };
  const encoded = Data.to(
    {
      id: Data.from(eventId, SDK.OutputReference),
      info: {
        body,
        signature: SDK.signWithdrawalBody(privateKey, body),
        validity: "WithdrawalIsValid",
      },
    },
    SDK.WithdrawalEvent,
  );
  return {
    kind: "withdrawal",
    eventId,
    nonceOutRef: `${"01".repeat(32)}#0`,
    policyId: "00".repeat(28),
    assetNameHex: "00".repeat(32),
    inclusionTime: "5",
    eventCborHex:
      depth === undefined
        ? encoded
        : encoded.replace(`4c${MARKER}`, `${"81".repeat(depth)}00`),
    originalAssetsCborHex: null,
    admission: {} as WatcherUserEvent["admission"],
  };
};

const replayed = async (read: WatcherUserEvent, settlement: string) =>
  Effect.runPromiseExit(
    watcherReplayMaterial({
      network: "Preprod",
      header: { startTime: 0n, endTime: 10n } as SDK.Header,
      blockTimeMs: 10,
      event: async () => read,
    }).replayUserEvent({
      step: { phase: "Withdrawal" } as unknown as SDK.TransitionStep,
      source: [eventId, settlement],
      ledger,
    }),
  );

const localFaults = (exit: Exit.Exit<unknown, unknown>) =>
  Exit.isFailure(exit)
    ? [...Cause.defects(exit.cause), ...Cause.failures(exit.cause)].filter(
        isReplayLocalFault,
      )
    : [];

describe("watcher withdrawal replay under a classifier runtime fault", () => {
  it("raises a local fault on an event it handles, and replays it once the fault clears", async () => {
    const read = await withdrawalEvent();
    const info = Data.from(read.eventCborHex, SDK.WithdrawalEvent).info;
    const classification = await Effect.runPromise(
      SDK.classifyWithdrawalFromLedger({
        l2Owner: info.body.l2_owner,
        l2ValueCbor: Data.to(info.body.l2_value, SDK.Value),
        eventInfoCbor: Data.to(info, SDK.WithdrawalInfo),
        ledgerOutRef: Buffer.from(outRefKey, "hex"),
        ledgerOutput: ledger.get(outRefKey)!,
      }),
    );
    expect(classification.validity).toBe("WithdrawalIsValid");
    const settlement = classification.settlementEventInfo.toString("hex");

    fault.on = true;
    const faulted = await replayed(read, settlement);
    fault.on = false;
    expect(Exit.isFailure(faulted)).toBe(true);
    expect(localFaults(faulted)).toHaveLength(1);

    const exit = await replayed(read, settlement);
    expect(Exit.isSuccess(exit) ? exit.value : exit).toEqual([
      { key: outRefKey, output: null },
    ]);
  });

  it("never names a local fault on a datum nested past what the Data parser reads", async () => {
    const read = await withdrawalEvent(20_000);
    for (const on of [false, true]) {
      fault.on = on;
      const exit = await replayed(read, "00");
      fault.on = false;
      expect(Exit.isFailure(exit)).toBe(true);
      expect(localFaults(exit)).toEqual([]);
    }
  });
});
