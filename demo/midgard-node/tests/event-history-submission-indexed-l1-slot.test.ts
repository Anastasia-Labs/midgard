import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { registerL1ProviderView } from "../src/l1-provider-view.js";
import {
  indexedL1Slot,
  settleExpiredHistoryAttempt,
} from "../src/transactions/event-history-submission.indexed-l1-slot.js";

describe("indexed L1 slot outside the emulator", () => {
  it("reads the follower's synchronized view point registered for the client", async () => {
    const lucid = await Lucid(new Emulator([]), "Custom");
    const reads: string[] = [];
    registerL1ProviderView([lucid], {
      submitSlotSnapshot: () =>
        Effect.fail(
          new Error("the submit-slot snapshot is not the indexed slot"),
        ),
      viewPoint: () =>
        Effect.sync(() => {
          reads.push("view");
          return { slot: 48_271, id: "ab".repeat(32) };
        }),
    });
    expect(await Effect.runPromise(indexedL1Slot(lucid))).toBe(48_271);
    expect(reads).toEqual(["view"]);
  });

  it("treats a follower behind the node's tip as no indexed slot", async () => {
    const lucid = await Lucid(new Emulator([]), "Custom");
    registerL1ProviderView([lucid], {
      submitSlotSnapshot: () => Effect.die("unused"),
      viewPoint: () =>
        Effect.fail(
          new L1ProviderTransientError("follower", "behind_node_tip"),
        ),
    });
    expect(await Effect.runPromise(indexedL1Slot(lucid))).toBeUndefined();
  });

  it("treats a client with no registered view and no emulator as no indexed slot", async () => {
    expect(
      await Effect.runPromise(
        indexedL1Slot({
          config: () => ({ provider: undefined }),
        } as unknown as LucidEvolution),
      ),
    ).toBeUndefined();
  });
});

describe("settling an expired history attempt", () => {
  const setup = async () => {
    const wallet = generateEmulatorAccount({ lovelace: 100_000_000n });
    const emulator = new Emulator([wallet]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(wallet.seedPhrase);
    emulator.awaitSlot(100);
    const tx = await lucid
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 10_000_000n })
      .validTo(emulator.now() + 20_000)
      .complete({ localUPLCEval: true });
    const attempt: SDK.EventHistorySubmissionAttempt = {
      phase: "Admission",
      txHash: tx.toHash(),
      outputIndex: 0,
      transactionCbor: tx.toCBOR(),
    };
    const ttl = Number(CML.Transaction.from_cbor_hex(tx.toCBOR()).body().ttl());
    const toSlot = (slot: number) =>
      emulator.awaitSlot(slot - lucid.unixTimeToSlot(emulator.now()));
    return { lucid, attempt, ttl, toSlot };
  };
  const settle = (
    lucid: LucidEvolution,
    attempt: SDK.EventHistorySubmissionAttempt,
    kind: SDK.EventHistorySubmissionOutcome["kind"],
  ) => {
    const observe = vi.fn(async () => ({ kind }));
    return {
      observe,
      settled: Effect.runPromise(
        settleExpiredHistoryAttempt(lucid, attempt, observe),
      ),
    };
  };

  it("leaves an attempt whose TTL is at or above the indexed slot unsettled, without observing it", async () => {
    const h = await setup();
    h.toSlot(h.ttl);
    const { observe, settled } = settle(h.lucid, h.attempt, "Pending");
    expect(await settled).toBeUndefined();
    expect(observe).not.toHaveBeenCalled();
  });

  it("settles an expired attempt that the later observation did not see as an input conflict", async () => {
    const h = await setup();
    h.toSlot(h.ttl + 1);
    const { observe, settled } = settle(h.lucid, h.attempt, "Pending");
    expect(await settled).toEqual({ kind: "InputConflict" });
    expect(observe).toHaveBeenCalledOnce();
  });

  it("confirms an expired attempt that the later observation found landed", async () => {
    const h = await setup();
    h.toSlot(h.ttl + 1);
    const { settled } = settle(h.lucid, h.attempt, "Confirmed");
    expect(await settled).toEqual({ kind: "Confirmed" });
  });
});
