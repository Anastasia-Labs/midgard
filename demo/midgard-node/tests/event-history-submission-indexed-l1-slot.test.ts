import type * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { NodeConfig } from "../src/services/index.js";
import {
  indexedL1Slot,
  settleExpiredHistoryAttempt,
} from "../src/transactions/event-history-submission.indexed-l1-slot.js";

/** Kupo's `/health` Prometheus text, as lc1's Kupo serves it. */
const kupoHealth = (checkpoint: string) =>
  [
    "# TYPE kupo_configuration_indexes gauge",
    "kupo_configuration_indexes  1.0",
    "# TYPE kupo_connection_status gauge",
    "kupo_connection_status  1.0",
    "# TYPE kupo_most_recent_checkpoint counter",
    `kupo_most_recent_checkpoint  ${checkpoint}`,
    "# TYPE kupo_most_recent_node_tip counter",
    "kupo_most_recent_node_tip  48280",
    "",
  ].join("\n");

const fromKupo = (response: Response) => {
  const fetch = vi.fn(async () => response);
  vi.stubGlobal("fetch", fetch);
  // A Lucid with no emulator provider reads the configured Kupo.
  const slot = Effect.runPromise(
    indexedL1Slot({} as LucidEvolution).pipe(
      Effect.provideService(NodeConfig, {
        L1_KUPO_KEY: "http://kupo.test/",
      } as never),
    ),
  );
  return { fetch, slot };
};

afterEach(() => {
  vi.unstubAllGlobals();
});

describe("indexed L1 slot outside the emulator", () => {
  it("reads Kupo's most recent checkpoint from its health metrics", async () => {
    const { fetch, slot } = fromKupo(new Response(kupoHealth("48271")));
    expect(await slot).toBe(48271);
    expect(fetch).toHaveBeenCalledWith("http://kupo.test/health", {
      headers: { accept: "text/plain" },
      signal: expect.any(AbortSignal),
    });
  });

  it("reads a checkpoint printed as a whole float, like Kupo's gauges", async () => {
    expect(await fromKupo(new Response(kupoHealth("48271.0"))).slot).toBe(
      48271,
    );
  });

  it.each([
    ["an unhealthy Kupo", new Response(kupoHealth("48271"), { status: 503 })],
    ["a missing checkpoint", new Response("# TYPE kupo_x gauge\nkupo_x  1.0")],
    ["a fractional checkpoint", new Response(kupoHealth("48271.5"))],
  ])("treats %s as no indexed slot", async (_, response) => {
    expect(await fromKupo(response).slot).toBeUndefined();
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
