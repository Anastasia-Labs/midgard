import { Lucid, PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { l1SlotNow } from "midgard-node/l1-heads";
import { afterEach, describe, expect, it, vi } from "vitest";

import { journeyL1Access, journeyLucid } from "./journey-lucid.js";
import {
  JourneyLocalKupmios,
  type JourneyNativeNodeQuery,
} from "./local-kupmios.js";

const native: JourneyNativeNodeQuery = {
  binaryPath: "/unused/native-query",
  timeoutMs: 10_000,
  watcherConfig: {
    targetNetwork: "Preprod",
    l1: {
      source: {
        sourceMode: "local_node",
        authorityNodeId: "test-node",
        chainSync: {
          kind: "cardano_node_socket",
          socketPath: "/unused/node.socket",
          nodeConfigPath: "/unused/node.json",
          genesisConfigPath: "/unused/genesis.json",
          genesisIdentitySha256: "ab".repeat(32),
        },
      },
    },
  },
};
const ogmiosUrl = "http://ogmios.test";
const network = {
  kupoUrl: "http://kupo.test",
  ogmiosUrl,
  customNetwork: {
    slotConfig: { zeroTime: 1_700_000_000_000, zeroSlot: 0, slotLength: 100 },
  },
};
const TIP_SLOT = 4_242;

/** The harness provider: the live Kupmios kind, never an emulator. */
const harnessProvider = () => {
  const provider = new JourneyLocalKupmios(
    "http://kupo.test",
    ogmiosUrl,
    native,
  );
  provider.getProtocolParameters = async () => PROTOCOL_PARAMETERS_DEFAULT;
  return provider;
};

/** Ogmios answers its tip query; anything else is unexpected. */
const stubOgmiosTip = () => {
  const fetch = vi.fn(async (url: unknown, init?: RequestInit) => {
    expect(String(url)).toBe(ogmiosUrl);
    expect(JSON.parse(String(init?.body)).method).toBe("queryNetwork/tip");
    return Response.json({
      jsonrpc: "2.0",
      result: { slot: TIP_SLOT, id: "ab".repeat(32) },
    });
  });
  vi.stubGlobal("fetch", fetch);
  return fetch;
};

describe("journey Lucid heads tip source", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("gives a harness-built live Lucid an L1 slot from the devnet tip", async () => {
    const fetch = stubOgmiosTip();
    const lucid = await journeyLucid(harnessProvider(), network);

    const slot = await Effect.runPromise(l1SlotNow(lucid));
    expect(slot).toBeGreaterThanOrEqual(TIP_SLOT);
    expect(slot).toBeLessThan(TIP_SLOT + 100);
    expect(fetch).toHaveBeenCalled();
  });

  it("leaves a live Lucid over no L1 access slot-unknown, and gives every Lucid over the harness provider its clock", async () => {
    stubOgmiosTip();
    const provider = harnessProvider();
    const unopened = await Lucid(provider, "Custom", {
      slotConfig: network.customNetwork.slotConfig,
    });

    const failure = await Effect.runPromise(Effect.flip(l1SlotNow(unopened)));
    expect(failure).toMatchObject({ _tag: "L1SlotUnknownError" });

    // A capture test builds its own Lucid over the harness provider: once
    // the harness access is open on it, that Lucid has the same clock.
    const access = journeyL1Access(provider, network);
    expect(journeyL1Access(provider, network)).toBe(access);
    await expect(
      Effect.runPromise(l1SlotNow(unopened)),
    ).resolves.toBeGreaterThanOrEqual(TIP_SLOT);
  });
});
