import { Lucid, PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { l1SlotNow } from "midgard-node/l1-heads";
import { afterEach, describe, expect, it, vi } from "vitest";

import { journeyLucid, registerJourneyL1Tip } from "./journey-lucid.js";
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
    return Response.json({ jsonrpc: "2.0", result: { slot: TIP_SLOT } });
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

  it("leaves a live Lucid without the harness source slot-unknown", async () => {
    stubOgmiosTip();
    const unregistered = await Lucid(harnessProvider(), "Custom", {
      slotConfig: network.customNetwork.slotConfig,
    });

    const failure = await Effect.runPromise(
      Effect.flip(l1SlotNow(unregistered)),
    );
    expect(failure).toMatchObject({
      _tag: "L1SlotUnknownError",
      message: "L1 slot unknown: this Lucid client has no tip source",
    });

    // The capture tests build their own Lucid and register the same source.
    registerJourneyL1Tip(unregistered, network);
    await expect(
      Effect.runPromise(l1SlotNow(unregistered)),
    ).resolves.toBeGreaterThanOrEqual(TIP_SLOT);
  });
});
