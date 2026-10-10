import type { TransportReadiness } from "@al-ft/l1-node-transport";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import type { SlotConfig } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type L1ProviderPreflightConfig,
  runL1ProviderPreflight,
} from "../src/commands/l1-provider-preflight.js";
import { ledgerSubmitSlotSnapshot } from "../src/services/l1-provider.js";

const NOW_MS = 1_800_000_000_000;
const SLOT_CONFIG: SlotConfig = {
  zeroTime: NOW_MS - 1_000_000,
  zeroSlot: 0,
  slotLength: 1_000,
};
const BOUND_MS = 200_000;
const READY: TransportReadiness = { ready: true, nodeToClientVersion: 16 };

const config = (
  overrides: Partial<L1ProviderPreflightConfig> = {},
): L1ProviderPreflightConfig => ({
  network: "Preprod",
  endpoint: "/run/cardano/node.socket",
  timeoutMs: 1_000,
  transportReadiness: () => READY,
  readSubmitSlotSnapshot: async () =>
    ledgerSubmitSlotSnapshot({
      slotConfig: SLOT_CONFIG,
      ledgerTipSlot: 995,
      nowMs: NOW_MS,
      boundMs: BOUND_MS,
    }),
  ...overrides,
});

describe("the node's L1 preflight", () => {
  it("passes on a ready transport and a ledger tip within the bound", async () => {
    const report = await runL1ProviderPreflight({
      config: config(),
      nowMs: NOW_MS,
    });
    expect(report.ok).toBe(true);
    expect(report.route).toEqual({ primary: "l1_node", network: "Preprod" });
    expect(report.healthySources).toEqual(["l1_node"]);
    expect(report.unhealthySources).toEqual([]);
    expect(report.sources[0]).toMatchObject({
      source: "l1_node",
      endpoint: "/run/cardano/node.socket",
      healthy: true,
      localLedgerSlot: {
        source: "l1_node_tip",
        currentSlot: 1_000,
        ledgerTipSlot: 995,
      },
    });
  });

  it("names the transport's unready reason when the read fails on a down transport", async () => {
    const report = await runL1ProviderPreflight({
      config: config({
        transportReadiness: () => ({
          ready: false,
          reason: "node_unreachable",
          detail: "connect ENOENT /run/cardano/node.socket",
        }),
        readSubmitSlotSnapshot: () =>
          Promise.reject(
            new L1ProviderTransientError("transport", "node_unreachable"),
          ),
      }),
    });
    expect(report.ok).toBe(false);
    expect(report.unhealthySources).toEqual(["l1_node"]);
    expect(report.sources[0]).toMatchObject({
      healthy: false,
      failureKind: "transport:node_unreachable",
      bodySummary: "connect ENOENT /run/cardano/node.socket",
    });
  });

  it("names a ledger tip behind wall time past the bound", async () => {
    const report = await runL1ProviderPreflight({
      config: config({
        readSubmitSlotSnapshot: async () =>
          ledgerSubmitSlotSnapshot({
            slotConfig: SLOT_CONFIG,
            ledgerTipSlot: 700,
            nowMs: NOW_MS,
            boundMs: BOUND_MS,
          }),
      }),
    });
    expect(report.ok).toBe(false);
    expect(report.sources[0]).toMatchObject({
      healthy: false,
      failureKind: "l1_node_behind",
    });
  });

  it("names a transient read failure on a ready transport by its source and reason", async () => {
    const report = await runL1ProviderPreflight({
      config: config({
        readSubmitSlotSnapshot: () =>
          Promise.reject(
            new L1ProviderTransientError("follower", "behind_node_tip"),
          ),
      }),
    });
    expect(report.sources[0]).toMatchObject({
      healthy: false,
      failureKind: "follower:behind_node_tip",
    });
  });

  it("keeps the nested cause of a non-transient read failure", async () => {
    const report = await runL1ProviderPreflight({
      config: config({
        readSubmitSlotSnapshot: () =>
          Promise.reject(
            new TypeError("ledger query failed", {
              cause: new Error("era history is malformed"),
            }),
          ),
      }),
    });
    expect(report.sources[0]).toMatchObject({
      healthy: false,
      failureKind: "l1_read_failed",
      bodySummary:
        "TypeError: ledger query failed; cause=Error: era history is malformed",
    });
  });

  it("fails a read that outlives the timeout", async () => {
    const report = await runL1ProviderPreflight({
      config: config({
        timeoutMs: 5,
        readSubmitSlotSnapshot: () => new Promise(() => undefined),
      }),
    });
    expect(report.sources[0]).toMatchObject({
      healthy: false,
      failureKind: "l1_read_failed",
      bodySummary: "Error: the L1 read exceeded 5ms",
    });
  });
});
