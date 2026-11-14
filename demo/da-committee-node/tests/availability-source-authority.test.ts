import { afterEach, describe, expect, it } from "vitest";

import { assertAvailabilityResponderSourceHealthy } from "../src/availability/source-authority.js";
import { l1SourceAuthorityDigest } from "../src/config.js";
import type { CommitteeStore, L1SourceState } from "../src/store.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";

const nativeLedger = {
  authorityNodeId: "node-a",
  socketPath: "/run/cardano/node.socket",
  nodeConfigPath: "/etc/cardano/config.json",
  binaryPath: "/usr/local/bin/midgard-l1-node-transport",
};
const l1Origin = { slot: 100, blockHash: "ab".repeat(32) };
const network = "Preprod";
const responderConfig = { network, nativeLedger, l1Origin };

const sourceState = (): L1SourceState => ({
  schemaVersion: 1,
  sourceMode: "local_node",
  network,
  authoritySha256: l1SourceAuthorityDigest(responderConfig),
  status: "healthy",
  observations: [],
  observedAt: "2026-10-01T00:00:00.000Z",
});

const openStores = new Set<CommitteeStore>();

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close?.()));
  openStores.clear();
});

const openStore = async (): Promise<CommitteeStore> => {
  const store = await openTestCommitteeStore();
  openStores.add(store);
  return store;
};

describe("availability responder chain authority", () => {
  it("accepts only a source bound to the configured authority", async () => {
    const store = await openStore();
    await expect(
      assertAvailabilityResponderSourceHealthy(store, responderConfig),
    ).rejects.toThrow(/bound to its configured L1 source/u);
    await store.saveL1SourceState(sourceState());
    await expect(
      assertAvailabilityResponderSourceHealthy(store, responderConfig),
    ).resolves.toBeUndefined();
    await expect(
      assertAvailabilityResponderSourceHealthy(store, {
        ...responderConfig,
        nativeLedger: { ...nativeLedger, authorityNodeId: "node-b" },
      }),
    ).rejects.toThrow(/bound to its configured L1 source/u);
    // The L1 origin is not part of the authority: correcting L1_ORIGIN
    // keeps the binding (and every retirement floor bound to it).
    await expect(
      assertAvailabilityResponderSourceHealthy(store, {
        ...responderConfig,
        l1Origin: { ...l1Origin, blockHash: "cd".repeat(32) },
      } as typeof responderConfig),
    ).resolves.toBeUndefined();
    // A source recorded on another network keeps the configured authority
    // digest, so only the network clause can refuse it.
    const unbound = await openStore();
    await unbound.saveL1SourceState({ ...sourceState(), network: "Mainnet" });
    await expect(
      assertAvailabilityResponderSourceHealthy(unbound, responderConfig),
    ).rejects.toThrow(/bound to its configured L1 source/u);
  });
});
