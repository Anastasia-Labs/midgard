import { afterEach, describe, expect, it } from "vitest";

import { assertAvailabilityResponderSourceHealthy } from "../src/availability/source-authority.js";
import { l1SourceAuthorityDigest } from "../src/config.js";
import type { CommitteeStore, L1SourceState } from "../src/store.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";

const l1Source = {
  sourceMode: "local_node" as const,
  authorityNodeId: "node-a",
  chainSyncProviderUrl: "chain-sync:ogmios:ws://ogmios.local",
  queryProviderUrls: ["kupmios:http://kupo.local|ws://ogmios.local"],
};
const network = "Preprod";
const responderConfig = { network, l1Source };

const sourceState = (): L1SourceState => ({
  schemaVersion: 1,
  sourceMode: "local_node",
  network,
  authoritySha256: l1SourceAuthorityDigest(network, l1Source),
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
    ).rejects.toThrow(/healthy authenticated/u);
    await store.saveL1SourceState(sourceState());
    await expect(
      assertAvailabilityResponderSourceHealthy(store, responderConfig),
    ).resolves.toBeUndefined();
    await expect(
      assertAvailabilityResponderSourceHealthy(store, {
        network,
        l1Source: { ...l1Source, authorityNodeId: "node-b" },
      }),
    ).rejects.toThrow(/healthy authenticated/u);
    // A source recorded on another network keeps the configured authority
    // digest, so only the network clause can refuse it.
    const unbound = await openStore();
    await unbound.saveL1SourceState({ ...sourceState(), network: "Mainnet" });
    await expect(
      assertAvailabilityResponderSourceHealthy(unbound, responderConfig),
    ).rejects.toThrow(/healthy authenticated/u);
  });
});
