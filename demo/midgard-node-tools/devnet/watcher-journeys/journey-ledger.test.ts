import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk/core";
import { expect, it } from "vitest";

import { depositEventsRetainedBlock } from "./journey-fixture-ledger.test-support.js";
import { checkedJourneyConstruction } from "./journey-ledger.js";

const emptyBlock = () =>
  depositEventsRetainedBlock({
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    operatorVkey: "12".repeat(28),
    startTime: 0n,
    endTime: 60_000n,
    blockSlot: 1n,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    priorLedger: [],
    events: [],
  });

it("admits a constructed header the node would commit", async () => {
  const block = await emptyBlock();
  await expect(
    checkedJourneyConstruction(block, block.ledgerEntries),
  ).resolves.toMatchObject({ headerHash: block.headerHash });
});

it("refuses a constructed header whose validation-trace pair the node would refuse", async () => {
  const block = await emptyBlock();
  await expect(
    checkedJourneyConstruction(
      { ...block, header: { ...block.header, validationTraceCount: 1n } },
      block.ledgerEntries,
    ),
  ).rejects.toThrow(
    "Header validation-trace pair must be the empty tree with count zero",
  );
});

it("refuses a constructed header whose event counts do not add up", async () => {
  const block = await emptyBlock();
  await expect(
    checkedJourneyConstruction(
      { ...block, header: { ...block.header, totalEventCount: 1n } },
      block.ledgerEntries,
    ),
  ).rejects.toThrow(
    "Header transition total_event_count does not match source event counts",
  );
});
