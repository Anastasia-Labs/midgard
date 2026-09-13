import { existsSync } from "node:fs";
import { join } from "node:path";

import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { verifyDuplicateEventSource } from "./history-settlement.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const path =
  runDirectory === undefined
    ? undefined
    : join(runDirectory, "work/journeys/transition-trace/staged.json");

it.skipIf(path === undefined || !existsSync(path))(
  "binds settlement source content to the real deposit and its unchanged seven-day maturity",
  async () => {
    const staged = await readJourneyArtifact<PublishedDepositTraceCheckpoint>(
      path!,
    );
    const block = await depositEventsRetainedBlock({
      operatorVkey: staged.current.header.operatorVkey,
      startTime: staged.predecessor.header.endTime,
      endTime: staged.current.header.endTime,
      blockSlot: staged.current.header.blockSlot,
      prevHeaderHash: staged.predecessor.headerHash,
      prevUtxosRoot: staged.predecessor.header.utxosRoot,
      priorLedger: staged.predecessor.payload.block_body.utxos,
      events: [
        {
          event: staged.depositEvent,
          depositPolicyId: staged.depositMetadata.depositAuthUnit.slice(0, 56),
          assetName: staged.depositMetadata.depositAssetName,
          honest: true,
        },
      ],
    });
    // Reconstructed local content is deliberately not submitted or represented
    // as a verified live successor. The production adoption path requires the
    // independent result/native evidence gate before this content check.
    const source = { ...block, commitTxHash: staged.commits[1]! };
    const datum = staged.depositEvent.datum!;
    expect(await verifyDuplicateEventSource(source, datum)).toBe(
      block.header.endTime + 604_800_000n,
    );
    const changedEvent = Data.from(datum, SDK.DepositDatum);
    changedEvent.event.id.outputIndex += 1n;
    await expect(
      verifyDuplicateEventSource(
        source,
        Data.to(changedEvent, SDK.DepositDatum),
      ),
    ).rejects.toThrow("exact genuine deposit");
    await expect(
      verifyDuplicateEventSource(
        { ...source, headerHash: "00".repeat(28) },
        datum,
      ),
    ).rejects.toThrow("header hash changed");
  },
);
