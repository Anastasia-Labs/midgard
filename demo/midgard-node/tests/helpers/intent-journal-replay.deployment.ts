/**
 * Replays the intents a deposit-flow emulator flow journaled onto a
 * production follower of its chain (`replayJournaledOnFollower`), as the
 * fixture's operator node: its operator wallet, and the reference-script
 * wallet that published every script and holds them.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect } from "vitest";

import type { NodeIntentFamily } from "../../src/services/intent-journal.js";
import {
  type EmulatorFixture,
  makeNodeConfigForFixture,
  runNodeDatabaseEffect,
} from "../deposit-flow-emulator-shared.js";
import { followedChainOf } from "./follower-emulator.chain.js";
import { followedEmulatorHeights } from "./intent-journal-replay.anchor.js";
import { expectReplayedFamilies } from "./intent-journal-replay.expect.js";
import { replayJournaledOnFollower } from "./intent-journal-replay.js";

/**
 * The deployment's families: replayed only when the flow's process deployed
 * the shared fixture (a restored one's deployment intents were journaled by
 * the process that deployed it).
 */
const DEPLOYMENT = ["reference_funding", "reference_publication"] as const;

/** The operator's families from the fixture's registration to a merged block. */
export const TO_MERGED_BLOCK: readonly NodeIntentFamily[] = [
  "script_reward_registration",
  "phas_membership",
  "register",
  "activate",
  "scheduler_refresh",
  "commit",
  "attest",
  "merge",
];

/**
 * Replays the flow, expects exactly `families` (and any deployment family),
 * each recorded and wanted at the block before it landed, and expects the
 * first commit to be `firstCommitHeader`'s, its deposits judged: bound to an
 * L1 event key the follower admitted, not vacuously settled.
 */
export const expectFixtureFlowReplayed = async (
  fixture: EmulatorFixture,
  families: readonly NodeIntentFamily[],
  firstCommitHeader: string,
) => {
  const referenceScripts = await fixture.referenceScriptsLucid
    .wallet()
    .address();
  const replayed = expectReplayedFamilies(
    await replayJournaledOnFollower({
      emulator: fixture.emulator,
      contracts: fixture.contracts,
      config: {
        ...(await makeNodeConfigForFixture(fixture)),
        L1_REFERENCE_SCRIPT_SEED_PHRASE:
          fixture.referenceScriptsAccount.seedPhrase,
        L1_REFERENCE_SCRIPT_ADDRESS: referenceScripts,
        L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: referenceScripts,
      },
      slotToPosixMs: (slot) => fixture.operatorLucid.slotToUnixTime(slot),
      operatorKeyHash: fixture.operatorKeyHash,
      anchorEmulatorHeight: followedEmulatorHeights(
        followedChainOf(fixture.emulator),
      ),
    }),
    families,
    DEPLOYMENT,
  );
  expect(replayed.commit![0]!.contentRef).toBe(firstCommitHeader);
  const [bound] = await runNodeDatabaseEffect(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ readonly n: string }>`SELECT count(*) AS n
        FROM pending_block_finalization_deposits m
        JOIN deposits_utxos e ON e.event_id = m.member_id
        WHERE m.header_hash = ${Buffer.from(firstCommitHeader, "hex")}
          AND e.l1_event_key IS NOT NULL`,
    ),
  );
  expect(Number(bound!.n)).toBeGreaterThan(0);
  return replayed;
};
