/**
 * Real operator-lifecycle transactions on the node intent journal (I1-fix
 * F5): the registration and deregistration a real emulator flow journals,
 * replayed onto a production follower of the same chain
 * (`replayJournaledOnFollower`), are recorded, wanted at the block before
 * each landed, and landed by it. The deposit-to-payout journey
 * (`deposit-flow-emulator-merge-payout`) replays the block families.
 */
import { describe, expect, it } from "vitest";

import {
  deregisterOperatorProgram,
  registerOperatorProgram,
} from "../src/transactions/register-active-operator.js";
import {
  drainJournaledWithoutFollower,
  runWithoutFollower,
} from "./helpers/intent-journal.js";
import {
  expectReplayedFamilies,
  walletReplayConfig,
} from "./helpers/intent-journal-replay.expect.js";
import { replayJournaledOnFollower } from "./helpers/intent-journal-replay.js";
import {
  EMULATOR_REQUIRED_BOND_LOVELACE,
  initOperatorLifecycleFixture,
} from "./operator-lifecycle-emulator.build-operator-lifecycle-snapshot.js";

/** Journaled on demand when a flow publishes a reference script it needs. */
const ON_DEMAND = ["reference_funding", "reference_publication"] as const;

describe("operator intent families on a follower of the real flow's chain", () => {
  it("registration then deregistration", async () => {
    const fixture = await initOperatorLifecycleFixture();
    drainJournaledWithoutFollower();
    const { lucid, referenceScriptsLucid, contracts } = fixture;

    await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    const { deregisterTxHash } = await runWithoutFollower(
      deregisterOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );

    const families = expectReplayedFamilies(
      await replayJournaledOnFollower({
        emulator: fixture.emulator,
        contracts,
        config: walletReplayConfig({
          operatorSeed: fixture.operatorSeedPhrase,
          referenceScriptsSeed: fixture.referenceScriptsSeedPhrase,
          referenceScriptsAddress: await referenceScriptsLucid
            .wallet()
            .address(),
        }),
        slotToPosixMs: (slot) => lucid.slotToUnixTime(slot),
        operatorKeyHash: fixture.operatorKeyHash,
      }),
      ["register", "deregister"],
      ON_DEMAND,
    );
    expect(families.deregister![0]!.txHash).toBe(deregisterTxHash);
  }, 600_000);
});
