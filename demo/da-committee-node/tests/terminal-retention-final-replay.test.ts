import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it, vi } from "vitest";

import { StateQueueHistoryNotExtendingAnchorError } from "../src/l1/source-integrity.js";
import {
  catchUpRetentionOutcomes,
  terminalRetentionOutcomes,
} from "../src/l1/terminal-retention-observation.js";
import { fixtureHeaderBase } from "./helpers.js";
import { createStateQueueChain } from "./helpers/state-queue-chain.js";

// An SDK that accepts a history at depth one but finds no path through its
// final prefix. The real SDK cannot do that (a final prefix of accepted
// history is itself accepted), so this pins the observation's own refusal
// rather than relying on that property.
const sdk = vi.hoisted(() => ({ refuseFinalPrefix: false }));
vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof SDK>();
  return {
    ...actual,
    replayStateQueueAuthenticatedCheckpoints: (
      input: Parameters<
        typeof actual.replayStateQueueAuthenticatedCheckpoints
      >[0],
    ) =>
      sdk.refuseFinalPrefix && input.minimumFinalityDepth > 1n
        ? null
        : actual.replayStateQueueAuthenticatedCheckpoints(input),
  };
});

afterEach(() => {
  sdk.refuseFinalPrefix = false;
});

const deployment = "aa".repeat(32);
const policy = "bb".repeat(28);
const finalityDepth = 3;

const finalHistory = async () => {
  const header = {
    ...fixtureHeaderBase(),
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const headerHash = "11".repeat(28);
  const chain = createStateQueueChain({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    headers: [{ header, headerHash }],
    tip: 1,
  });
  const anchor = chain.queue();
  // Its only header is merged, so the queue it ends at is the bare root.
  chain.mine({ attest: headerHash });
  chain.mine("merge");
  for (let block = 0; block <= finalityDepth; block += 1) chain.mine();
  const checkpoints = await chain.fetchStateQueueReplayCheckpoints(
    anchor,
    chain.queue(),
    chain.tip,
    64,
  );
  return {
    checkpoints,
    snapshot: chain.snapshot(),
    config: {
      deploymentFingerprint: deployment,
      deploymentIdentityDigest: deployment,
      stateQueuePolicyId: policy,
      finalityDepth,
      replayAnchor: {
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: policy,
        queue: anchor,
        blockNo: "1",
        transactionIndex: "0",
      },
    },
  };
};

describe("final-prefix replay the SDK finds no path through", () => {
  it("is history not extending the anchor, in a scan and in a catch-up", async () => {
    const { checkpoints, snapshot, config } = await finalHistory();
    // Unmocked, both accept it.
    expect(
      terminalRetentionOutcomes([], [], checkpoints, snapshot, config)
        .finalAnchor?.blockNo,
    ).toBe(checkpoints.at(-1)!.blockNo);
    expect(
      catchUpRetentionOutcomes([], checkpoints, config)?.finalAnchor.blockNo,
    ).toBe(checkpoints.at(-1)!.blockNo);
    sdk.refuseFinalPrefix = true;
    for (const observe of [
      () => terminalRetentionOutcomes([], [], checkpoints, snapshot, config),
      () => catchUpRetentionOutcomes([], checkpoints, config),
    ]) {
      expect(observe).toThrow(StateQueueHistoryNotExtendingAnchorError);
      expect(observe).toThrow(
        "state-queue final checkpoint history is non-canonical or does not extend the durable cursor",
      );
    }
  });
});
