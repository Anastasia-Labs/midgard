import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { describe, expect, it } from "vitest";

import type { StateQueueHeaderRecord } from "../src/domain.js";
import {
  hashBlockHeader,
  scanStateQueue,
  type StateQueueL1View,
  type StateQueueReplayAnchor,
} from "../src/l1/state-queue-scanner.js";
import {
  finalityHeldHeaderHashes,
  retentionQueueReference,
} from "../src/store/retention.js";
import { makePayloadFixture } from "./helpers.js";
import {
  type ChainHeader,
  createStateQueueChain,
  type StateQueueChain,
} from "./helpers/state-queue-chain.js";

const finalityDepth = 3;

const common = {
  deploymentFingerprint: "11".repeat(32),
  deploymentIdentityDigest: "11".repeat(32),
  stateQueuePolicyId: "22".repeat(28),
  daAttestationPolicyId: "33".repeat(28),
  finalityDepth,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
} as const;

const chainHeaders = async (count: number): Promise<readonly ChainHeader[]> => {
  const { header: base } = await makePayloadFixture();
  return Array.from({ length: count }, (_, index) => {
    const header = { ...base, endTime: base.endTime + BigInt(index) };
    return {
      header,
      headerHash: hashBlockHeader(header),
      daAttestation: { Attested: { commitment_hash: "33".repeat(32) } },
    };
  });
};

/**
 * Scans `chain` the way the committee does: from the durable anchor of the
 * prior scan and every header record it has stored so far.
 */
const scanner = (chain: StateQueueChain) => {
  let durable: StateQueueReplayAnchor | undefined;
  const stored = new Map<string, StateQueueHeaderRecord>();
  return async (): Promise<StateQueueL1View> => {
    let view: StateQueueL1View | undefined;
    let deferred: ReadonlySet<string> = new Set();
    const records = await scanStateQueue(
      {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
        fetchStateQueueReplayCheckpoints:
          chain.fetchStateQueueReplayCheckpoints,
      },
      {
        ...common,
        ...(durable === undefined ? {} : { terminalReplayAnchor: durable }),
        previousHeaders: [...stored.values()],
        recordReplayAnchor: (anchor) => {
          durable = anchor;
        },
        recordReplayedHeaderSteps: ({ deferredHeaderHashes }) => {
          deferred = new Set(deferredHeaderHashes);
        },
        recordL1View: (next) => {
          view = next;
        },
      },
    );
    // A header a young checkpoint moved keeps its stored record.
    for (const record of records)
      if (!deferred.has(record.headerHash))
        stored.set(record.headerHash, record);
    return view!;
  };
};

const referenceOf = (view: StateQueueL1View, headerHash: string) =>
  retentionQueueReference(headerHash, {
    confirmedHeadHash: view.confirmedHeaderHash,
    liveQueueHeaderHashes: new Set(view.liveQueueHeaderHashes),
  });

describe("state-queue scanner L1 view held to finality", () => {
  it("keeps headers merged by young checkpoints, and the head they extend, until those merges are final", async () => {
    const headers = await chainHeaders(3);
    const [first, second, third] = headers.map(({ headerHash }) => headerHash);
    const chain = createStateQueueChain({
      deploymentIdentityDigest: common.deploymentIdentityDigest,
      stateQueuePolicyId: common.stateQueuePolicyId,
      headers,
      tip: 5,
    });
    const scan = scanner(chain);
    await scan();
    chain.mine("merge");
    for (let block = 0; block < finalityDepth; block += 1) chain.mine();
    // `first`'s merge is final: it is the confirmed head at both depths.
    const settled = await scan();
    expect(settled.confirmedHeaderHash).toBe(first);
    expect(settled.liveQueueHeaderHashes).toEqual(
      expect.arrayContaining([second, third]),
    );

    // Two young merges: at its tip L1 shows `third` as the head and nothing
    // queued, while a release-final reader still sees `first` as the head
    // with `second` and `third` queued on it.
    chain.mine("merge");
    chain.mine("merge");
    const young = await scan();
    expect(young.confirmedHeaderHash).toBe(third);
    expect([...young.liveQueueHeaderHashes].sort()).toEqual(
      [first!, second!, third!].sort(),
    );
    expect(referenceOf(young, first!)).toBe("live_in_queue");
    expect(referenceOf(young, second!)).toBe("live_in_queue");

    for (let block = 0; block < finalityDepth; block += 1) chain.mine();
    const final = await scan();
    expect(final.confirmedHeaderHash).toBe(third);
    expect(final.liveQueueHeaderHashes).toEqual([third]);
    expect(referenceOf(final, first!)).toBe("none");
    expect(referenceOf(final, second!)).toBe("none");
  });

  it("finds the newest final merge across every stored header record", () => {
    // Stored header records are never pruned, so a long-running committee
    // holds far more than one call can take as spread arguments.
    const merged = (headerHash: string, blockHeight: number) =>
      ({
        headerHash,
        status: "merged",
        finalized: true,
        observedChainPoint: {
          providerSource: "authenticated_state_queue_transition_v1",
          blockHeight,
        },
      }) as unknown as StateQueueHeaderRecord;
    const records = Array.from({ length: 400_000 }, (_, index) =>
      merged(index.toString(16).padStart(56, "0"), index),
    );
    expect(finalityHeldHeaderHashes(["aa".repeat(28)], records)).toEqual([
      "aa".repeat(28),
      (399_999).toString(16).padStart(56, "0"),
    ]);
  });
});
