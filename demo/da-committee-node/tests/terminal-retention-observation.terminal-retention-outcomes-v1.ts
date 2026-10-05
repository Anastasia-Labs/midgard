import { describe, expect, it } from "vitest";

import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import { terminalRetentionOutcomes } from "../src/l1/terminal-retention-observation.js";
import { createStateQueueChain } from "./helpers/state-queue-chain.js";
import {
  config,
  deployment,
  h28,
  merge,
  outRef,
  point,
  policy,
  record,
  snapshot,
  thrownBy,
  timeout,
} from "./terminal-retention-observation.derive.js";

describe("terminalRetentionOutcomesV1", () => {
  it("does not infer a terminal outcome from disappearance", () => {
    const prior = record(h28("1"), outRef("1", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: prior.headerHash, outRef: prior.stateQueueOutRef },
    ];
    expect(
      terminalRetentionOutcomes(
        [prior],
        [],
        [],
        snapshot(prior.headerHash, outRef("0", 0)),
        config(initial),
      ).records,
    ).toEqual([]);
  });

  it("rejects unanchored and disconnected canonical histories", () => {
    const first = record(h28("1"), outRef("1", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
    ];
    const transition = merge(1, initial);
    expect(() =>
      terminalRetentionOutcomes(
        [first],
        [],
        [transition],
        snapshot(first.headerHash, transition.nextQueue[0]!.outRef),
        { ...config(initial), replayAnchor: undefined },
      ),
    ).toThrow(/durable prior/u);
    expect(() =>
      terminalRetentionOutcomes(
        [first],
        [],
        [transition],
        snapshot(first.headerHash, transition.nextQueue[0]!.outRef),
        config([{ headerHash: null, outRef: outRef("9", 0) }]),
      ),
    ).toThrow(/does not extend/u);
  });

  it("classifies history that fails to extend the durable cursor as integrity", () => {
    const first = record(h28("1"), outRef("1", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
    ];
    const transition = merge(1, initial);
    const failure = thrownBy(() =>
      terminalRetentionOutcomes(
        [first],
        [],
        [transition],
        snapshot(first.headerHash, transition.nextQueue[0]!.outRef),
        config([{ headerHash: null, outRef: outRef("9", 0) }]),
      ),
    );
    expect((failure as Error).message).toMatch(/does not extend/u);
    expect(failure).toBeInstanceOf(L1SourceIntegrityError);
  });

  it("defers the outcome of history younger than the finality depth instead of failing", () => {
    const first = record(h28("1"), outRef("1", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
    ];
    const transition = merge(1, initial);
    // The root snapshot itself is younger than the finality depth.
    const young = {
      ...snapshot(first.headerHash, transition.nextQueue[0]!.outRef),
      observedChainPoint: point(100, 1),
    };

    // The checkpoint has 29 blocks on top; the committee requires 30.
    const early = terminalRetentionOutcomes([first], [], [transition], young, {
      ...config(initial),
      finalityDepth: 30,
    });
    expect(early).toEqual({
      records: [],
      deferredHeaderHashes: [first.headerHash],
      finalSteps: new Map(),
    });

    // Once it is final, the deferred outcome is recorded and the queue after
    // it becomes the final anchor.
    const final = terminalRetentionOutcomes(
      [first],
      [],
      [transition],
      young,
      config(initial),
    );
    expect(final.records.map(({ status }) => status)).toEqual(["merged"]);
    expect(final.deferredHeaderHashes).toEqual([]);
    expect(final.finalSteps).toEqual(
      new Map([
        [
          first.headerHash,
          [
            {
              fromOutRef: first.stateQueueOutRef,
              slot: Number(transition.slot),
              blockHash: transition.blockHash,
            },
          ],
        ],
      ]),
    );
    expect(final.finalAnchor).toEqual({
      queue: transition.nextQueue,
      blockNo: transition.blockNo,
      transactionIndex: transition.transactionIndex,
    });
  });

  it("records only the final prefix of the history and defers the rest", () => {
    const first = record(h28("1"), outRef("1", 0));
    const second = record(h28("2"), outRef("2", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
      { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
    ];
    const one = merge(1, initial, 30);
    const two = merge(2, [...one.nextQueue], 29);
    const result = terminalRetentionOutcomes(
      [first, second],
      [],
      [one, two],
      snapshot(second.headerHash, two.nextQueue[0]!.outRef),
      config(initial),
    );
    expect(
      result.records.map(({ headerHash, status }) => [headerHash, status]),
    ).toEqual([[first.headerHash, "merged"]]);
    expect(result.deferredHeaderHashes).toEqual([second.headerHash]);
    expect(result.finalAnchor).toEqual({
      queue: one.nextQueue,
      blockNo: one.blockNo,
      transactionIndex: one.transactionIndex,
    });
  });

  it("still checks history younger than the finality depth for integrity", () => {
    const first = record(h28("1"), outRef("1", 0));
    const second = record(h28("2"), outRef("2", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
      { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
    ];
    const one = merge(1, initial, 30);
    const two = merge(2, [...one.nextQueue], 1);
    // The young checkpoint is canonical but does not reproduce the snapshot.
    const mismatch = thrownBy(() =>
      terminalRetentionOutcomes(
        [first, second],
        [],
        [one, two],
        snapshot(second.headerHash, outRef("9", 0)),
        config(initial),
      ),
    );
    expect((mismatch as Error).message).toMatch(/does not match the exact/u);
    expect(mismatch).toBeInstanceOf(L1SourceIntegrityError);
    // A young checkpoint that does not extend the final one.
    const disconnected = thrownBy(() =>
      terminalRetentionOutcomes(
        [first, second],
        [],
        [one, merge(3, initial, 1)],
        snapshot(first.headerHash, outRef("2", 0)),
        config(initial),
      ),
    );
    expect((disconnected as Error).message).toMatch(/does not extend/u);
    expect(disconnected).toBeInstanceOf(L1SourceIntegrityError);
  });

  it.each([
    ["final", true],
    ["not final", false],
  ] as const)(
    "defers a header whose final history lands on an output the snapshot reports %s only when it is not final",
    async (_label, finalized) => {
      const moved = record(h28("1"), outRef("1", 0));
      const chain = createStateQueueChain({
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: policy,
        headers: [{ header: moved.header, headerHash: moved.headerHash }],
        tip: 1,
      });
      const anchor = chain.queue();
      chain.mine({ attest: moved.headerHash });
      const history = await chain.fetchStateQueueReplayCheckpoints(
        anchor,
        chain.queue(),
        // Final history, however the snapshot judged its output.
        40,
        64,
      );
      const current = {
        ...moved,
        stateQueueOutRef: chain.queue()[1]!.outRef,
        finalized,
      };
      const result = terminalRetentionOutcomes(
        [{ ...moved, stateQueueOutRef: anchor[1]!.outRef }],
        [current],
        history,
        snapshot("00".repeat(28), anchor[0]!.outRef),
        {
          ...config([...anchor]),
          replayAnchor: {
            ...config([...anchor]).replayAnchor,
            blockNo: "1",
          },
        },
      );
      expect(result.finalSteps.get(moved.headerHash)?.at(-1)?.toOutRef).toBe(
        current.stateQueueOutRef,
      );
      if (finalized) {
        expect(result.deferredHeaderHashes).toEqual([]);
        expect(result.finalAnchor?.queue).toEqual(chain.queue());
      } else {
        // Judged on the disagreement it would look unexplained; it waits.
        expect(result.deferredHeaderHashes).toEqual([moved.headerHash]);
        expect(result.finalAnchor).toBeUndefined();
      }
    },
  );

  it("replays two merges and records both exact outcomes", () => {
    const first = record(h28("1"), outRef("1", 0));
    const second = record(h28("2"), outRef("2", 0));
    const initial = [
      { headerHash: null, outRef: outRef("0", 0) },
      { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
      { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
    ];
    const one = merge(1, initial);
    const two = merge(2, [...one.nextQueue]);
    const result = terminalRetentionOutcomes(
      [first, second],
      [],
      [one, two],
      snapshot(second.headerHash, two.nextQueue[0]!.outRef),
      config(initial),
    );
    expect(
      result.records.map(({ headerHash, status }) => [headerHash, status]),
    ).toEqual([
      [first.headerHash, "merged"],
      [second.headerHash, "merged"],
    ]);
    expect(
      result.records.map(({ finalized, observedChainPoint }) => ({
        finalized,
        source: observedChainPoint.providerSource,
        depth: observedChainPoint.depth,
      })),
    ).toEqual([
      {
        finalized: true,
        source: "authenticated_state_queue_transition_v1",
        depth: 29,
      },
      {
        finalized: true,
        source: "authenticated_state_queue_transition_v1",
        depth: 29,
      },
    ]);
  });

  it.each(["merge_first", "removal_first"] as const)(
    "replays merge+timeout removal in %s order",
    (order) => {
      const first = record(h28("1"), outRef("1", 0));
      const second = record(h28("2"), outRef("2", 0));
      const initial = [
        { headerHash: null, outRef: outRef("0", 0) },
        { headerHash: first.headerHash, outRef: first.stateQueueOutRef },
        { headerHash: second.headerHash, outRef: second.stateQueueOutRef },
      ];
      const one =
        order === "merge_first" ? merge(1, initial) : timeout(1, initial);
      const two =
        order === "merge_first"
          ? timeout(2, [...one.nextQueue])
          : merge(2, [...one.nextQueue]);
      const result = terminalRetentionOutcomes(
        [first, second],
        [],
        [one, two],
        snapshot(first.headerHash, two.nextQueue[0]!.outRef),
        config(initial),
      );
      expect(
        new Map(
          result.records.map(({ headerHash, status }) => [headerHash, status]),
        ),
      ).toEqual(
        new Map([
          [first.headerHash, "merged"],
          [second.headerHash, "removed"],
        ]),
      );
    },
  );
});

describe("confirmation admission and strict recovery retirement", () => {
  it.each([13, 2161, 2162])(
    "keeps replay until inclusive checkpoint depth %i",
    (inclusiveDepth) => {
      const prior = record(h28("1"), outRef("1", 0));
      const initial = [
        { headerHash: null, outRef: outRef("0", 0) },
        { headerHash: prior.headerHash, outRef: prior.stateQueueOutRef },
      ];
      const checkpoint = merge(1, initial, inclusiveDepth);
      const observation = terminalRetentionOutcomes(
        [prior],
        [],
        [checkpoint],
        snapshot(prior.headerHash, checkpoint.nextQueue[0]!.outRef),
        {
          ...config(initial),
          finalityDepth: 12,
          automaticRecoveryMaxDepth: 2160,
        },
      );
      // Admission remains at confirmation depth; retained replay refreshes the
      // exact transition's depth on later scans, including after restart.
      expect(observation.records[0]).toMatchObject({
        status: "merged",
        finalized: true,
        observedChainPoint: { depth: inclusiveDepth - 1 },
      });
      expect(observation.finalSteps.has(prior.headerHash)).toBe(true);
      expect(observation.finalAnchor?.blockNo).toBe(
        inclusiveDepth > 2161 ? checkpoint.blockNo : undefined,
      );
    },
  );
});

describe("retained terminal authority after restart", () => {
  it("reports a provisional terminal whose checkpoint fell behind the replay anchor", () => {
    const prior = {
      ...record(h28("1"), outRef("1", 0)),
      status: "merged" as const,
      finalized: true,
      observedChainPoint: { ...point(1, 12), finalized: true },
    };
    const observation = terminalRetentionOutcomes(
      [prior],
      [],
      [],
      snapshot(h28("2"), outRef("0", 0)),
      {
        ...config([{ headerHash: null, outRef: outRef("0", 0) }]),
        finalityDepth: 12,
        automaticRecoveryMaxDepth: 2160,
      },
    );
    expect(observation.recoveryProofUnavailable).toBe(true);
    expect(observation.records[0]).toEqual(prior);
  });
});
