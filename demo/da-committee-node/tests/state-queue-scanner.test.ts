import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { StateQueueHeaderRecord } from "../src/domain.js";
import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import {
  hashBlockHeader,
  scanStateQueue,
  type StateQueueProvider,
  type StateQueueReplayAnchor,
  stateQueueReplayWalkLimit,
} from "../src/l1/state-queue-scanner.js";
import type { StateQueueOutputStep } from "../src/l1/terminal-retention-observation.js";
import { makeObservedNode, makePayloadFixture } from "./helpers.js";
import {
  type ChainHeader,
  createStateQueueChain,
  type StateQueueChain,
} from "./helpers/state-queue-chain.js";

describe("state queue scanner", () => {
  it("records a deployment-bound durable queue anchor and refuses unproven advancement", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const node = makeObservedNode({
      header,
      headerHash,
      depth: 30,
      outRef: `${"44".repeat(32)}#1`,
    });
    const snapshot = {
      nodes: [{ ...node, chainPoint: { ...node.chainPoint, blockHeight: 90 } }],
      confirmedHeaderHash: "55".repeat(28),
      confirmedStateOutRef: `${"66".repeat(32)}#0`,
      observedChainPoint: {
        ...node.chainPoint,
        blockHeight: 89,
        depth: 30,
      },
    };
    const provider: StateQueueProvider = {
      fetchStateQueueNodes: async () => snapshot.nodes,
      fetchStateQueueSnapshot: async () => snapshot,
    };
    let anchor:
      | Parameters<
          NonNullable<
            Parameters<typeof scanStateQueue>[1]["recordReplayAnchor"]
          >
        >[0]
      | undefined;
    const common = {
      deploymentFingerprint: "11".repeat(32),
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 30,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    } as const;
    await scanStateQueue(provider, {
      ...common,
      recordReplayAnchor: (next) => {
        anchor = next;
      },
    });
    expect(anchor).toMatchObject({
      deploymentIdentityDigest: common.deploymentIdentityDigest,
      stateQueuePolicyId: common.stateQueuePolicyId,
      blockNo: "90",
      transactionIndex: "0",
    });

    const changed = {
      ...snapshot,
      nodes: [
        {
          ...snapshot.nodes[0]!,
          outRef: `${"77".repeat(32)}#0`,
        },
      ],
    };
    // A replay source that shows no history for a changed queue is the
    // source contradicting itself.
    const limits: number[] = [];
    const unauthenticated = scanStateQueue(
      {
        fetchStateQueueNodes: async () => changed.nodes,
        fetchStateQueueSnapshot: async () => ({ ...changed, tipBlockNo: 120 }),
        fetchStateQueueReplayCheckpoints: async (_a, _c, _t, limit) => {
          limits.push(limit);
          return [];
        },
      },
      { ...common, terminalReplayAnchor: anchor! },
    );
    await expect(unauthenticated).rejects.toThrow(
      /without an authenticated replay checkpoint/u,
    );
    await expect(unauthenticated).rejects.toBeInstanceOf(
      L1SourceIntegrityError,
    );
    expect(limits).toEqual([stateQueueReplayWalkLimit(30)]);
    expect(stateQueueReplayWalkLimit(30)).toBe(2 * 31 * 16);
    // A provider with no replay source at all is missing a capability, which
    // says nothing about the chain: an observation failure.
    const incapable = scanStateQueue(
      {
        fetchStateQueueNodes: async () => changed.nodes,
        fetchStateQueueSnapshot: async () => changed,
      },
      { ...common, terminalReplayAnchor: anchor! },
    );
    await expect(incapable).rejects.toThrow(
      "state-queue provider has no authenticated ordered history source",
    );
    await expect(incapable).rejects.not.toBeInstanceOf(L1SourceIntegrityError);

    await expect(
      scanStateQueue(provider, {
        ...common,
        terminalReplayAnchor: {
          ...anchor!,
          deploymentIdentityDigest: "99".repeat(32),
        },
      }),
    ).rejects.toThrow(/durable replay anchor release mismatch/u);
  });

  it("finds finalized unattested headers and ignores the root node", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const provider: StateQueueProvider = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({ header, headerHash, linkedListKey: "Empty" }),
        makeObservedNode({ header, headerHash, depth: 3 }),
      ],
    };
    const records = await scanStateQueue(provider, {
      deploymentFingerprint: "dep",
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 2,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    });
    expect(records).toHaveLength(1);
    expect(records[0]!.status).toBe("unattested");
    expect(records[0]!.finalized).toBe(true);
  });

  it("marks attested and conflicted state-queue nodes", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const provider: StateQueueProvider = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({
          header,
          headerHash,
          daAttestation: { Attested: { da_bond_asset_name: "33".repeat(32) } },
        }),
        makeObservedNode({
          header,
          headerHash,
          assetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${"44".repeat(28)}`,
        }),
        makeObservedNode({
          header,
          headerHash,
          daAttestation: { Attested: { da_bond_asset_name: "55".repeat(32) } },
        }),
      ],
    };
    const records = await scanStateQueue(provider, {
      deploymentFingerprint: "dep",
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 0,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    });
    // The third node carries an attestation marker, which is now a
    // legitimate StateQueueStatusV1 state rather than a conflict
    // (`unexpected_da_attestation_marker` was removed with the ByteArray
    // marker). Replacement adversarial coverage for the new status sum is
    // tracked in https://github.com/Anastasia-Labs/midgard/issues/645.
    expect(records.map((record) => record.status)).toEqual([
      "attested",
      "conflicted",
      "attested",
    ]);
    expect(records[1]!.validationErrors).toContain(
      "block_asset_suffix_mismatch",
    );
    expect(records[2]!.validationErrors).toEqual([]);
  });

  it("never marks finalized unattested headers out of scope", async () => {
    const { header, headerHash } = await makePayloadFixture();
    const provider: StateQueueProvider = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({ header, headerHash, depth: 10 }),
      ],
    };

    const records = await scanStateQueue(provider, {
      deploymentFingerprint: "dep",
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth: 0,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    });

    expect(records).toHaveLength(1);
    expect(records[0]!.status).toBe("unattested");
    expect(records[0]!.validationErrors).toEqual([]);
  });

  describe("replay anchor finality", () => {
    const finalityDepth = 3;
    const common = {
      deploymentFingerprint: "11".repeat(32),
      deploymentIdentityDigest: "11".repeat(32),
      stateQueuePolicyId: "22".repeat(28),
      daAttestationPolicyId: "33".repeat(28),
      finalityDepth,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    } as const;
    const chainHeaders = async (
      count: number,
    ): Promise<readonly ChainHeader[]> => {
      const { header: base } = await makePayloadFixture();
      return Array.from({ length: count }, (_, index) => {
        const header = { ...base, endTime: base.endTime + BigInt(index) };
        return {
          header,
          headerHash: hashBlockHeader(header),
          daAttestation: {
            Attested: { da_bond_asset_name: "33".repeat(32) },
          },
        };
      });
    };
    const chainOf = (headers: readonly ChainHeader[], tip: number) =>
      createStateQueueChain({
        deploymentIdentityDigest: common.deploymentIdentityDigest,
        stateQueuePolicyId: common.stateQueuePolicyId,
        headers,
        tip,
      });
    /** One scan, reporting everything it recorded. */
    const scan = async (
      chain: StateQueueChain,
      prior: {
        readonly durable?: StateQueueReplayAnchor;
        readonly candidate?: StateQueueReplayAnchor;
        readonly previousHeaders?: readonly StateQueueHeaderRecord[];
        /** Replaces the chain's replay source. */
        readonly replay?: StateQueueChain["fetchStateQueueReplayCheckpoints"];
      } = {},
    ) => {
      const recorded: {
        durable?: StateQueueReplayAnchor;
        candidate?: StateQueueReplayAnchor;
        discarded?: string;
        deferred?: readonly string[];
        steps?: ReadonlyMap<string, readonly StateQueueOutputStep[]>;
      } = {};
      const records = await scanStateQueue(
        {
          fetchStateQueueNodes: async () => chain.snapshot().nodes,
          fetchStateQueueSnapshot: async () => chain.snapshot(),
          fetchStateQueueReplayCheckpoints:
            prior.replay ?? chain.fetchStateQueueReplayCheckpoints,
        },
        {
          ...common,
          ...(prior.durable === undefined
            ? {}
            : { terminalReplayAnchor: prior.durable }),
          ...(prior.candidate === undefined
            ? {}
            : { provisionalReplayAnchor: prior.candidate }),
          ...(prior.previousHeaders === undefined
            ? {}
            : { previousHeaders: prior.previousHeaders }),
          recordReplayAnchor: (anchor) => {
            recorded.durable = anchor;
          },
          recordProvisionalReplayAnchor: (anchor) => {
            recorded.candidate = anchor;
          },
          recordDiscardedReplayAnchorCandidate: (_candidate, reason) => {
            recorded.discarded = reason;
          },
          recordReplayedHeaderSteps: ({ deferredHeaderHashes, finalSteps }) => {
            recorded.deferred = deferredHeaderHashes;
            recorded.steps = finalSteps;
          },
        },
      );
      return { records, ...recorded };
    };

    it("holds a young bootstrap queue only as a candidate and records it once final", async () => {
      // Genesis outputs have two blocks on top: one short of final.
      const chain = chainOf(await chainHeaders(1), 3);
      const young = await scan(chain);
      expect(young.durable).toBeUndefined();
      expect(young.candidate?.queue).toEqual(chain.queue());

      chain.mine();
      const final = await scan(chain, { candidate: young.candidate! });
      expect(final.durable).toEqual(young.candidate);
      expect(chain.isFinal(final.durable!.queue, finalityDepth)).toBe(true);
      expect(final.candidate).toBeUndefined();
    });

    it("records only final queues as the anchor while appends land every block", async () => {
      const headers = await chainHeaders(6);
      const chain = chainOf(headers.slice(0, 1), 5);
      let { durable } = await scan(chain);
      expect(durable?.queue).toEqual(chain.queue());
      const anchors: StateQueueReplayAnchor[] = [];
      for (const [index, header] of headers.slice(1).entries()) {
        chain.mine({ append: header });
        const scanned = await scan(chain, { durable: durable! });
        // Each append moves the tail it continues. The three appends with
        // fewer than the finality depth of blocks on top leave their tails
        // without a final output.
        expect(scanned.deferred).toEqual(
          headers
            .slice(Math.max(0, index - 2), index + 1)
            .map(({ headerHash }) => headerHash),
        );
        expect(scanned.records.map(({ headerHash }) => headerHash)).toEqual(
          expect.arrayContaining(
            chain
              .queue()
              .slice(1)
              .map(({ headerHash }) => headerHash),
          ),
        );
        durable = scanned.durable;
        expect(chain.isFinal(durable!.queue, finalityDepth)).toBe(true);
        anchors.push(durable!);
      }
      // The anchor trails the tip by the finality depth, and advances.
      expect(anchors.at(-1)!.queue).not.toEqual(chain.queue());
      expect(Number(anchors.at(-1)!.blockNo)).toBe(chain.tip - finalityDepth);
    });

    it("survives a shallow rollback of the latest append", async () => {
      const headers = await chainHeaders(5);
      const chain = chainOf(headers.slice(0, 1), 5);
      let { durable } = await scan(chain);
      chain.mine({ append: headers[1]! });
      chain.mine({ append: headers[2]! });
      durable = (await scan(chain, { durable: durable! })).durable;

      chain.rollback(1);
      chain.mine({ append: headers[3]! });
      const afterRollback = await scan(chain, { durable: durable! });
      expect(chain.isFinal(afterRollback.durable!.queue, finalityDepth)).toBe(
        true,
      );
      chain.mine({ append: headers[4]! });
      await expect(
        scan(chain, { durable: afterRollback.durable! }),
      ).resolves.toBeDefined();
    });

    it("discards a rolled-back bootstrap candidate but not a rolled-back durable anchor", async () => {
      const headers = await chainHeaders(3);
      const chain = chainOf(headers.slice(0, 1), 5);
      chain.mine({ append: headers[1]! });
      const young = await scan(chain);
      expect(young.durable).toBeUndefined();
      const candidate = young.candidate!;

      chain.rollback(1);
      chain.mine({ append: headers[2]! });
      const rebootstrapped = await scan(chain, { candidate });
      expect(rebootstrapped.durable).toBeUndefined();
      expect(rebootstrapped.candidate?.queue).toEqual(chain.queue());

      // A recorded anchor is final: history that no longer extends it is an
      // integrity failure, never retried as a candidate.
      const durable = scan(chain, { durable: candidate });
      await expect(durable).rejects.toThrow(/Kupo does not know/u);
      await expect(durable).rejects.toBeInstanceOf(L1SourceIntegrityError);
    });

    it("keeps a bootstrap candidate whose replay fails on anything but not extending it, and the failure stands", async () => {
      const headers = await chainHeaders(3);
      const chain = chainOf(headers.slice(0, 1), 5);
      chain.mine({ append: headers[1]! });
      const young = await scan(chain);
      expect(young.durable).toBeUndefined();
      chain.mine({ append: headers[2]! });

      const corrupt = new L1SourceIntegrityError(
        "Kupo replay assigned one transaction to competing points",
      );
      const failed = await scan(chain, {
        candidate: young.candidate!,
        replay: async () => {
          throw corrupt;
        },
      }).then(
        () => undefined,
        (error: unknown) => error,
      );
      expect(failed).toBe(corrupt);
      expect(
        await scan(chain, { candidate: young.candidate! }),
      ).not.toHaveProperty("discarded");

      // Only history that does not extend the candidate discards it.
      chain.rollback(2);
      chain.mine({ append: headers[1]! });
      const rolledBack = await scan(chain, { candidate: young.candidate! });
      expect(rolledBack.discarded).toMatch(/Kupo does not know/u);
      expect(rolledBack.candidate?.queue).toEqual(chain.queue());
    });

    it("reaches a durable anchor from a young bootstrap while appends land every block", async () => {
      const headers = await chainHeaders(8);
      // The bootstrap snapshot's tail is always the latest append: the
      // snapshot itself is never final.
      const chain = chainOf(headers.slice(0, 1), 1);
      chain.mine({ append: headers[1]! });
      let scanned = await scan(chain);
      expect(scanned.durable).toBeUndefined();
      let blocks = 0;
      for (const header of headers.slice(2)) {
        chain.mine({ append: header });
        blocks += 1;
        scanned = await scan(chain, { candidate: scanned.candidate! });
        if (scanned.durable !== undefined) break;
        expect(chain.snapshot().nodes.at(-1)!.chainPoint.depth).toBe(0);
      }
      // The first append after the candidate is final after the finality
      // depth of blocks on top of it.
      expect(scanned.durable).toBeDefined();
      expect(blocks).toBe(finalityDepth + 1);
      expect(chain.isFinal(scanned.durable!.queue, finalityDepth)).toBe(true);
    });

    it("refuses to replay against a snapshot that carries no tip height", async () => {
      const headers = await chainHeaders(2);
      const chain = chainOf(headers.slice(0, 1), 5);
      const { durable } = await scan(chain);
      chain.mine({ append: headers[1]! });
      const failure = scanStateQueue(
        {
          fetchStateQueueNodes: async () => chain.snapshot().nodes,
          fetchStateQueueSnapshot: async () => {
            const { tipBlockNo: _tip, ...rest } = chain.snapshot();
            return rest;
          },
          fetchStateQueueReplayCheckpoints:
            chain.fetchStateQueueReplayCheckpoints,
        },
        { ...common, terminalReplayAnchor: durable! },
      );
      await expect(failure).rejects.toThrow(/carries no tip height/u);
      await expect(failure).rejects.not.toBeInstanceOf(L1SourceIntegrityError);
    });

    it("holds the anchor behind a header whose history is only partly final", async () => {
      const headers = await chainHeaders(2);
      const tail = headers[0]!.headerHash;
      const chain = chainOf(headers.slice(0, 1), 5);
      const first = await scan(chain);
      chain.mine({ append: headers[1]! });
      const appended = chain.queue();
      chain.mine();
      chain.mine();
      chain.mine({ attest: tail });
      // The append that moved the tail is final, the attestation is not.
      const partly = await scan(chain, { durable: first.durable! });
      expect(partly.deferred).toEqual([tail]);
      expect(partly.durable).toEqual(first.durable);

      chain.mine();
      chain.mine();
      chain.mine();
      const final = await scan(chain, { durable: partly.durable! });
      expect(final.deferred).toEqual([]);
      expect(final.durable?.queue).toEqual(chain.queue());
      const at = (queue: readonly SDK.StateQueueTransitionNode[]) =>
        queue.find(({ headerHash }) => headerHash === tail)!.outRef;
      expect(final.steps!.get(tail)).toMatchObject([
        { fromOutRef: at(first.durable!.queue), toOutRef: at(appended) },
        { fromOutRef: at(appended), toOutRef: at(chain.queue()) },
      ]);
    });

    it("defers a merge until it is final, then records it", async () => {
      const headers = await chainHeaders(5);
      const chain = chainOf(headers.slice(0, 2), 5);
      const first = await scan(chain);
      const merged = headers[0]!.headerHash;
      chain.mine("merge", { append: headers[2]! });
      const early = await scan(chain, {
        durable: first.durable!,
        previousHeaders: first.records,
      });
      // The merge takes one header out; the append moves the tail.
      expect(early.deferred).toEqual([merged, headers[1]!.headerHash]);
      expect(early.records.map(({ headerHash }) => headerHash)).not.toContain(
        merged,
      );
      expect(early.durable).toEqual(first.durable);

      chain.mine({ append: headers[3]! });
      chain.mine({ append: headers[4]! });
      chain.mine();
      const final = await scan(chain, {
        durable: early.durable!,
        previousHeaders: first.records,
      });
      expect(final.deferred).toEqual([
        headers[2]!.headerHash,
        headers[3]!.headerHash,
      ]);
      expect(
        final.records.find(({ headerHash }) => headerHash === merged),
      ).toMatchObject({ status: "merged", finalized: true });
      // The final block's steps: the merge took the header out, and the
      // append moved the tail to the output the snapshot now shows.
      const tail = final.records.find(
        ({ headerHash }) => headerHash === headers[1]!.headerHash,
      )!;
      expect([...final.steps!.keys()].sort()).toEqual(
        [merged, headers[1]!.headerHash].sort(),
      );
      expect(final.steps!.get(merged)).toEqual([
        {
          fromOutRef: first.records.find(
            ({ headerHash }) => headerHash === merged,
          )!.stateQueueOutRef,
          slot: 6 * 20,
          blockHash: expect.stringMatching(/^[0-9a-f]{64}$/u),
        },
      ]);
      expect(final.steps!.get(headers[1]!.headerHash)).toEqual([
        {
          fromOutRef: first.records.find(
            ({ headerHash }) => headerHash === headers[1]!.headerHash,
          )!.stateQueueOutRef,
          toOutRef: tail.stateQueueOutRef,
          slot: tail.observedChainPoint.slot,
          blockHash: tail.observedChainPoint.blockHash,
        },
      ]);
    });

    describe("history longer than one walk", () => {
      const walkLimit = stateQueueReplayWalkLimit(finalityDepth);
      /** A durable anchor at genesis, then `appends` appends, one a block. */
      const longHistory = async (appends: number) => {
        const headers = await chainHeaders(appends + 1);
        const chain = chainOf(headers.slice(0, 1), 5);
        const { durable } = await scan(chain);
        expect(durable).toBeDefined();
        for (const header of headers.slice(1)) chain.mine({ append: header });
        return { chain, durable: durable! };
      };

      it("refuses a provider that returns more checkpoints than asked for, as an observation failure", async () => {
        const { chain, durable } = await longHistory(2);
        const overLimit = scan(chain, {
          durable,
          replay: async (anchor, current, tip, limit) => {
            const [first] = await chain.fetchStateQueueReplayCheckpoints(
              anchor,
              current,
              tip,
              limit,
            );
            return Array.from({ length: limit + 1 }, () => first!);
          },
        });
        await expect(overLimit).rejects.toThrow(
          `state-queue provider returned ${(walkLimit + 1).toString()} replay checkpoints where at most ${walkLimit.toString()} were asked for`,
        );
        await expect(overLimit).rejects.not.toBeInstanceOf(
          L1SourceIntegrityError,
        );
      });

      it("refuses to catch up when the caller cannot record catch-up progress", async () => {
        const { chain, durable } = await longHistory(walkLimit + 1);
        const unrecorded = scan(chain, { durable });
        await expect(unrecorded).rejects.toThrow(
          "state-queue history is longer than one scan replays, and this scan cannot record catch-up progress",
        );
        await expect(unrecorded).rejects.not.toBeInstanceOf(
          L1SourceIntegrityError,
        );
      });
    });
  });
});
