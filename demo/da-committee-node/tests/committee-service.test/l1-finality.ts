import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it, vi } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import {
  type ChainSyncCursor,
  type ChainSyncEvent,
} from "../../src/l1/provider.js";
import { L1SourceIntegrityError } from "../../src/l1/source-integrity.js";
import {
  hashBlockHeader,
  type StateQueueProvider,
  stateQueueReplayWalkLimit,
} from "../../src/l1/state-queue-scanner.js";
import { loadDaSigner, validateDaSignerMembership } from "../../src/signer.js";
import { UNKNOWN_STATE_QUEUE_STATUS } from "../../src/store.js";
import { createCommitteeTickRunner } from "../../src/tick-runner.js";
import { bytesToHex } from "../../src/utils/hex.js";
import {
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from ".././helpers.js";
import {
  type ChainHeader,
  createStateQueueChain,
} from ".././helpers/state-queue-chain.js";
import {
  openTestCommitteeStore,
  runAnchorAheadOfObservations,
} from "./fixtures.js";

export const registerL1FinalityTests = () => {
  describe("L1 history younger than the finality depth", () => {
    const finalityDepth = 3;
    const blockMs = 20_000;

    /**
     * A committee over a live state queue whose genesis (block 1) holds only
     * `signed`, an unattested header this committee signs, so it is the tail
     * the first append continues; with `signedIsTail` false an attested
     * header follows it, so appends leave it alone. `spare` are attested
     * headers to append. `service(false)` builds a committee that signs
     * nothing; `events` holds the structured events the committee logged.
     */
    const chainCommittee = async (
      seedByte: string,
      tip: number,
      spareCount = 8,
      signedIsTail = true,
    ) => {
      const dir = await tempDir();
      const { header, headerHash, payloadCbor } = await makePayloadFixture();
      const seed = "00".repeat(31) + seedByte;
      const signer = await loadDaSigner(`hex:${seed}`);
      const base = minimalConfig({
        manifestPath: `${dir}/manifest.json`,
        deploymentInfoPath: `${dir}/deployment.json`,
        signerSeed: seed,
        signerPublicKey: signer.publicKeyHex,
      });
      const config = {
        ...base,
        finalityDepth,
        daParams: {
          ...base.daParams,
          committeeSignersHash: bytesToHex(
            blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
          ),
        },
      };
      const signerValidation = validateDaSignerMembership({
        daParams: config.daParams,
        signer,
        signerIndex: 0,
      });
      const attested = Array.from(
        { length: spareCount + 1 },
        (_, index): ChainHeader => {
          const variant = {
            ...header,
            endTime: header.endTime + 1n + BigInt(index),
          };
          return {
            header: variant,
            headerHash: hashBlockHeader(variant),
            daAttestation: {
              Attested: { commitment_hash: "33".repeat(32) },
            },
          };
        },
      );
      const chain = createStateQueueChain({
        deploymentIdentityDigest: config.deploymentFingerprint,
        stateQueuePolicyId: config.stateQueuePolicyId,
        headers: [
          { header, headerHash },
          ...(signedIsTail ? [] : [attested[spareCount]!]),
        ],
        tip,
      });
      const clock = { nowMs: Date.parse("2026-07-28T00:00:00.000Z") };
      const store = await openTestCommitteeStore();
      const events: Record<string, unknown>[] = [];
      const chainProvider: StateQueueProvider = {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
        fetchStateQueueReplayCheckpoints:
          chain.fetchStateQueueReplayCheckpoints,
      };
      const service = (withSigner = true, provider = chainProvider) =>
        new CommitteeService({
          config,
          store,
          stateQueueProvider: provider,
          payloadSource: payloadSourceFromBytes(payloadCbor),
          ...(withSigner
            ? {
                signer,
                signerValidation,
                coordinator: {
                  // Republishing each tick makes every tick bind a decision
                  // to the signed header's current output.
                  retryPublishedSignatures: true,
                  publishSignature: async () => "posted" as const,
                },
              }
            : {}),
          now: () => new Date(clock.nowMs),
          writeEvent: (line) => {
            events.push(JSON.parse(line) as Record<string, unknown>);
          },
        });
      return {
        chain,
        clock,
        store,
        service,
        config,
        events,
        chainProvider,
        signed: headerHash,
        spare: attested.slice(0, spareCount),
      };
    };

    it("keeps ticking with a fresh L1 view while appends land every block, and records a merge once it is final", async () => {
      const { chain, clock, store, service, signed, spare } =
        await chainCommittee("51", 5);
      const committee = service();
      await committee.initialize();
      const results: Awaited<ReturnType<CommitteeService["tick"]>>[] = [];
      const runner = createCommitteeTickRunner({
        tick: async () => {
          const result = await committee.tick();
          results.push(result);
          return result;
        },
        runAvailabilityResponse: async () => undefined,
        runRetention: async () => undefined,
        latestL1View: () => committee.latestL1View(),
        latestL1ProgressAtMs: () => committee.latestL1ProgressAtMs(),
        setRetentionReadiness: () => undefined,
        l1ViewFatalMs: 3 * blockMs,
        startedAtMs: clock.nowMs,
        nowMs: () => clock.nowMs,
        write: () => undefined,
      });

      // The signed header is final and the tail when it is signed.
      await runner.runTick();
      expect(results[0]!.signedHeaders).toBe(1);
      results.length = 0;
      // Every block appends a header, the first continuing the signed tail;
      // one of them also merges the signed header. The tail is never final.
      const blocks = spare
        .slice(0, 7)
        .map((header, index) =>
          index === 1
            ? ["merge" as const, { append: header }]
            : [{ append: header }],
        );
      const anchors: string[] = [];
      for (const [index, block] of blocks.entries()) {
        chain.mine(...block);
        clock.nowMs += blockMs;
        await runner.runTick();
        expect(runner.liveness().l1ViewUnavailable).toBeUndefined();
        expect(results).toHaveLength(index + 1);
        expect(results.at(-1)!.errors).toEqual([]);
        expect(committee.latestL1View()?.observedAtMs).toBe(clock.nowMs);
        const state = await store.getL1SourceState();
        expect(state?.status).toBe("healthy");
        if (state?.stateQueueReplayAnchor !== undefined) {
          expect(
            chain.isFinal(state.stateQueueReplayAnchor.queue, finalityDepth),
          ).toBe(true);
          anchors.push(JSON.stringify(state.stateQueueReplayAnchor.queue));
        }
      }
      expect(new Set(anchors).size).toBeGreaterThan(1);
      // The merge (block 2 of 7) is final by now; its outcome is recorded.
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        status: "merged",
        finalized: true,
      });
      expect(
        (await store.getL1SourceState())?.observations.find(
          ({ headerHash }) => headerHash === signed,
        ),
      ).toMatchObject({
        stateQueueStatus: "merged",
        hasPersistedDecision: true,
      });
    });

    it("keeps ticking through a shallow rollback of the latest append", async () => {
      const { chain, clock, store, service, spare } = await chainCommittee(
        "52",
        5,
      );
      const committee = service();
      await committee.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
        const state = await store.getL1SourceState();
        expect(state?.status).toBe("healthy");
        // The rollback never undoes an output the anchor names.
        if (state?.stateQueueReplayAnchor !== undefined) {
          expect(chain.isFinal(state.stateQueueReplayAnchor.queue, 1)).toBe(
            true,
          );
        }
      };

      // Rolled back while the committee holds only a bootstrap candidate.
      chain.mine({ append: spare[0]! });
      await tick();
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      chain.rollback(1);
      chain.mine({ append: spare[1]! });
      await tick();

      // Rolled back after a durable anchor is recorded: the first append on
      // top of the candidate is final once the finality depth of blocks is
      // on top of it.
      for (const header of spare.slice(2, 6)) {
        chain.mine({ append: header });
        await tick();
      }
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeDefined();
      chain.rollback(1);
      await tick();
      chain.mine({ append: spare[6]! });
      await tick();
      chain.mine({ append: spare[7]! });
      await tick();
    });

    it("keeps the replay anchor when a tick fails between a signature and the healthy-state write", async () => {
      const { chain, clock, store, service, signed } = await chainCommittee(
        "53",
        5,
      );
      // A committee that signs nothing records the durable anchor first.
      const observer = service(false);
      await observer.initialize();
      await expect(observer.tick()).resolves.toMatchObject({
        signedHeaders: 0,
      });
      const anchor = (await store.getL1SourceState())?.stateQueueReplayAnchor;
      expect(anchor).toBeDefined();

      const committee = service();
      await committee.initialize();
      clock.nowMs += blockMs;
      const listL1Submissions = vi
        .spyOn(store, "listL1Submissions")
        .mockRejectedValueOnce(new Error("store read failed"));
      await expect(committee.tick()).rejects.toThrow("store read failed");
      expect(listL1Submissions).toHaveBeenCalled();
      const afterFailure = await store.getL1SourceState();
      expect(afterFailure).toMatchObject({
        status: "healthy",
        stateQueueReplayAnchor: anchor,
      });
      expect(
        afterFailure?.observations.find(
          ({ headerHash }) => headerHash === signed,
        ),
      ).toMatchObject({ hasPersistedDecision: true });

      // The signed header is merged; once that is final the committee
      // records it rather than seeing a persisted decision disappear.
      chain.mine("merge");
      chain.mine();
      chain.mine();
      chain.mine();
      clock.nowMs += blockMs;
      await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        status: "merged",
      });
    });

    /** A committee over `chainCommittee`, ticking one block's time apart. */
    const ticking = async (seedByte: string) => {
      const harness = await chainCommittee(seedByte, 5);
      const committee = harness.service();
      await committee.initialize();
      const tick = async () => {
        harness.clock.nowMs += blockMs;
        const result = await committee.tick();
        expect(result.errors).toEqual([]);
        expect((await harness.store.getL1SourceState())?.status).toBe(
          "healthy",
        );
        return result;
      };
      const observed = async () =>
        (await harness.store.getL1SourceState())?.observations.find(
          ({ headerHash }) => headerHash === harness.signed,
        );
      const output = () =>
        harness.chain
          .queue()
          .find(({ headerHash }) => headerHash === harness.signed)!.outRef;
      const decidedOutputs = async () =>
        new Set(
          (await harness.store.listDecisionOutbox(harness.signed)).map(
            ({ stateQueueOutRef }) => stateQueueOutRef,
          ),
        );
      return { ...harness, tick, observed, output, decidedOutputs };
    };

    it("follows a signed tail header to the output the next append continues it at, once that append is final", async () => {
      const {
        chain,
        signed,
        spare,
        store,
        tick,
        observed,
        output,
        decidedOutputs,
      } = await ticking("54");
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 1 });
      const signedAt = output();
      expect(await observed()).toMatchObject({
        stateQueueOutRef: signedAt,
        hasPersistedDecision: true,
      });

      // The next append respends the signed tail. While that is younger than
      // the finality depth the decision keeps its output and nothing binds to
      // the new one.
      chain.mine({ append: spare[0]! });
      const continued = output();
      expect(continued).not.toBe(signedAt);
      await tick();
      chain.mine();
      await tick();
      chain.mine();
      await tick();
      expect(await observed()).toMatchObject({ stateQueueOutRef: signedAt });
      expect(await decidedOutputs()).not.toContain(continued);

      // Once it is final, the decision follows the continued output, and the
      // committee keeps deciding on it.
      chain.mine();
      await tick();
      expect(await observed()).toMatchObject({
        stateQueueOutRef: continued,
        stateQueueStatus: "unattested",
        finalized: true,
        hasPersistedDecision: true,
      });
      expect(await decidedOutputs()).toContain(continued);
      // The republished signature is bound where the header now is.
      expect(await store.listDaSignatures(signed)).toMatchObject([
        { validation: { stateQueueOutRef: continued } },
      ]);

      // Its attestation lands on that output and is followed the same way.
      chain.mine({ attest: signed });
      chain.mine({ append: spare[1]! });
      await tick();
      expect(await observed()).toMatchObject({ stateQueueOutRef: continued });
      chain.mine();
      chain.mine();
      chain.mine();
      await tick();
      expect(await observed()).toMatchObject({
        stateQueueOutRef: output(),
        stateQueueStatus: "attested",
        finalized: true,
        hasPersistedDecision: true,
      });
    });

    it("follows a datum update attesting a signed header, once it is final", async () => {
      const { chain, signed, tick, observed, output } = await ticking("55");
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 1 });
      const signedAt = output();
      chain.mine({ attest: signed });
      await tick();
      chain.mine();
      await tick();
      chain.mine();
      await tick();
      expect(await observed()).toMatchObject({
        stateQueueOutRef: signedAt,
        stateQueueStatus: "unattested",
      });
      chain.mine();
      await tick();
      expect(output()).not.toBe(signedAt);
      expect(await observed()).toMatchObject({
        stateQueueOutRef: output(),
        stateQueueStatus: "attested",
        finalized: true,
        hasPersistedDecision: true,
      });
    });

    it("quarantines at the scan a snapshot showing a signed header at an output authenticated replay does not reproduce", async () => {
      const { chain, clock, store, service, signed } = await chainCommittee(
        "56",
        5,
      );
      let forged = false;
      const committee = service(true, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => {
          const snapshot = chain.snapshot();
          return forged
            ? {
                ...snapshot,
                nodes: snapshot.nodes.map((node) =>
                  node.linkedListKey === signed
                    ? {
                        ...node,
                        outRef: `${"5f".repeat(32)}#0`,
                        chainPoint: { ...node.chainPoint, slot: 200 },
                      }
                    : node,
                ),
              }
            : snapshot;
        },
        fetchStateQueueReplayCheckpoints:
          chain.fetchStateQueueReplayCheckpoints,
      });
      await committee.initialize();
      await expect(committee.tick()).resolves.toMatchObject({
        signedHeaders: 1,
      });
      forged = true;
      chain.mine();
      clock.nowMs += blockMs;
      await expect(committee.tick()).rejects.toBeInstanceOf(
        L1SourceIntegrityError,
      );
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "quarantined",
        quarantineReason: expect.stringContaining("l1_source_integrity_failed"),
      });
    });

    it("quarantines a final move of a signed header that no authenticated step from the durable anchor explains", async () => {
      const { chain, store, signed, tick, observed, output } =
        await ticking("59");
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 1 });
      const signedAt = output();
      chain.mine({ attest: signed });
      for (let block = 0; block <= finalityDepth; block += 1) chain.mine();
      // The record is final, the snapshot agrees with the replay, and the
      // replay from an anchor already past the move carries no step for it.
      await runAnchorAheadOfObservations(store, chain.queue());
      await expect(tick()).rejects.toThrow();
      expect(await observed()).toMatchObject({ stateQueueOutRef: signedAt });
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "quarantined",
        quarantineReason: `l1_source_decision_forked:${signed}`,
      });
    });

    it("judges replayed history at the snapshot's tip when a block lands before the replay reads it", async () => {
      const { chain, clock, store, service, signed } = await chainCommittee(
        "5a",
        5,
      );
      let mineDuringReplay = false;
      const tips: { readonly snapshot: number; readonly replay: number }[] = [];
      let snapshotTip = 0;
      const committee = service(true, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => {
          const snapshot = chain.snapshot();
          snapshotTip = snapshot.tipBlockNo!;
          return snapshot;
        },
        fetchStateQueueReplayCheckpoints: async (
          anchor,
          current,
          tip,
          limit,
        ) => {
          tips.push({ snapshot: snapshotTip, replay: tip });
          if (mineDuringReplay) {
            mineDuringReplay = false;
            chain.mine();
          }
          return chain.fetchStateQueueReplayCheckpoints(
            anchor,
            current,
            tip,
            limit,
          );
        },
      });
      await committee.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      };
      await tick();
      const signedAt = chain.queue()[1]!.outRef;
      // The attestation's output has one block fewer than the finality depth
      // on top when the snapshot is read, and the finality depth once the
      // block mined during the replay lands.
      chain.mine({ attest: signed });
      await tick();
      chain.mine();
      chain.mine();
      mineDuringReplay = true;
      await tick();
      expect(tips.every(({ snapshot, replay }) => snapshot === replay)).toBe(
        true,
      );
      const observedSigned = async () =>
        (await store.getL1SourceState())?.observations.find(
          ({ headerHash }) => headerHash === signed,
        );
      expect(await observedSigned()).toMatchObject({
        stateQueueOutRef: signedAt,
      });
      await tick();
      expect(await observedSigned()).toMatchObject({
        stateQueueOutRef: chain.queue()[1]!.outRef,
        stateQueueStatus: "attested",
        finalized: true,
      });
    });

    it("defers, rather than forks, a move replay judges final at a later tip than the snapshot", async () => {
      const { chain, clock, store, service, signed } = await chainCommittee(
        "5b",
        5,
      );
      let mineDuringReplay = false;
      // A replay source that reads its own, later tip.
      const committee = service(true, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
        fetchStateQueueReplayCheckpoints: async (anchor, current, _, limit) => {
          if (mineDuringReplay) {
            mineDuringReplay = false;
            chain.mine();
          }
          return chain.fetchStateQueueReplayCheckpoints(
            anchor,
            current,
            chain.tip,
            limit,
          );
        },
      });
      await committee.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      };
      await tick();
      const signedAt = chain.queue()[1]!.outRef;
      chain.mine({ attest: signed });
      await tick();
      chain.mine();
      chain.mine();
      mineDuringReplay = true;
      await tick();
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      expect(
        state?.observations.find(({ headerHash }) => headerHash === signed),
      ).toMatchObject({ stateQueueOutRef: signedAt });
    });

    it("makes no decision before a durable anchor, reaches one within the finality depth while appends land every block, then signs", async () => {
      const { chain, clock, store, service, signed, spare } =
        await chainCommittee("5c", 5);
      // Appends have landed every block: the header to sign is final, but
      // the queue's tail is always younger than the finality depth.
      for (const header of spare.slice(0, finalityDepth + 1)) {
        chain.mine({ append: header });
      }
      const committee = service();
      await committee.initialize();
      const noAnchor =
        "L1 state queue has no durable replay anchor yet: no decision is made until its history is final";
      const tick = async () => {
        clock.nowMs += blockMs;
        const result = await committee.tick();
        expect(result.errors).toEqual([]);
        expect((await store.getL1SourceState())?.status).toBe("healthy");
        return result;
      };
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 0 });
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        status: "unattested",
        finalized: true,
      });
      expect((await committee.readinessSnapshot()).reasons).toContain(noAnchor);
      let blocks = 0;
      for (const header of spare.slice(finalityDepth + 1)) {
        chain.mine({ append: header });
        blocks += 1;
        const result = await tick();
        const state = await store.getL1SourceState();
        if (state?.stateQueueReplayAnchor !== undefined) {
          expect(result.signedHeaders).toBe(1);
          break;
        }
        expect(result.signedHeaders).toBe(0);
        expect(await store.listDecisionOutbox(signed)).toEqual([]);
        expect(await store.listDaSignatures(signed)).toEqual([]);
        expect(
          state?.observations.every(
            ({ hasPersistedDecision }) => !hasPersistedDecision,
          ),
        ).toBe(true);
        expect((await committee.readinessSnapshot()).reasons).toContain(
          noAnchor,
        );
      }
      // The first append is final once the finality depth of blocks is on
      // top of it, and the anchor is the queue after it.
      expect(blocks).toBe(finalityDepth + 1);
      expect(
        chain.isFinal(
          (await store.getL1SourceState())!.stateQueueReplayAnchor!.queue,
          finalityDepth,
        ),
      ).toBe(true);
      expect((await committee.readinessSnapshot()).reasons).not.toContain(
        noAnchor,
      );
    });

    it("makes no decision and quarantines nothing across a restart before any durable anchor", async () => {
      const { chain, clock, store, service, signed, spare } =
        await chainCommittee("5d", 5);
      const first = service();
      await first.initialize();
      // Appends every block: the header to sign is final, the tail young,
      // and the first committee holds only a candidate.
      for (const header of spare.slice(0, finalityDepth + 1)) {
        chain.mine({ append: header });
      }
      await expect(first.tick()).resolves.toMatchObject({
        signedHeaders: 0,
        errors: [],
      });
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        status: "unattested",
        finalized: true,
      });
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();

      // Restarted, it sees that header moved by an attestation it has no
      // anchor to authenticate.
      const restarted = service();
      await restarted.initialize();
      chain.mine({ attest: signed });
      clock.nowMs += blockMs;
      await expect(restarted.tick()).resolves.toMatchObject({
        signedHeaders: 0,
        errors: [],
      });
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      expect(state?.stateQueueReplayAnchor).toBeUndefined();
      expect(await store.listDecisionOutbox(signed)).toEqual([]);
      expect(await store.listDaSignatures(signed)).toEqual([]);
      expect(
        state?.observations.every(
          ({ hasPersistedDecision }) => !hasPersistedDecision,
        ),
      ).toBe(true);
      // Its own bootstrap candidate, read after the attestation, becomes the
      // durable anchor once it is final; still healthy.
      for (let block = 0; block < finalityDepth; block += 1) chain.mine();
      clock.nowMs += blockMs;
      await expect(restarted.tick()).resolves.toMatchObject({ errors: [] });
      const anchored = await store.getL1SourceState();
      expect(anchored?.status).toBe("healthy");
      expect(anchored?.stateQueueReplayAnchor).toBeDefined();
    });

    it("keeps a durable anchor established by a tick that fails before its healthy write", async () => {
      const { chain, clock, store, service, spare } = await chainCommittee(
        "5e",
        1,
      );
      const observer = service(false);
      await observer.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        return observer.tick();
      };
      await tick();
      for (let block = 0; block < finalityDepth; block += 1) {
        chain.mine({ append: spare[block]! });
        await tick();
      }
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      // This block makes the first append final: the tick establishes the
      // durable anchor, then fails before writing it.
      chain.mine({ append: spare[finalityDepth]! });
      vi.spyOn(store, "listL1Submissions").mockRejectedValueOnce(
        new Error("store read failed"),
      );
      await expect(tick()).rejects.toThrow("store read failed");
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      expect((await observer.readinessSnapshot()).reasons).not.toContain(
        "L1 state queue has no durable replay anchor yet: no decision is made until its history is final",
      );
      // The next tick replays from it, rather than bootstrapping again and
      // waiting out the finality depth once more.
      chain.mine({ append: spare[finalityDepth + 1]! });
      await expect(tick()).resolves.toMatchObject({ errors: [] });
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeDefined();
    });

    it("writes the durable anchor with the first decision, so a restart after that decision can still explain its moves", async () => {
      const { chain, clock, store, service, signed, spare } =
        await chainCommittee("5f", 1);
      const committee = service();
      await committee.initialize();
      const tick = async (on: CommitteeService) => {
        clock.nowMs += blockMs;
        return on.tick();
      };
      await tick(committee);
      for (let block = 0; block < finalityDepth; block += 1) {
        chain.mine({ append: spare[block]! });
        await tick(committee);
      }
      // The tick that establishes the anchor signs, then fails before its
      // healthy write.
      chain.mine({ append: spare[finalityDepth]! });
      vi.spyOn(store, "listL1Submissions").mockRejectedValueOnce(
        new Error("store read failed"),
      );
      await expect(tick(committee)).rejects.toThrow("store read failed");
      const afterFailure = await store.getL1SourceState();
      expect(afterFailure?.stateQueueReplayAnchor).toBeDefined();
      const signedAt = chain.queue()[1]!.outRef;
      expect(
        afterFailure?.observations.find(
          ({ headerHash }) => headerHash === signed,
        ),
      ).toMatchObject({
        stateQueueOutRef: signedAt,
        hasPersistedDecision: true,
      });

      // Restarted, it follows the signed header's attestation.
      const restarted = service();
      await restarted.initialize();
      chain.mine({ attest: signed });
      for (let block = 0; block <= finalityDepth; block += 1) chain.mine();
      await expect(tick(restarted)).resolves.toMatchObject({ errors: [] });
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      expect(
        state?.observations.find(({ headerHash }) => headerHash === signed),
      ).toMatchObject({
        stateQueueOutRef: chain.queue()[1]!.outRef,
        stateQueueStatus: "attested",
        hasPersistedDecision: true,
      });
    });

    it("keeps a deferred header's stored record at its last final output, and records its terminal outcome on the output it was last moved to", async () => {
      const { chain, store, signed, tick, output } = await ticking("60");
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 1 });
      const signedAt = output();
      chain.mine({ attest: signed });
      const attestedAt = output();
      await tick();
      // The attestation is young: the stored record stays where it was
      // final, rather than taking the output a rollback could still undo.
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        stateQueueOutRef: signedAt,
        status: "unattested",
      });
      chain.mine("merge");
      await tick();
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        stateQueueOutRef: signedAt,
        status: "unattested",
      });
      for (let block = 0; block < finalityDepth; block += 1) {
        chain.mine();
        await tick();
      }
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        stateQueueOutRef: attestedAt,
        status: "merged",
        finalized: true,
      });
      expect(
        (await store.getL1SourceState())?.observations.find(
          ({ headerHash }) => headerHash === signed,
        ),
      ).toMatchObject({
        stateQueueOutRef: attestedAt,
        stateQueueStatus: "merged",
        hasPersistedDecision: true,
      });
    });

    it("logs one structured event each time it discards a rolled-back bootstrap candidate", async () => {
      const { chain, clock, service, events, spare } = await chainCommittee(
        "57",
        5,
      );
      const committee = service(false);
      await committee.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      };
      // The latest append is young: the first tick holds only a candidate.
      chain.mine({ append: spare[0]! });
      await tick();
      expect(events).toEqual([]);
      // The candidate names outputs of the rolled-back append, which Kupo
      // no longer knows.
      const discarded = (transactionHash: string) => ({
        event: "l1_replay_anchor_candidate_discarded",
        reason: expect.stringMatching(
          new RegExp(
            `^Kupo does not know state-queue output ${transactionHash}#[0-9]+ exactly once \\(0 matches\\)$`,
            "u",
          ),
        ),
        candidateBlockNo: "6",
        candidateTransactionIndex: "0",
      });
      for (const header of spare.slice(1, 3)) {
        const rolledBack = chain.queue().at(-1)!.outRef.slice(0, 64);
        chain.rollback(1);
        chain.mine({ append: header });
        await tick();
        expect(events.at(-1)).toEqual(discarded(rolledBack));
      }
      expect(events).toHaveLength(2);
    });

    it("quarantines, rather than discards, a bootstrap candidate whose replay fails on anything but not extending it", async () => {
      const { chain, clock, store, service, events, spare } =
        await chainCommittee("58", 5);
      let corrupt = false;
      const committee = service(false, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
        fetchStateQueueReplayCheckpoints: async (
          anchor,
          current,
          tip,
          limit,
        ) => {
          if (corrupt) {
            throw new L1SourceIntegrityError(
              "Kupo replay assigned one transaction to competing points",
            );
          }
          return chain.fetchStateQueueReplayCheckpoints(
            anchor,
            current,
            tip,
            limit,
          );
        },
      });
      await committee.initialize();
      chain.mine({ append: spare[0]! });
      await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      corrupt = true;
      chain.mine({ append: spare[1]! });
      clock.nowMs += blockMs;
      await expect(committee.tick()).rejects.toThrow(/competing points/u);
      expect(events).toEqual([]);
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "quarantined",
        quarantineReason: expect.stringContaining(
          "l1_source_integrity_failed: Kupo replay assigned one transaction to competing points",
        ),
      });
    });

    it(
      "catches up after downtime longer than one replay walk, moving its durable anchor, then follows its headers and decides again",
      { timeout: 120_000 },
      async () => {
        const walkLimit = stateQueueReplayWalkLimit(2);
        const { chain, clock, store, service, events, signed, spare, config } =
          await chainCommittee("6a", 5, walkLimit + 1);
        expect(config.automaticRecoveryMaxDepth).toBe(2);
        const committee = service();
        await committee.initialize();
        await expect(committee.tick()).resolves.toMatchObject({
          signedHeaders: 1,
          errors: [],
        });
        const anchorOf = async () =>
          (await store.getL1SourceState())!.stateQueueReplayAnchor!;
        expect((await anchorOf()).blockNo).toBe("1");
        // Offline for one more append than the k2 walk allows.
        for (const header of spare) chain.mine({ append: header });
        const view = committee.latestL1View();
        clock.nowMs += 3_600_000;
        const catchUps: string[] = [];
        for (;;) {
          clock.nowMs += blockMs;
          const outcome = await committee.tick().then(
            (result) => result,
            (error: unknown) => error as Error,
          );
          if (!(outcome instanceof Error)) {
            expect(outcome.errors).toEqual([]);
            break;
          }
          expect(outcome.message).toMatch(
            /^state-queue replay is catching up/u,
          );
          // Not a quarantine, and no view to decide on: only the anchor moved.
          const state = await store.getL1SourceState();
          expect(state?.status).toBe("healthy");
          expect(committee.latestL1View()).toEqual(view);
          catchUps.push((await anchorOf()).blockNo);
          expect(catchUps.length).toBeLessThan(4);
        }
        // The anchor is beyond k2: more than two blocks after inclusion.
        expect(catchUps).toEqual([(chain.tip - finalityDepth).toString()]);
        expect(
          events.filter(
            ({ event }) => event === "l1_state_queue_replay_catching_up",
          ),
        ).toEqual([
          {
            event: "l1_state_queue_replay_catching_up",
            walkedCheckpoints: walkLimit,
            anchorBlockNo: catchUps[0],
            anchorTransactionIndex: "0",
          },
        ]);
        const state = await store.getL1SourceState();
        expect(state?.status).toBe("healthy");
        expect(
          chain.isFinal(state!.stateQueueReplayAnchor!.queue, finalityDepth),
        ).toBe(true);
        expect(committee.latestL1View()?.observedAtMs).toBe(clock.nowMs);
        // The observation follows the signed header at its first offline append.
        expect(
          state?.observations.find(({ headerHash }) => headerHash === signed),
        ).toMatchObject({
          stateQueueOutRef: chain.queue()[1]!.outRef,
          stateQueueStatus: "unattested",
          hasPersistedDecision: true,
        });
        chain.mine();
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      },
    );

    it(
      "does not pass the L1 view deadline while a long catch-up keeps moving its anchor, and records the outcome it caught up past",
      { timeout: 120_000 },
      async () => {
        const walkLimit = stateQueueReplayWalkLimit(2);
        const offline = 3 * walkLimit;
        const { chain, clock, store, service, signed, spare, config } =
          await chainCommittee("6b", 5, offline + 16);
        expect(config.automaticRecoveryMaxDepth).toBe(2);
        const committee = service();
        await committee.initialize();
        const results: Awaited<ReturnType<CommitteeService["tick"]>>[] = [];
        const l1ViewFatalMs = 3 * blockMs;
        const runner = createCommitteeTickRunner({
          tick: async () => {
            const result = await committee.tick();
            results.push(result);
            return result;
          },
          runAvailabilityResponse: async () => undefined,
          runRetention: async () => undefined,
          latestL1View: () => committee.latestL1View(),
          latestL1ProgressAtMs: () => committee.latestL1ProgressAtMs(),
          setRetentionReadiness: () => undefined,
          l1ViewFatalMs,
          startedAtMs: clock.nowMs,
          nowMs: () => clock.nowMs,
          write: () => undefined,
        });
        await runner.runTick();
        expect(results).toMatchObject([{ signedHeaders: 1, errors: [] }]);
        results.length = 0;
        // Offline: the signed header is attested and merged, then three
        // walks' worth of appends land.
        chain.mine({ attest: signed });
        chain.mine({ append: spare[0]! });
        chain.mine("merge");
        for (const header of spare.slice(1, offline))
          chain.mine({ append: header });
        // Every tick is further apart than the deadline, and one more append
        // lands between ticks.
        const anchors: bigint[] = [];
        let next = offline;
        while (results.length === 0) {
          chain.mine({ append: spare[next]! });
          next += 1;
          clock.nowMs += l1ViewFatalMs + blockMs;
          await runner.runTick();
          expect(runner.liveness().l1ViewUnavailable).toBeUndefined();
          const state = await store.getL1SourceState();
          expect(state?.status).toBe("healthy");
          anchors.push(BigInt(state!.stateQueueReplayAnchor!.blockNo));
          expect(anchors.length).toBeLessThan(8);
        }
        expect(anchors.length).toBeGreaterThanOrEqual(3);
        expect(anchors).toEqual([...anchors].sort((a, b) => (a < b ? -1 : 1)));
        expect(new Set(anchors).size).toBe(anchors.length);
        expect(results).toMatchObject([{ errors: [] }]);
        expect(committee.latestL1View()?.observedAtMs).toBe(clock.nowMs);
        await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
          status: "merged",
          finalized: true,
        });
        expect(
          (await store.getL1SourceState())?.observations.find(
            ({ headerHash }) => headerHash === signed,
          ),
        ).toMatchObject({
          stateQueueStatus: "merged",
          hasPersistedDecision: true,
        });
        // Once caught up, the deadline applies to the views again.
        clock.nowMs += l1ViewFatalMs + blockMs;
        vi.spyOn(committee, "tick").mockRejectedValueOnce(
          new Error("provider unreachable"),
        );
        await runner.runTick();
        expect(runner.liveness().l1ViewUnavailable).toBeDefined();
      },
    );

    it("counts no decision made before any durable anchor, so a header with a decision record from then follows its attestation healthily once anchored", async () => {
      const { chain, clock, store, service, config, signed, spare } =
        await chainCommittee("6c", 5);
      // A record of a decision about the signed header exists before any
      // anchor does.
      await store.saveL1Submission({
        deploymentFingerprint: config.deploymentFingerprint,
        headerHash: signed,
        txKind: "init",
        txHash: "71".repeat(32),
        inputsUsed: [],
        submittedAt: "2026-07-28T00:00:00.000Z",
        resultStatus: "submitted",
      });
      const first = service(false);
      await first.initialize();
      chain.mine({ append: spare[0]! });
      await expect(first.tick()).resolves.toMatchObject({ errors: [] });
      const before = await store.getL1SourceState();
      expect(before?.stateQueueReplayAnchor).toBeUndefined();
      expect(
        before?.observations.every(
          ({ hasPersistedDecision }) => !hasPersistedDecision,
        ),
      ).toBe(true);

      // Restarted, it sees the header attested, and anchors past it.
      const restarted = service(false);
      await restarted.initialize();
      chain.mine({ attest: signed });
      for (let block = 0; block < finalityDepth; block += 1) chain.mine();
      clock.nowMs += blockMs;
      await expect(restarted.tick()).resolves.toMatchObject({ errors: [] });
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      expect(state?.stateQueueReplayAnchor).toBeDefined();
      expect(
        state?.observations.find(({ headerHash }) => headerHash === signed),
      ).toMatchObject({
        stateQueueOutRef: chain.queue()[1]!.outRef,
        stateQueueStatus: "attested",
        hasPersistedDecision: true,
      });
    });

    it("discards, rather than quarantines, a bootstrap candidate whose history the SDK finds no path through", async () => {
      const { chain, clock, store, service, events, spare } =
        await chainCommittee("6d", 5);
      let tamper = false;
      const committee = service(false, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
        fetchStateQueueReplayCheckpoints: async (
          anchor,
          current,
          tip,
          limit,
        ) => {
          const checkpoints = await chain.fetchStateQueueReplayCheckpoints(
            anchor,
            current,
            tip,
            limit,
          );
          return tamper
            ? checkpoints.map((checkpoint) => ({
                ...checkpoint,
                checkpointDigest: "00".repeat(32),
              }))
            : checkpoints;
        },
      });
      await committee.initialize();
      chain.mine({ append: spare[0]! });
      await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      tamper = true;
      chain.mine({ append: spare[1]! });
      clock.nowMs += blockMs;
      await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      expect(events).toEqual([
        {
          event: "l1_replay_anchor_candidate_discarded",
          reason:
            "state-queue checkpoint history is non-canonical or does not extend the durable cursor",
          candidateBlockNo: "6",
          candidateTransactionIndex: "0",
        },
      ]);
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "healthy",
      });
    });

    it("fails the tick, without quarantine, when a provider with no replay source sees the queue change", async () => {
      const { chain, clock, store, service, spare } = await chainCommittee(
        "6e",
        10,
      );
      const committee = service(false, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
      });
      await committee.initialize();
      for (let tick = 0; tick < 2; tick += 1) {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      }
      chain.mine({ append: spare[0]! });
      for (let tick = 0; tick < 2; tick += 1) {
        clock.nowMs += blockMs;
        await expect(committee.tick()).rejects.toThrow(
          "state-queue provider has no authenticated ordered history source",
        );
        await expect(store.getL1SourceState()).resolves.toMatchObject({
          status: "healthy",
        });
      }
    });

    describe("catching up on history longer than one walk", () => {
      const walkLimit = stateQueueReplayWalkLimit(finalityDepth);
      const catchingUp = /^state-queue replay is catching up/u;
      const observationOf = async (
        store: Awaited<ReturnType<typeof chainCommittee>>["store"],
        headerHash: string,
      ) =>
        (await store.getL1SourceState())?.observations.find(
          (observation) => observation.headerHash === headerHash,
        );

      it(
        "takes a decided header's status from the snapshot when the walk leaves it at the output the snapshot shows",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, store, service, signed, spare } =
            await chainCommittee("70", 5, walkLimit, false);
          const committee = service();
          await committee.initialize();
          await expect(committee.tick()).resolves.toMatchObject({
            signedHeaders: 1,
          });
          // Offline: a datum update attests the signed header, then a walk's
          // worth of appends lands after it.
          chain.mine({ attest: signed });
          const attestedAt = chain.queue()[1]!.outRef;
          for (const header of spare) chain.mine({ append: header });
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
          for (let tick = 0; tick < 3; tick += 1) {
            clock.nowMs += blockMs;
            await expect(committee.tick()).resolves.toMatchObject({
              errors: [],
            });
            expect((await store.getL1SourceState())?.status).toBe("healthy");
            chain.mine();
          }
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
        },
      );

      /**
       * A signed header attested in the final part of a walk and attested
       * again in its young part: the catch-up leaves it at the first
       * attestation's output, which the snapshot no longer shows.
       */
      const movedAgainWhileYoung = async (
        seedByte: string,
        spareCount = walkLimit,
      ) => {
        const setup = await chainCommittee(seedByte, 5, spareCount, false);
        const { chain, clock, store, service, signed, spare } = setup;
        const committee = service();
        await committee.initialize();
        await expect(committee.tick()).resolves.toMatchObject({
          signedHeaders: 1,
        });
        chain.mine({ attest: signed });
        const attestedAt = chain.queue()[1]!.outRef;
        for (const header of spare.slice(0, walkLimit - finalityDepth)) {
          chain.mine({ append: header });
        }
        chain.mine({ attest: signed });
        const reattestedAt = chain.queue()[1]!.outRef;
        for (const header of spare.slice(
          walkLimit - finalityDepth,
          walkLimit - 1,
        )) {
          chain.mine({ append: header });
        }
        const outbox = (await store.listDecisionOutbox(signed)).length;
        clock.nowMs += blockMs;
        await expect(committee.tick()).rejects.toThrow(catchingUp);
        await expect(observationOf(store, signed)).resolves.toMatchObject({
          stateQueueOutRef: attestedAt,
          stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
          finalized: true,
          hasPersistedDecision: true,
        });
        expect(await store.listDecisionOutbox(signed)).toHaveLength(outbox);
        return { ...setup, committee, attestedAt, reattestedAt, outbox };
      };

      it(
        "records an unknown status when the walk leaves a decided header where the snapshot no longer shows it, and fills it from the next final move",
        { timeout: 120_000 },
        async () => {
          const {
            chain,
            clock,
            store,
            committee,
            signed,
            reattestedAt,
            outbox,
          } = await movedAgainWhileYoung("71");
          // The second attestation is still young: the header is deferred, and
          // nothing is decided on it while its status is unknown.
          clock.nowMs += blockMs;
          await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
          });
          expect(await store.listDecisionOutbox(signed)).toHaveLength(outbox);
          // Once it is final, replay explains the move from the unknown one.
          chain.mine();
          clock.nowMs += blockMs;
          await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
          expect((await store.getL1SourceState())?.status).toBe("healthy");
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: reattestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
        },
      );

      it(
        "fills an unknown status from the next observation of the same output",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, store, committee, signed, attestedAt } =
            await movedAgainWhileYoung("72");
          // The young second attestation is rolled back: the header is back
          // at the output the catch-up left it at.
          chain.rollback(finalityDepth);
          for (let block = 0; block < finalityDepth; block += 1) chain.mine();
          clock.nowMs += blockMs;
          await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
          expect((await store.getL1SourceState())?.status).toBe("healthy");
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
        },
      );

      it(
        "fills an unknown status on a later catch-up that finds the header at the same output",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, store, committee, signed, attestedAt, spare } =
            await movedAgainWhileYoung("79", 2 * walkLimit + 1);
          // The young second attestation is rolled back, and another walk's
          // worth of appends lands after the anchor before the next tick.
          chain.rollback(finalityDepth);
          for (const header of spare.slice(walkLimit)) {
            chain.mine({ append: header });
          }
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          expect((await store.getL1SourceState())?.status).toBe("healthy");
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
        },
      );

      it("still quarantines a decided header whose known status is contradicted at an unchanged output", async () => {
        const { chainProvider, clock, store, service, signed } =
          await chainCommittee("73", 5);
        let contradict = false;
        const lying = async () => {
          const snapshot = await chainProvider.fetchStateQueueSnapshot!();
          return {
            ...snapshot,
            nodes: snapshot.nodes.map((node) =>
              contradict && node.linkedListKey === signed
                ? {
                    ...node,
                    daAttestation: {
                      Attested: { commitment_hash: "44".repeat(32) },
                    },
                  }
                : node,
            ),
          };
        };
        const committee = service(true, {
          ...chainProvider,
          fetchStateQueueNodes: async () => (await lying()).nodes,
          fetchStateQueueSnapshot: lying,
        });
        await committee.initialize();
        expect((await committee.tick()).signedHeaders).toBe(1);
        contradict = true;
        clock.nowMs += blockMs;
        const { outRef } = (await lying()).nodes[0]!;
        const message = `state-queue status disagreement at unchanged output ${outRef}: stored=Unattested, observed=Attested:${"44".repeat(32)}`;
        await expect(committee.tick()).rejects.toThrow(message);
        await expect(store.getL1SourceState()).resolves.toMatchObject({
          status: "quarantined",
          quarantineReason: `l1_source_integrity_failed: ${message}`,
        });
      });

      it(
        "records no merge its walk saw only in young history, and records it once final",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, store, service, signed, spare } =
            await chainCommittee("74", 5, walkLimit, false);
          const committee = service();
          await committee.initialize();
          await expect(committee.tick()).resolves.toMatchObject({
            signedHeaders: 1,
          });
          // Offline: the signed header is attested (final by the catch-up
          // tick), appends fill the walk, and its last block merges it (young).
          chain.mine({ attest: signed });
          const attestedAt = chain.queue()[1]!.outRef;
          for (const header of spare.slice(0, walkLimit - 2)) {
            chain.mine({ append: header });
          }
          chain.mine("merge");
          chain.mine({ append: spare[walkLimit - 2]! });
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          // Neither the stored record nor the observation is merged yet.
          await expect(
            store.getStateQueueHeader(signed),
          ).resolves.toMatchObject({
            status: "unattested",
          });
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
          });
          let healthyTicks = 0;
          for (let tick = 0; tick < 4; tick += 1) {
            clock.nowMs += blockMs;
            const outcome = await committee.tick().then(
              (result) => result,
              (error: unknown) => error as Error,
            );
            expect(outcome).not.toBeInstanceOf(Error);
            expect((await store.getL1SourceState())?.status).toBe("healthy");
            healthyTicks += 1;
            chain.mine();
          }
          expect(healthyTicks).toBe(4);
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueStatus: "merged",
            hasPersistedDecision: true,
          });
          await expect(
            store.getStateQueueHeader(signed),
          ).resolves.toMatchObject({
            status: "merged",
            finalized: true,
          });
        },
      );

      it(
        "converges on a decided header attested before a walk's worth of appends and merged beyond the walk",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, store, service, signed, spare } =
            await chainCommittee("7c", 5, walkLimit, false);
          const committee = service();
          await committee.initialize();
          await expect(committee.tick()).resolves.toMatchObject({
            signedHeaders: 1,
          });
          // Offline: the signed header is attested, more than a walk of
          // appends lands after it, and it is then merged, all of it final.
          // The output the walk leaves it at is one no snapshot shows again.
          chain.mine({ attest: signed });
          const attestedAt = chain.queue()[1]!.outRef;
          for (const header of spare) chain.mine({ append: header });
          chain.mine("merge");
          for (let block = 0; block <= finalityDepth; block += 1) chain.mine();
          const runner = createCommitteeTickRunner({
            tick: () => committee.tick(),
            runAvailabilityResponse: async () => undefined,
            runRetention: async () => undefined,
            latestL1View: () => committee.latestL1View(),
            latestL1ProgressAtMs: () => committee.latestL1ProgressAtMs(),
            setRetentionReadiness: () => undefined,
            l1ViewFatalMs: 3 * blockMs,
            startedAtMs: clock.nowMs,
            nowMs: () => clock.nowMs,
            write: () => undefined,
          });
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
            lastKnownStatus: "unattested",
            hasPersistedDecision: true,
          });
          // The next tick carries on from the anchor through the merge, and
          // the node stays healthy past the L1 view deadline.
          for (let tick = 0; tick < 5; tick += 1) {
            clock.nowMs += blockMs;
            await runner.runTick();
            expect((await store.getL1SourceState())?.status).toBe("healthy");
            chain.mine();
          }
          expect(runner.liveness().l1ViewUnavailable).toBeUndefined();
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueStatus: "merged",
            hasPersistedDecision: true,
          });
          await expect(
            store.getStateQueueHeader(signed),
          ).resolves.toMatchObject({
            status: "merged",
            finalized: true,
          });
        },
      );

      it(
        "replays a history of exactly one walk that reaches the snapshot as a normal tick",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, service, events, spare } = await chainCommittee(
            "75",
            5,
            stateQueueReplayWalkLimit(2),
          );
          const committee = service();
          await committee.initialize();
          await committee.tick();
          for (const header of spare) chain.mine({ append: header });
          clock.nowMs += blockMs;
          await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
          expect(
            events.filter(
              ({ event }) => event === "l1_state_queue_replay_catching_up",
            ),
          ).toEqual([]);
        },
      );

      it(
        "makes no progress, and still meets the L1 view deadline, on a walk none of whose checkpoints is final",
        { timeout: 120_000 },
        async () => {
          const perBlock = Math.ceil((walkLimit + 1) / finalityDepth);
          const { chain, clock, store, service, spare } = await chainCommittee(
            "76",
            5,
            perBlock * finalityDepth,
          );
          const committee = service();
          await committee.initialize();
          await committee.tick();
          const anchor = (await store.getL1SourceState())!
            .stateQueueReplayAnchor;
          // More than a walk of checkpoints, all inside the young window.
          for (let block = 0; block < finalityDepth; block += 1) {
            chain.mine(
              ...spare
                .slice(block * perBlock, (block + 1) * perBlock)
                .map((header) => ({ append: header })),
            );
          }
          const l1ViewFatalMs = 3 * blockMs;
          const runner = createCommitteeTickRunner({
            tick: () => committee.tick(),
            runAvailabilityResponse: async () => undefined,
            runRetention: async () => undefined,
            latestL1View: () => committee.latestL1View(),
            latestL1ProgressAtMs: () => committee.latestL1ProgressAtMs(),
            setRetentionReadiness: () => undefined,
            l1ViewFatalMs,
            startedAtMs: clock.nowMs,
            nowMs: () => clock.nowMs,
            write: () => undefined,
          });
          clock.nowMs += blockMs;
          const outcome = await committee.tick().then(
            () => undefined,
            (error: unknown) => error,
          );
          // An observation failure, not an integrity one.
          expect(outcome).toBeInstanceOf(Error);
          expect(outcome).not.toBeInstanceOf(L1SourceIntegrityError);
          expect((outcome as Error).message).toMatch(
            /^state-queue replay is catching up, but none of the next \d+ checkpoints is final yet$/u,
          );
          expect(committee.latestL1ProgressAtMs()).toBeUndefined();
          const state = await store.getL1SourceState();
          expect(state?.status).toBe("healthy");
          expect(state?.stateQueueReplayAnchor).toEqual(anchor);
          clock.nowMs += l1ViewFatalMs;
          await runner.runTick();
          expect(runner.liveness().l1ViewUnavailable).toBeDefined();
          expect((await store.getL1SourceState())?.status).toBe("healthy");
        },
      );

      it(
        "quarantines a decided observation its walk's final steps do not lead from",
        { timeout: 120_000 },
        async () => {
          // Replay from the anchor always starts where a decided observation
          // is, so only durable state that disagrees with the anchor reaches
          // this check: here a decided observation of the queue's tail at an
          // output it never had.
          const { chain, clock, store, service, spare } = await chainCommittee(
            "77",
            5,
            walkLimit + 1,
            false,
          );
          const committee = service();
          await committee.initialize();
          await committee.tick();
          const tail = chain.queue()[2]!.headerHash!;
          for (const header of spare) chain.mine({ append: header });
          const read = store.getL1SourceState.bind(store);
          vi.spyOn(store, "getL1SourceState").mockImplementationOnce(
            async () => {
              const state = (await read())!;
              return {
                ...state,
                observations: state.observations.map((observation) =>
                  observation.headerHash === tail
                    ? {
                        ...observation,
                        stateQueueOutRef: `${"ee".repeat(32)}#0`,
                        hasPersistedDecision: true,
                      }
                    : observation,
                ),
              };
            },
          );
          clock.nowMs += blockMs;
          await committee.tick();
          await expect(store.getL1SourceState()).resolves.toMatchObject({
            status: "quarantined",
            quarantineReason: `l1_source_decision_forked:${tail}`,
          });
        },
      );

      it(
        "quarantines a decided unknown observation its walk's final steps do not lead from",
        { timeout: 120_000 },
        async () => {
          // As above, but the tampered observation's status is unknown: it is
          // checked like a known one, not left to the store to refuse.
          const { chain, clock, store, service, spare } = await chainCommittee(
            "7a",
            5,
            walkLimit + 1,
            false,
          );
          const committee = service();
          await committee.initialize();
          await committee.tick();
          const tail = chain.queue()[2]!.headerHash!;
          for (const header of spare) chain.mine({ append: header });
          const read = store.getL1SourceState.bind(store);
          vi.spyOn(store, "getL1SourceState").mockImplementationOnce(
            async () => {
              const state = (await read())!;
              return {
                ...state,
                observations: state.observations.map((observation) =>
                  observation.headerHash === tail
                    ? {
                        ...observation,
                        stateQueueOutRef: `${"ee".repeat(32)}#0`,
                        stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
                        lastKnownStatus: "attested" as const,
                        hasPersistedDecision: true,
                      }
                    : observation,
                ),
              };
            },
          );
          clock.nowMs += blockMs;
          await committee.tick();
          await expect(store.getL1SourceState()).resolves.toMatchObject({
            status: "quarantined",
            quarantineReason: `l1_source_decision_forked:${tail}`,
          });
        },
      );

      it(
        "quarantines a decided header whose known status a snapshot contradicts across an unknown one",
        { timeout: 120_000 },
        async () => {
          const { chain, chainProvider, clock, store, service, signed, spare } =
            await chainCommittee("7b", 5, walkLimit, false);
          let contradict = false;
          const lying = async () => {
            const snapshot = await chainProvider.fetchStateQueueSnapshot!();
            return {
              ...snapshot,
              nodes: snapshot.nodes.map((node) =>
                contradict && node.linkedListKey === signed
                  ? { ...node, daAttestation: SDK.NO_DA_ATTESTATION }
                  : node,
              ),
            };
          };
          const committee = service(true, {
            ...chainProvider,
            fetchStateQueueNodes: async () => (await lying()).nodes,
            fetchStateQueueSnapshot: lying,
          });
          await committee.initialize();
          await expect(committee.tick()).resolves.toMatchObject({
            signedHeaders: 1,
          });
          // The node sees the signed header attested, and that final.
          chain.mine({ attest: signed });
          for (let block = 0; block <= finalityDepth; block += 1) {
            chain.mine();
            clock.nowMs += blockMs;
            await committee.tick();
          }
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
          // Offline: it is attested again in the final part of a walk and
          // again in its young part; the snapshot then reports it unattested.
          chain.mine({ attest: signed });
          for (const header of spare.slice(0, walkLimit - finalityDepth)) {
            chain.mine({ append: header });
          }
          chain.mine({ attest: signed });
          for (const header of spare.slice(
            walkLimit - finalityDepth,
            walkLimit - 1,
          )) {
            chain.mine({ append: header });
          }
          contradict = true;
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
            lastKnownStatus: "attested",
          });
          expect((await store.getL1SourceState())?.status).toBe("healthy");
          // Once the young move is final, the unattested status it would fill
          // the unknown one in with contradicts the attested one known before.
          for (let tick = 0; tick <= finalityDepth; tick += 1) {
            clock.nowMs += blockMs;
            await committee.tick().catch(() => undefined);
            chain.mine();
          }
          await expect(store.getL1SourceState()).resolves.toMatchObject({
            status: "quarantined",
            quarantineReason: `l1_source_decision_forked:${signed}`,
          });
        },
      );

      it(
        "acknowledges the chain-sync rollback feed it checked when it catches up",
        { timeout: 120_000 },
        async () => {
          const { chain, chainProvider, clock, config, store, service, spare } =
            await chainCommittee("78", 5, walkLimit + 1);
          const cursorAt = (sequence: number): ChainSyncCursor => ({
            sequence,
            point: {
              network: config.network,
              slot: sequence,
              blockHash: sequence.toString(16).padStart(64, "0"),
              providerSource: "chain-sync:node-a",
              observedAt: "2026-07-28T00:00:00.000Z",
            },
            rollbackGeneration: 0,
          });
          const feed: { current: ChainSyncCursor; consumed?: ChainSyncCursor } =
            { current: cursorAt(0) };
          const committee = service(true, {
            ...chainProvider,
            fetchStateQueueSnapshot: async () => ({
              ...chain.snapshot(),
              chainSyncCursor: feed.current,
            }),
            currentChainSyncCursor: async () => feed.current,
            replayChainSyncEvents: async (fromSequence: number) =>
              Array.from(
                { length: feed.current.sequence - fromSequence },
                (_, index): ChainSyncEvent => ({
                  direction: "roll_forward",
                  point: cursorAt(fromSequence + index + 1).point,
                }),
              ),
            loadConsumedChainSyncCursor: async () => feed.consumed,
            acknowledgeChainSyncCursor: async (cursor: ChainSyncCursor) => {
              feed.consumed = cursor;
              return { rollbackSinceCapture: false };
            },
          } as StateQueueProvider);
          await committee.initialize();
          await expect(committee.tick()).resolves.toMatchObject({
            signedHeaders: 1,
          });
          expect(feed.consumed).toEqual(cursorAt(0));
          for (const header of spare) chain.mine({ append: header });
          feed.current = cursorAt(1);
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          expect(feed.consumed).toEqual(cursorAt(1));
          expect((await store.getL1SourceState())?.status).toBe("healthy");
        },
      );
    });
  });
};
