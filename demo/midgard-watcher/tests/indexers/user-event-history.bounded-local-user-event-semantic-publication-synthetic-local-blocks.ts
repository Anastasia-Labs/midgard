import { h32 } from "@al-ft/midgard-test-support/hex";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { createWatcherLocalUserEventPublisher } from "../../src/indexers/user-event-history.js";
import {
  acceptWatcherLocalUserEventPublication,
  assertWatcherLocalUserEventAuthorityCurrent,
  createWatcherLocalUserEventHistory,
  prepareWatcherLocalUserEventTransition,
  readWatcherLocalUserEventAuthority,
  readWatcherLocalUserEventHistory,
  readWatcherLocalUserEventTransition,
} from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  createWatcherDurableRuntime,
  persistWatcherUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  makeEmptyWatcherDurableStore,
  makeWatcherDurableStore,
} from "../../src/storage/durable-store.js";
import { watcherUserEventArchiveDigest } from "../../src/storage/user-event-checkpoint.js";
import {
  evaluateWatcherBlockReplay,
  readWatcherBlockReplayEventAuthorityRecords,
} from "../../src/verification/block-replay.js";
import { evaluateWatcherPhaseABlock } from "../../src/verification/phase-a-verifier.js";
import type { WatcherRuleBundle } from "../../src/verification/rule-bundle.js";
import { computeWatcherRuleBundleCommitment } from "../../src/verification/rule-bundle.js";
import { makeLocalDepositReplayFixture } from "../support/local-event-replay-fixture.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";
import { sha256 } from "./user-event-history.native-history-retirement-frontier.js";

describe("bounded local user-event semantic publication (synthetic local blocks)", () => {
  it.each([false, true])(
    "publishes the whole activation block, empty successor and ordered same-block deposit lifecycle only after archive/CAS (original data encoding: %s)",
    async (preserveDataEncoding) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      try {
        const { pair, input, origin, facts } = await openOrigin(fixture);
        const globalStore = makeWatcherDurableStore({
          deploymentMarker: fixture.deploymentIdentity.durableMarker,
          revision: "0",
          records: {
            ...makeEmptyWatcherDurableStore(
              fixture.deploymentIdentity.durableMarker,
            ),
            chainPoints: [
              {
                chainPointId: facts.block.chainPoint.chainPointId,
                providerId: facts.block.provider.providerId,
                blockHash: facts.block.chainPoint.blockHash,
                slot: facts.block.chainPoint.slot,
                blockNo: facts.block.chainPoint.blockNo,
                depth: facts.block.chainPoint.depth,
              },
            ],
          },
        });
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(pair.finality).policy,
          globalStore,
        );
        const publisher = await createWatcherLocalUserEventPublisher({
          ...input,
          origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        const bootstrap = publisher.read();
        expect(bootstrap.store.revision).toBe("0");
        expect(bootstrap.store.l1Observations).toEqual([]);
        expect(Reflect.set(bootstrap.store, "revision", "99")).toBe(false);
        expect(
          Reflect.set(
            bootstrap.store.deploymentMarker,
            "manifestId",
            h32(0xff),
          ),
        ).toBe(false);
        expect(Reflect.set(bootstrap.store.chainPoints, "0", {})).toBe(false);
        expect(
          Reflect.set(bootstrap.policy.deposit, "policyId", "ff".repeat(28)),
        ).toBe(false);
        let releaseWrite!: () => void;
        let enteredWrite!: () => void;
        const released = new Promise<void>((resolve) => {
          releaseWrite = resolve;
        });
        const entered = new Promise<void>((resolve) => {
          enteredWrite = resolve;
        });
        durable.setBeforePut(async () => {
          durable.setBeforePut(null);
          enteredWrite();
          await released;
        });
        const publishing = publisher.publish(pair);
        await entered;
        expect(publisher.read()).toMatchObject({
          cursor: null,
          retainedEntries: 0,
          // Only the origin archive is retained before the first entry.
          retainedArchive: { objects: 1 },
          anchorDue: false,
          status: "publication_pending",
          store: { revision: "0" },
        });
        await expect(publisher.publish(pair)).rejects.toThrow(
          "already in flight",
        );
        releaseWrite();
        const accepted = await publishing;
        expect(accepted.cursor).toEqual(fixture.activationBlock.point);
        const activated = publisher.read();
        expect(activated.store.revision).toBe("1");
        expect(activated.checkpoint?.checkpointSequence).toBe("0");
        expect(activated.store.l1Observations).toHaveLength(1);
        const recordedBlock = JSON.parse(
          Buffer.from(
            activated.store.l1Observations[0]!.payload.cborHex,
            "hex",
          ).toString("utf8"),
        ) as { transactions: readonly unknown[] };
        expect(recordedBlock.transactions).toHaveLength(
          facts.block.transactions.length,
        );
        expect(
          Reflect.set(
            activated.store.l1Observations[0]!.payload,
            "cborHex",
            "00",
          ),
        ).toBe(false);
        const countAfterActivation = durable.casCount();
        expect(await publisher.publish(pair)).toEqual(accepted);
        expect(durable.casCount()).toBe(countAfterActivation);
        expect(publisher.read().store.revision).toBe("1");
        const archivedEvidence = [...durable.objects.values()]
          .map(
            (bytes) =>
              JSON.parse(Buffer.from(bytes).toString("utf8")) as Record<
                string,
                unknown
              >,
          )
          .find(
            (value) =>
              value.schemaVersion ===
              "midgard-watcher-local-user-event-block-evidence-v1",
          );
        expect(archivedEvidence?.numericEncoding).toBe("exact-decimal-strings");
        const archivedWitnesses = archivedEvidence!.witnesses as {
          first: { finality: { startedAtMonotonicMs: string } };
          current: { finality: { admittedAtMonotonicMs: string } };
        };
        expect(
          Number(archivedWitnesses.first.finality.startedAtMonotonicMs),
        ).toBe(facts.originalWitness.first.finality.startedAtMonotonicMs);
        expect(
          Number(archivedWitnesses.current.finality.admittedAtMonotonicMs),
        ).toBe(facts.originalWitness.current.finality.admittedAtMonotonicMs);
        await pair.close();
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        await publisher.publish(empty);
        expect(publisher.read()).toMatchObject({
          cursor: fixture.emptySuccessorBlock.point,
          retainedEntries: 2,
          store: { revision: "2" },
        });
        expect(publisher.read().store.l1Observations).toHaveLength(2);
        const lifecycle = historyLifecycle(facts, preserveDataEncoding);
        const eventBlock = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create, lifecycle.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        const events = await fixture.openFinalizedBlock(eventBlock);
        await empty.close();
        await publisher.publish(events);
        const completed = publisher.read();
        expect(completed).toMatchObject({
          cursor: eventBlock.point,
          retainedEntries: 3,
          store: { revision: "3" },
          snapshot: { activeEvents: [] },
        });
        expect(completed.snapshot.terminalEvents).toHaveLength(1);
        const originalOutput = CML.Transaction.from_cbor_hex(lifecycle.create)
          .body()
          .outputs()
          .get(0);
        const originalDatum = originalOutput.datum()!.as_datum()!;
        expect(
          originalDatum.to_cbor_hex() !== originalDatum.to_canonical_cbor_hex(),
        ).toBe(preserveDataEncoding);
        expect(completed.snapshot.terminalEvents[0]).toMatchObject({
          eventId: lifecycle.expectedEventId,
          transactionHash: lifecycle.createId,
          originBlockHash: eventBlock.point.blockHash,
          terminalBlockHash: eventBlock.point.blockHash,
          terminalStatus: "absorbed",
          finalityStatus: "final",
          terminalFinalityStatus: "final",
          datumCborHex: originalDatum.to_cbor_hex(),
          datumDigest: sha256(originalDatum.to_cbor_bytes()),
          outputCborHex: originalOutput.to_cbor_hex(),
        });
        expect(completed.store.protocolUtxos).toEqual([]);
        expect(completed.store.spentProtocolUtxos).toEqual([]);
        expect(
          Reflect.set(completed.snapshot.terminalEvents[0]!, "eventId", "00"),
        ).toBe(false);
        expect(durable.runtime.read().currentStore).toEqual(globalStore);
        await expect(
          publisher.eventAuthority({
            ...events,
            eventId: lifecycle.expectedEventId,
            kind: "deposit",
          }),
        ).rejects.toThrow("fresh post-publication capture");
        const fresh = await fixture.openFinalizedBlock(eventBlock);
        await expect(
          publisher.eventAuthority({
            ...fresh,
            eventId: lifecycle.expectedEventId,
            kind: "withdrawal",
          }),
        ).rejects.toThrow("event is not retained");
        const authority = await publisher.eventAuthority({
          ...fresh,
          eventId: lifecycle.expectedEventId,
          kind: "deposit",
        });
        const authorityRead =
          await readWatcherLocalUserEventAuthority(authority);
        expect(authorityRead).toMatchObject({
          deploymentManifestId: fixture.deploymentIdentity.manifestId,
          blueprintHash: completed.policy.blueprintHash,
          event: completed.snapshot.terminalEvents[0],
          checkpointDigest: completed.checkpoint!.checkpointDigest,
          checkpointPayloadDigest: completed.checkpoint!.payloadDigest,
          snapshotDigest: completed.snapshot.snapshotDigest,
          historyEntryDigests: [completed.entryDigest],
        });
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(authority),
        ).not.toThrow();
        const replayInput = await makeLocalDepositReplayFixture(
          authority,
          fixture.deploymentIdentity.programCommitments,
        );
        const replayEventAuthority = replayInput.eventAuthorities![0]!;
        if (replayEventAuthority.localUserEvent === undefined)
          throw new Error("local replay fixture has parser authority");
        const replayResult = await evaluateWatcherBlockReplay(replayInput);
        expect(replayResult).toMatchObject({
          action: "accept",
          reasonCodes: [],
          eventRoots: [{ stepIndex: 0, phase: "Deposit", mutationCount: 1 }],
        });
        expect(replayResult.postStateRoot).not.toBe(
          replayResult.priorStateRoot,
        );
        expect(
          readWatcherBlockReplayEventAuthorityRecords(replayResult),
        ).toMatchObject([
          {
            phase: "Deposit",
            event: authorityRead.event,
            origin: {
              source: "local_publication",
              deploymentManifestId: fixture.deploymentIdentity.manifestId,
              checkpointDigest: authorityRead.checkpointDigest,
            },
          },
        ]);
        const mismatchedBundles: readonly WatcherRuleBundle[] = [
          { ...replayInput.ruleBundle, deploymentManifestId: "f1".repeat(32) },
          { ...replayInput.ruleBundle, blueprintHash: "f2".repeat(32) },
          { ...replayInput.ruleBundle, network: "Preview" },
        ];
        for (const ruleBundle of mismatchedBundles) {
          const ruleBundleCommitment =
            computeWatcherRuleBundleCommitment(ruleBundle);
          const phaseA = await evaluateWatcherPhaseABlock({
            ...replayInput,
            ruleBundle,
            ruleBundleCommitment,
          });
          expect(phaseA.action).toBe("accept");
          expect(
            await evaluateWatcherBlockReplay({
              ...replayInput,
              phaseA,
              ruleBundle,
              ruleBundleCommitment,
            }),
          ).toMatchObject({
            action: "error",
            reasonCodes: ["user_event_authority_identity_mismatch"],
          });
        }
        const copiedAuthority = { ...authority };
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(copiedAuthority),
        ).toThrow("not privately admitted");
        expect(
          await evaluateWatcherBlockReplay({
            ...replayInput,
            eventAuthorities: [
              {
                ...replayEventAuthority,
                localUserEvent: copiedAuthority,
              },
            ],
          }),
        ).toMatchObject({
          action: "error",
          reasonCodes: ["user_event_authority_invalid"],
        });
        await expect(
          readWatcherLocalUserEventAuthority({ ...authority }),
        ).rejects.toThrow("not privately admitted");
        await expect(
          Reflect.apply(readWatcherLocalUserEventAuthority, undefined, [
            authorityRead,
          ]),
        ).rejects.toThrow("not privately admitted");
        const successorBlock = await fixture.makeBlock({
          parent: eventBlock,
          transactions: [],
        });
        const successor = await fixture.openFinalizedBlock(successorBlock);
        await publisher.publish(successor);
        await expect(
          readWatcherLocalUserEventAuthority(authority),
        ).rejects.toThrow("no longer matches");
        const freshSuccessor = await fixture.openFinalizedBlock(successorBlock);
        const nextAuthority = await publisher.eventAuthority({
          ...freshSuccessor,
          eventId: lifecycle.expectedEventId,
          kind: "deposit",
        });
        expect(
          (await readWatcherLocalUserEventAuthority(nextAuthority)).event,
        ).toEqual(authorityRead.event);
        expect(durable.runtime.read().currentStore).toEqual(globalStore);
        await freshSuccessor.close();
        await expect(
          readWatcherLocalUserEventAuthority(nextAuthority),
        ).rejects.toThrow();
        const finalPair = await fixture.openFinalizedBlock(successorBlock);
        const finalAuthority = await publisher.eventAuthority({
          ...finalPair,
          eventId: lifecycle.expectedEventId,
          kind: "deposit",
        });
        publisher.close();
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(finalAuthority),
        ).toThrow("closed");
        expect(
          await evaluateWatcherBlockReplay({
            ...replayInput,
            eventAuthorities: [
              {
                ...replayEventAuthority,
                localUserEvent: finalAuthority,
              },
            ],
          }),
        ).toMatchObject({
          action: "error",
          reasonCodes: ["user_event_authority_invalid"],
        });
        await expect(
          readWatcherLocalUserEventAuthority(finalAuthority),
        ).rejects.toThrow("closed");
        expect(() => publisher.read()).toThrow("closed");
      } finally {
        await fixture.close();
      }
    },
    120_000,
  );

  it("retains an unpublished cursor after uncertain CAS and accepts only the exact freshly protected candidate once", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publication = await readWatcherProtectedUserEventCheckpoint(
        durable.runtime,
      );
      const history = createWatcherLocalUserEventHistory({
        ...input,
        origin,
        publication,
      });
      expect(() => readWatcherLocalUserEventHistory({ ...history })).toThrow(
        "not privately admitted",
      );
      expect(() =>
        prepareWatcherLocalUserEventTransition({
          history,
          ...pair,
          referenceAuthority: { ...pair.referenceAuthority },
          publication,
        }),
      ).toThrow();
      const transition = prepareWatcherLocalUserEventTransition({
        history,
        ...pair,
        publication,
      });
      expect(() =>
        readWatcherLocalUserEventTransition({ ...transition }),
      ).toThrow("not privately admitted");
      const prepared = readWatcherLocalUserEventTransition(transition);
      expect(readWatcherLocalUserEventHistory(history).cursor).toBeNull();
      expect(() =>
        acceptWatcherLocalUserEventPublication({
          history,
          transition,
          publication,
        }),
      ).toThrow("exact prepared frame");
      for (const object of prepared.archiveObjects)
        expect(
          await durable.archive.put(Buffer.from(object.bytesHex, "hex")),
        ).toBe(object.digest);
      durable.interruptNextReadBack();
      await expect(
        persistWatcherUserEventCheckpoint(durable.runtime, prepared),
      ).rejects.toThrow("fixture read-back interruption");
      expect(readWatcherLocalUserEventHistory(history)).toMatchObject({
        cursor: null,
        retainedEntries: 0,
        store: { revision: "0" },
      });
      await expect(
        readWatcherProtectedUserEventCheckpoint(durable.runtime),
      ).rejects.toThrow();
      const restartedRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const committed =
        await readWatcherProtectedUserEventCheckpoint(restartedRuntime);
      expect(
        readWatcherProtectedUserEventCheckpointReceipt(committed).checkpoint,
      ).toEqual(prepared.nextCheckpoint);
      expect(() =>
        acceptWatcherLocalUserEventPublication({
          history,
          transition,
          publication: { ...committed },
        }),
      ).toThrow("not admitted");
      const accepted = acceptWatcherLocalUserEventPublication({
        history,
        transition,
        publication: committed,
      });
      expect(accepted.cursor).toEqual(fixture.activationBlock.point);
      expect(
        acceptWatcherLocalUserEventPublication({
          history,
          transition,
          publication: committed,
        }),
      ).toEqual(accepted);
      expect(readWatcherLocalUserEventHistory(history)).toMatchObject({
        retainedEntries: 1,
        store: { revision: "1" },
      });
      expect(() =>
        createWatcherLocalUserEventHistory({
          ...input,
          origin,
          publication: committed,
        }),
      ).toThrow("absent matching protected checkpoint");
    } finally {
      await fixture.close();
    }
  }, 120_000);

  it("writes only newly required archive objects while retaining the full protected closure", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      const put = vi.spyOn(durable.archive, "put");
      await publisher.publish(pair);
      const previous = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durable.runtime),
      ).checkpoint!;
      const protectedDigests = new Set(previous.requiredArchiveDigests);
      expect(put).toHaveBeenCalledTimes(protectedDigests.size);
      put.mockClear();
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      const next = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durable.runtime),
      ).checkpoint!;
      const written = put.mock.calls.map(([bytes]) =>
        watcherUserEventArchiveDigest(bytes),
      );
      expect(written.sort()).toEqual(
        next.requiredArchiveDigests.filter(
          (digest) => !protectedDigests.has(digest),
        ),
      );
      expect(written).toHaveLength(4);
      expect(next.requiredArchiveDigests).toHaveLength(
        protectedDigests.size + written.length,
      );
      expect(
        previous.requiredArchiveDigests.every((digest) =>
          next.requiredArchiveDigests.includes(digest),
        ),
      ).toBe(true);
      expect(publisher.read().store.revision).toBe("2");
      publisher.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);

  it.each(["missing", "tampered"] as const)(
    "refuses %s protected archive bytes before publication and after the initial fresh read",
    async (mode) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      try {
        const { pair, input, origin } = await openOrigin(fixture);
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(pair.finality).policy,
        );
        const publisher = await createWatcherLocalUserEventPublisher({
          ...input,
          origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        await publisher.publish(pair);
        const previous = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        ).checkpoint!;
        const digest = previous.requiredArchiveDigests[0]!;
        const original = Uint8Array.from(durable.objects.get(digest)!);
        const damage = () => {
          if (mode === "missing") durable.objects.delete(digest);
          else {
            const changed = Uint8Array.from(original);
            changed[0] = changed[0]! ^ 1;
            durable.objects.set(digest, changed);
          }
        };
        const expectedError =
          mode === "missing"
            ? "archive object is missing"
            : "archive digest differs";
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        const casBefore = durable.casCount();
        const put = vi.spyOn(durable.archive, "put");

        damage();
        await expect(publisher.publish(empty)).rejects.toThrow(expectedError);
        expect(put).not.toHaveBeenCalled();
        expect(durable.casCount()).toBe(casBefore);
        durable.objects.set(digest, Uint8Array.from(original));

        // The first new write happens after the initial protected read. The
        // refresh must still inspect every old object and refuse this change.
        durable.setBeforePut(async () => {
          durable.setBeforePut(null);
          damage();
        });
        await expect(publisher.publish(empty)).rejects.toThrow(expectedError);
        expect(put).toHaveBeenCalledTimes(4);
        expect(
          put.mock.calls.every(
            ([bytes]) =>
              !previous.requiredArchiveDigests.includes(
                watcherUserEventArchiveDigest(bytes),
              ),
          ),
        ).toBe(true);
        expect(durable.casCount()).toBe(casBefore);
        expect(durable.objects.get(digest)).not.toEqual(original);
        expect(publisher.read()).toMatchObject({
          cursor: fixture.activationBlock.point,
          status: "publication_pending",
          store: { revision: "1" },
        });

        durable.objects.set(digest, Uint8Array.from(original));
        await publisher.publish(empty);
        expect(publisher.read().store.revision).toBe("2");
        expect(durable.casCount()).toBe(casBefore + 1);
        publisher.close();
      } finally {
        await fixture.close();
      }
    },
    120_000,
  );

  it("retries the same archive candidate and refuses first-block substitution and skipped event coverage", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await expect(publisher.publish(empty)).rejects.toThrow(
        "exact activation pair",
      );
      durable.setBeforePut(async () => {
        durable.setBeforePut(null);
        throw new Error("fixture archive interruption");
      });
      await expect(publisher.publish(pair)).rejects.toThrow(
        "fixture archive interruption",
      );
      expect(publisher.read()).toMatchObject({
        cursor: null,
        retainedEntries: 0,
        status: "publication_pending",
      });
      await expect(publisher.publish(empty)).rejects.toThrow(
        "reconcile the original candidate",
      );
      await publisher.publish(pair);
      const skippedBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [],
      });
      const skipped = await fixture.openFinalizedBlock(skippedBlock);
      await expect(publisher.publish(skipped)).rejects.toThrow(
        "strict full-point successor",
      );
      expect(publisher.read()).toMatchObject({
        cursor: fixture.activationBlock.point,
        retainedEntries: 1,
        store: { revision: "1" },
      });
      await publisher.publish(empty);
      expect(publisher.read().store.revision).toBe("2");
      publisher.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});
