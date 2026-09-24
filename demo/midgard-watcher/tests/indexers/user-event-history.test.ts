import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  DepositEvent,
  DepositInfo,
  EventHistoryNode,
  ForcedInclusionTxV1,
  WithdrawalEvent,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  historyRawField,
  historyWithdrawalPayoutDatum,
} from "../../src/indexers/authenticated-event-history.js";
import {
  createWatcherLocalUserEventPublisher,
  recoverWatcherLocalUserEventPublisher,
  replaceWatcherLocalUserEventPublisher,
  resumeWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import {
  acceptWatcherLocalUserEventPublication,
  assertWatcherLocalUserEventAuthorityCurrent,
  createWatcherLocalUserEventHistory,
  prepareWatcherLocalUserEventTransition,
  readWatcherLocalUserEventAuthority,
  readWatcherLocalUserEventHistory,
  readWatcherLocalUserEventTransition,
  type WatcherLocalUserEventEntry,
  type WatcherLocalUserEventHeaderCutoff,
  type WatcherUserEventObservation,
  type WatcherUserEventSnapshot,
} from "../../src/indexers/user-event-indexer.js";
import {
  readWatcherUserEventReferenceEvidence,
  watcherUserEventReferenceOutput,
} from "../../src/indexers/user-event-reference-authority.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import {
  assertWatcherUserEventRuntime,
  createWatcherUserEventRuntime,
} from "../../src/runtime/user-event-runtime.js";
import {
  createWatcherDurableRuntime,
  persistWatcherUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  makeEmptyWatcherDurableStore,
  makeWatcherDurableStore,
  watcherCanonicalJson,
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "../../src/storage/durable-store.js";
import {
  makeWatcherUserEventCheckpoint,
  watcherUserEventArchiveDigest,
} from "../../src/storage/user-event-checkpoint.js";
import { createInMemoryWatcherUserEventCoverageStore } from "../../src/storage/user-event-coverage-store.js";
import {
  assertWatcherAuthenticatedReplayTranscript,
  createWatcherAuthenticatedReplayTranscript,
  replayWatcherAuthenticatedReplayTranscript,
  watcherAuthenticatedReplayTranscriptCborHex,
  watcherReplayRawRecordCborHex,
} from "../../src/verification/authenticated-replay-transcript.js";
import {
  evaluateWatcherBlockReplay,
  readWatcherBlockReplayEventAuthorityRecords,
  watcherBlockReplayDownstreamInputDigest,
  watcherBlockReplayEventAuthorityManifest,
} from "../../src/verification/block-replay.js";
import type { WatcherCommittedEventClaim } from "../../src/verification/event-claims.js";
import { deriveWatcherLocalEventReplayAuthority } from "../../src/verification/local-event-replay-authority.js";
import { evaluateWatcherPhaseABlock } from "../../src/verification/phase-a-verifier.js";
import { readWatcherReplayTranscriptRecords } from "../../src/verification/replay-transcript-records.js";
import type { WatcherRuleBundle } from "../../src/verification/rule-bundle.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.js";
import { makeLocalDepositReplayFixture } from "../support/local-event-replay-fixture.js";
import {
  durableFixture,
  historyLifecycle,
  historyPointerContinuation,
  openOrigin,
  ordinaryLocalOrderCreation,
  syntheticUserEventTransaction as transaction,
  transactionInput,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import {
  createSyntheticUserEventOriginFixture,
  type SyntheticUserEventBlock,
} from "../support/user-event-origin-fixture.js";

// Vitest's structural `toEqual` walks byte arrays element by element through
// its generic iterable-equality path, which costs seconds per megabyte on
// archive objects. Compare bytes as bytes.
const expectSameArchiveBytes = (
  actual: Uint8Array | null,
  expected: Uint8Array,
): void => {
  expect(actual).not.toBeNull();
  expect(Buffer.compare(actual!, expected), "archive bytes differ").toBe(0);
};

const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
const h32 = (byte: string): string => byte.repeat(32);

describe("native history retirement frontier", () => {
  it.each([
    { kind: "deposit", offset: -1n },
    { kind: "deposit", offset: 0n },
    { kind: "deposit", offset: 1n },
    { kind: "withdrawal", offset: -1n },
    { kind: "withdrawal", offset: 0n },
    { kind: "withdrawal", offset: 1n },
  ] as const)(
    "binds $kind retirement to confirmed inclusion frontier offset $offset",
    async ({ kind, offset }) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      let publisher:
        | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
        | undefined;
      try {
        const initial = await openOrigin(fixture);
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(initial.pair.finality).policy,
        );
        publisher = await createWatcherLocalUserEventPublisher({
          ...initial.input,
          origin: initial.origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        await publisher.publish(initial.pair);
        await initial.pair.close();
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        await publisher.publish(empty);
        await empty.close();
        const lifecycle = historyLifecycle(initial.facts, false, {
          kind,
          confirmedEndOffset: offset,
        });
        const admission = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        const admitted = await fixture.openFinalizedBlock(admission);
        await publisher.publish(admitted);
        await admitted.close();
        const original = publisher.read().snapshot.activeEvents[0]!;
        expect(original.kind).toBe(kind);
        const readCheckpoint = async () =>
          readWatcherProtectedUserEventCheckpointReceipt(
            await readWatcherProtectedUserEventCheckpoint(durable.runtime),
          );
        const before = await readCheckpoint();
        const casBefore = durable.casCount();
        const snapshotBefore = publisher.read().snapshot;
        const retirement = await fixture.makeBlock({
          parent: admission,
          transactions: [lifecycle.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        const retired = await fixture.openFinalizedBlock(retirement);
        try {
          if (offset < 0n) {
            await expect(publisher.publish(retired)).rejects.toThrow(
              "whole-block event semantics differ",
            );
            const after = await readCheckpoint();
            expect(after.checkpoint).toEqual(before.checkpoint);
            expect(after.trustedHead).toEqual(before.trustedHead);
            expectSameArchiveBytes(after.payload, before.payload!);
            expect(durable.casCount()).toBe(casBefore);
            expect(publisher.read().snapshot).toEqual(snapshotBefore);
            expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
            expect(publisher.read().snapshot.terminalEvents).toHaveLength(0);
          } else {
            await publisher.publish(retired);
            expect(publisher.read().snapshot.activeEvents).toHaveLength(0);
            const terminal = publisher.read().snapshot.terminalEvents;
            expect(terminal).toHaveLength(1);
            expect(terminal[0]).toMatchObject({
              eventId: original.eventId,
              eventCborHex: original.eventCborHex,
              historyPayloadCborHex: original.historyPayloadCborHex,
              inclusionTime: original.inclusionTime,
              originPointDigest: original.originPointDigest,
              terminalStatus: kind === "deposit" ? "absorbed" : "refunded",
              terminalFinalityStatus: "final",
            });
            const after = await readCheckpoint();
            expect(after.checkpoint?.checkpointSequence).toBe(
              (BigInt(before.checkpoint!.checkpointSequence) + 1n).toString(),
            );
            expect(after.checkpoint?.rollbackGeneration).toBe(
              before.checkpoint?.rollbackGeneration,
            );
            expect(durable.casCount()).toBe(casBefore + 1);
          }
        } finally {
          await retired.close();
        }
      } finally {
        publisher?.close();
        await fixture.close();
      }
    },
    120_000,
  );
});

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
            h32("ff"),
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

describe("native withdrawal payout retirement", () => {
  it("rejects Spend and other Withdraw pointers without CAS, then publishes payout initialization and restores it", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    const pairs: Awaited<ReturnType<typeof fixture.openFinalizedBlock>>[] = [];
    let publisher:
      | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
      | undefined;
    try {
      const initial = await openOrigin(fixture);
      pairs.push(initial.pair);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(initial.pair.finality).policy,
      );
      publisher = await createWatcherLocalUserEventPublisher({
        ...initial.input,
        origin: initial.origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(initial.pair);
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      pairs.push(empty);
      await publisher.publish(empty);
      const options = { kind: "withdrawal" as const, withdrawalPayout: true };
      const lifecycle = historyLifecycle(initial.facts, false, options);
      const admission = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create],
        creatingBodies: [fixture.initializationBodyCbor],
      });
      const admitted = await fixture.openFinalizedBlock(admission);
      pairs.push(admitted);
      await publisher.publish(admitted);
      const savedAdmission = publisher.read();
      expect(savedAdmission.snapshot.activeEvents).toHaveLength(1);
      const authorityPair = await fixture.openFinalizedBlock(admission);
      pairs.push(authorityPair);
      const admittedAuthority = await publisher.eventAuthority({
        ...authorityPair,
        kind: "withdrawal",
        eventId: lifecycle.expectedEventId,
      });
      const checkpointBefore = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durable.runtime),
      );
      const casBefore = durable.casCount();
      const consumeBody = CML.Transaction.from_cbor_hex(
        lifecycle.consume,
      ).body();
      const rewards = consumeBody.withdrawals()!.keys();
      const listIndex = Array.from(
        { length: rewards.len() },
        (_, index) => index,
      ).find(
        (index) =>
          rewards.get(index).payment().as_script()?.to_hex() ===
          initial.facts.scripts.withdrawal.policyId,
      );
      if (listIndex === undefined) throw new Error("Missing list observer");
      // Both tempting substitutions are wrong: a Spend pointer and the list's
      // own zero Withdraw pointer. Only the exact retirement Withdraw authorizes payout.
      const wrongIndexes = [
        0n,
        BigInt(2 + consumeBody.mint()!.keys().len() + listIndex),
      ];
      for (const [index, wrong] of wrongIndexes.entries()) {
        const invalid = historyLifecycle(initial.facts, false, {
          ...options,
          payoutRetirementRedeemerIndex: wrong,
        });
        const tx = CML.Transaction.from_cbor_hex(invalid.consume);
        const invalidBody = tx.body();
        // Give these deliberately non-ledger-valid native candidates distinct
        // body identities as well as distinct redeemer bytes.
        invalidBody.set_script_data_hash(
          CML.ScriptDataHash.from_hex(h32(index === 0 ? "c8" : "c9")),
        );
        const bad = await fixture.makeBlock({
          parent: admission,
          transactions: [
            CML.Transaction.new(
              invalidBody,
              tx.witness_set(),
              true,
            ).to_canonical_cbor_hex(),
          ],
          creatingBodies: [
            fixture.initializationBodyCbor,
            invalid.settlementBody,
          ],
        });
        const badPair = await fixture.openFinalizedBlock(bad);
        pairs.push(badPair);
        await expect(publisher.publish(badPair)).rejects.toThrow(
          "whole-block event semantics differ",
        );
        const after = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        );
        expect(after.checkpoint).toEqual(checkpointBefore.checkpoint);
        expect(after.trustedHead).toEqual(checkpointBefore.trustedHead);
        expectSameArchiveBytes(after.payload, checkpointBefore.payload!);
        expect(durable.casCount()).toBe(casBefore);
        expect(publisher.read().snapshot).toEqual(savedAdmission.snapshot);
        expect(publisher.read().cursor).toEqual(admission.point);
        expect(
          (await readWatcherLocalUserEventAuthority(admittedAuthority)).event,
        ).toEqual(savedAdmission.snapshot.activeEvents[0]);
        await fixture.selectCanonicalBranch(admission.point);
      }
      const retirement = await fixture.makeBlock({
        parent: admission,
        transactions: [lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const retired = await fixture.openFinalizedBlock(retirement);
      pairs.push(retired);
      await publisher.publish(retired);
      const saved = publisher.read();
      expect(saved.snapshot.activeEvents).toHaveLength(0);
      expect(saved.snapshot.terminalEvents).toHaveLength(1);
      const original = savedAdmission.snapshot.activeEvents[0]!;
      expect(saved.snapshot.terminalEvents[0]).toMatchObject({
        kind: "withdrawal",
        eventId: original.eventId,
        eventCborHex: original.eventCborHex,
        historyPayloadCborHex: original.historyPayloadCborHex,
        inclusionTime: original.inclusionTime,
        originPointDigest: original.originPointDigest,
        terminalStatus: "payout_initialized",
        terminalFinalityStatus: "final",
      });
      expect(saved.checkpoint?.checkpointSequence).toBe(
        (
          BigInt(checkpointBefore.checkpoint!.checkpointSequence) + 1n
        ).toString(),
      );
      expect(saved.checkpoint?.rollbackGeneration).toBe(
        checkpointBefore.checkpoint?.rollbackGeneration,
      );
      expect(durable.casCount()).toBe(casBefore + 1);
      expect(() =>
        assertWatcherLocalUserEventAuthorityCurrent(admittedAuthority),
      ).toThrow();
      const terminalPair = await fixture.openFinalizedBlock(retirement);
      pairs.push(terminalPair);
      const terminalAuthority = await publisher.eventAuthority({
        ...terminalPair,
        kind: "withdrawal",
        eventId: lifecycle.expectedEventId,
      });
      expect(
        (await readWatcherLocalUserEventAuthority(terminalAuthority)).event,
      ).toEqual(saved.snapshot.terminalEvents[0]);
      publisher.close();
      publisher = undefined;
      expect(() =>
        assertWatcherLocalUserEventAuthorityCurrent(terminalAuthority),
      ).toThrow();
      for (const pair of pairs) await pair.close();
      pairs.length = 0;
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const fresh = await openOrigin(fixture);
      pairs.push(fresh.pair);
      publisher = await resumeWatcherLocalUserEventPublisher({
        ...fresh.input,
        origin: fresh.origin,
        referenceAuthority: fresh.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        readHead: (point) => {
          expect(point).toEqual(retirement.point);
          return fixture.openFinalizedBlock(retirement);
        },
      });
      expect(publisher.read().checkpoint).toEqual(saved.checkpoint);
      expect(publisher.read().snapshot).toEqual(saved.snapshot);
      expect(publisher.read().cursor).toEqual(retirement.point);
      expect(durable.casCount()).toBe(casBefore + 1);
      const renewedPair = await fixture.openFinalizedBlock(retirement);
      pairs.push(renewedPair);
      const renewed = await publisher.eventAuthority({
        ...renewedPair,
        kind: "withdrawal",
        eventId: lifecycle.expectedEventId,
      });
      expect((await readWatcherLocalUserEventAuthority(renewed)).event).toEqual(
        saved.snapshot.terminalEvents[0],
      );
    } finally {
      publisher?.close();
      await Promise.allSettled(pairs.map((pair) => pair.close()));
      await fixture.close();
    }
  }, 120_000);
});

describe("retired history IDs across durable restart", () => {
  it.each(["deposit", "withdrawal"] as const)(
    "refuses a distinct native %s re-admission of a retired ID after reopening",
    async (kind) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      const pairs: Awaited<ReturnType<typeof fixture.openFinalizedBlock>>[] =
        [];
      let publisher:
        | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
        | undefined;
      try {
        const initial = await openOrigin(fixture);
        pairs.push(initial.pair);
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(initial.pair.finality).policy,
        );
        publisher = await createWatcherLocalUserEventPublisher({
          ...initial.input,
          origin: initial.origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        await publisher.publish(initial.pair);
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        pairs.push(empty);
        await publisher.publish(empty);
        const lifecycle = historyLifecycle(initial.facts, false, { kind });
        const admission = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        const admitted = await fixture.openFinalizedBlock(admission);
        pairs.push(admitted);
        await publisher.publish(admitted);
        expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
        const retirement = await fixture.makeBlock({
          parent: admission,
          transactions: [lifecycle.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        const retired = await fixture.openFinalizedBlock(retirement);
        pairs.push(retired);
        await publisher.publish(retired);
        const saved = publisher.read();
        expect(saved.snapshot.activeEvents).toHaveLength(0);
        expect(saved.snapshot.terminalEvents).toHaveLength(1);
        expect(saved.snapshot.terminalEvents[0]).toMatchObject({
          eventId: lifecycle.expectedEventId,
          terminalStatus: kind === "deposit" ? "absorbed" : "refunded",
          terminalFinalityStatus: "final",
        });
        publisher.close();
        publisher = undefined;
        for (const pair of pairs) await pair.close();
        pairs.length = 0;
        let runtime = await createWatcherDurableRuntime(durable.runtimeInput);
        const reopen = async () => {
          const fresh = await openOrigin(fixture);
          pairs.push(fresh.pair);
          return resumeWatcherLocalUserEventPublisher({
            ...fresh.input,
            origin: fresh.origin,
            referenceAuthority: fresh.pair.referenceAuthority,
            runtime,
            archive: durable.archive,
            readHead: async (point) => {
              expect(point).toEqual(retirement.point);
              return fixture.openFinalizedBlock(retirement);
            },
          });
        };
        const casBefore = durable.casCount();
        publisher = await reopen();
        expect(publisher.read().checkpoint).toEqual(saved.checkpoint);
        expect(publisher.read().snapshot).toEqual(saved.snapshot);
        const terminalPair = await fixture.openFinalizedBlock(retirement);
        pairs.push(terminalPair);
        const terminalAuthority = await publisher.eventAuthority({
          ...terminalPair,
          kind,
          eventId: lifecycle.expectedEventId,
        });
        expect(
          (await readWatcherLocalUserEventAuthority(terminalAuthority)).event,
        ).toEqual(saved.snapshot.terminalEvents[0]);
        const before = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(runtime),
        );

        // A different transaction/outref with the exact retired ID exercises
        // the ID guard, not a repeated transaction-hash check. This synthetic
        // native frame does not claim that Cardano permits nonce re-spending.
        const original = CML.Transaction.from_cbor_hex(lifecycle.create);
        const reusedBody = original.body();
        reusedBody.set_validity_interval_start(0n);
        const reused = CML.Transaction.new(
          reusedBody,
          original.witness_set(),
          true,
        ).to_canonical_cbor_hex();
        const reusedHash = CML.hash_transaction(reusedBody).to_hex();
        expect(reusedHash).not.toBe(lifecycle.createId);
        expect(reusedBody.outputs().get(0).to_cbor_hex()).toBe(
          original.body().outputs().get(0).to_cbor_hex(),
        );
        const duplicate = await fixture.makeBlock({
          parent: retirement,
          transactions: [reused],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        const duplicatePair = await fixture.openFinalizedBlock(duplicate);
        pairs.push(duplicatePair);
        await expect(publisher.publish(duplicatePair)).rejects.toThrow(
          "whole-block event semantics differ",
        );
        const after = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(runtime),
        );
        expect(after.checkpoint).toEqual(before.checkpoint);
        expect(after.trustedHead).toEqual(before.trustedHead);
        expectSameArchiveBytes(after.payload, before.payload!);
        expect(durable.casCount()).toBe(casBefore);
        expect(publisher.read().snapshot).toEqual(saved.snapshot);
        expect(publisher.read().cursor).toEqual(retirement.point);
        expect(
          (await readWatcherLocalUserEventAuthority(terminalAuthority)).event,
        ).toEqual(saved.snapshot.terminalEvents[0]);

        publisher.close();
        publisher = undefined;
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(terminalAuthority),
        ).toThrow();
        for (const pair of pairs) await pair.close();
        pairs.length = 0;
        runtime = await createWatcherDurableRuntime(durable.runtimeInput);
        publisher = await reopen();
        expect(publisher.read().checkpoint).toEqual(saved.checkpoint);
        expect(publisher.read().snapshot).toEqual(saved.snapshot);
        expect(publisher.read().cursor).toEqual(retirement.point);
        expect(durable.casCount()).toBe(casBefore);
      } finally {
        publisher?.close();
        await Promise.allSettled(pairs.map((pair) => pair.close()));
        await fixture.close();
      }
    },
    120_000,
  );
});

describe("durable local user-event restart", () => {
  it("restores validated events using only the current head, preserves progress, and continues incrementally", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const initial = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(initial.pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...initial.input,
        origin: initial.origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(initial.pair);
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      await empty.close();
      const lifecycle = historyLifecycle(initial.facts);
      const block = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const pair = await fixture.openFinalizedBlock(block);
      await publisher.publish(pair);
      const original = publisher.read();
      publisher.close();
      await initial.pair.close();
      await pair.close();
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const fresh = await openOrigin(fixture);
      const requests: string[] = [];
      const reopened = await resumeWatcherLocalUserEventPublisher({
        ...fresh.input,
        origin: fresh.origin,
        referenceAuthority: fresh.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        readHead: async (point) => {
          requests.push(point.blockHash);
          expect(point).toEqual(block.point);
          return fixture.openFinalizedBlock(block);
        },
      });
      expect(requests).toEqual([block.point.blockHash]);
      expect(reopened.read().checkpoint).toEqual(original.checkpoint);
      expect(reopened.read().snapshot).toEqual(original.snapshot);
      expect(reopened.read().store).toEqual(original.store);
      const head = await fixture.openFinalizedBlock(block);
      const authority = await reopened.eventAuthority({
        ...head,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(authority)).checkpointDigest,
      ).toBe(original.checkpoint!.checkpointDigest);
      await head.close();
      const nextBlock = await fixture.makeBlock({
        parent: block,
        transactions: [],
      });
      const nextPair = await fixture.openFinalizedBlock(nextBlock);
      await reopened.publish(nextPair);
      expect(reopened.read().cursor).toEqual(nextBlock.point);
      expect(reopened.read().checkpoint!.checkpointSequence).toBe(
        (BigInt(original.checkpoint!.checkpointSequence) + 1n).toString(),
      );
      reopened.close();
      await nextPair.close();
      // A valid pair for another block cannot corroborate the protected head.
      await expect(
        resumeWatcherLocalUserEventPublisher({
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime,
          archive: durable.archive,
          readHead: async () => fixture.openFinalizedBlock(block),
        }),
      ).rejects.toThrow("saved head is no longer canonical");
      const current = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(runtime),
      );
      const checkpoint = current.checkpoint!;
      const nextCheckpoint = makeWatcherUserEventCheckpoint({
        ...checkpoint,
        checkpointSequence: (
          BigInt(checkpoint.checkpointSequence) + 1n
        ).toString(),
        predecessorCheckpointDigest: checkpoint.checkpointDigest,
      });
      // A caller-created object cannot stamp semantic validation.
      await expect(
        persistWatcherUserEventCheckpoint(runtime, {
          expectedCheckpointDigest: checkpoint.checkpointDigest,
          expectedCheckpointSequence: checkpoint.checkpointSequence,
          nextCheckpoint,
          validationCandidate: {},
        }),
      ).rejects.toThrow();
      await persistWatcherUserEventCheckpoint(runtime, {
        expectedCheckpointDigest: checkpoint.checkpointDigest,
        expectedCheckpointSequence: checkpoint.checkpointSequence,
        nextCheckpoint,
      });
      await expect(
        resumeWatcherLocalUserEventPublisher({
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime,
          archive: durable.archive,
          readHead: async () => {
            throw new Error("must refuse before native reads");
          },
        }),
      ).rejects.toThrow("restart requires durable semantic validation");
      await fresh.pair.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});

describe("explicit local user-event semantic recovery (synthetic local blocks)", () => {
  it("replays actual archived history across repeated durable reopen and refuses protected semantic corruption", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
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
      const emptyPair = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(emptyPair);
      const lifecycle = historyLifecycle(facts);
      const eventBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const eventPair = await fixture.openFinalizedBlock(eventBlock);
      await publisher.publish(eventPair);
      const original = publisher.read();
      const originalObjects = new Map(
        [...durable.objects].map(([digest, bytes]) => [
          digest,
          Uint8Array.from(bytes),
        ]),
      );
      await pair.close();
      await eventPair.close();
      await emptyPair.close();
      publisher.close();
      const blocks = new Map([
        [
          fixture.emptySuccessorBlock.point.blockHash,
          fixture.emptySuccessorBlock,
        ],
        [eventBlock.point.blockHash, eventBlock],
      ]);
      const requests: string[] = [];
      const replayBlock = async (point: typeof eventBlock.point) => {
        const block = blocks.get(point.blockHash);
        if (
          block === undefined ||
          !watcherSameCanonicalJson(point, block.point)
        )
          throw new Error("unexpected replay point");
        requests.push(point.blockHash);
        return await fixture.openFinalizedBlock(block);
      };
      const reopen = async () => {
        const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
        const fresh = await openOrigin(fixture);
        try {
          const resumed = await recoverWatcherLocalUserEventPublisher({
            ...fresh.input,
            origin: fresh.origin,
            referenceAuthority: fresh.pair.referenceAuthority,
            runtime,
            archive: durable.archive,
            replayBlock,
          });
          return { resumed, runtime };
        } finally {
          await fresh.pair.close();
        }
      };
      const first = await reopen();
      expect(requests).toEqual([
        fixture.emptySuccessorBlock.point.blockHash,
        eventBlock.point.blockHash,
      ]);
      expect(first.resumed.read().checkpoint!.checkpointSequence).toBe(
        (BigInt(original.checkpoint!.checkpointSequence) + 1n).toString(),
      );
      expect(first.resumed.read().snapshot.terminalEvents[0]).toMatchObject({
        eventId: lifecycle.expectedEventId,
        terminalStatus: "absorbed",
        eventCborHex: original.snapshot.terminalEvents[0]!.eventCborHex,
      });
      expect(first.resumed.read().snapshot.snapshotDigest).not.toBe(
        original.snapshot.snapshotDigest,
      );
      const firstFresh = await fixture.openFinalizedBlock(eventBlock);
      const firstAuthority = await first.resumed.eventAuthority({
        ...firstFresh,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(firstAuthority))
          .checkpointDigest,
      ).toBe(first.resumed.read().checkpoint!.checkpointDigest);
      await firstFresh.close();
      first.resumed.close();
      // A second restart reads an explicit readmission payload, with the original closure intact.
      const second = await reopen();
      expect(requests).toEqual([
        fixture.emptySuccessorBlock.point.blockHash,
        eventBlock.point.blockHash,
        fixture.emptySuccessorBlock.point.blockHash,
        eventBlock.point.blockHash,
      ]);
      const successorBlock = await fixture.makeBlock({
        parent: eventBlock,
        transactions: [],
      });
      blocks.set(successorBlock.point.blockHash, successorBlock);
      const successor = await fixture.openFinalizedBlock(successorBlock);
      await second.resumed.publish(successor);
      expect(second.resumed.read().cursor).toEqual(successorBlock.point);
      await successor.close();
      second.resumed.close();
      // An ordinary successor payload must also remain semantically restartable.
      const third = await reopen();
      expect(requests.slice(-2)).toEqual([
        eventBlock.point.blockHash,
        successorBlock.point.blockHash,
      ]);
      expect(third.resumed.read().cursor).toEqual(successorBlock.point);
      for (const [digest, bytes] of originalObjects) {
        expectSameArchiveBytes(await durable.archive.read(digest), bytes);
        expect(
          third.resumed.read().checkpoint!.requiredArchiveDigests,
        ).toContain(digest);
      }
      const finalBlock = await fixture.makeBlock({
        parent: successorBlock,
        transactions: [],
      });
      blocks.set(finalBlock.point.blockHash, finalBlock);
      const finalPair = await fixture.openFinalizedBlock(finalBlock);
      await third.resumed.publish(finalPair);
      await finalPair.close();
      third.resumed.close();
      // The real lower structural publisher can protect bytes but cannot grant
      // event semantics. Rehash every affected final-entry binding and require
      // fresh whole-block replay to detect the changed inclusion time.
      const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(third.runtime),
      );
      const checkpoint = protectedHead.checkpoint!;
      const payload: {
        head: WatcherLocalUserEventEntry;
        retainedEntries: readonly WatcherLocalUserEventEntry[];
        snapshot: WatcherUserEventSnapshot;
      } = JSON.parse(Buffer.from(protectedHead.payload!).toString("utf8"));
      let priorObservation: WatcherUserEventObservation | undefined;
      for (const bytes of durable.objects.values()) {
        const entryArchive: {
          entry?: WatcherLocalUserEventEntry;
          observation?: WatcherUserEventObservation;
        } = JSON.parse(Buffer.from(bytes).toString("utf8"));
        if (entryArchive.entry?.entryDigest === payload.head.entryDigest)
          priorObservation = entryArchive.observation;
      }
      if (priorObservation === undefined)
        throw new Error("archived final observation is absent");
      const { snapshotDigest: _snapshotDigest, ...snapshotFields } =
        payload.snapshot;
      const terminal = snapshotFields.terminalEvents[0]!;
      const badSnapshotFields = {
        ...snapshotFields,
        terminalEvents: [
          {
            ...terminal,
            inclusionTime: (BigInt(terminal.inclusionTime) + 1n).toString(),
          },
        ],
      };
      const badSnapshot = {
        ...badSnapshotFields,
        snapshotDigest: watcherSha256CanonicalJson(badSnapshotFields),
      };
      const { observationDigest: _observationDigest, ...observationFields } =
        priorObservation;
      const badObservationFields = {
        ...observationFields,
        snapshot: badSnapshot,
      };
      const badObservation = {
        ...badObservationFields,
        observationDigest: watcherSha256CanonicalJson(badObservationFields),
      };
      const { entryDigest: _entryDigest, ...entryFields } = payload.head;
      const badEntryFields = {
        ...entryFields,
        snapshotDigest: badSnapshot.snapshotDigest,
        observationDigest: badObservation.observationDigest,
      };
      const badEntry = {
        ...badEntryFields,
        entryDigest: watcherSha256CanonicalJson(badEntryFields),
      };
      const badEntryDigest = await durable.archive.put(
        Buffer.from(
          watcherCanonicalJson({
            entry: badEntry,
            observation: badObservation,
          }),
          "utf8",
        ),
      );
      const badPayloadDigest = await durable.archive.put(
        Buffer.from(
          watcherCanonicalJson({
            ...payload,
            head: badEntry,
            snapshot: badSnapshot,
            retainedEntries: [
              ...payload.retainedEntries.slice(0, -1),
              badEntry,
            ],
          }),
          "utf8",
        ),
      );
      await persistWatcherUserEventCheckpoint(third.runtime, {
        expectedCheckpointDigest: checkpoint.checkpointDigest,
        expectedCheckpointSequence: checkpoint.checkpointSequence,
        nextCheckpoint: makeWatcherUserEventCheckpoint({
          ...checkpoint,
          checkpointSequence: (
            BigInt(checkpoint.checkpointSequence) + 1n
          ).toString(),
          predecessorCheckpointDigest: checkpoint.checkpointDigest,
          payloadDigest: badPayloadDigest,
          requiredArchiveDigests: [
            ...new Set([
              ...checkpoint.requiredArchiveDigests,
              badEntryDigest,
              badPayloadDigest,
            ]),
          ].sort(),
        }),
      });
      await expect(reopen()).rejects.toThrow(
        "fresh semantic replay differs from the archived event fold",
      );
      for (const [digest, bytes] of originalObjects)
        expectSameArchiveBytes(await durable.archive.read(digest), bytes);
    } finally {
      await fixture.close();
    }
  }, 120_000);
});

// Both fixture transports retain real private admission. Switching their test
// network boundaries only selects which synthetic chain answers a fresh query.
describe("local deposit transcript semantic renewal", () => {
  it("replays an ordinary deposit through fresh durable owner and actual queue admission", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    const eventNetwork = {
      fetch: globalThis.fetch,
      WebSocket: globalThis.WebSocket,
    };
    let queue:
      | Awaited<ReturnType<typeof createSyntheticStateQueueObservationFixture>>
      | undefined;
    let resumed:
      | Awaited<ReturnType<typeof recoverWatcherLocalUserEventPublisher>>
      | undefined;
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
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
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      const lifecycle = historyLifecycle(facts);
      const eventBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const eventPair = await fixture.openFinalizedBlock(eventBlock);
      await publisher.publish(eventPair);
      const firstFresh = await fixture.openFinalizedBlock(eventBlock);
      const firstAuthority = await publisher.eventAuthority({
        ...firstFresh,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      const before = await readWatcherLocalUserEventAuthority(firstAuthority);
      const replayInput = await makeLocalDepositReplayFixture(
        firstAuthority,
        fixture.deploymentIdentity.programCommitments,
      );
      queue = await createSyntheticStateQueueObservationFixture({
        header: replayInput.observation.header,
        ruleBundleCommitment: replayInput.ruleBundleCommitment,
      });
      const queueNetwork = {
        fetch: globalThis.fetch,
        WebSocket: globalThis.WebSocket,
      };
      const initialQueue = await queue.observeFresh();
      expect(queue.transport.deploymentIdentity.manifestId).toBe(
        fixture.deploymentIdentity.manifestId,
      );
      const transcriptInput = {
        deploymentIdentity: queue.transport.deploymentIdentity,
        stateQueueObservation: initialQueue.observation,
        header: initialQueue.header,
        payloadEnvelopeCbor: replayInput.payloadEnvelopeCbor,
        daProvenance: replayInput.daProvenance,
        priorState: replayInput.priorState,
        eventAuthorities: replayInput.eventAuthorities,
        ruleBundle: replayInput.ruleBundle,
        ruleBundleCommitment: replayInput.ruleBundleCommitment,
      };
      const original = await createWatcherAuthenticatedReplayTranscript({
        ...transcriptInput,
        coordinate: { domain: "transition_step", index: "0" },
      });
      assertWatcherAuthenticatedReplayTranscript(original);
      const persistedTranscriptCborHex =
        watcherAuthenticatedReplayTranscriptCborHex(original);
      await initialQueue.close();
      publisher.close();
      await pair.close();
      await empty.close();
      await eventPair.close();
      await firstFresh.close();

      vi.stubGlobal("fetch", eventNetwork.fetch);
      vi.stubGlobal("WebSocket", eventNetwork.WebSocket);
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const freshOrigin = await openOrigin(fixture);
      const blocks = [fixture.emptySuccessorBlock, eventBlock];
      resumed = await recoverWatcherLocalUserEventPublisher({
        ...freshOrigin.input,
        origin: freshOrigin.origin,
        referenceAuthority: freshOrigin.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = blocks.find((candidate) =>
            watcherSameCanonicalJson(candidate.point, point),
          );
          if (block === undefined)
            throw new Error("unexpected transcript replay point");
          return await fixture.openFinalizedBlock(block);
        },
      });
      const renewedPair = await fixture.openFinalizedBlock(eventBlock);
      const renewedAuthority = await resumed.eventAuthority({
        ...renewedPair,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      const after = await readWatcherLocalUserEventAuthority(renewedAuthority);
      expect(after.checkpointDigest).not.toBe(before.checkpointDigest);
      expect(after.snapshotDigest).not.toBe(before.snapshotDigest);
      expect(after.event).toMatchObject({
        eventCborHex: before.event.eventCborHex,
        outputCborHex: before.event.outputCborHex,
        originBlockHash: before.event.originBlockHash,
        originSlot: before.event.originSlot,
        originBlockNo: before.event.originBlockNo,
      });
      vi.stubGlobal("fetch", queueNetwork.fetch);
      vi.stubGlobal("WebSocket", queueNetwork.WebSocket);
      const renewedQueue = await queue.observeFresh();
      expect(renewedQueue.observation).not.toBe(initialQueue.observation);
      const authority = replayInput.eventAuthorities![0]!;
      if (authority.localUserEvent === undefined)
        throw new Error("local deposit transcript requires local authority");
      const renewedInput = {
        ...transcriptInput,
        stateQueueObservation: renewedQueue.observation,
        header: renewedQueue.header,
        daProvenance: {
          ...replayInput.daProvenance,
          sourceId: "fresh-permissionless-da-peer",
        },
        eventAuthorities: [{ ...authority, localUserEvent: renewedAuthority }],
        persistedTranscriptCborHex,
      };
      const renewed =
        await replayWatcherAuthenticatedReplayTranscript(renewedInput);
      assertWatcherAuthenticatedReplayTranscript(renewed);
      expect(renewed.transcriptDigest).not.toBe(original.transcriptDigest);
      expect(renewed.eventAuthorityRecordsCborHex).not.toEqual(
        original.eventAuthorityRecordsCborHex,
      );
      expect(renewed.blockReplayResultDigest).not.toBe(
        original.blockReplayResultDigest,
      );
      expect(renewed.payloadEnvelopeSha256).toBe(
        original.payloadEnvelopeSha256,
      );
      expect(renewed.coordinate).toEqual(original.coordinate);
      const originalRecords = await readWatcherReplayTranscriptRecords(
        persistedTranscriptCborHex,
        30,
      );
      const renewedRecords = await readWatcherReplayTranscriptRecords(
        watcherAuthenticatedReplayTranscriptCborHex(renewed),
        30,
      );
      expect(renewedRecords.blockReplay.priorStateRoot).toBe(
        originalRecords.blockReplay.priorStateRoot,
      );
      expect(renewedRecords.blockReplay.postStateRoot).toBe(
        originalRecords.blockReplay.postStateRoot,
      );
      expect(renewedRecords.blockReplay.action).toBe("accept");
      expect(renewedRecords.blockReplay.eventRoots).toMatchObject([
        { stepIndex: 0, phase: "Deposit", mutationCount: 1 },
      ]);
      expect(watcherAuthenticatedReplayTranscriptCborHex(original)).toBe(
        persistedTranscriptCborHex,
      );
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...renewedInput,
          eventAuthorities: replayInput.eventAuthorities,
        }),
      ).rejects.toThrow();
      resumed.close();
      await renewedPair.close();
      await freshOrigin.pair.close();
    } finally {
      resumed?.close();
      await queue?.close();
      await fixture.close();
    }
  }, 120_000);
});

describe("local user-event materialized history (synthetic local blocks)", () => {
  it("rotates protected anchors beyond 128 blocks, retains old event provenance and cold replays each indexed segment once", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      let runtime = durable.runtime;
      let publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime,
        archive: durable.archive,
      });
      const blocks = [fixture.activationBlock, fixture.emptySuccessorBlock];
      await publisher.publish(pair);
      await pair.close();
      const emptyPair = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(emptyPair);
      await emptyPair.close();
      const lifecycle = historyLifecycle(facts);
      const eventBlock = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [lifecycle.create, lifecycle.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      blocks.push(eventBlock);
      let head = eventBlock;
      while (blocks.length < 128) {
        head = await fixture.makeBlock({ parent: head, transactions: [] });
        blocks.push(head);
      }
      for (const block of blocks.slice(2)) {
        const finalized = await fixture.openFinalizedBlock(block);
        try {
          await publisher.publish(finalized);
        } finally {
          await finalized.close();
        }
      }
      const before = publisher.read();
      expect(before.retainedEntries).toBe(128);
      expect(before.store.l1Observations).toHaveLength(128);
      const originalObjects = new Map(
        [...durable.objects].map(([digest, bytes]) => [
          digest,
          Uint8Array.from(bytes),
        ]),
      );
      const successorBlock = await fixture.makeBlock({
        parent: head,
        transactions: [],
      });
      blocks.push(successorBlock);
      let successor = await fixture.openFinalizedBlock(successorBlock);
      await expect(publisher.publish(successor)).rejects.toThrow(
        "semantic anchor rotation required",
      );
      const anchorPair = await fixture.openFinalizedBlock(head);
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
      const rotating = publisher.rotate(anchorPair);
      await entered;
      expect(publisher.read()).toMatchObject({
        status: "publication_pending",
        retainedEntries: 128,
        anchorDue: true,
        store: { revision: before.store.revision },
      });
      await expect(publisher.publish(successor)).rejects.toThrow(
        "already in flight",
      );
      durable.interruptNextReadBack();
      releaseWrite();
      await expect(rotating).rejects.toThrow();
      expect(watcherCanonicalJson(publisher.read().store)).toBe(
        watcherCanonicalJson(before.store),
      );
      await expect(publisher.rotate(anchorPair)).rejects.toThrow(
        "trusted-head read-back differs",
      );
      publisher.close();
      await anchorPair.close();
      await successor.close();
      runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const recoveryOrigin = await openOrigin(fixture);
      const recoveryRequests: string[] = [];
      const recoveryBlocks = new Map(
        blocks.map((block) => [block.point.blockHash, block]),
      );
      publisher = await recoverWatcherLocalUserEventPublisher({
        ...recoveryOrigin.input,
        origin: recoveryOrigin.origin,
        referenceAuthority: recoveryOrigin.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = recoveryBlocks.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected recovery replay point");
          recoveryRequests.push(point.blockHash);
          return await fixture.openFinalizedBlock(block);
        },
      });
      await recoveryOrigin.pair.close();
      expect(recoveryRequests).toEqual(
        blocks.slice(1, 128).map((block) => block.point.blockHash),
      );
      const anchored = publisher.read();
      expect(anchored.retainedEntries).toBe(64);
      expect(anchored.anchorDue).toBe(false);
      expect(anchored.retainedArchive.objects).toBe(
        anchored.checkpoint!.requiredArchiveDigests.length,
      );
      expect(anchored.retainedArchive.bytes).toBeGreaterThan(0);
      expect(anchored.retainedArchive.nodes).toBeGreaterThan(0);
      expect(anchored.store.l1Observations).toHaveLength(65);
      expect(anchored.snapshot.terminalEvents[0]).toMatchObject({
        eventId: lifecycle.expectedEventId,
        terminalStatus: "absorbed",
      });
      expect(anchored.snapshot.snapshotDigest).not.toBe(
        before.snapshot.snapshotDigest,
      );
      expect(anchored.store.revision).toBe(
        (BigInt(before.store.revision) + 1n).toString(),
      );
      expect(anchored.checkpoint!.requiredArchiveDigests.length).toBeLessThan(
        before.checkpoint!.requiredArchiveDigests.length,
      );
      await anchorPair.close();
      const postAnchor = await fixture.openFinalizedBlock(head);
      const authority = await publisher.eventAuthority({
        ...postAnchor,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(authority)).event,
      ).toEqual(anchored.snapshot.terminalEvents[0]);
      await postAnchor.close();
      successor = await fixture.openFinalizedBlock(successorBlock);
      await publisher.publish(successor);
      await successor.close();
      head = successorBlock;
      // Five real protected rotations exercise immediate and power-of-two ancestor links.
      for (let index = 0; index < 4; index += 1) {
        const current = await fixture.openFinalizedBlock(head);
        try {
          await publisher.rotate(current);
        } finally {
          await current.close();
        }
        if (index < 3) {
          head = await fixture.makeBlock({ parent: head, transactions: [] });
          blocks.push(head);
          const finalized = await fixture.openFinalizedBlock(head);
          try {
            await publisher.publish(finalized);
          } finally {
            await finalized.close();
          }
        }
      }
      const final = publisher.read();
      expect(final.retainedEntries).toBe(64);
      expect(final.store.l1Observations).toHaveLength(65);
      const savedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(runtime),
      );
      const savedPayload: {
        anchor: { indexDigest: string; indexSequence: string };
      } = JSON.parse(Buffer.from(savedHead.payload!).toString("utf8"));
      expect(savedPayload.anchor.indexSequence).toBe("4");
      const archivedIndex: {
        ancestorDigests: readonly string[];
        materializedStoreDigest: string;
      } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(savedPayload.anchor.indexDigest))!,
        ).toString("utf8"),
      );
      expect(archivedIndex.ancestorDigests).toHaveLength(3);
      publisher.close();
      const firstIndex: { sourcePayloadDigest: string } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(archivedIndex.ancestorDigests[2]!))!,
        ).toString("utf8"),
      );
      for (const digest of [
        savedHead.checkpoint!.payloadDigest,
        archivedIndex.materializedStoreDigest,
        firstIndex.sourcePayloadDigest,
      ]) {
        const originalBytes = durable.objects.get(digest)!;
        expect(originalBytes).toBeDefined();
        const missingOrigin = await openOrigin(fixture);
        durable.objects.delete(digest);
        try {
          await expect(
            recoverWatcherLocalUserEventPublisher({
              ...missingOrigin.input,
              origin: missingOrigin.origin,
              referenceAuthority: missingOrigin.pair.referenceAuthority,
              runtime,
              archive: durable.archive,
              replayBlock: async () => {
                throw new Error(
                  "missing archive dependency must fail before replay",
                );
              },
            }),
          ).rejects.toThrow(/absent|missing/);
        } finally {
          durable.objects.set(digest, originalBytes);
          await missingOrigin.pair.close();
        }
      }
      runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const fresh = await openOrigin(fixture);
      const requested: string[] = [];
      const byHash = new Map(
        blocks.map((block) => [block.point.blockHash, block]),
      );
      const resumed = await recoverWatcherLocalUserEventPublisher({
        ...fresh.input,
        origin: fresh.origin,
        referenceAuthority: fresh.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = byHash.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected indexed replay point");
          requested.push(point.blockHash);
          return await fixture.openFinalizedBlock(block);
        },
      });
      await fresh.pair.close();
      expect(requested).toEqual(
        blocks.slice(1).map((block) => block.point.blockHash),
      );
      expect(resumed.read()).toMatchObject({
        cursor: head.point,
        retainedEntries: 64,
        store: { revision: final.store.revision },
      });
      expect(resumed.read().store.l1Observations).toHaveLength(65);
      const freshHead = await fixture.openFinalizedBlock(head);
      const retained = await resumed.eventAuthority({
        ...freshHead,
        eventId: lifecycle.expectedEventId,
        kind: "deposit",
      });
      expect(
        (await readWatcherLocalUserEventAuthority(retained)).event,
      ).toMatchObject({
        eventId: lifecycle.expectedEventId,
        terminalStatus: "absorbed",
      });
      await freshHead.close();
      for (const [digest, bytes] of originalObjects)
        expectSameArchiveBytes(await durable.archive.read(digest), bytes);
      const nextBlock = await fixture.makeBlock({
        parent: head,
        transactions: [],
      });
      const next = await fixture.openFinalizedBlock(nextBlock);
      await resumed.publish(next);
      expect(resumed.read().cursor).toEqual(nextBlock.point);
      await next.close();
      resumed.close();
      byHash.set(nextBlock.point.blockHash, nextBlock);
      blocks.push(nextBlock);
      const secondRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const secondOrigin = await openOrigin(fixture);
      const secondRequests: string[] = [];
      const secondResumed = await resumeWatcherLocalUserEventPublisher({
        ...secondOrigin.input,
        origin: secondOrigin.origin,
        referenceAuthority: secondOrigin.pair.referenceAuthority,
        runtime: secondRuntime,
        archive: durable.archive,
        readHead: async (point) => {
          const block = byHash.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected second indexed replay point");
          secondRequests.push(point.blockHash);
          return await fixture.openFinalizedBlock(block);
        },
      });
      await secondOrigin.pair.close();
      expect(secondRequests).toEqual([nextBlock.point.blockHash]);
      expect(secondResumed.read().cursor).toEqual(nextBlock.point);
      expect(secondResumed.read().retainedEntries).toBe(65);
      secondResumed.close();
      runtime = secondRuntime;
      // A structurally protected but incorrect ancestry link grants no replay authority.
      const currentProtected = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(runtime),
      );
      const currentCheckpoint = currentProtected.checkpoint!;
      const currentPayload: {
        anchor: { indexDigest: string };
        [field: string]: unknown;
      } = JSON.parse(Buffer.from(currentProtected.payload!).toString("utf8"));
      const indexValue: {
        ancestorDigests: readonly string[];
        [field: string]: unknown;
      } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(currentPayload.anchor.indexDigest))!,
        ).toString("utf8"),
      );
      const badIndex = {
        ...indexValue,
        ancestorDigests: [
          indexValue.ancestorDigests[0]!,
          indexValue.ancestorDigests[1]!,
          indexValue.ancestorDigests[0]!,
        ],
      };
      const badIndexBytes = Buffer.from(watcherCanonicalJson(badIndex), "utf8");
      const badIndexDigest = await durable.archive.put(badIndexBytes);
      const badPayloadBytes = Buffer.from(
        watcherCanonicalJson({
          ...currentPayload,
          anchor: { ...currentPayload.anchor, indexDigest: badIndexDigest },
        }),
        "utf8",
      );
      const badPayloadDigest = await durable.archive.put(badPayloadBytes);
      const badCheckpoint = makeWatcherUserEventCheckpoint({
        ...currentCheckpoint,
        checkpointSequence: (
          BigInt(currentCheckpoint.checkpointSequence) + 1n
        ).toString(),
        predecessorCheckpointDigest: currentCheckpoint.checkpointDigest,
        payloadDigest: badPayloadDigest,
        requiredArchiveDigests: [
          ...new Set([
            ...currentCheckpoint.requiredArchiveDigests,
            badIndexDigest,
            badPayloadDigest,
          ]),
        ].sort(),
      });
      await persistWatcherUserEventCheckpoint(runtime, {
        expectedCheckpointDigest: currentCheckpoint.checkpointDigest,
        expectedCheckpointSequence: currentCheckpoint.checkpointSequence,
        nextCheckpoint: badCheckpoint,
      });
      const corruptRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const corruptOrigin = await openOrigin(fixture);
      await expect(
        recoverWatcherLocalUserEventPublisher({
          ...corruptOrigin.input,
          origin: corruptOrigin.origin,
          referenceAuthority: corruptOrigin.pair.referenceAuthority,
          runtime: corruptRuntime,
          archive: durable.archive,
          replayBlock: async () => {
            throw new Error("corrupt archive must fail before replay");
          },
        }),
      ).rejects.toThrow("ancestor sequence differs");
      await corruptOrigin.pair.close();
    } finally {
      await fixture.close();
    }
  }, 600_000);
});

describe("local event replay authority derivation", () => {
  it("derives ordinary deposit, withdrawal and forced inputs from actual capabilities and snapshots awaited sources", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    let publisher:
      | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
      | undefined;
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      const native = makeNativeTx();
      const deposit = historyLifecycle(facts);
      const withdrawal = ordinaryLocalOrderCreation(
        facts,
        "withdrawal",
        native.txCbor,
      );
      const forced = ordinaryLocalOrderCreation(
        facts,
        "forced_order",
        native.txCbor,
      );
      const block = await fixture.makeBlock({
        parent: fixture.emptySuccessorBlock,
        transactions: [deposit.create, withdrawal.cbor, forced.cbor],
        creatingBodies: [fixture.initializationBodyCbor],
      });
      const published = await fixture.openFinalizedBlock(block);
      await publisher.publish(published);
      const fresh = await fixture.openFinalizedBlock(block);
      const depositCap = await publisher.eventAuthority({
        ...fresh,
        kind: "deposit",
        eventId: deposit.expectedEventId,
      });
      const withdrawalCap = await publisher.eventAuthority({
        ...fresh,
        kind: "withdrawal",
        eventId: withdrawal.eventIdCborHex,
      });
      const forcedCap = await publisher.eventAuthority({
        ...fresh,
        kind: "forced_order",
        eventId: forced.eventIdCborHex,
      });
      const depositEvent = (
        await readWatcherLocalUserEventAuthority(depositCap)
      ).event;
      const depositOrigin = Data.from(depositEvent.eventCborHex, DepositEvent);
      const depositClaim: WatcherCommittedEventClaim = {
        phase: "Deposit",
        eventIdCborHex: depositEvent.eventId,
        valueCborHex: Data.to(depositOrigin.info, DepositInfo),
        canonicalNativeTxCborHex: null,
      };
      const mutableDepositClaim = { ...depositClaim };
      const pendingDeposit = deriveWatcherLocalEventReplayAuthority({
        localUserEvent: depositCap,
        committedClaim: mutableDepositClaim,
        programMaterial: [],
      });
      mutableDepositClaim.eventIdCborHex = "00";
      mutableDepositClaim.valueCborHex = "00";
      const depositAuthority = await pendingDeposit;
      const replay = await makeLocalDepositReplayFixture(
        depositCap,
        fixture.deploymentIdentity.programCommitments,
      );
      expect(
        await evaluateWatcherBlockReplay({
          ...replay,
          eventAuthorities: [depositAuthority],
        }),
      ).toMatchObject({
        action: "accept",
        reasonCodes: [],
        eventRoots: [{ phase: "Deposit", mutationCount: 1 }],
      });
      const withdrawalEvent = (
        await readWatcherLocalUserEventAuthority(withdrawalCap)
      ).event;
      const withdrawalOrigin = Data.from(
        withdrawalEvent.eventCborHex,
        WithdrawalEvent,
      );
      for (const validity of [
        "WithdrawalIsValid",
        "NonExistentWithdrawalUtxo",
      ] as const) {
        const claim: WatcherCommittedEventClaim = {
          phase: "Withdrawal",
          eventIdCborHex: withdrawalEvent.eventId,
          valueCborHex: Data.to(
            { ...withdrawalOrigin.info, validity },
            WithdrawalInfo,
          ),
          canonicalNativeTxCborHex: null,
        };
        const authority = await deriveWatcherLocalEventReplayAuthority({
          localUserEvent: withdrawalCap,
          committedClaim: claim,
          programMaterial: [],
        });
        if (authority.phase !== "Withdrawal")
          throw new Error("withdrawal authority has another phase");
        expect(authority.transitionEffect.operations).toEqual(
          validity === "WithdrawalIsValid"
            ? [
                {
                  type: "delete",
                  outRefCbor: encodeMidgardSpendInputItem({
                    txId: Buffer.from(withdrawal.eventId.transactionId, "hex"),
                    outputIndex: 0,
                  }),
                },
              ]
            : [],
        );
      }
      const submittedCbor = encodeMidgardForcedTxCanonical(native.tx);
      const forcedClaim: WatcherCommittedEventClaim = {
        phase: "ForcedTransaction",
        eventIdCborHex: forced.eventIdCborHex,
        valueCborHex: Data.to(
          {
            tx_id: forced.payload.tx_id,
            submitted_source: forced.payload.submitted_source,
            verdict: "ForcedTxValid",
          },
          ForcedInclusionTxV1,
        ),
        canonicalNativeTxCborHex: submittedCbor.toString("hex"),
      };
      const mutableForcedClaim = { ...forcedClaim };
      const material: [string, string][] = [];
      const pendingForced = deriveWatcherLocalEventReplayAuthority({
        localUserEvent: forcedCap,
        committedClaim: mutableForcedClaim,
        programMaterial: material,
      });
      mutableForcedClaim.canonicalNativeTxCborHex = "00";
      mutableForcedClaim.valueCborHex = "00";
      material.push(["00", "00"]);
      const forcedAuthority = await pendingForced;
      expect(forcedAuthority).toMatchObject({
        phase: "ForcedTransaction",
        canonicalNativeTxCbor: submittedCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
      });
      expect("transitionEffect" in forcedAuthority).toBe(false);
      for (const claim of [
        { ...forcedClaim, canonicalNativeTxCborHex: "00" },
        { ...forcedClaim, eventIdCborHex: depositClaim.eventIdCborHex },
      ]) {
        await expect(
          deriveWatcherLocalEventReplayAuthority({
            localUserEvent: forcedCap,
            committedClaim: claim,
            programMaterial: [],
          }),
        ).rejects.toThrow();
      }
      await expect(
        deriveWatcherLocalEventReplayAuthority({
          localUserEvent: { ...depositCap },
          committedClaim: depositClaim,
          programMaterial: [],
        }),
      ).rejects.toThrow("not privately admitted");
      const inFlight = deriveWatcherLocalEventReplayAuthority({
        localUserEvent: depositCap,
        committedClaim: depositClaim,
        programMaterial: [],
      });
      publisher.close();
      await expect(inFlight).rejects.toThrow("closed");
      await expect(
        deriveWatcherLocalEventReplayAuthority({
          localUserEvent: forcedCap,
          committedClaim: forcedClaim,
          programMaterial: [],
        }),
      ).rejects.toThrow("closed");
    } finally {
      publisher?.close();
      await fixture.close();
    }
  }, 120_000);
});

describe("local user-event challenged-header cutoff (synthetic local blocks)", () => {
  it.each(
    (["deposit", "withdrawal"] as const).flatMap((kind) =>
      (["before_creation", "before_pointer", "later_pointer"] as const).map(
        (order) => ({ kind, order }),
      ),
    ),
  )(
    "keeps immutable $kind admission at header cutoff after pointer movement: $order",
    async ({ kind, order }) => {
      const state: {
        lifecycle: ReturnType<typeof historyLifecycle> | null;
        pointer: string | null;
      } = { lifecycle: null, pointer: null };
      const fixture = await createSyntheticStateQueueObservationFixture({
        composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
          const origin = await openOrigin(transport);
          state.lifecycle = historyLifecycle(origin.facts, false, { kind });
          state.pointer = historyPointerContinuation(
            origin.facts,
            state.lifecycle,
            kind,
          );
          await origin.pair.close();
          const create = state.lifecycle.create;
          return {
            transactions:
              order === "before_creation"
                ? [commitTransactionCbor, create, state.pointer]
                : order === "before_pointer"
                  ? [create, commitTransactionCbor, state.pointer]
                  : [create, commitTransactionCbor],
            creatingBodies: [transport.initializationBodyCbor],
          };
        },
      });
      try {
        const lifecycle = state.lifecycle!;
        const pointer = state.pointer!;
        const head =
          order === "later_pointer"
            ? await fixture.transport.makeBlock({
                parent: fixture.commitBlock,
                transactions: [pointer],
                creatingBodies: [fixture.transport.initializationBodyCbor],
              })
            : fixture.commitBlock;
        const { pair, input, origin } = await openOrigin(fixture.transport);
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
        await pair.close();
        for (const block of [
          fixture.transport.emptySuccessorBlock,
          fixture.initializationBlock,
          fixture.commitBlock,
          ...(head === fixture.commitBlock ? [] : [head]),
        ]) {
          const finalized = await fixture.transport.openFinalizedBlock(block);
          try {
            await publisher.publish(finalized);
          } finally {
            await finalized.close();
          }
        }
        const pointerId = CML.hash_transaction(
          CML.Transaction.from_cbor_hex(pointer).body(),
        ).to_hex();
        expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
        expect(publisher.read().snapshot.activeEvents[0]).toMatchObject({
          kind,
          eventId: lifecycle.expectedEventId,
          transactionHash: pointerId,
          outRef: `${pointerId}#0`,
          originBlockHash: fixture.commitBlock.point.blockHash,
        });
        const captured = await fixture.observeFresh();
        const fresh = await fixture.transport.openFinalizedBlock(head);
        try {
          const request = {
            ...fresh,
            kind,
            eventId: lifecycle.expectedEventId,
            throughHeader: captured.header,
          };
          if (order === "before_creation") {
            await expect(publisher.eventAuthority(request)).rejects.toThrow(
              "origin occurs after the challenged header",
            );
          } else {
            const receipt = await publisher.eventAuthority(request);
            const scoped = await readWatcherLocalUserEventAuthority(receipt);
            expect(scoped.event).toMatchObject({
              kind,
              eventId: lifecycle.expectedEventId,
              transactionHash: pointerId,
              outRef: `${pointerId}#0`,
              eventCborHex:
                publisher.read().snapshot.activeEvents[0]!.eventCborHex,
            });
            expect("terminalStatus" in scoped.event).toBe(false);
            expect(scoped.throughHeader).toMatchObject({
              observedBlockHash: fixture.commitBlock.point.blockHash,
              observedTransactionHash: captured.header.observedTransactionHash,
              transactionIndex: "1",
            });
          }
        } finally {
          await fresh.close();
        }
      } finally {
        await fixture.close();
      }
    },
  );

  it.each(["before_creation", "before_terminal", "after_terminal"] as const)(
    "uses actual same-block SQ/event order: %s",
    async (order) => {
      const state: { lifecycle: ReturnType<typeof historyLifecycle> | null } = {
        lifecycle: null,
      };
      const fixture = await createSyntheticStateQueueObservationFixture({
        composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
          const origin = await openOrigin(transport);
          state.lifecycle = historyLifecycle(origin.facts);
          await origin.pair.close();
          const { create, consume, settlementBody } = state.lifecycle;
          return {
            transactions:
              order === "before_creation"
                ? [commitTransactionCbor, create, consume]
                : order === "before_terminal"
                  ? [create, commitTransactionCbor, consume]
                  : [create, consume, commitTransactionCbor],
            creatingBodies: [transport.initializationBodyCbor, settlementBody],
          };
        },
      });
      try {
        const lifecycle = state.lifecycle!;
        const { pair, input, origin } = await openOrigin(fixture.transport);
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
        await pair.close();
        for (const block of [
          fixture.transport.emptySuccessorBlock,
          fixture.initializationBlock,
          fixture.commitBlock,
        ]) {
          const finalized = await fixture.transport.openFinalizedBlock(block);
          try {
            await publisher.publish(finalized);
          } finally {
            await finalized.close();
          }
        }
        const captured = await fixture.observeFresh();
        const fresh = await fixture.transport.openFinalizedBlock(
          fixture.commitBlock,
        );
        const request = {
          ...fresh,
          kind: "deposit" as const,
          eventId: lifecycle.expectedEventId,
          throughHeader: captured.header,
        };
        if (order === "before_creation")
          await expect(publisher.eventAuthority(request)).rejects.toThrow(
            "origin occurs after the challenged header",
          );
        else {
          const receipt = await publisher.eventAuthority(request);
          const scoped = await readWatcherLocalUserEventAuthority(receipt);
          expect(scoped.throughHeader).toMatchObject({
            headerHash: captured.header.headerHash,
            headerCborHex: captured.header.headerCborHex,
            observedTransactionHash: captured.header.observedTransactionHash,
            observedBlockHash: fixture.commitBlock.point.blockHash,
            transactionIndex: order === "before_terminal" ? "1" : "2",
          });
          expect(scoped.event.eventId).toBe(lifecycle.expectedEventId);
          expect("terminalStatus" in scoped.event).toBe(
            order === "after_terminal",
          );
          if (order === "after_terminal")
            expect(scoped.event).toMatchObject({ terminalStatus: "absorbed" });
          expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
          await expect(
            publisher.eventAuthority({
              ...request,
              throughHeader: { ...captured.header },
            }),
          ).rejects.toThrow("not admitted by the production source");
          await fresh.close();
          await expect(
            readWatcherLocalUserEventAuthority(receipt),
          ).rejects.toThrow();
        }
        await fresh.close();
        await captured.close();
        publisher.close();
      } finally {
        await fixture.close();
      }
    },
    120_000,
  );

  it("scopes an older sealed header without future terminal facts and retains the same cutoff through cold replay", async () => {
    const state: {
      lifecycle: ReturnType<typeof historyLifecycle> | null;
      creation: SyntheticUserEventBlock | null;
    } = { lifecycle: null, creation: null };
    const fixture = await createSyntheticStateQueueObservationFixture({
      composeCommitBlock: async ({
        transport,
        initializationBlock,
        commitTransactionCbor,
      }) => {
        const origin = await openOrigin(transport);
        state.lifecycle = historyLifecycle(origin.facts);
        await origin.pair.close();
        state.creation = await transport.makeBlock({
          parent: initializationBlock,
          transactions: [state.lifecycle.create],
          creatingBodies: [
            transport.initializationBodyCbor,
            state.lifecycle.settlementBody,
          ],
        });
        return {
          transactions: [commitTransactionCbor],
          parent: state.creation,
        };
      },
    });
    try {
      const lifecycle = state.lifecycle!;
      const terminal = await fixture.transport.makeBlock({
        parent: fixture.commitBlock,
        transactions: [lifecycle.consume],
        creatingBodies: [
          fixture.transport.initializationBodyCbor,
          lifecycle.settlementBody,
        ],
      });
      const blocks = [
        fixture.transport.activationBlock,
        fixture.transport.emptySuccessorBlock,
        fixture.initializationBlock,
        state.creation!,
        fixture.commitBlock,
        terminal,
      ];
      let head = terminal;
      for (let index = 0; index < 67; index += 1) {
        head = await fixture.transport.makeBlock({
          parent: head,
          transactions: [],
        });
        blocks.push(head);
      }
      const captured = await fixture.observeFresh();
      const { pair, input, origin } = await openOrigin(fixture.transport);
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
      await pair.close();
      for (const block of blocks.slice(1)) {
        const finalized = await fixture.transport.openFinalizedBlock(block);
        try {
          await publisher.publish(finalized);
        } finally {
          await finalized.close();
        }
      }
      const toRotate = await fixture.transport.openFinalizedBlock(head);
      await publisher.rotate(toRotate);
      await toRotate.close();
      expect(publisher.read().retainedEntries).toBe(64);
      expect(
        publisher
          .read()
          .store.chainPoints.some(
            (point) => point.blockHash === fixture.commitBlock.point.blockHash,
          ),
      ).toBe(false);
      const fresh = await fixture.transport.openFinalizedBlock(head);
      const receipt = await publisher.eventAuthority({
        ...fresh,
        kind: "deposit",
        eventId: lifecycle.expectedEventId,
        throughHeader: captured.header,
      });
      const scoped = await readWatcherLocalUserEventAuthority(receipt);
      expect(scoped.throughHeader).toMatchObject({
        headerHash: captured.header.headerHash,
        observedTransactionHash: captured.header.observedTransactionHash,
        observedBlockHash: fixture.commitBlock.point.blockHash,
        transactionIndex: "0",
      });
      expect("terminalStatus" in scoped.event).toBe(false);
      expect(scoped.historyEntryDigests).toContain(
        scoped.throughHeader!.historyEntryDigest,
      );
      expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
      const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(durable.runtime),
      );
      const payload: { anchor: { indexDigest: string } } = JSON.parse(
        Buffer.from(protectedHead.payload!).toString("utf8"),
      );
      const archiveIndex: { sourcePayloadDigest: string } = JSON.parse(
        Buffer.from(
          (await durable.archive.read(payload.anchor.indexDigest))!,
        ).toString("utf8"),
      );
      const originalPayload = durable.objects.get(
        archiveIndex.sourcePayloadDigest,
      )!;
      durable.objects.delete(archiveIndex.sourcePayloadDigest);
      await expect(
        publisher.eventAuthority({
          ...fresh,
          kind: "deposit",
          eventId: lifecycle.expectedEventId,
          throughHeader: captured.header,
        }),
      ).rejects.toThrow("absent or corrupt");
      durable.objects.set(archiveIndex.sourcePayloadDigest, originalPayload);
      await fresh.close();
      publisher.close();
      await captured.close();
      const runtime = await createWatcherDurableRuntime(durable.runtimeInput);
      const newOrigin = await openOrigin(fixture.transport);
      const byHash = new Map(
        blocks.map((block) => [block.point.blockHash, block]),
      );
      const resumed = await recoverWatcherLocalUserEventPublisher({
        ...newOrigin.input,
        origin: newOrigin.origin,
        referenceAuthority: newOrigin.pair.referenceAuthority,
        runtime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = byHash.get(point.blockHash);
          if (
            block === undefined ||
            !watcherSameCanonicalJson(point, block.point)
          )
            throw new Error("unexpected cutoff replay point");
          return await fixture.transport.openFinalizedBlock(block);
        },
      });
      await newOrigin.pair.close();
      const newCaptured = await fixture.observeFresh();
      const newFresh = await fixture.transport.openFinalizedBlock(head);
      const renewed = await resumed.eventAuthority({
        ...newFresh,
        kind: "deposit",
        eventId: lifecycle.expectedEventId,
        throughHeader: newCaptured.header,
      });
      const newScoped = await readWatcherLocalUserEventAuthority(renewed);
      expect("terminalStatus" in newScoped.event).toBe(false);
      const { historyEntryDigest: oldDigest, ...oldCutoff } =
        scoped.throughHeader!;
      const { historyEntryDigest: newDigest, ...newCutoff } =
        newScoped.throughHeader!;
      expect(newCutoff).toEqual(oldCutoff);
      expect(newDigest).not.toBe(oldDigest);
      expect(newScoped.event).toMatchObject({
        eventId: scoped.event.eventId,
        eventCborHex: scoped.event.eventCborHex,
        transactionHash: scoped.event.transactionHash,
      });
      await newFresh.close();
      await newCaptured.close();
      resumed.close();
    } finally {
      await fixture.close();
    }
  }, 300_000);
});

// Prepare deterministic ordinary block contents before the SQ transport builds
// its commit. This retired template capability never authorizes the later replay.
const ordinaryDepositReplayTemplate = async () => {
  const fixture = await createSyntheticUserEventOriginFixture();
  let publisher:
    | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
    | undefined;
  try {
    const { pair, input, origin, facts } = await openOrigin(fixture);
    const durable = await durableFixture(
      readWatcherLocalBackfillFinality(pair.finality).policy,
    );
    publisher = await createWatcherLocalUserEventPublisher({
      ...input,
      origin,
      runtime: durable.runtime,
      archive: durable.archive,
    });
    await publisher.publish(pair);
    await publisher.publish(
      await fixture.openFinalizedBlock(fixture.emptySuccessorBlock),
    );
    const deposit = historyLifecycle(facts);
    const block = await fixture.makeBlock({
      transactions: [deposit.create],
      creatingBodies: [fixture.initializationBodyCbor],
    });
    await publisher.publish(await fixture.openFinalizedBlock(block));
    const authority = await publisher.eventAuthority({
      ...(await fixture.openFinalizedBlock(block)),
      kind: "deposit",
      eventId: deposit.expectedEventId,
    });
    const replay = await makeLocalDepositReplayFixture(
      authority,
      fixture.deploymentIdentity.programCommitments,
    );
    return {
      header: replay.observation.header,
      ruleBundleCommitment: replay.ruleBundleCommitment,
    };
  } finally {
    publisher?.close();
    await fixture.close();
  }
};

describe("header-scoped replay authorities", () => {
  it("binds actual same-block event cutoff to W25, transcript and fresh semantic recovery", async () => {
    const template = await ordinaryDepositReplayTemplate();
    let eventId = "";
    const queue = await createSyntheticStateQueueObservationFixture({
      ...template,
      composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
        const source = await openOrigin(transport);
        try {
          const deposit = historyLifecycle(source.facts);
          eventId = deposit.expectedEventId;
          return {
            transactions: [
              deposit.create,
              commitTransactionCbor,
              deposit.consume,
            ],
            creatingBodies: [
              transport.initializationBodyCbor,
              deposit.settlementBody,
            ],
          };
        } finally {
          await source.pair.close();
        }
      },
    });
    const fixture = queue.transport;
    let publisher:
      | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
      | undefined;
    let resumed:
      | Awaited<ReturnType<typeof recoverWatcherLocalUserEventPublisher>>
      | undefined;
    try {
      const { pair, input, origin } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      const historyBlocks = [
        fixture.emptySuccessorBlock,
        queue.initializationBlock,
        queue.commitBlock,
      ];
      for (const block of historyBlocks)
        await publisher.publish(await fixture.openFinalizedBlock(block));
      expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
      const captured = await queue.observeFresh();
      const freshPair = await fixture.openFinalizedBlock(queue.commitBlock);
      const authority = await publisher.eventAuthority({
        ...freshPair,
        throughHeader: captured.header,
        kind: "deposit",
        eventId,
      });
      const local = await readWatcherLocalUserEventAuthority(authority);
      expect("terminalStatus" in local.event).toBe(false);
      expect(local.throughHeader).toMatchObject({
        headerHash: queue.headerHash,
        queueOutRef: captured.header.queueOutRef,
        transactionIndex: "1",
      });
      expect(local.historyEntryDigests).toContain(
        local.throughHeader!.historyEntryDigest,
      );
      const replay = await makeLocalDepositReplayFixture(
        authority,
        fixture.deploymentIdentity.programCommitments,
      );
      const result = await evaluateWatcherBlockReplay(replay);
      expect(result).toMatchObject({
        action: "accept",
        reasonCodes: [],
        eventRoots: [{ phase: "Deposit", mutationCount: 1 }],
      });
      expect(
        await evaluateWatcherBlockReplay({
          ...replay,
          observation: {
            ...replay.observation,
            chainPoint: {
              ...replay.observation.chainPoint,
              blockHash: h32("fe"),
            },
          },
        }),
      ).toMatchObject({
        action: "error",
        reasonCodes: ["user_event_authority_identity_mismatch"],
      });
      const transcriptInput = {
        deploymentIdentity: fixture.deploymentIdentity,
        stateQueueObservation: captured.observation,
        header: captured.header,
        payloadEnvelopeCbor: replay.payloadEnvelopeCbor,
        daProvenance: replay.daProvenance,
        priorState: replay.priorState,
        ruleBundle: replay.ruleBundle,
        ruleBundleCommitment: replay.ruleBundleCommitment,
        eventAuthorities: replay.eventAuthorities,
      };
      const original = await createWatcherAuthenticatedReplayTranscript({
        ...transcriptInput,
        coordinate: { domain: "transition_step", index: "0" },
      });
      const persistedTranscriptCborHex =
        watcherAuthenticatedReplayTranscriptCborHex(original);
      const originalRecords = await readWatcherReplayTranscriptRecords(
        persistedTranscriptCborHex,
        30,
      );
      const originalEvent = originalRecords.events[0]!;
      if (
        originalEvent.origin.source !== "local_publication" ||
        originalEvent.origin.throughHeader === null
      )
        throw new Error("scoped transcript lacks a cutoff");
      const originalOrigin = originalEvent.origin;
      const cutoff = originalOrigin.throughHeader!;
      const rewriteCutoff = (
        changes: Partial<WatcherLocalUserEventHeaderCutoff>,
      ) => {
        const eventRecord = {
          ...originalEvent,
          origin: {
            ...originalOrigin,
            throughHeader: { ...cutoff, ...changes },
          },
        };
        const blockReplay = {
          ...originalRecords.blockReplay,
          authorityManifestDigest: watcherSha256CanonicalJson([
            watcherBlockReplayEventAuthorityManifest(eventRecord),
          ]),
        };
        const { resultDigest: _oldResult, ...replayMaterial } = {
          ...blockReplay,
          downstreamPrerequisite: {
            ...blockReplay.downstreamPrerequisite,
            inputDigest: watcherBlockReplayDownstreamInputDigest(blockReplay),
          },
        };
        const rewrittenReplay = {
          ...replayMaterial,
          resultDigest: watcherSha256CanonicalJson(replayMaterial),
        };
        const { transcriptDigest: _oldTranscript, ...transcriptMaterial } = {
          ...original,
          blockReplayRecordCborHex:
            watcherReplayRawRecordCborHex(rewrittenReplay),
          blockReplayResultDigest: rewrittenReplay.resultDigest,
          eventAuthorityRecordsCborHex: [
            watcherReplayRawRecordCborHex(eventRecord),
          ],
        };
        return watcherReplayRawRecordCborHex({
          ...transcriptMaterial,
          transcriptDigest:
            computeDeploymentManifestJsonDigest(transcriptMaterial),
        });
      };
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...transcriptInput,
          persistedTranscriptCborHex: rewriteCutoff({
            historyEntryDigest: h32("fd"),
          }),
        }),
      ).rejects.toThrow("event cutoff history membership");
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...transcriptInput,
          persistedTranscriptCborHex: rewriteCutoff({
            queueOutRef: `${h32("fc")}#0`,
          }),
        }),
      ).rejects.toThrow("event cutoff queueOutRef");
      await expect(
        replayWatcherAuthenticatedReplayTranscript({
          ...transcriptInput,
          persistedTranscriptCborHex: rewriteCutoff({ transactionIndex: "2" }),
        }),
      ).rejects.toThrow("differs from fresh authenticated replay semantics");
      publisher.close();
      await captured.close();
      await freshPair.close();
      const newRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const freshOrigin = await openOrigin(fixture);
      resumed = await recoverWatcherLocalUserEventPublisher({
        ...freshOrigin.input,
        origin: freshOrigin.origin,
        referenceAuthority: freshOrigin.pair.referenceAuthority,
        runtime: newRuntime,
        archive: durable.archive,
        replayBlock: async (point) => {
          const block = historyBlocks.find((candidate) =>
            watcherSameCanonicalJson(candidate.point, point),
          );
          if (block === undefined)
            throw new Error("unexpected cutoff replay point");
          return await fixture.openFinalizedBlock(block);
        },
      });
      const renewedCapture = await queue.observeFresh();
      const renewedAuthority = await resumed.eventAuthority({
        ...(await fixture.openFinalizedBlock(queue.commitBlock)),
        throughHeader: renewedCapture.header,
        kind: "deposit",
        eventId,
      });
      const renewedLocal =
        await readWatcherLocalUserEventAuthority(renewedAuthority);
      expect(renewedLocal.throughHeader).toMatchObject({
        headerHash: cutoff.headerHash,
        headerCborHex: cutoff.headerCborHex,
        queueOutRef: cutoff.queueOutRef,
        observedTransactionHash: cutoff.observedTransactionHash,
        observedBlockHash: cutoff.observedBlockHash,
        observedSlot: cutoff.observedSlot,
        observedBlockNo: cutoff.observedBlockNo,
        transactionIndex: cutoff.transactionIndex,
      });
      expect(renewedLocal.throughHeader!.historyEntryDigest).not.toBe(
        cutoff.historyEntryDigest,
      );
      const eventAuthority = replay.eventAuthorities![0]!;
      if (eventAuthority.localUserEvent === undefined)
        throw new Error("scoped replay requires local capability");
      const renewed = await replayWatcherAuthenticatedReplayTranscript({
        ...transcriptInput,
        stateQueueObservation: renewedCapture.observation,
        header: renewedCapture.header,
        eventAuthorities: [
          { ...eventAuthority, localUserEvent: renewedAuthority },
        ],
        persistedTranscriptCborHex,
      });
      assertWatcherAuthenticatedReplayTranscript(renewed);
      expect(renewed.transcriptDigest).not.toBe(original.transcriptDigest);
      expect(watcherAuthenticatedReplayTranscriptCborHex(original)).toBe(
        persistedTranscriptCborHex,
      );
      // A second actual queue contains the same header at another native point.
      const network = {
        fetch: globalThis.fetch,
        WebSocket: globalThis.WebSocket,
      };
      const otherQueue =
        await createSyntheticStateQueueObservationFixture(template);
      try {
        const other = await otherQueue.observeFresh();
        expect(other.header.headerHash).toBe(renewedCapture.header.headerHash);
        expect(other.header.observedBlockHash).not.toBe(
          renewedCapture.header.observedBlockHash,
        );
        await expect(
          createWatcherAuthenticatedReplayTranscript({
            ...transcriptInput,
            stateQueueObservation: other.observation,
            header: other.header,
            eventAuthorities: [
              { ...eventAuthority, localUserEvent: renewedAuthority },
            ],
            coordinate: { domain: "transition_step", index: "0" },
          }),
        ).rejects.toThrow();
      } finally {
        await otherQueue.close();
        vi.stubGlobal("fetch", network.fetch);
        vi.stubGlobal("WebSocket", network.WebSocket);
      }
    } finally {
      publisher?.close();
      resumed?.close();
      await queue.close();
    }
  }, 120_000);
});

describe("local user-event same-process rollback suspension", () => {
  it("retires old capabilities synchronously and requires fresh protected-head W12 corroboration", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
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
      await pair.close();
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      await empty.close();
      const lifecycle = historyLifecycle(facts);
      const block = await fixture.makeBlock({
        transactions: [lifecycle.create],
        creatingBodies: [fixture.initializationBodyCbor],
      });
      const published = await fixture.openFinalizedBlock(block);
      await publisher.publish(published);
      await published.close();
      const before = await fixture.openFinalizedBlock(block);
      const event = publisher.read().snapshot.activeEvents[0]!;
      const old = await publisher.eventAuthority({
        ...before,
        eventId: event.eventId,
        kind: "deposit",
      });
      await expect(
        readWatcherLocalUserEventAuthority(old),
      ).resolves.toBeDefined();
      publisher.suspend();
      await expect(readWatcherLocalUserEventAuthority(old)).rejects.toThrow(
        /suspended/u,
      );
      await expect(publisher.publish(before)).rejects.toThrow(/suspended/u);
      await expect(publisher.resume(before)).rejects.toThrow(/freshly/u);
      const fresh = await fixture.openFinalizedBlock(block);
      await publisher.resume(fresh);
      await fresh.close();
      await before.close();
      await expect(readWatcherLocalUserEventAuthority(old)).rejects.toThrow();
      const corroborated = await fixture.openFinalizedBlock(block);
      const authority = await publisher.eventAuthority({
        ...corroborated,
        eventId: event.eventId,
        kind: "deposit",
      });
      await expect(
        readWatcherLocalUserEventAuthority(authority),
      ).resolves.toBeDefined();
      publisher.suspend();
      const rollbackPair = await fixture.openFinalizedBlock(block);
      // A changed protected head cannot be accepted as the old same-process fold.
      const otherRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const reopenedOrigin = await openOrigin(fixture);
      const other = await recoverWatcherLocalUserEventPublisher({
        ...reopenedOrigin.input,
        origin: reopenedOrigin.origin,
        ...reopenedOrigin.pair,
        runtime: otherRuntime,
        archive: durable.archive,
        replayBlock: async (point) =>
          fixture.openFinalizedBlock(
            point.blockHash === fixture.emptySuccessorBlock.point.blockHash
              ? fixture.emptySuccessorBlock
              : block,
          ),
      });
      other.close();
      await reopenedOrigin.pair.close();
      await expect(publisher.resume(rollbackPair)).rejects.toThrow();
      await rollbackPair.close();
      await corroborated.close();
      publisher.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});

describe("owned user-event runtime ordinary unavailable candidates", () => {
  it("refuses an absent claim, then issues a valid header-scoped capability and keeps indexing", async () => {
    const construction = await createSyntheticUserEventOriginFixture();
    const identity = construction.deploymentIdentity;
    const ruleBundle = makeWatcherCanonicalRuleBundle({
      constructionIdentity: {
        manifestId: identity.manifestId,
        network: identity.network,
        blueprintHash: identity.blueprintHash,
        programCommitments: identity.programCommitments,
      },
      targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
    });
    await construction.close();
    const state: {
      lifecycle: ReturnType<typeof historyLifecycle> | null;
      depositAddressHex: string | null;
    } = { lifecycle: null, depositAddressHex: null };
    const fixture = await createSyntheticStateQueueObservationFixture({
      ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
      composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
        const origin = await openOrigin(transport);
        state.lifecycle = historyLifecycle(origin.facts);
        state.depositAddressHex = origin.facts.scripts.deposit.addressHex;
        await origin.pair.close();
        return {
          transactions: [state.lifecycle.create, commitTransactionCbor],
          creatingBodies: [transport.initializationBodyCbor],
        };
      },
    });
    let service: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      const transport = fixture.transport;
      const signed = transport.deployment;
      const deploymentAuthority = await loadWatcherVerifiedDeploymentAuthority({
        path: "/unit/authority.json",
        ruleBundlePath: "/unit/rules.json",
        unsafeReadFileForTest: async (path) =>
          new TextEncoder().encode(
            JSON.stringify(
              path === "/unit/authority.json"
                ? {
                    signedIdentity: signed.signedIdentity,
                    policy: signed.policy,
                    trustRoots: signed.trustRoots,
                    durableMarker: signed.marker,
                  }
                : ruleBundle,
            ),
          ),
      });
      const origin = await openOrigin(transport);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(origin.pair.finality).policy,
      );
      await origin.pair.close();
      service = await createWatcherUserEventRuntime({
        watcherConfig: transport.watcherConfig,
        deploymentAuthority,
        blueprintBytes: await readFile(
          process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
            fileURLToPath(
              new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
            ),
        ),
        nativeChainSyncBinaryPath: transport.nativeChainSyncBinaryPath,
        runtime: durable.runtime,
        archive: durable.archive,
        coverage: createInMemoryWatcherUserEventCoverageStore(),
      });
      await service.advanceThrough(fixture.commitBlock.point);
      const captured = await fixture.observeFresh();
      try {
        const request = {
          kind: "deposit" as const,
          eventId: state.lifecycle!.expectedEventId,
          throughHeader: captured.header,
        };
        const before = service.read().currentPoint;
        await expect(
          service.eventAuthority({ ...request, eventId: `${h32("ab")}#0` }),
        ).rejects.toThrow(/event is not retained/u);
        expect(service.read()).toMatchObject({
          status: "ready",
          currentPoint: before,
        });
        expect(() => assertWatcherUserEventRuntime(service!)).not.toThrow();
        const cap = await service.eventAuthority(request);
        expect(
          (await readWatcherLocalUserEventAuthority(cap)).event.eventId,
        ).toBe(request.eventId);
        // A quiet successor is covered without a publication: the issued
        // authority stays current because no new checkpoint retired it.
        const next = await transport.makeBlock({
          transactions: [],
          parent: fixture.commitBlock,
        });
        await service.advanceThrough(next.point);
        expect(service.read()).toMatchObject({
          status: "ready",
          currentPoint: next.point,
          headCursor: fixture.commitBlock.point,
        });
        expect(
          (await readWatcherLocalUserEventAuthority(cap)).event.eventId,
        ).toBe(request.eventId);
        // A touched successor (a plain payment at the deposit credential,
        // which the fold ignores) publishes a new checkpoint and retires it.
        const touchedInputs = CML.TransactionInputList.new();
        touchedInputs.add(transactionInput(`${h32("c3")}#0`));
        const touchedOutputs = CML.TransactionOutputList.new();
        touchedOutputs.add(
          CML.TransactionOutput.new(
            CML.Address.from_hex(state.depositAddressHex!),
            CML.Value.new(2_000_000n, CML.MultiAsset.new()),
          ),
        );
        const touched = await transport.makeBlock({
          transactions: [
            transaction(
              CML.TransactionBody.new(touchedInputs, touchedOutputs, 200_000n),
              [],
            ),
          ],
          parent: next,
        });
        await service.advanceThrough(touched.point);
        expect(service.read()).toMatchObject({
          status: "ready",
          currentPoint: touched.point,
          headCursor: touched.point,
        });
        await expect(readWatcherLocalUserEventAuthority(cap)).rejects.toThrow();
        const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        );
        durable.objects.delete(protectedHead.checkpoint!.payloadDigest);
        await expect(
          service.eventAuthority({ ...request, eventId: `${h32("ab")}#0` }),
        ).rejects.toThrow();
        expect(service.read().status).toBe("failed");
        await expect(service.done).rejects.toThrow();
      } finally {
        await captured.close();
      }
    } finally {
      await service?.close();
      await fixture.close();
    }
  }, 120_000);
});

describe("canonical semantic rollback replacement", () => {
  it.each([
    "admission",
    "pointer",
    "retirement",
    "withdrawal payout retirement",
  ] as const)(
    "replays %s rollback from fresh native blocks, publishes once and restores on restart",
    async (scenario) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      try {
        const initial = await openOrigin(fixture);
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(initial.pair.finality).policy,
        );
        const publisher = await createWatcherLocalUserEventPublisher({
          ...initial.input,
          origin: initial.origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        await publisher.publish(initial.pair);
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        await publisher.publish(empty);
        await empty.close();
        const kind =
          scenario === "withdrawal payout retirement"
            ? "withdrawal"
            : "deposit";
        const lifecycle = historyLifecycle(initial.facts, false, {
          kind,
          withdrawalPayout: kind === "withdrawal",
        });
        const common =
          scenario === "admission"
            ? fixture.emptySuccessorBlock
            : await fixture.makeBlock({
                parent: fixture.emptySuccessorBlock,
                transactions: [lifecycle.create],
                creatingBodies: [fixture.initializationBodyCbor],
              });
        if (scenario !== "admission") {
          const pair = await fixture.openFinalizedBlock(common);
          await publisher.publish(pair);
          await pair.close();
        }
        let operation =
          scenario === "admission" ? lifecycle.create : lifecycle.consume;
        if (scenario === "pointer")
          operation = historyPointerContinuation(
            initial.facts,
            lifecycle,
            kind,
          );
        const old = await fixture.makeBlock({
          parent: common,
          transactions: [operation],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        const oldPair = await fixture.openFinalizedBlock(old);
        await publisher.publish(oldPair);
        const prior = publisher.read();
        if (kind === "withdrawal")
          expect(prior.snapshot.terminalEvents[0]).toMatchObject({
            kind: "withdrawal",
            terminalStatus: "payout_initialized",
            terminalFinalityStatus: "final",
          });
        const authorityPair = await fixture.openFinalizedBlock(old);
        const issued = await publisher.eventAuthority({
          ...authorityPair,
          kind,
          eventId: lifecycle.expectedEventId,
        });
        publisher.suspend();
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(issued),
        ).toThrow();
        const replacement = await fixture.makeBlock({
          parent: common,
          transactions: [],
          slot: Number(old.point.slot) + 1,
        });
        await fixture.selectCanonicalBranch(replacement.point);
        const fresh = await openOrigin(fixture);
        const replay = [
          fixture.emptySuccessorBlock,
          ...(scenario === "admission" ? [] : [common]),
          replacement,
        ];
        const request = {
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime: durable.runtime,
          archive: durable.archive,
          replayCanonical: async function* () {
            for (const block of replay)
              yield await fixture.openFinalizedBlock(block);
          },
        };
        const casBefore = durable.casCount();
        const recovered = await replaceWatcherLocalUserEventPublisher(request);
        expect(durable.casCount()).toBe(casBefore + 1);
        expect(recovered.read().checkpoint).toMatchObject({
          rollbackGeneration: "1",
          predecessorCheckpointDigest: prior.checkpoint!.checkpointDigest,
          checkpointSequence: (
            BigInt(prior.checkpoint!.checkpointSequence) + 1n
          ).toString(),
        });
        expect(recovered.read().snapshot.terminalEvents).toHaveLength(0);
        expect(recovered.read().snapshot.activeEvents).toHaveLength(
          scenario === "admission" ? 0 : 1,
        );
        if (scenario !== "admission")
          expect(recovered.read().snapshot.activeEvents[0]!.outRef).toBe(
            `${lifecycle.createId}#0`,
          );
        expect(() =>
          assertWatcherLocalUserEventAuthorityCurrent(issued),
        ).toThrow();
        const checkpoint = recovered.read().checkpoint;
        recovered.close();
        publisher.close();
        await authorityPair.close();
        await oldPair.close();
        await initial.pair.close();
        await fresh.pair.close();
        const restarted = await createWatcherDurableRuntime(
          durable.runtimeInput,
        );
        const restartOrigin = await openOrigin(fixture);
        const resumed = await resumeWatcherLocalUserEventPublisher({
          ...restartOrigin.input,
          origin: restartOrigin.origin,
          referenceAuthority: restartOrigin.pair.referenceAuthority,
          runtime: restarted,
          archive: durable.archive,
          readHead: () => fixture.openFinalizedBlock(replacement),
        });
        expect(resumed.read().checkpoint).toEqual(checkpoint);
        expect(durable.casCount()).toBe(casBefore + 1);
        resumed.close();
        await restartOrigin.pair.close();
      } finally {
        await fixture.close();
      }
    },
    120_000,
  );

  it("requires a positive conflicting full-height branch and preserves the protected head on failed replay", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const initial = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(initial.pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...initial.input,
        origin: initial.origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(initial.pair);
      const pair = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(pair);
      const checkpoint = publisher.read().checkpoint;
      const fresh = await openOrigin(fixture);
      const base = {
        ...fresh.input,
        origin: fresh.origin,
        referenceAuthority: fresh.pair.referenceAuthority,
        runtime: durable.runtime,
        archive: durable.archive,
      };
      await expect(
        replaceWatcherLocalUserEventPublisher({
          ...base,
          replayCanonical: async function* () {
            yield await fixture.openFinalizedBlock(fixture.emptySuccessorBlock);
          },
        }),
      ).rejects.toThrow("no conflicting");
      await expect(
        replaceWatcherLocalUserEventPublisher({
          ...base,
          replayCanonical: async function* () {
            yield await Promise.reject(new Error("source unavailable"));
          },
        }),
      ).rejects.toThrow("source unavailable");
      expect(
        readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        ).checkpoint,
      ).toEqual(checkpoint);
      publisher.close();
      await pair.close();
      await initial.pair.close();
      await fresh.pair.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});

describe("raw history payload publication", () => {
  it.each([
    { kind: "deposit", withdrawalPayout: false, pointerOnly: false },
    { kind: "withdrawal", withdrawalPayout: false, pointerOnly: false },
    { kind: "withdrawal", withdrawalPayout: true, pointerOnly: false },
    { kind: "deposit", withdrawalPayout: false, pointerOnly: true },
    { kind: "withdrawal", withdrawalPayout: false, pointerOnly: true },
  ] as const)(
    "retains duplicate map pairs through $kind transition payout=$withdrawalPayout pointer=$pointerOnly and restart",
    async ({ kind, withdrawalPayout, pointerOnly }) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      let publisher:
        | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
        | undefined;
      const pairs: Awaited<ReturnType<typeof fixture.openFinalizedBlock>>[] =
        [];
      try {
        const initial = await openOrigin(fixture);
        pairs.push(initial.pair);
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(initial.pair.finality).policy,
        );
        publisher = await createWatcherLocalUserEventPublisher({
          ...initial.input,
          origin: initial.origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        await publisher.publish(initial.pair);
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        pairs.push(empty);
        await publisher.publish(empty);
        const rawDatumCbor = "a302a20102010301000102";
        const lifecycle = historyLifecycle(initial.facts, true, {
          kind,
          withdrawalPayout,
          rawDatumCbor,
        });
        const admission = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        const admitted = await fixture.openFinalizedBlock(admission);
        pairs.push(admitted);
        await publisher.publish(admitted);
        const original = publisher.read().snapshot.activeEvents[0]!;
        expect(original.historyPayloadCborHex).toContain(rawDatumCbor);
        expect(original.eventCborHex).toContain(rawDatumCbor);
        if (kind === "withdrawal") {
          const funds = CML.Transaction.from_cbor_hex(lifecycle.consume)
            .body()
            .outputs()
            .get(0);
          expect(
            historyRawField(funds.datum()!.as_datum()!.to_cbor_hex(), []),
          ).toBe(
            withdrawalPayout
              ? historyWithdrawalPayoutDatum(original.historyPayloadCborHex!)
              : rawDatumCbor,
          );
        }
        const retirement = await fixture.makeBlock({
          parent: admission,
          transactions: [
            pointerOnly
              ? historyPointerContinuation(initial.facts, lifecycle, kind)
              : lifecycle.consume,
          ],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        const retired = await fixture.openFinalizedBlock(retirement);
        pairs.push(retired);
        await publisher.publish(retired);
        const saved = publisher.read();
        if (pointerOnly) {
          expect(saved.snapshot.activeEvents).toHaveLength(1);
          const continued = saved.snapshot.activeEvents[0]!;
          expect(continued.outRef).not.toBe(original.outRef);
          expect(continued.outputCborHex).toContain(rawDatumCbor);
          expect(continued.datumCborHex).toContain(rawDatumCbor);
          expect(continued.historyPayloadCborHex).toBe(
            original.historyPayloadCborHex,
          );
          expect(continued.eventCborHex).toBe(original.eventCborHex);
        } else {
          expect(saved.snapshot.activeEvents).toHaveLength(0);
          expect(saved.snapshot.terminalEvents[0]).toMatchObject({
            eventCborHex: original.eventCborHex,
            historyPayloadCborHex: original.historyPayloadCborHex,
            terminalStatus:
              kind === "deposit"
                ? "absorbed"
                : withdrawalPayout
                  ? "payout_initialized"
                  : "refunded",
          });
        }
        publisher.close();
        for (const pair of pairs.splice(0)) await pair.close();
        const fresh = await openOrigin(fixture);
        pairs.push(fresh.pair);
        publisher = await resumeWatcherLocalUserEventPublisher({
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime: await createWatcherDurableRuntime(durable.runtimeInput),
          archive: durable.archive,
          readHead: () => fixture.openFinalizedBlock(retirement),
        });
        expect(publisher.read().snapshot).toEqual(saved.snapshot);
        const authorityPair = await fixture.openFinalizedBlock(retirement);
        pairs.push(authorityPair);
        const authority = await publisher.eventAuthority({
          ...authorityPair,
          kind,
          eventId: lifecycle.expectedEventId,
        });
        expect(
          (await readWatcherLocalUserEventAuthority(authority)).event
            .historyPayloadCborHex,
        ).toBe(original.historyPayloadCborHex);
      } finally {
        publisher?.close();
        for (const pair of pairs) await pair.close();
        await fixture.close();
      }
    },
    120_000,
  );
});

/** Native creating-body references are authenticated by the fixture's local
 * follower. These synthetic blocks do not claim public-chain ledger acceptance. */
describe("raw external history native lifecycle", () => {
  const cases = (["deposit", "withdrawal"] as const).flatMap((kind) => [
    { kind, fault: null },
    ...(["admission", "retirement"] as const).flatMap((stage) =>
      (["missing", "substituted"] as const).map((faultKind) => ({
        kind,
        fault: { stage, kind: faultKind },
      })),
    ),
  ]);
  it.each(cases)(
    "$kind preserves external raw history through pointer/retirement/restart; fault=$fault",
    async ({ kind, fault }) => {
      const fixture = await createSyntheticUserEventOriginFixture();
      let publisher:
        | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
        | undefined;
      const pairs: Awaited<ReturnType<typeof fixture.openFinalizedBlock>>[] =
        [];
      try {
        const initial = await openOrigin(fixture);
        pairs.push(initial.pair);
        const durable = await durableFixture(
          readWatcherLocalBackfillFinality(initial.pair.finality).policy,
        );
        publisher = await createWatcherLocalUserEventPublisher({
          ...initial.input,
          origin: initial.origin,
          runtime: durable.runtime,
          archive: durable.archive,
        });
        await publisher.publish(initial.pair);
        const empty = await fixture.openFinalizedBlock(
          fixture.emptySuccessorBlock,
        );
        pairs.push(empty);
        await publisher.publish(empty);
        const rawDatumCbor = historyRawField(
          `a402a2010201030100010203590400${"ab".repeat(1024)}`,
          [],
        );
        const options = {
          kind,
          external: true,
          rawDatumCbor,
          ...(fault === null ? {} : { externalFault: fault }),
        };
        const lifecycle = historyLifecycle(initial.facts, true, options);
        expect(lifecycle.expectedPayloadCbor).toContain(
          "a402a20102010301000102",
        );
        const readCheckpoint = async () =>
          readWatcherProtectedUserEventCheckpointReceipt(
            await readWatcherProtectedUserEventCheckpoint(durable.runtime),
          );
        const assertRefused = async (pair: typeof empty) => {
          const before = await readCheckpoint();
          const snapshot = publisher!.read().snapshot;
          const cas = durable.casCount();
          await expect(publisher!.publish(pair)).rejects.toThrow(
            "whole-block event semantics differ",
          );
          const after = await readCheckpoint();
          expect(after.checkpoint).toEqual(before.checkpoint);
          expect(after.trustedHead).toEqual(before.trustedHead);
          expectSameArchiveBytes(after.payload, before.payload!);
          expect(durable.casCount()).toBe(cas);
          expect(publisher!.read().snapshot).toEqual(snapshot);
        };
        const admission = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create],
          creatingBodies: [
            fixture.initializationBodyCbor,
            ...lifecycle.externalBodies,
          ],
        });
        const admitted = await fixture.openFinalizedBlock(admission);
        pairs.push(admitted);
        if (fault?.stage === "admission") {
          await assertRefused(admitted);
          return;
        }
        const retainedBody = CML.TransactionBody.from_cbor_hex(
          lifecycle.externalBodies[0]!,
        );
        const retainedOutRef = `${CML.hash_transaction(retainedBody).to_hex()}#0`;
        const actualRetainedOutput = watcherUserEventReferenceOutput(
          readWatcherUserEventReferenceEvidence(admitted.referenceAuthority),
          lifecycle.createId,
          retainedOutRef,
        )!;
        expect(actualRetainedOutput.to_cbor_hex()).toBe(
          retainedBody.outputs().get(0).to_cbor_hex(),
        );
        expect(actualRetainedOutput.to_cbor_hex()).not.toBe(
          actualRetainedOutput.to_canonical_cbor_hex(),
        );
        expect(
          historyRawField(
            actualRetainedOutput.datum()!.as_datum()!.to_cbor_hex(),
            [1],
          ),
        ).toBe(lifecycle.expectedPayloadCbor);
        await publisher.publish(admitted);
        const original = publisher.read().snapshot.activeEvents[0]!;
        expect(original.historyPayloadCborHex).toBe(
          lifecycle.expectedPayloadCbor,
        );
        expect(original.eventCborHex).toBe(
          historyRawField(lifecycle.expectedPayloadCbor!, [0]),
        );
        const admittedNode = Data.from(original.datumCborHex, EventHistoryNode);
        expect(admittedNode.payload).toMatchObject({
          Order: { facts: { location: { External: {} } } },
        });
        const pointerTx = historyPointerContinuation(
          initial.facts,
          lifecycle,
          kind,
        );
        const pointer = await fixture.makeBlock({
          parent: admission,
          transactions: [pointerTx],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        const pointed = await fixture.openFinalizedBlock(pointer);
        pairs.push(pointed);
        await publisher.publish(pointed);
        const continued = publisher.read().snapshot.activeEvents[0]!;
        expect(continued.outRef).not.toBe(original.outRef);
        expect(continued.historyPayloadCborHex).toBe(
          original.historyPayloadCborHex,
        );
        expect(continued.eventCborHex).toBe(original.eventCborHex);
        expect(continued.inclusionTime).toBe(original.inclusionTime);
        const retirementLifecycle = historyLifecycle(initial.facts, true, {
          ...options,
          retirementOrderOutRef: continued.outRef,
          retirementOrderNext: "ff".repeat(32),
        });
        expect(retirementLifecycle.create).toBe(lifecycle.create);
        const retirement = await fixture.makeBlock({
          parent: pointer,
          transactions: [retirementLifecycle.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            retirementLifecycle.settlementBody,
            ...retirementLifecycle.externalBodies,
          ],
        });
        const retired = await fixture.openFinalizedBlock(retirement);
        pairs.push(retired);
        if (fault?.stage === "retirement") {
          await assertRefused(retired);
          return;
        }
        await publisher.publish(retired);
        const saved = publisher.read();
        expect(saved.snapshot.activeEvents).toHaveLength(0);
        expect(saved.snapshot.terminalEvents).toHaveLength(1);
        expect(saved.snapshot.terminalEvents[0]).toMatchObject({
          eventCborHex: original.eventCborHex,
          historyPayloadCborHex: original.historyPayloadCborHex,
          inclusionTime: original.inclusionTime,
          terminalStatus: kind === "deposit" ? "absorbed" : "refunded",
        });
        publisher.close();
        for (const pair of pairs.splice(0)) await pair.close();
        const fresh = await openOrigin(fixture);
        pairs.push(fresh.pair);
        publisher = await resumeWatcherLocalUserEventPublisher({
          ...fresh.input,
          origin: fresh.origin,
          referenceAuthority: fresh.pair.referenceAuthority,
          runtime: await createWatcherDurableRuntime(durable.runtimeInput),
          archive: durable.archive,
          readHead: () => fixture.openFinalizedBlock(retirement),
        });
        expect(publisher.read().snapshot).toEqual(saved.snapshot);
        const current = await fixture.openFinalizedBlock(retirement);
        pairs.push(current);
        const authority = await publisher.eventAuthority({
          ...current,
          kind,
          eventId: lifecycle.expectedEventId,
        });
        expect(
          (await readWatcherLocalUserEventAuthority(authority)).event
            .historyPayloadCborHex,
        ).toBe(lifecycle.expectedPayloadCbor);
      } finally {
        publisher?.close();
        for (const pair of pairs) await pair.close();
        await fixture.close();
      }
    },
    120_000,
  );
});
