import "./user-event-history.raw-history-payload-publication.js";

import { EventHistoryNode } from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { historyRawField } from "../../src/indexers/authenticated-event-history.js";
import {
  createWatcherLocalUserEventPublisher,
  resumeWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import { readWatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import {
  readWatcherUserEventReferenceEvidence,
  watcherUserEventReferenceOutput,
} from "../../src/indexers/user-event-reference-authority.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  createWatcherDurableRuntime,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  durableFixture,
  historyLifecycle,
  historyPointerContinuation,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";
import { expectSameArchiveBytes } from "./user-event-history.native-history-retirement-frontier.js";

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
