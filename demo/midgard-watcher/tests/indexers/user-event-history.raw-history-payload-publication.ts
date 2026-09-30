import "./user-event-history.canonical-semantic-rollback-replacement.js";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  historyRawField,
  historyWithdrawalPayoutDatum,
} from "../../src/indexers/authenticated-event-history.js";
import {
  createWatcherLocalUserEventPublisher,
  resumeWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import { readWatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  durableFixture,
  historyLifecycle,
  historyPointerContinuation,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

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
