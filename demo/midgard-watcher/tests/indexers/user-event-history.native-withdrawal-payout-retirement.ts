import "./user-event-history.bounded-local-user-event-semantic-publication-synthetic-local-blocks.js";

import { h32 } from "@al-ft/midgard-test-support/hex";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  resumeWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import {
  assertWatcherLocalUserEventAuthorityCurrent,
  readWatcherLocalUserEventAuthority,
} from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  createWatcherDurableRuntime,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";
import { expectSameArchiveBytes } from "./user-event-history.native-history-retirement-frontier.js";

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
          CML.ScriptDataHash.from_hex(h32(index === 0 ? 0xc8 : 0xc9)),
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
