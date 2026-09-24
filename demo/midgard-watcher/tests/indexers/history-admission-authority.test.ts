import {
  EventHistoryNode,
  eventHistoryOriginalAssets,
  EventHistoryPayload,
} from "@al-ft/midgard-sdk";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { createWatcherLocalUserEventPublisher } from "../../src/indexers/user-event-history.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  durableFixture,
  historyLifecycle,
  historyPointerContinuation,
  ledgerReferenceIndex,
  openOrigin,
  ordinaryLocalOrderCreation,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

// Production local-origin/publication authority over synthetic native blocks.
// These semantic fixture transactions do not establish Cardano ledger validity.
const openScenario = async (
  kind: "deposit" | "withdrawal",
  withdrawalPayout = false,
  rawDatumCbor?: string,
) => {
  const fixture = await createSyntheticUserEventOriginFixture();
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
  const publish = async (
    block: Parameters<typeof fixture.openFinalizedBlock>[0],
  ) => {
    const pair = await fixture.openFinalizedBlock(block);
    try {
      await publisher.publish(pair);
    } finally {
      await pair.close();
    }
  };
  try {
    await publisher.publish(initial.pair);
    await initial.pair.close();
    await publish(fixture.emptySuccessorBlock);
    const options = {
      kind,
      withdrawalPayout,
      structuralLovelace: 1_000_000n,
      rawDatumCbor,
    };
    const lifecycle = historyLifecycle(initial.facts, true, options);
    const admission = await fixture.makeBlock({
      parent: fixture.emptySuccessorBlock,
      transactions: [lifecycle.create],
      creatingBodies: [fixture.initializationBodyCbor],
    });
    await publish(admission);
    return {
      fixture,
      initial,
      durable,
      publisher,
      publish,
      options,
      lifecycle,
      admission,
      close: async () => {
        publisher.close();
        await fixture.close();
      },
    };
  } catch (cause) {
    publisher.close();
    await initial.pair.close();
    await fixture.close();
    throw cause;
  }
};

describe("history admission through local watcher publication", () => {
  it.each(["deposit", "withdrawal"] as const)(
    "indexes an authenticated %s Order and preserves the forced-order witness path",
    async (kind) => {
      const scenario = await openScenario(kind);
      try {
        const { publisher, fixture, initial, admission, publish } = scenario;
        const events = publisher.read().snapshot.activeEvents;
        expect(events).toHaveLength(1);
        const event = events[0]!;
        expect(event.witnessScriptHash).toBeUndefined();
        expect(event.historyPayloadCborHex).toBeTypeOf("string");
        const node = Data.from(event.datumCborHex, EventHistoryNode);
        expect(node.payload).toHaveProperty("Order");
        expect(
          Data.from(event.historyPayloadCborHex!, EventHistoryPayload),
        ).toHaveProperty(
          kind === "deposit" ? "DepositPayload" : "WithdrawalPayload",
        );
        expect(
          eventHistoryOriginalAssets(
            node,
            { lovelace: 4_000_000n, [event.policyId + event.assetNameHex]: 1n },
            event.policyId,
          ),
        ).toEqual({ lovelace: 3_000_000n });
        const forced = ordinaryLocalOrderCreation(
          initial.facts,
          "forced_order",
          makeNativeTx().txCbor,
        );
        const block = await fixture.makeBlock({
          parent: admission,
          transactions: [forced.cbor],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        await publish(block);
        expect(
          publisher
            .read()
            .snapshot.activeEvents.find(({ kind }) => kind === "forced_order")
            ?.witnessScriptHash,
        ).toBeTypeOf("string");
      } finally {
        await scenario.close();
      }
    },
    120_000,
  );

  it("preserves deposit identity and original funds through repeated predecessor churn and authenticated retirement", async () => {
    const scenario = await openScenario("deposit");
    try {
      const { publisher, fixture, initial, lifecycle, publish } = scenario;
      const original = publisher.read().snapshot.activeEvents[0]!;
      let current = original;
      let parent = scenario.admission;
      for (let count = 0; count < 3; count++) {
        const cbor = historyPointerContinuation(
          initial.facts,
          lifecycle,
          "deposit",
          current,
        );
        const block = await fixture.makeBlock({
          parent,
          transactions: [cbor],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        await publish(block);
        const next = publisher.read().snapshot.activeEvents[0]!;
        expect(next.eventId).toBe(original.eventId);
        expect(next.originPointDigest).toBe(original.originPointDigest);
        expect(next.historyPayloadCborHex).toBe(original.historyPayloadCborHex);
        expect(next.outRef).not.toBe(current.outRef);
        expect(publisher.read().snapshot.terminalEvents).toHaveLength(0);
        current = next;
        parent = block;
      }
      const retired = historyLifecycle(initial.facts, true, {
        ...scenario.options,
        retirementOrderOutRef: current.outRef,
        retirementOrderNext:
          Data.from(current.datumCborHex, EventHistoryNode).next ?? undefined,
      });
      const block = await fixture.makeBlock({
        parent,
        transactions: [retired.consume],
        creatingBodies: [
          fixture.initializationBodyCbor,
          retired.settlementBody,
        ],
      });
      await publish(block);
      expect(publisher.read().snapshot.activeEvents).toHaveLength(0);
      expect(publisher.read().snapshot.terminalEvents[0]).toMatchObject({
        eventId: original.eventId,
        originPointDigest: original.originPointDigest,
        terminalStatus: "absorbed",
      });
    } finally {
      await scenario.close();
    }
  }, 120_000);

  it.each(
    [false, true].flatMap((payout) =>
      [false, true].map((rawMaps) => ({ payout, rawMaps })),
    ),
  )(
    "publishes withdrawal retirement with exact funds (payout: $payout, raw maps: $rawMaps)",
    async ({ payout, rawMaps }) => {
      const scenario = await openScenario(
        "withdrawal",
        payout,
        rawMaps ? "a302a20102010301000102" : undefined,
      );
      try {
        const { fixture, lifecycle, admission, publish, publisher } = scenario;
        const block = await fixture.makeBlock({
          parent: admission,
          transactions: [lifecycle.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            lifecycle.settlementBody,
          ],
        });
        await publish(block);
        expect(publisher.read().snapshot.activeEvents).toHaveLength(0);
        expect(
          publisher.read().snapshot.terminalEvents[0]?.terminalStatus,
        ).toBe(payout ? "payout_initialized" : "refunded");
        const outputs = CML.Transaction.from_cbor_hex(lifecycle.consume)
          .body()
          .outputs();
        expect(outputs.get(0).amount().coin()).toBe(3_000_000n);
        expect(outputs.get(2).amount().coin()).toBe(1_000_000n);
      } finally {
        await scenario.close();
      }
    },
    120_000,
  );

  const faults = [
    {
      name: "wrong retirement observer",
      kind: "deposit",
      options: { retirementScriptHash: "aa".repeat(28) },
    },
    {
      name: "wrong confirmed frontier reference",
      kind: "deposit",
      options: { confirmedReferenceIndex: 0n },
    },
    {
      name: "missing structural refund claim",
      kind: "deposit",
      options: { omitStructuralRefundClaim: true },
    },
    {
      name: "deposit amount substitution",
      kind: "deposit",
      options: { fundsLovelaceOffset: 1n },
    },
    {
      name: "withdrawal amount substitution",
      kind: "withdrawal",
      options: { fundsLovelaceOffset: -1n },
    },
    {
      name: "refund address substitution",
      kind: "withdrawal",
      options: { fundsAddressHex: `60${"aa".repeat(28)}` },
    },
    {
      name: "refund datum substitution",
      kind: "withdrawal",
      options: { fundsDatumCborOverride: "00" },
    },
    {
      name: "wrong payout retirement redeemer",
      kind: "withdrawal",
      payout: true,
      options: { payoutRetirementRedeemerIndex: 0n },
    },
    {
      name: "payout datum substitution",
      kind: "withdrawal",
      payout: true,
      options: { fundsDatumCborOverride: "00" },
    },
  ] as const;
  it.each(
    faults.flatMap((fault) =>
      [false, true].map((rawMaps) => ({ ...fault, rawMaps })),
    ),
  )(
    "rejects $name without advancing archive/CAS (raw maps: $rawMaps)",
    async (fault) => {
      const scenario = await openScenario(
        fault.kind,
        "payout" in fault && fault.payout,
        fault.rawMaps ? "a302a20102010301000102" : undefined,
      );
      try {
        const { fixture, initial, durable, publisher, admission, publish } =
          scenario;
        const invalid = historyLifecycle(initial.facts, true, {
          ...scenario.options,
          ...fault.options,
          ...(fault.name === "wrong confirmed frontier reference"
            ? {
                confirmedReferenceIndex: ledgerReferenceIndex(
                  CML.Transaction.from_cbor_hex(scenario.lifecycle.consume)
                    .body()
                    .reference_inputs()!,
                  initial.facts.activation.hubOutRef,
                ),
              }
            : {}),
        });
        const snapshot = publisher.read().snapshot;
        const cas = durable.casCount();
        const before = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        );
        const block = await fixture.makeBlock({
          parent: admission,
          transactions: [invalid.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            invalid.settlementBody,
          ],
        });
        await expect(publish(block)).rejects.toThrow(
          "whole-block event semantics differ",
        );
        const after = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        );
        expect(after.checkpoint).toEqual(before.checkpoint);
        expect(after.trustedHead).toEqual(before.trustedHead);
        expect(
          Buffer.compare(
            Buffer.from(after.payload!),
            Buffer.from(before.payload!),
          ),
        ).toBe(0);
        expect(durable.casCount()).toBe(cas);
        expect(publisher.read().snapshot).toEqual(snapshot);
      } finally {
        await scenario.close();
      }
    },
    120_000,
  );
});
