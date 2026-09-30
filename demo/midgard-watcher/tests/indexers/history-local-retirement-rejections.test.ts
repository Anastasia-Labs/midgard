import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation/tests/validation-fixtures";
import "@lucid-evolution/lucid";
import "@noble/hashes/blake2.js";
import "vitest";
import "../../src/indexers/user-event-history.js";
import "../../src/l1/finality-engine.js";
import "../../src/storage/durable-runtime.js";
import "../support/local-user-event-authority-fixture.js";
import "../support/user-event-origin-fixture.js";
import "./history-local-retirement-rejections.open-scenario.js";
import "./history-local-retirement-rejections.forced-terminal.js";

import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  historyLifecycle,
  ledgerReferenceIndex,
  ordinaryLocalOrderCreation,
} from "../support/local-user-event-authority-fixture.js";
import { forcedTerminal } from "./history-local-retirement-rejections.forced-terminal.js";
import {
  adjacentConstructor,
  openScenario,
  redeemersOf,
  rejectWithoutPublication,
  replaceRedeemer,
  truncatedConstructor,
} from "./history-local-retirement-rejections.open-scenario.js";

describe("local publication rejects malformed history and forced terminal redeemers", () => {
  it.each(["deposit", "withdrawal"] as const)(
    "rejects adjacent/truncated %s admission observers without archive/CAS mutation",
    async (kind) => {
      const scenario = await openScenario();
      try {
        const { fixture, initial, publisher, publish } = scenario;
        const lifecycle = historyLifecycle(initial.facts, false, { kind });
        const entries = redeemersOf(lifecycle.create);
        const observer = entries.findIndex(
          ({ tag }) => tag === CML.RedeemerTag.Reward,
        );
        expect(observer).toBeGreaterThanOrEqual(0);
        for (const [index, mutate] of [
          adjacentConstructor,
          truncatedConstructor,
        ].entries()) {
          await rejectWithoutPublication(
            scenario,
            fixture.emptySuccessorBlock,
            replaceRedeemer(
              lifecycle.create,
              observer,
              mutate(entries[observer]!.cbor),
              index + 1,
            ),
            [fixture.initializationBodyCbor],
          );
        }
        await fixture.selectCanonicalBranch(fixture.emptySuccessorBlock.point);
        const valid = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        await publish(valid);
        expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
        expect(publisher.read().snapshot.terminalEvents).toHaveLength(0);
      } finally {
        await scenario.close();
      }
    },
    120_000,
  );

  it.each(["deposit", "withdrawal", "forced_order"] as const)(
    "rejects each malformed %s terminal redeemer, then accepts the unchanged valid terminal",
    async (kind) => {
      const scenario = await openScenario();
      try {
        const { fixture, initial, publisher, publish } = scenario;
        const history =
          kind === "forced_order"
            ? null
            : historyLifecycle(initial.facts, false, {
                kind,
                withdrawalPayout: kind === "withdrawal",
              });
        const create =
          history?.create ??
          ordinaryLocalOrderCreation(
            initial.facts,
            "forced_order",
            makeNativeTx().txCbor,
          ).cbor;
        const admission = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [create],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        await publish(admission);
        expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
        const event = publisher.read().snapshot.activeEvents[0]!;
        const terminal = history ?? forcedTerminal(initial.facts, event);
        const entries = redeemersOf(terminal.consume);
        const orderIndex = ledgerReferenceIndex(
          CML.Transaction.from_cbor_hex(terminal.consume).body().inputs(),
          event.outRef,
        );
        let mutations = 0;
        for (const [index, entry] of entries.entries()) {
          if (
            kind !== "forced_order" &&
            entry.tag === CML.RedeemerTag.Mint &&
            Data.from(entry.cbor) === 0n
          )
            continue;
          if (
            kind !== "forced_order" &&
            entry.tag === CML.RedeemerTag.Spend &&
            entry.index !== orderIndex
          )
            continue;
          const mutators =
            kind !== "forced_order" && entry.tag === CML.RedeemerTag.Spend
              ? [
                  () => Data.to(99n),
                  () =>
                    CML.PlutusData.new_constr_plutus_data(
                      CML.ConstrPlutusData.new(0n, CML.PlutusDataList.new()),
                    ).to_cbor_hex(),
                ]
              : CML.PlutusData.from_cbor_hex(entry.cbor).as_list() !== undefined
                ? [() => Data.to([]), () => Data.to(0n)]
                : [adjacentConstructor, truncatedConstructor];
          for (const mutate of mutators) {
            mutations++;
            await rejectWithoutPublication(
              scenario,
              admission,
              replaceRedeemer(
                terminal.consume,
                index,
                mutate(entry.cbor),
                mutations,
              ),
              [fixture.initializationBodyCbor, terminal.settlementBody],
            );
          }
        }
        expect(mutations).toBe(kind === "deposit" ? 6 : 8);
        await fixture.selectCanonicalBranch(admission.point);
        const valid = await fixture.makeBlock({
          parent: admission,
          transactions: [terminal.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            terminal.settlementBody,
          ],
        });
        await publish(valid);
        expect(publisher.read().snapshot.activeEvents).toHaveLength(0);
        expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
        expect(publisher.read().snapshot.terminalEvents[0]).toMatchObject({
          eventId: event.eventId,
          terminalStatus:
            kind === "deposit"
              ? "absorbed"
              : kind === "withdrawal"
                ? "payout_initialized"
                : "processed",
        });
      } finally {
        await scenario.close();
      }
    },
    120_000,
  );
});
