import "./user-event-history.local-user-event-materialized-history-synthetic-local-blocks.js";

import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import {
  DepositEvent,
  DepositInfo,
  ForcedInclusionTxV1,
  WithdrawalEvent,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { createWatcherLocalUserEventPublisher } from "../../src/indexers/user-event-history.js";
import { readWatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import { evaluateWatcherBlockReplay } from "../../src/verification/block-replay.js";
import type { WatcherCommittedEventClaim } from "../../src/verification/event-claims.js";
import { deriveWatcherLocalEventReplayAuthority } from "../../src/verification/local-event-replay-authority.js";
import { makeLocalDepositReplayFixture } from "../support/local-event-replay-fixture.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
  ordinaryLocalOrderCreation,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

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
