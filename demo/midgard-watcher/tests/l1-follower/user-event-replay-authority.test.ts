/**
 * `deriveWatcherUserEventReplayAuthority` over capabilities read from
 * follower facts: a deposit, a withdrawal and a forced order admitted in a
 * state-queue commit's block before the commit, each bound to its committed
 * claim. Refused on a claim that differs from the follower's event, on a
 * copied capability, and once a rewind or close retires the capability.
 */
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  ForcedInclusionTxV1,
  WithdrawalEvent,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import type { WatcherCommittedEventClaim } from "../../src/verification/event-claims.js";
import {
  readWatcherUserEventAuthority,
  type WatcherUserEventAuthority,
} from "../../src/verification/user-event.js";
import { deriveWatcherUserEventReplayAuthority } from "../../src/verification/user-event-replay-authority.js";
import {
  type FollowerUserEvents,
  followerUserEventsDeployment,
  forcedOrderTransaction,
  listOrderTransaction,
  openFollowerUserEvents,
  syntheticChain,
  transactionHash,
  userEventId,
} from "../support/follower-user-events-fixture.js";
import {
  commitTransaction,
  createSyntheticStateQueueHeader,
  initializationTransaction,
} from "../support/state-queue-observation-fixture.commit-transaction.js";
import { genuineUserEventForcedPayloadForCanonicalTx } from "../support/user-event-forced-order-fixture.js";

const deployment = followerUserEventsDeployment();
const ID = Object.freeze({
  deposit: userEventId("d1"),
  withdrawal: userEventId("e1"),
  forced: userEventId("f1"),
});
const native = makeNativeTx();
const submittedCbor = encodeMidgardForcedTxCanonical(native.tx);
const forcedPayload =
  genuineUserEventForcedPayloadForCanonicalTx(submittedCbor);

type Mutable<T> = { -readonly [K in keyof T]: T[K] };

const opened: FollowerUserEvents[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((follower) => follower.close()));
});

/** Init, then the commit's block: deposit, withdrawal, forced order, commit. */
const capabilities = async () => {
  const follower = await openFollowerUserEvents({ deployment });
  opened.push(follower);
  const chain = syntheticChain();
  const initializationTx = initializationTransaction(deployment.authority);
  const initializationBlock = chain.next([initializationTx]);
  chain.next([
    listOrderTransaction(deployment, "deposit", ID.deposit),
    listOrderTransaction(deployment, "withdrawal", ID.withdrawal),
    forcedOrderTransaction(deployment, ID.forced, forcedPayload),
    commitTransaction(
      deployment.authority,
      transactionHash(initializationTx),
      createSyntheticStateQueueHeader(),
    ),
  ]);
  chain.empties(RELEASE_FINALITY_DEPTH - 1);
  await follower.apply(chain.blocks);
  const [header] = (await follower.observe()).finalizedHeaders;
  if (header === undefined) throw new Error("the commit was not observed");
  const read = (
    kind: "deposit" | "withdrawal" | "forced_order",
    eventId: string,
  ): Promise<WatcherUserEventAuthority> =>
    follower.userEvents.eventAuthority({
      kind,
      eventId,
      throughHeader: header,
    });
  const deposit = await read("deposit", ID.deposit.cborHex);
  const withdrawal = await read("withdrawal", ID.withdrawal.cborHex);
  const forced = await read("forced_order", ID.forced.cborHex);
  const depositEvent = (await readWatcherUserEventAuthority(deposit)).event;
  const withdrawalEvent = (await readWatcherUserEventAuthority(withdrawal))
    .event;
  const depositClaim: WatcherCommittedEventClaim = {
    phase: "Deposit",
    eventIdCborHex: ID.deposit.cborHex,
    valueCborHex: aikenSerialisedPlutusDataCborPreservingMapOrder(
      plutusConstrFieldCbor(depositEvent.eventCborHex, [1]),
    ),
    canonicalNativeTxCborHex: null,
  };
  // The committed value is the event's info with the operator's validity,
  // in the event's own (map-order preserving) encoding.
  const withdrawalClaim = (
    validity: "WithdrawalIsValid" | "NonExistentWithdrawalUtxo",
  ): WatcherCommittedEventClaim => ({
    phase: "Withdrawal",
    eventIdCborHex: ID.withdrawal.cborHex,
    valueCborHex: aikenSerialisedPlutusDataCborPreservingMapOrder(
      replacePlutusConstrFieldCbor(
        plutusConstrFieldCbor(withdrawalEvent.eventCborHex, [1]),
        [2],
        plutusConstrFieldCbor(
          Data.to(
            {
              ...Data.from(withdrawalEvent.eventCborHex, WithdrawalEvent).info,
              validity,
            },
            WithdrawalInfo,
          ),
          [2],
        ),
      ),
    ),
    canonicalNativeTxCborHex: null,
  });
  const forcedClaim: WatcherCommittedEventClaim = {
    phase: "ForcedTransaction",
    eventIdCborHex: ID.forced.cborHex,
    valueCborHex: Data.to(
      {
        tx_id: forcedPayload.tx_id,
        submitted_source: forcedPayload.submitted_source,
        verdict: "ForcedTxValid",
      },
      ForcedInclusionTxV1,
    ),
    canonicalNativeTxCborHex: submittedCbor.toString("hex"),
  };
  const claims = {
    deposit: depositClaim,
    withdrawal: withdrawalClaim,
    forced: forcedClaim,
  };
  return {
    follower,
    initializationBlock,
    capability: { deposit, withdrawal, forced },
    claims,
  };
};

describe("user-event replay authority over follower facts", () => {
  it("derives deposit, withdrawal and forced replay inputs from the follower's events, snapshotting its inputs", async () => {
    const { capability, claims } = await capabilities();

    const depositClaim: Mutable<WatcherCommittedEventClaim> = {
      ...claims.deposit,
    };
    const pendingDeposit = deriveWatcherUserEventReplayAuthority({
      userEvent: capability.deposit,
      committedClaim: depositClaim,
      programMaterial: [],
    });
    depositClaim.eventIdCborHex = "00";
    depositClaim.valueCborHex = "00";
    const deposit = await pendingDeposit;
    expect(deposit).toMatchObject({
      phase: "Deposit",
      userEvent: capability.deposit,
      eventKey: { DepositEventKey: { deposit_id: ID.deposit.id } },
    });
    if (deposit.phase !== "Deposit") throw new Error("not a deposit");
    expect(deposit.transitionEffect.operations).toHaveLength(1);
    expect(deposit.transitionEffect.operations[0]!.type).toBe("insert");

    for (const validity of [
      "WithdrawalIsValid",
      "NonExistentWithdrawalUtxo",
    ] as const) {
      const withdrawal = await deriveWatcherUserEventReplayAuthority({
        userEvent: capability.withdrawal,
        committedClaim: claims.withdrawal(validity),
        programMaterial: [],
      });
      if (withdrawal.phase !== "Withdrawal")
        throw new Error("not a withdrawal");
      expect(withdrawal.eventKey).toEqual({
        WithdrawalEventKey: { withdrawal_id: ID.withdrawal.id },
      });
      // The fixture's withdrawal names its own event id as its L2 outref.
      expect(withdrawal.transitionEffect.operations).toEqual(
        validity === "WithdrawalIsValid"
          ? [
              {
                type: "delete",
                outRefCbor: encodeMidgardSpendInputItem({
                  txId: Buffer.from(ID.withdrawal.id.transactionId, "hex"),
                  outputIndex: Number(ID.withdrawal.id.outputIndex),
                }),
              },
            ]
          : [],
      );
    }

    const forcedClaim: Mutable<WatcherCommittedEventClaim> = {
      ...claims.forced,
    };
    const material: [string, string][] = [];
    const pendingForced = deriveWatcherUserEventReplayAuthority({
      userEvent: capability.forced,
      committedClaim: forcedClaim,
      programMaterial: material,
    });
    forcedClaim.canonicalNativeTxCborHex = "00";
    forcedClaim.valueCborHex = "00";
    material.push(["00", "00"]);
    const forced = await pendingForced;
    expect(forced).toMatchObject({
      phase: "ForcedTransaction",
      eventKey: { ForcedTransactionEventKey: { tx_order_id: ID.forced.id } },
      canonicalNativeTxCbor: submittedCbor,
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
    });
    expect("transitionEffect" in forced).toBe(false);
  });

  it("refuses a claim that differs from the follower's event", async () => {
    const { capability, claims } = await capabilities();
    const refusals: readonly [
      WatcherUserEventAuthority,
      WatcherCommittedEventClaim,
      RegExp,
    ][] = [
      [
        capability.deposit,
        { ...claims.deposit, eventIdCborHex: ID.withdrawal.cborHex },
        /committed event key differs/u,
      ],
      [
        capability.deposit,
        {
          ...claims.deposit,
          valueCborHex: claims.withdrawal("WithdrawalIsValid").valueCborHex,
        },
        /committed deposit differs/u,
      ],
      [
        capability.deposit,
        {
          ...claims.deposit,
          canonicalNativeTxCborHex: submittedCbor.toString("hex"),
        },
        /deposit claim carries forced native transaction bytes/u,
      ],
      [
        capability.withdrawal,
        {
          ...claims.withdrawal("WithdrawalIsValid"),
          canonicalNativeTxCborHex: submittedCbor.toString("hex"),
        },
        /withdrawal claim carries forced native transaction bytes/u,
      ],
      [
        capability.withdrawal,
        { ...claims.deposit, eventIdCborHex: ID.withdrawal.cborHex },
        /committed event phase differs/u,
      ],
      [
        capability.forced,
        { ...claims.forced, canonicalNativeTxCborHex: "00" },
        /./u, // the forced codec refuses the bytes
      ],
      [
        capability.forced,
        { ...claims.forced, eventIdCborHex: ID.deposit.cborHex },
        /committed event key differs/u,
      ],
      [
        capability.forced,
        {
          ...claims.forced,
          canonicalNativeTxCborHex: encodeMidgardForcedTxCanonical(
            makeNativeTx({ fee: 2n }).tx,
          ).toString("hex"),
        },
        /committed forced source differs/u,
      ],
    ];
    for (const [userEvent, committedClaim, message] of refusals)
      await expect(
        deriveWatcherUserEventReplayAuthority({
          userEvent,
          committedClaim,
          programMaterial: [],
        }),
      ).rejects.toThrow(message);
  });

  it("refuses a copied capability, and one a rewind or close retired", async () => {
    const { follower, initializationBlock, capability, claims } =
      await capabilities();
    await expect(
      deriveWatcherUserEventReplayAuthority({
        userEvent: { ...capability.deposit },
        committedClaim: claims.deposit,
        programMaterial: [],
      }),
    ).rejects.toThrow(/not admitted/u);

    // Rewinding to the Init block removes the commit's block, the cutoff.
    await follower.rewindTo(initializationBlock.point);
    for (const [userEvent, committedClaim] of [
      [capability.deposit, claims.deposit],
      [capability.forced, claims.forced],
    ] as const)
      await expect(
        deriveWatcherUserEventReplayAuthority({
          userEvent,
          committedClaim,
          programMaterial: [],
        }),
      ).rejects.toThrow(/retired by an L1 rewind/u);

    // A close while a derivation is in flight retires it before it returns.
    const closing = await capabilities();
    const inFlight = deriveWatcherUserEventReplayAuthority({
      userEvent: closing.capability.withdrawal,
      committedClaim: closing.claims.withdrawal("WithdrawalIsValid"),
      programMaterial: [],
    });
    closing.follower.userEvents.close();
    await expect(inFlight).rejects.toThrow();
    await expect(
      deriveWatcherUserEventReplayAuthority({
        userEvent: closing.capability.forced,
        committedClaim: closing.claims.forced,
        programMaterial: [],
      }),
    ).rejects.toThrow();
  });
});
