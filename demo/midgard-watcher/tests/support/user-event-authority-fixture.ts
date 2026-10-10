import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { eventKeyOfId } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { type Assets, Data } from "@lucid-evolution/lucid";

import {
  admitWatcherUserEventAuthority,
  type WatcherUserEvent,
  type WatcherUserEventAuthority,
  type WatcherUserEventAuthorityRead,
  type WatcherUserEventHeaderCutoff,
  type WatcherUserEventNetwork,
} from "../../src/verification/user-event.js";
import { RULE_BUNDLE } from "./block-replay-public-fixture.committed-steps-for-effects.js";
import type { GenuineUserEventForcedPayload } from "./user-event-forced-order-fixture.js";

/**
 * Fixture user events for the W25 block-replay suites, whose subject is
 * replay, not event sourcing. Each record is the read a follower source would
 * return for an originating deposit, withdrawal or forced order; the replay
 * input mints its capability at the replayed header's own cutoff
 * ({@link admitFixtureUserEventAt}). Reading user events out of follower facts
 * is pinned by the follower source's own suites, not here.
 */

/** The deployment the replay suites' fixed rule bundle names. */
export const FIXTURE_USER_EVENT_DEPLOYMENT = Object.freeze({
  deploymentManifestId: RULE_BUNDLE.deploymentManifestId,
  blueprintHash: RULE_BUNDLE.blueprintHash,
  network: RULE_BUNDLE.network,
});

const POLICIES = Object.freeze({
  deposit: h28(0x31),
  withdrawal: h28(0x32),
  forced_order: h28(0x33),
});

/** Inclusion time every fixture event carries; replay windows bracket it. */
export const FIXTURE_INCLUSION_TIME = 1_700_000_000_000n;

/** An originating event as its follower read would report it. */
export type FixtureUserEvent = Readonly<{
  event: WatcherUserEvent;
  network: WatcherUserEventNetwork;
  /** A deposit's original assets, as the fixture deposited them; else null. */
  originalAssets: Assets | null;
}>;

const eventIdOf = (nonceByte: string) => {
  const id: SDK.OutputReference = {
    transactionId: nonceByte.repeat(32),
    outputIndex: 0n,
  };
  return { id, cborHex: Data.to(id, SDK.OutputReference) };
};

const fixtureEvent = (
  kind: WatcherUserEvent["kind"],
  nonceByte: string,
  eventCborHex: string,
  originalAssets: Assets | null,
): FixtureUserEvent => {
  const { id, cborHex } = eventIdOf(nonceByte);
  return Object.freeze({
    event: Object.freeze({
      kind,
      eventId: cborHex,
      nonceOutRef: `${id.transactionId}#0`,
      policyId: POLICIES[kind],
      assetNameHex: eventKeyOfId(Buffer.from(cborHex, "hex")).toString("hex"),
      inclusionTime: FIXTURE_INCLUSION_TIME.toString(),
      eventCborHex,
      originalAssetsCborHex:
        originalAssets === null
          ? null
          : Data.to(SDK.assetsToValue(originalAssets), SDK.Value),
      admission: Object.freeze({
        blockHash: h32(0x41),
        slot: "4000",
        blockNo: "400",
        transactionHash: id.transactionId,
        transactionIndex: "0",
        outputIndex: "0",
      }),
    }),
    network: FIXTURE_USER_EVENT_DEPLOYMENT.network,
    originalAssets:
      originalAssets === null ? null : Object.freeze({ ...originalAssets }),
  });
};

export const fixtureDepositEvent = (
  input: Readonly<{
    nonceByte: string;
    l2Address: SDK.AddressData;
    originalAssets: Assets;
  }>,
): FixtureUserEvent => {
  const { id } = eventIdOf(input.nonceByte);
  return fixtureEvent(
    "deposit",
    input.nonceByte,
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to(
        {
          id,
          info: {
            l2_address: input.l2Address,
            l2_network_id: 0n,
            l2_datum: null,
          },
        },
        SDK.DepositEvent,
      ),
    ),
    input.originalAssets,
  );
};

export const fixtureWithdrawalEvent = (
  input: Readonly<{ nonceByte: string; info: SDK.WithdrawalInfo }>,
): FixtureUserEvent => {
  const { id } = eventIdOf(input.nonceByte);
  return fixtureEvent(
    "withdrawal",
    input.nonceByte,
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to({ id, info: input.info }, SDK.WithdrawalEvent),
    ),
    null,
  );
};

export const fixtureForcedOrderEvent = (
  input: Readonly<{
    nonceByte: string;
    payload: GenuineUserEventForcedPayload;
  }>,
): FixtureUserEvent => {
  const { id } = eventIdOf(input.nonceByte);
  return fixtureEvent(
    "forced_order",
    input.nonceByte,
    Data.to(
      {
        id,
        tx: {
          tx_id: input.payload.tx_id,
          transaction_commitment: input.payload.transaction_commitment,
          submitted_source: input.payload.submitted_source,
        },
      },
      SDK.TxOrderEvent,
    ),
    null,
  );
};

/** The header binding a replay compares a capability's cutoff against. */
export type FixtureHeaderCutoff = Pick<
  WatcherUserEventHeaderCutoff,
  "headerHash" | "headerCborHex" | "observedBlockHash" | "observedSlot"
>;

/** A replayed header observation's cutoff, as a follower read scopes one. */
export const fixtureHeaderCutoff = (
  observation: Readonly<{
    headerHash: string;
    header: SDK.Header;
    chainPoint: Readonly<{ slot: bigint; blockHash: string }>;
  }>,
): FixtureHeaderCutoff =>
  Object.freeze({
    headerHash: observation.headerHash,
    headerCborHex: Data.to(observation.header, SDK.Header),
    observedBlockHash: observation.chainPoint.blockHash,
    observedSlot: observation.chainPoint.slot.toString(),
  });

/**
 * Mints the capability for `origin` read at `cutoff`. `current` lets a suite
 * retire the read, as a rewind below the cutoff does.
 */
export const admitFixtureUserEventAt = (
  origin: FixtureUserEvent,
  cutoff: FixtureHeaderCutoff,
  current: () => boolean = () => true,
): WatcherUserEventAuthority => {
  const read: WatcherUserEventAuthorityRead = Object.freeze({
    ...FIXTURE_USER_EVENT_DEPLOYMENT,
    network: origin.network,
    event: origin.event,
    throughHeader: Object.freeze({
      ...cutoff,
      queueOutRef: `${h32(0x42)}#0`,
      observedTransactionHash: h32(0x43),
      observedBlockNo: "424",
      transactionIndex: "0",
    }),
  });
  return admitWatcherUserEventAuthority({
    read: () => Promise.resolve(read),
    current,
  });
};
