import type * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import { canonicalJson } from "../l1/canonical-json.js";
import type { ChainSyncCursor, ChainSyncEvent } from "../l1/provider.js";
import { samePersistedCursor } from "../l1/provider.same-persisted-cursor.js";
import { committeePromiseOwnedRead } from "./promise-owned-read.js";
import type { AvailabilityResponderChallenge } from "./responder.js";

export type AvailabilityDiscoveryObservation = Readonly<{
  boundary: SDK.DaAvailabilityCanonicalBoundary &
    Readonly<{ blockHash: string; blockNo: number }>;
  cursor: ChainSyncCursor;
}>;

/** The configured native authority journals every adopted chain-sync event.
 * A generation change or a missing journal interval cannot prove continuity. */
const assertForwardInterval = async (
  before: AvailabilityDiscoveryObservation,
  after: AvailabilityDiscoveryObservation,
  replay: (sequence: number) => Promise<readonly ChainSyncEvent[]>,
): Promise<void> => {
  const start = before.cursor;
  const end = after.cursor;
  const count = end.sequence - start.sequence;
  if (
    end.rollbackGeneration !== start.rollbackGeneration ||
    end.point.network !== start.point.network ||
    end.point.providerSource !== start.point.providerSource ||
    count < 0
  )
    throw new Error("Availability discovery authority changed");
  if (count === 0) {
    if (
      !samePersistedCursor(start, end) ||
      before.boundary.blockNo !== after.boundary.blockNo
    )
      throw new Error("Availability discovery changed at an unchanged cursor");
    return;
  }
  if (after.boundary.blockNo - before.boundary.blockNo !== count)
    throw new Error(
      "Availability discovery height differs from native progress",
    );
  const events = await replay(start.sequence);
  if (events.length < count)
    throw new Error(
      "Availability discovery native journal interval is unavailable",
    );
  let point = start.point;
  for (const event of events.slice(0, count)) {
    if (
      event.direction !== "roll_forward" ||
      event.point.network !== start.point.network ||
      event.point.providerSource !== start.point.providerSource ||
      event.point.slot <= point.slot
    )
      throw new Error(
        "Availability discovery native interval is not forward-only",
      );
    point = event.point;
  }
  if (!samePersistedCursor({ ...end, point }, end))
    throw new Error(
      "Availability discovery native journal does not reach its cursor",
    );
};

const inputIdentity = (utxo: UTxO): string =>
  canonicalJson({
    address: utxo.address,
    assets: utxo.assets,
    datum: utxo.datum ?? null,
    datumHash: utxo.datumHash ?? null,
    scriptRef: utxo.scriptRef ?? null,
  });

/** Forward progress may create later obligations, but it cannot change or
 * spend the inputs of the authenticated challenges returned by this attempt. */
export const discoverConsistentAvailabilityChallenges = async (input: {
  scope?: SDK.DaAvailabilityReadScope;
  assertActuationCurrent: (
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<void>;
  readObservation: (
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<AvailabilityDiscoveryObservation>;
  replay: (sequence: number) => Promise<readonly ChainSyncEvent[]>;
  readInputs: (
    refs: readonly Readonly<{ txHash: string; outputIndex: number }>[],
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<UTxO[]>;
  discover: () => Promise<readonly AvailabilityResponderChallenge[]>;
}): Promise<readonly AvailabilityResponderChallenge[]> => {
  const { scope } = input;
  const read = committeePromiseOwnedRead(scope);
  await input.assertActuationCurrent(scope);
  const before = await input.readObservation(scope);
  const challenges = await input.discover();
  const after = await input.readObservation(scope);
  await assertForwardInterval(before, after, (sequence) =>
    read(() => input.replay(sequence)),
  );
  const expected = new Map<string, UTxO>();
  for (const challenge of challenges) {
    for (const utxo of [
      challenge.record.utxo,
      challenge.terminal.utxo,
      challenge.queue,
      ...challenge.tranches.flatMap((tranche) =>
        tranche.carrier ? [tranche.utxo, tranche.carrier] : [tranche.utxo],
      ),
    ]) {
      const ref = `${utxo.txHash}#${utxo.outputIndex}`;
      const prior = expected.get(ref);
      if (prior && inputIdentity(prior) !== inputIdentity(utxo))
        throw new Error(
          "Availability discovery contains conflicting input observations",
        );
      expected.set(ref, utxo);
    }
  }
  if (expected.size > 0) {
    const current = await read(() =>
      input.readInputs([...expected.values()], scope),
    );
    const seen = new Set<string>();
    for (const utxo of current) {
      const ref = `${utxo.txHash}#${utxo.outputIndex}`;
      const original = expected.get(ref);
      if (
        !original ||
        seen.has(ref) ||
        inputIdentity(original) !== inputIdentity(utxo)
      )
        throw new Error("Availability discovery input observation changed");
      seen.add(ref);
    }
    if (seen.size !== expected.size)
      throw new Error("Availability discovery input is no longer unspent");
  }
  await input.assertActuationCurrent(scope);
  scope?.assertCurrent();
  return challenges;
};
