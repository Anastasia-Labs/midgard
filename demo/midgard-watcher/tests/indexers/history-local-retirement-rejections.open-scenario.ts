import { CML } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { createWatcherLocalUserEventPublisher } from "../../src/indexers/user-event-history.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  durableFixture,
  openOrigin,
  syntheticUserEventTransaction,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

// Semantic transactions in synthetic native blocks, through the production local
// publisher. This file makes no Cardano ledger acceptance claim.
export const adjacentConstructor = (cbor: string): string => {
  const constructor =
    CML.PlutusData.from_cbor_hex(cbor).as_constr_plutus_data()!;
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(
      constructor.alternative() + 1n,
      constructor.fields(),
    ),
  ).to_cbor_hex();
};

export const truncatedConstructor = (cbor: string): string => {
  const constructor =
    CML.PlutusData.from_cbor_hex(cbor).as_constr_plutus_data()!;
  const fields = constructor.fields();
  const truncated = CML.PlutusDataList.new();
  for (let index = 0; index + 1 < fields.len(); index++)
    truncated.add(fields.get(index));
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(constructor.alternative(), truncated),
  ).to_cbor_hex();
};

export const redeemersOf = (cbor: string) => {
  const entries = CML.Transaction.from_cbor_hex(cbor)
    .witness_set()
    .redeemers()!
    .as_arr_legacy_redeemer()!;
  return Array.from({ length: entries.len() }, (_, index) => {
    const entry = entries.get(index);
    return {
      tag: entry.tag(),
      index: entry.index(),
      cbor: entry.data().to_cbor_hex(),
    };
  });
};

export const replaceRedeemer = (
  cbor: string,
  index: number,
  replacement: string,
  variant: number,
): string => {
  const transaction = CML.Transaction.from_cbor_hex(cbor);
  const entries = redeemersOf(cbor);
  entries[index] = { ...entries[index]!, cbor: replacement };
  const result = CML.Transaction.from_cbor_hex(
    syntheticUserEventTransaction(transaction.body(), entries),
  );
  const body = result.body();
  // Bind each hostile witness to distinct transaction bytes: sibling branches
  // must not collide in the local fixture's creating-transaction body cache.
  const hash = Buffer.alloc(32, 0xc8);
  hash.writeUInt32BE(variant, 28);
  body.set_script_data_hash(CML.ScriptDataHash.from_raw_bytes(hash));
  return CML.Transaction.new(
    body,
    result.witness_set(),
    true,
    undefined,
  ).to_canonical_cbor_hex();
};

export const openScenario = async () => {
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
    return {
      fixture,
      initial,
      durable,
      publisher,
      publish,
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

export const rejectWithoutPublication = async (
  scenario: Awaited<ReturnType<typeof openScenario>>,
  parent: Parameters<typeof scenario.fixture.openFinalizedBlock>[0],
  transaction: string,
  creatingBodies: string[],
) => {
  const { fixture, publisher, durable, publish } = scenario;
  await fixture.selectCanonicalBranch(parent.point);
  const snapshot = publisher.read().snapshot;
  const cas = durable.casCount();
  const before = readWatcherProtectedUserEventCheckpointReceipt(
    await readWatcherProtectedUserEventCheckpoint(durable.runtime),
  );
  const hostile = await fixture.makeBlock({
    parent,
    transactions: [transaction],
    creatingBodies,
  });
  await expect(publish(hostile)).rejects.toThrow(
    "whole-block event semantics differ",
  );
  const after = readWatcherProtectedUserEventCheckpointReceipt(
    await readWatcherProtectedUserEventCheckpoint(durable.runtime),
  );
  expect(after.checkpoint).toEqual(before.checkpoint);
  expect(after.trustedHead).toEqual(before.trustedHead);
  expect(
    Buffer.compare(Buffer.from(after.payload!), Buffer.from(before.payload!)),
  ).toBe(0);
  expect(durable.casCount()).toBe(cas);
  expect(publisher.read().snapshot).toEqual(snapshot);
};
