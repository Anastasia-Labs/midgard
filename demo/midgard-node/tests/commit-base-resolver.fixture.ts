/**
 * The commit-base resolver's fixtures: a frontier tail block spending `E0`
 * and producing `E1`, the queue tail and confirmed-state tips the commit
 * worker receives, and the outcomes the resolver's tests expect.
 */
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger as StoreLedger,
  ledgerAt,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { expect } from "vitest";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import {
  Frontier,
  insertRow,
  type LandedBlockRow,
} from "../src/landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import { NodeConfig } from "../src/services/config.make-config.js";
import type { Database } from "../src/services/database.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import {
  LANDED_COMMIT_BASE_PENDING,
  resolveCommitBase,
} from "../src/workers/commit-block-header.resolve-commit-base.js";
import { serializeStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import { simHeader } from "./helpers/state-queue-sim.fixtures.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

export const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const entry = (label: string, lovelace: bigint): Ledger.MinimalEntry => ({
  outref: makeOutRefCbor(simDigest(`base:${label}`), 0),
  output: simOutput(lovelace),
});

export const E0 = entry("e0", 2_000_000n);
export const E1 = entry("e1", 3_000_000n);
export const FRONTIER_HASH = "f0".repeat(28);

export const run = <A, E>(effect: Effect.Effect<A, E, Database | NodeConfig>) =>
  Effect.runPromise(provideDatabaseLayers(effect));

export const rootOf = (entries: readonly Ledger.MinimalEntry[]) =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([...entries]));

/**
 * The ledger store a commit reads its base from, as the native owner holds
 * it: the ledgers of `ledgers` admitted at their roots. `openLedger` opens
 * one at a root, its nodes from the store and its outputs from the reader
 * the resolver gives it.
 */
export const ledgerStoreOf = async (
  ledgers: readonly (readonly Ledger.MinimalEntry[])[],
) => {
  const storage = createMemoryLedgerStorage();
  const store = createLedgerStore(storage);
  for (const entries of ledgers)
    await store.admit(
      entries.map((item) => ({ outRef: item.outref, output: item.output })),
      await rootOf(entries),
    );
  return (
    root: string,
    outputs: Parameters<typeof ledgerAt>[1]["outputs"],
  ): StoreLedger =>
    ledgerAt(root, {
      node: (hash) => storage.readNodes([hash]).then(([node]) => node),
      outputs,
    });
};

/** Every entry of `ledger`, as out-ref and output hex, sorted as
 * `hexEntries(...).sort()` is. */
export const ledgerHex = async (ledger: StoreLedger) => {
  const entries: string[][] = [];
  for await (const { outRef, output } of ledger.entries())
    entries.push([outRef.toString("hex"), output.toString("hex")]);
  return entries.sort();
};

/** The queue tail carrying `header`, as the commit worker receives it. */
export const tailOf = async (header: SDK.Header) => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const assetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
  const credential = getAddressDetails(address).paymentCredential!;
  const datum: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData({
      proven_fraud: null,
      header,
      block_time: header.endTime,
      da_attestation: SDK.NO_DA_ATTESTATION,
    }) as SDK.LinkedListNodeView["data"],
  };
  return {
    headerHash,
    tail: await Effect.runPromise(
      serializeStateQueueUTxO({
        utxo: {
          txHash: "74".repeat(32),
          outputIndex: 0,
          address,
          assets: {
            lovelace: 3_000_000n,
            [toUnit(credential.hash, assetName)]: 1n,
          },
          datum: SDK.encodeLinkedListNodeView(datum),
        },
        datum,
        assetName,
      }),
    ),
  };
};

type Setup = Readonly<{
  /** Write the frontier (`confirmed_ledger` at it holds `E0`). */
  frontier: boolean;
  /** The landed row for the tail, if any. */
  row?: Partial<LandedBlockRow>;
}>;

/** A tail block spending `E0` and producing `E1` on the frontier. */
export const fixture = async (setup: Setup) => {
  const r0 = await rootOf([E0]);
  const r1 = await rootOf([E1]);
  const header: SDK.Header = {
    ...simHeader(7, FRONTIER_HASH),
    prevUtxosRoot: r0,
    utxosRoot: r1,
  };
  const { headerHash, tail } = await tailOf(header);
  // Reset before taking the gate: the worker's database may hold a follower
  // view an earlier file applied, and the gate refuses a fixture write while
  // one is applied.
  await run(resetApplicationTables);
  await run(
    withFollowerWrite(
      Effect.gen(function* () {
        yield* ConfirmedLedgerDB.insertMultiple([
          ...(yield* ledgerRows([E0], new Map())),
        ]);
        if (setup.frontier)
          yield* Frontier.upsert({ headerHash: FRONTIER_HASH, utxosRoot: r0 });
        if (setup.row !== undefined)
          yield* insertRow({
            headerHash,
            parentHeaderHash: FRONTIER_HASH,
            parentUtxosRoot: r0,
            utxosRoot: r1,
            kind: "foreign",
            state: "processed",
            applied: true,
            spent: [E0.outref],
            produced: [E1],
            depositIds: [],
            withdrawals: [],
            forcedIds: [],
            txIds: [],
            ...setup.row,
          });
      }),
    ),
  );
  return { headerHash, tail, r0, r1 };
};

/** The resolver's outcome on `tail`, as a commit worker resolves it, its
 * ledgers from a store holding `E0`'s and `E1`'s (or `ledgers`'), with the
 * base ledger's entries as `ledgerHex` gives them. */
export const resolve = async (
  tail: Awaited<ReturnType<typeof tailOf>>["tail"],
  nativeMpfRoot: string,
  {
    ledgers = [[E0], [E1]],
  }: Readonly<{
    ledgers?: readonly (readonly Ledger.MinimalEntry[])[];
  }> = {},
) => {
  // The base's entries are read while the database the resolver read from
  // is still open, as a commit reads its base inside the worker's program.
  const resolved = Effect.either(
    resolveCommitBase({
      availableConfirmedBlock: tail,
      nativeMpfRoot,
      openLedger: await ledgerStoreOf(ledgers),
    }).pipe(
      Effect.flatMap((base) =>
        Effect.promise(() => ledgerHex(base.ledger)).pipe(
          Effect.map((entries) => ({ ...base, entries })),
        ),
      ),
    ),
  );
  return run(resolved);
};

export const expectPending = (
  outcome: Awaited<ReturnType<typeof resolve>>,
  headerHash: string,
) => {
  expect(Either.isLeft(outcome)).toBe(true);
  const error = (outcome as Either.Left<unknown, unknown>).left;
  expect(error).toBeInstanceOf(ForeignBlockVerificationError);
  expect(error).toMatchObject({
    foreignHeaderHash: headerHash,
    reason: "missing",
    detail: LANDED_COMMIT_BASE_PENDING,
  });
};

export const hexEntries = (entries: readonly Ledger.MinimalEntry[]) =>
  entries.map((item) => [
    Buffer.from(item.outref).toString("hex"),
    Buffer.from(item.output).toString("hex"),
  ]);

/** `confirmed_ledger` holds `entries` and the frontier is `frontier`. */
export const seed = (
  entries: readonly Ledger.MinimalEntry[],
  frontier: Readonly<{ headerHash: string; utxosRoot: string }>,
) =>
  // Reset before taking the gate, as `fixture` does.
  run(
    resetApplicationTables.pipe(
      Effect.zipRight(
        withFollowerWrite(
          Effect.gen(function* () {
            yield* ConfirmedLedgerDB.insertMultiple([
              ...(yield* ledgerRows(entries, new Map())),
            ]);
            yield* Frontier.upsert(frontier);
          }),
        ),
      ),
    ),
  );

/** The state-queue root carrying the confirmed state `state`, a tip with no
 * block after it. */
export const confirmedTipOf = (state: SDK.ConfirmedState) => {
  const datum: SDK.LinkedListNodeView = {
    key: "Empty",
    next: "Empty",
    data: SDK.castConfirmedStateToData(state) as SDK.LinkedListNodeView["data"],
  };
  const credential = getAddressDetails(address).paymentCredential!;
  return Effect.runPromise(
    serializeStateQueueUTxO({
      utxo: {
        txHash: "75".repeat(32),
        outputIndex: 0,
        address,
        assets: {
          lovelace: 3_000_000n,
          [toUnit(credential.hash, SDKStateQueue.STATE_QUEUE_ROOT_ASSET_NAME)]:
            1n,
        },
        datum: SDK.encodeLinkedListNodeView(datum),
      },
      datum,
      assetName: SDKStateQueue.STATE_QUEUE_ROOT_ASSET_NAME,
    }),
  );
};
