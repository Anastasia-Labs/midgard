/**
 * The commit base on a foreign queue tail (plan §7.3, N3): the commit
 * builds on it only once landed-block processing replayed it to its
 * header's root, the rebase applied it and the native root is that root.
 * Otherwise it waits (`LANDED_COMMIT_BASE_PENDING`), whichever of those is
 * missing; built on, the base is the landed ledger at that header.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

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
import type { NodeConfig } from "../src/services/config.js";
import type { Database } from "../src/services/database.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import {
  LANDED_COMMIT_BASE_PENDING,
  resolveCommitBaseLedgerEntries,
} from "../src/workers/commit-block-header.resolve-commit-base-ledger-entries.js";
import { serializeStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import { simHeader } from "./helpers/state-queue-sim.fixtures.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const entry = (label: string, lovelace: bigint): Ledger.MinimalEntry => ({
  outref: makeOutRefCbor(simDigest(`base:${label}`), 0),
  output: simOutput(lovelace),
});

const E0 = entry("e0", 2_000_000n);
const E1 = entry("e1", 3_000_000n);
const FRONTIER_HASH = "f0".repeat(28);

const run = <A, E>(effect: Effect.Effect<A, E, Database | NodeConfig>) =>
  Effect.runPromise(provideDatabaseLayers(effect));

const rootOf = (entries: readonly Ledger.MinimalEntry[]) =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([...entries]));

/** The queue tail carrying `header`, as the commit worker receives it. */
const tailOf = async (header: SDK.Header) => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const assetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
  const credential = getAddressDetails(address).paymentCredential!;
  const datum: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData({
      proven_fraud: null,
      header,
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
const fixture = async (setup: Setup) => {
  const r0 = await rootOf([E0]);
  const r1 = await rootOf([E1]);
  const header: SDK.Header = {
    ...simHeader(7, FRONTIER_HASH),
    prevUtxosRoot: r0,
    utxosRoot: r1,
  };
  const { headerHash, tail } = await tailOf(header);
  await run(
    withFollowerWrite(
      Effect.gen(function* () {
        yield* resetApplicationTables;
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

const resolve = (
  tail: Awaited<ReturnType<typeof tailOf>>["tail"],
  nativeMpfRoot: string,
) =>
  run(
    Effect.either(
      resolveCommitBaseLedgerEntries({
        availableConfirmedBlock: tail,
        nativeMpfRoot,
        requireEntries: true,
      }),
    ),
  );

const expectPending = (
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

const hexEntries = (entries: readonly Ledger.MinimalEntry[]) =>
  entries.map((item) => [
    Buffer.from(item.outref).toString("hex"),
    Buffer.from(item.output).toString("hex"),
  ]);

beforeEach(async () => {
  await run(resetApplicationTables);
});

describe("commit base on a foreign queue tail", { concurrent: false }, () => {
  it("builds on the landed ledger at a processed, applied tail at the native root", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: true,
      row: {},
    });
    const outcome = await resolve(tail, r1);
    expect(Either.isRight(outcome)).toBe(true);
    const base = Either.getOrThrow(outcome);
    expect(base.source).toBe(`landed:${headerHash}`);
    expect(base.root).toBe(r1);
    expect(hexEntries(base.entries ?? [])).toEqual(hexEntries([E1]));
  });

  it("waits while no frontier exists", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: false,
      row: {},
    });
    expectPending(await resolve(tail, r1), headerHash);
  });

  it("waits while the tail is not on the processed landed chain", async () => {
    for (const row of [undefined, { state: "removed" as const }]) {
      const { tail, headerHash, r1 } = await fixture({ frontier: true, row });
      expectPending(await resolve(tail, r1), headerHash);
    }
  });

  it("waits while the landed ledger at the tail misses its header's root", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: true,
      row: { utxosRoot: "ab".repeat(32) },
    });
    expectPending(await resolve(tail, r1), headerHash);
  });

  it("waits while the native root is not the tail's root", async () => {
    const { tail, headerHash, r0 } = await fixture({
      frontier: true,
      row: {},
    });
    expectPending(await resolve(tail, r0), headerHash);
  });

  it("waits while the rebase has not applied the tail", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: true,
      row: { applied: false },
    });
    expectPending(await resolve(tail, r1), headerHash);
  });
});
