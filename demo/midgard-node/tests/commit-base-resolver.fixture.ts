/**
 * The commit-base resolver's fixtures: a frontier tail block spending `E0`
 * and producing `E1`, the queue tail and confirmed-state tips the commit
 * worker receives, and the outcomes the resolver's tests expect.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { expect } from "vitest";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import {
  Frontier,
  type HeaderRoot,
  insertRow,
  type LandedBlockRow,
} from "../src/landed-blocks/store.js";
import { deriveConfirmedLedgerRoots } from "../src/mpf/confirmed-ledger-roots.js";
import { verifyConfirmedLedger } from "../src/mpf/confirmed-ledger-run.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { LocalLedgerDivergenceError } from "../src/mpf/local-ledger-divergence.js";
import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import { NodeConfig } from "../src/services/config.js";
import type { Database } from "../src/services/database.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { CommitBaseUnverifiedError } from "../src/workers/commit-block-header.landed-commit-base.js";
import {
  LANDED_COMMIT_BASE_PENDING,
  resolveCommitBaseLedgerEntries,
} from "../src/workers/commit-block-header.resolve-commit-base-ledger-entries.js";
import {
  serializeStateQueueUTxO,
  type VerifiedFrontier,
} from "../src/workers/utils/commit-block-header.js";
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

/**
 * The frontier a verification of the stored `confirmed_ledger` records, as
 * the main thread's does (`verifyConfirmedLedger`), at the frontier
 * generation it read, its roots derived in this thread: none unless it
 * passes.
 */
export const verifiedNow = () =>
  run(verifyConfirmedLedger(deriveConfirmedLedgerRoots)).then(
    (outcome): VerifiedFrontier | undefined =>
      outcome.kind === "verified"
        ? { ...outcome.frontier, generation: outcome.generation }
        : undefined,
  );

/** The resolver's outcome on `tail`, as a commit worker that received
 * `verified` (by default, what a verification now records) resolves it; a
 * `verified` frontier with no generation is taken at the frontier
 * generation stored now. */
export const resolve = async (
  tail: Awaited<ReturnType<typeof tailOf>>["tail"],
  nativeMpfRoot: string,
  genesisUtxos?: readonly UTxO[],
  verified: HeaderRoot | VerifiedFrontier | "none" | "verify" = "verify",
) => {
  const verifiedFrontier =
    verified === "verify"
      ? await verifiedNow()
      : verified === "none"
        ? undefined
        : "generation" in verified
          ? verified
          : {
              ...verified,
              generation:
                (await run(Frontier.retrieveWithGeneration))?.generation ?? "1",
            };
  const resolved = Effect.either(
    resolveCommitBaseLedgerEntries({
      availableConfirmedBlock: tail,
      nativeMpfRoot,
      requireEntries: true,
      verifiedFrontier,
    }),
  );
  return run(
    genesisUtxos === undefined
      ? resolved
      : Effect.flatMap(NodeConfig, (config) =>
          Effect.provideService(resolved, NodeConfig, {
            ...config,
            GENESIS_UTXOS: [...genesisUtxos],
          }),
        ),
  );
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

/** The resolver waits for the verification of the frontier it builds from. */
export const expectUnverified = (
  outcome: Awaited<ReturnType<typeof resolve>>,
  headerHash: string,
) => {
  expect(Either.isLeft(outcome)).toBe(true);
  const error = (outcome as Either.Left<unknown, unknown>).left;
  expect(error).toBeInstanceOf(CommitBaseUnverifiedError);
  expect(error).toMatchObject({ headerHash });
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
  run(
    withFollowerWrite(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* ConfirmedLedgerDB.insertMultiple([
          ...(yield* ledgerRows(entries, new Map())),
        ]);
        yield* Frontier.upsert(frontier);
      }),
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
          [toUnit(credential.hash, SDK.STATE_QUEUE_ROOT_ASSET_NAME)]: 1n,
        },
        datum: SDK.encodeLinkedListNodeView(datum),
      },
      datum,
      assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    }),
  );
};

export const expectDivergence = (
  outcome: Awaited<ReturnType<typeof resolve>>,
  divergence: Readonly<{
    check: string;
    headerHash: string;
    root: string;
    derivedRoot?: string;
    fault?: string;
  }>,
) => {
  expect(Either.isLeft(outcome)).toBe(true);
  const error = (outcome as Either.Left<unknown, unknown>).left;
  expect(error).toBeInstanceOf(LocalLedgerDivergenceError);
  expect(error).toMatchObject(divergence);
};
