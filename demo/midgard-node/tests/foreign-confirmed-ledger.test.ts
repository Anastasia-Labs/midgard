import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  CML,
  Data,
  Emulator,
  Lucid as makeLucid,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as Confirmed from "../src/database/confirmedLedger.js";
import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Segments from "../src/database/foreignVerifiedSegments.js";
import * as Engine from "../src/database/mpfEngineState.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/index.js";
import { HistoryPreparation } from "../src/services/event-history-recovery.js";
import { reconcileForeignConfirmedLedger } from "../src/services/foreign-confirmed-ledger.js";
import * as LandedQueue from "../src/services/landed-state-queue.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import type { VerifiedForeignCommitBase } from "../src/workers/commit-block-header.verify-foreign-base.js";
import { countsFromLengths, headerFor } from "./da-payload.record.js";
import {
  append,
  binding,
  hash,
  refusal,
  run,
  start,
} from "./event-history-recovery-plans.registration.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

// Actual PostgreSQL authority, retained ancestry, canonical codecs and MPF
// replay; the complete current L1 queue observation is the modeled boundary.
const address = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex("ab".repeat(28))),
)
  .to_address()
  .to_bech32();
const output = Buffer.from(
  makeMidgardTxOutput(address, CML.Value.from_coin(5_000_000n)).to_cbor_bytes(),
);
const first = {
  outref: makeOutRefCbor(41),
  output,
  tx_id: Buffer.alloc(32, 41),
  address,
};
const second = {
  outref: makeOutRefCbor(42),
  output,
  tx_id: Buffer.alloc(32, 42),
  address,
};
const parent = "bb".repeat(28);
const lease = "foreign-confirmed-component";
beforeEach(() => vi.restoreAllMocks());
beforeEach(async () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE foreign_native_adoptions,foreign_verified_segments,foreign_confirmed_frontier,confirmed_ledger,mpf_engine_state CASCADE`;
    }),
  ),
);

const fixture = async (unchanged = false) => {
  const { token, checkpoint } = await start();
  let current = await append(token, checkpoint);
  const beforeRoot = await Effect.runPromise(
    computeLedgerMpfRootFromLedgerEntries([first]),
  );
  const afterRoot = unchanged
    ? beforeRoot
    : await Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([second]));
  const header = {
    ...headerFor(
      {
        utxosRoot: afterRoot,
        withdrawalsRoot: hash(0),
        forcedTransactionsRoot: hash(0),
        transactionsRoot: hash(0),
        depositsRoot: hash(0),
        transitionTraceRoot: hash(0),
        eventToStepRoot: hash(0),
        validationTracesRoot: hash(0),
      },
      countsFromLengths({ forcedTransactions: 1 }),
    ),
    prevHeaderHash: parent,
    prevUtxosRoot: beforeRoot,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const coverage = () => ({
    bindingDigest: binding.digest,
    checkpointRevision: current.revision,
    point: { id: current.head.id, slot: current.head.slot },
    snapshotDigest: current.capture.snapshotDigest,
    includedThroughMs: 1000,
  });
  const base: VerifiedForeignCommitBase = {
    authority: "recovery",
    headerHash,
    root: afterRoot,
    entries: unchanged ? [first] : [second],
    history: { token, coverage: coverage() },
    observation: [],
    verification: { status: "verified", foreignHeaderHash: headerHash },
    importedBlocks: [
      {
        kind: "foreign",
        headerCbor: Data.to(header, SDK.Header),
        headerHash,
        parentHeaderHash: parent,
        parentUtxosRoot: beforeRoot,
        root: afterRoot,
        events: unchanged
          ? [[]]
          : [
              [
                { key: first.outref.toString("hex"), output: null },
                { key: second.outref.toString("hex"), output },
              ],
            ],
        eventRoots: [afterRoot],
        ledgerKeys: unchanged ? [] : [first.outref, second.outref],
        ledgerBefore: unchanged ? [] : [first],
        memberships: { deposits: [], forcedTransactions: [], withdrawals: [] },
      },
    ],
  };
  await run(
    Authority.withRecovery(token, Segments.retainVerifiedForeignSegments(base)),
  );
  await run(Confirmed.insertMultiple([first]));
  await run(Engine.acquireLedgerStoreLease({ owner: lease, ttlMs: 60000 }));
  const contracts = await loadRealMidgardContractsForTest({
    txHash: hash(900),
    outputIndex: 0,
  });
  const api = await makeLucid(new Emulator([]), "Custom");
  const lucid = Lucid.make({
    api,
    referenceScriptsApi: api,
    operatorMainAddress: address,
    operatorMergeAddress: address,
    referenceScriptsWalletAddress: address,
    referenceScriptsAddress: address,
    submitSlotSnapshot: () =>
      Effect.fail(new Error("Not a submission fixture")),
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.void,
    switchToReferenceScriptWallet: Effect.void,
  });
  const queue = (
    selectedHeader: string,
    root: string,
  ): SDK.StateQueueUTxO[] => [
    {
      utxo: {
        txHash: hash(90),
        outputIndex: 0,
        address: contracts.stateQueue.spendingScriptAddress,
        assets: { lovelace: 2_000_000n },
      },
      datum: {
        key: "Empty",
        next: "Empty",
        data: Data.castTo(
          {
            ...SDK.makeGenesisConfirmedState(0n),
            headerHash: selectedHeader,
            utxoRoot: root,
          },
          SDK.ConfirmedState,
        ),
      },
      assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    },
  ];
  let nodes = queue(headerHash, afterRoot);
  const fetch = vi
    .spyOn(LandedQueue, "landedStateQueueUTxOs")
    .mockImplementation(() => Effect.succeed(nodes));
  const preparation = { token, assertCurrent: Effect.void };
  const reconcile = () =>
    reconcileForeignConfirmedLedger({
      coverage: coverage(),
      preparation,
      leaseOwner: lease,
    }).pipe(
      Effect.provideService(HistoryPreparation, preparation),
      Effect.provideService(Lucid, lucid),
      Effect.provideService(
        MidgardContracts,
        MidgardContracts.make({
          ...contracts,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make({
          kind: "manifest",
          manifestId: token.deploymentIdentity,
          deploymentMarker: makeDeploymentMarker(token.deploymentIdentity),
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
    );
  const state = () =>
    run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const [frontier] = yield* sql<{
          header_hash: Buffer;
        }>`SELECT header_hash FROM foreign_confirmed_frontier`;
        const entries = yield* Confirmed.retrieve;
        return {
          header: frontier?.header_hash.toString("hex"),
          keys: entries.map((entry) => entry.outref.toString("hex")),
          root: yield* computeLedgerMpfRootFromLedgerEntries(entries),
        };
      }),
    );
  return {
    reconcile,
    state,
    headerHash,
    beforeRoot,
    afterRoot,
    fetch,
    queue,
    setQueue: (value: SDK.StateQueueUTxO[]) => {
      nodes = value;
    },
    orphanSource: async () => {
      const prior = current;
      current = await append(token, current);
      await run(
        Authority.withRecovery(
          token,
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE event_history_block_applications SET canonical=false WHERE block_hash=${Buffer.from(prior.head.id, "hex")}`;
          }),
        ),
      );
    },
  };
};

describe("retained verified foreign confirmed advancement", () => {
  it("replays exact retained deltas when the merged header has left the queue", async () => {
    const f = await fixture();
    await run(f.reconcile());
    expect(await f.state()).toEqual({
      header: f.headerHash,
      keys: [second.outref.toString("hex")],
      root: f.afterRoot,
    });
    await run(f.reconcile());
    expect((await f.state()).root).toBe(f.afterRoot);
  });
  it("advances exact header identity across an unchanged root", async () => {
    const f = await fixture(true);
    await run(f.reconcile());
    expect(await f.state()).toEqual({
      header: f.headerHash,
      keys: [first.outref.toString("hex")],
      root: f.beforeRoot,
    });
  });
  it("restores the retained parent after its confirmed source is rolled back", async () => {
    const f = await fixture();
    await run(f.reconcile());
    await f.orphanSource();
    f.setQueue(f.queue(parent, f.beforeRoot));
    await run(f.reconcile());
    expect(await f.state()).toEqual({
      header: parent,
      keys: [first.outref.toString("hex")],
      root: f.beforeRoot,
    });
  });
  it("refuses forward adoption from orphaned retained source evidence", async () => {
    const f = await fixture();
    await f.orphanSource();
    await refusal(f.reconcile(), "missing current canonical segment evidence");
    expect((await f.state()).header).toBe(parent);
    expect((await f.state()).root).toBe(f.beforeRoot);
  });
  it("refuses a queue change before SQL projection", async () => {
    const f = await fixture();
    f.fetch
      .mockReturnValueOnce(Effect.succeed(f.queue(f.headerHash, f.afterRoot)))
      .mockReturnValue(Effect.succeed(f.queue(parent, f.beforeRoot)));
    await refusal(f.reconcile(), "Canonical queue changed");
    expect((await f.state()).root).toBe(f.beforeRoot);
  });
  it("refuses a target root disagreement without mutating the frontier", async () => {
    const f = await fixture();
    f.setQueue(f.queue(f.headerHash, hash(987)));
    await refusal(f.reconcile(), "freshly observed header/root");
    expect((await f.state()).header).toBe(parent);
  });
});
