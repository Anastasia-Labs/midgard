/**
 * The production owner's correction-admission scenario on the emulator
 * (plan §7.4, §15 N4): an own block committed, confirmed and locally
 * applied, never attested, then removed on L1 by an attestation-timeout
 * correction. The node learns of the removal only from the follower's facts
 * (the landed state queue), as on a followed chain; the history owner's
 * synchronization is what runs landed-block processing and its rebase.
 *
 * `observe` reads everything the acceptance compares: the block's journal,
 * the deposits, the landed rows, the native MPF (durable root, child
 * restarts), the SQL ledger-store root, a fresh replay of the landed ledger
 * (`confirmed_ledger` plus every processed row's delta, rooted from scratch)
 * and the raised liveness reasons.
 */
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { expect, vi } from "vitest";

import {
  landedLedger,
  ledgerAfter,
  ledgerEntries,
} from "../../src/landed-blocks/ledger.js";
import { retrieveRows } from "../../src/landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../../src/mpf/ledger-hydration.js";
import { Database } from "../../src/services/database.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../../src/services/history-commit-window.js";
import {
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  SDK,
} from "../deposit-flow-emulator-shared.js";
import type { openHistoryProductionOwnerLifecycle } from "./history-production-owner-lifecycle.js";

/** Bounded wait: `work` must settle within `ms` of real time, so a wedge
 * fails here instead of hanging until the test timeout. */
const settleWithin = async <A>(work: Promise<A>, ms: number) => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  const stuck = new Promise<never>((_, reject) => {
    timer = setTimeout(
      () => reject(new Error(`Did not settle within ${ms} ms`)),
      ms,
    );
  });
  try {
    return await Promise.race([work, stuck]);
  } finally {
    clearTimeout(timer);
    work.catch(() => undefined);
  }
};

/** One history-owner synchronization, bounded, so a wedged owner fails here
 * instead of at the test timeout. */
export const synchronizeBounded = (h: {
  synchronize: () => Promise<unknown>;
}) => settleWithin(h.synchronize(), 240_000);

const read = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(program.pipe(Effect.provide(Database.layer)));

export type Lifecycle = Awaited<
  ReturnType<typeof openHistoryProductionOwnerLifecycle>
>;

/** Submit one deposit as the running node's users do; returns its L2
 * inclusion time. */
export const submitDeposit = async (h: Lifecycle, lovelace: bigint) => {
  const { fixture, lucidService } = h;
  const wallet = fixture.depositorLucid;
  lucidService.api.clearUTxOOverride();
  await ensureSeparateCollateralUtxo(wallet);
  await advanceHistoryAdmissionClock(fixture, "deposit");
  await alignCommitSchedulerBeforeTestWorker({
    fixture,
    lucidService,
    targetEndTimeMs:
      fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  });
  await h.synchronize();
  const built = await Effect.runPromise(
    SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
      l2Address: await wallet.wallet().address(),
      l2Datum: null,
      lovelace,
      additionalAssets: {},
      referenceScripts: fixture.referenceScripts.deposit,
    }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  expect(await wallet.awaitTx(await signed.submit())).toBe(true);
  await h.synchronize();
  return built.metadata.inclusionTime;
};

/** Confirm the committed block `headerHash` and apply it locally with the
 * production workers, as the running node does; it must succeed. */
export const finalizeLocally = async (h: Lifecycle, headerHash: string) => {
  const { fixture, lucidService, globals, production } = h;
  await runBlockConfirmation(
    globals,
    fixture.contracts,
    lucidService,
    production.nodeConfig,
    production,
  );
  const finalized = await runLocalFinalizationRecoveryWorker(
    globals,
    fixture.contracts,
    lucidService,
    fixture.runtimeOverrides!.deploymentIdentity,
    production.nodeConfig,
    { ...production, globals },
  );
  expect(finalized.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
  if (finalized.type !== "SuccessfulLocalFinalizationRecoveryOutput")
    throw new Error("The block must be applied locally");
  expect(finalized.finalizedHeaderHash).toBe(headerHash);
  await synchronizeBounded(h);
};

/** Commit the next block on the landed tail with the production commit
 * worker once `inclusionTime` is due, wait for L1 to take it, then confirm
 * and apply it locally; returns its header hash. */
export const commitLocallyAppliedBlock = async (
  h: Lifecycle,
  inclusionTime: number,
) => {
  const { fixture, lucidService, globals, production } = h;
  await h.deployment.chain.awaitLedgerTime(inclusionTime + 1000);
  vi.setSystemTime(fixture.emulator.now());
  await h.synchronize();
  const committed = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    ),
    nodeConfig: production.nodeConfig,
    production: { ...production, globals },
  });
  expect(await fixture.operatorLucid.awaitTx(committed.submittedTxHash)).toBe(
    true,
  );
  await h.synchronize();
  await finalizeLocally(h, committed.submittedHeaderHash);
  return committed.submittedHeaderHash;
};

const nativeOwner = async (h: Pick<Lifecycle, "globals">) => {
  const owner = await Effect.runPromise(Ref.get(h.globals.NATIVE_MPF_OWNER));
  if (owner === undefined) throw new Error("The native MPF owner is not up");
  return owner;
};

/** Everything the acceptance compares, read at once. */
export const observe = async (
  h: Pick<Lifecycle, "globals" | "landedHold">,
  headerHash: string,
) => {
  const native = await (await nativeOwner(h)).diagnostics();
  const raised = await Effect.runPromise(Ref.get(h.globals.LIVENESS_REASONS));
  const sqlState = await read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const header = Buffer.from(headerHash, "hex");
      const journal = yield* sql<{
        status: string;
        correction_transition_digest: Buffer | null;
        updated_at: Date;
      }>`SELECT status, correction_transition_digest, updated_at
        FROM pending_block_finalizations WHERE header_hash = ${header}`;
      const deposits = yield* sql<{
        status: string;
        projected_header_hash: Buffer | null;
      }>`SELECT status, projected_header_hash FROM deposits_utxos
        ORDER BY inclusion_time`;
      const [store] = yield* sql<{ root_hex: string }>`SELECT root_hex
        FROM mpf_engine_state WHERE store_name = 'ledger'`;
      const rows = yield* retrieveRows;
      // Landed-block processing bootstraps its frontier at the first merged
      // root it folds; before that there is nothing to replay.
      const landed = yield* landedLedger(rows);
      const replay =
        landed === undefined
          ? undefined
          : ledgerEntries(yield* ledgerAfter(landed.chain));
      return {
        journal:
          journal[0] === undefined
            ? undefined
            : {
                status: journal[0].status,
                corrected: journal[0].correction_transition_digest !== null,
                updatedAt: new Date(journal[0].updated_at).toISOString(),
              },
        deposits: deposits.map((row) => ({
          status: row.status,
          projectedHeader: row.projected_header_hash?.toString("hex") ?? null,
        })),
        landed: rows
          .map((row) => ({
            headerHash: row.headerHash,
            state: row.state,
            applied: row.applied,
          }))
          .sort((a, b) => (a.headerHash < b.headerHash ? -1 : 1)),
        landedTip:
          landed === undefined
            ? undefined
            : (landed.chain.at(-1)?.headerHash ?? landed.frontier.headerHash),
        sqlRoot: store?.root_hex,
        replayRoot:
          replay === undefined
            ? undefined
            : yield* computeLedgerMpfRootFromLedgerEntries(replay),
      };
    }),
  );
  return {
    ...sqlState,
    nativeRoot: native.durableRoot,
    childRestarts: native.childRestarts,
    liveness: [...raised].map(([source, reason]) => `${source}: ${reason}`),
    /** The landed-block hook's hold at the last driver run. */
    landedHold: h.landedHold(),
  };
};

export type Observed = Awaited<ReturnType<typeof observe>>;

/** The working ledger's three roots agree: native MPF, the SQL ledger-store
 * stamp, and a fresh replay of the landed ledger. Returns the root. */
export const expectOneRoot = (observed: Observed) => {
  expect(observed.sqlRoot).toBe(observed.nativeRoot);
  expect(observed.replayRoot).toBe(observed.nativeRoot);
  return observed.nativeRoot;
};
