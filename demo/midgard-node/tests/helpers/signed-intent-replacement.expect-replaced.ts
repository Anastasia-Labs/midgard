import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, Ref } from "effect";
import { expect, vi } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { reconcileStateQueueCorrections } from "../../src/fibers/attestation-timeout-correction.js";
import type { LedgerSnapshotOutput } from "../../src/l1-ledger-snapshot.js";
import {
  journalAbandonment,
  signedIntentReplacementDigest,
} from "../../src/services/canonical-journal-recovery.js";
import { Database } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import {
  read,
  readJournal,
  readLocalFinalizationJob,
  readSqlLedgerRoot,
} from "./correction-rewind-scenario.js";
import { makeStreamingHistoryTransport } from "./history-source-owner-emulator.js";
import {
  C,
  type Handle,
  nativeRoot,
  readDepositHeader,
  readLeaseStatus,
  readMempoolTxIds,
  readPlans,
  SIGNED_INTENT_RELEASE_DOMAIN,
  signedTtl,
} from "./signed-intent-replacement.admit-two-funded-transfers.js";

/** The durable and in-memory state of a replaced journal: abandoned under
 * its replacement digest (its signed content kept), its job row and lease
 * retired, the native root and SQL marker back at its base, its members
 * reopened. `globalsReset` is false when a revival followed in the same
 * reconciliation. */
export const expectReplaced = async (
  journal: Pending.Record,
  { globalsReset = true, handle }: { globalsReset?: boolean; handle: Handle },
) => {
  const header = journal[C.HEADER_HASH].toString("hex");
  const replaced = await readJournal(header);
  expect(replaced[C.STATUS]).toBe(Pending.Status.Abandoned);
  expect(replaced[C.CORRECTION_TRANSITION_DIGEST]).toBe(
    signedIntentReplacementDigest(journal),
  );
  expect(journalAbandonment(replaced)).toBe("replacement");
  expect(replaced[C.SIGNED_TX_CBOR]).toEqual(journal[C.SIGNED_TX_CBOR]);
  expect(replaced[C.INTENDED_TX_HASH]).toEqual(journal[C.INTENDED_TX_HASH]);
  const plans = await readPlans();
  const plan = plans.find(({ intent }) => intent.headerHash === header);
  expect(plan?.state).toBe("applied");
  expect(plan?.intent.domain).toBe(SIGNED_INTENT_RELEASE_DOMAIN);
  expect(plan?.intent.targetRoot).toBe(journal[C.BASE_UTXOS_ROOT]);
  expect(await readLocalFinalizationJob(header)).toBeUndefined();
  expect(await readLeaseStatus(journal[C.STATE_QUEUE_LEASE_TOKEN])).not.toBe(
    "active",
  );
  if (!globalsReset) return;
  expect(await nativeRoot(handle)).toBe(journal[C.BASE_UTXOS_ROOT]);
  expect((await readSqlLedgerRoot()).root_hex).toBe(journal[C.BASE_UTXOS_ROOT]);
  const g = handle.globals;
  expect(Effect.runSync(Ref.get(g.LOCAL_FINALIZATION_PENDING))).toBe(false);
  expect(Effect.runSync(Ref.get(g.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH))).toBe(
    "",
  );
  expect(Effect.runSync(Ref.get(g.AVAILABLE_LOCAL_FINALIZATION_BLOCK))).toBe(
    "",
  );
  const mempool = await readMempoolTxIds();
  for (const id of journal.mempoolTxIds)
    expect(mempool).toContain(id.toString("hex"));
  for (const eventId of journal.depositEventIds)
    expect(await readDepositHeader(eventId)).toBeNull();
};

/**
 * Lands a signed commit on the emulator as a fork on which it was included
 * inside its validity window would have it: the emulator cannot roll back,
 * so its slot is moved back to `ttl - 1` for the submission alone (where the
 * ledger rules and the state-queue validator accept it) and then restored;
 * the next emulator block includes it. The node's source then serves that
 * chain, so its authenticated view shows the commit holding its base's
 * state-queue slot, as after a shallow rollback to that fork. The history
 * journal itself is not rolled back.
 */
export const landSignedCommitAsFork = async (
  handle: Handle,
  signedCbor: Buffer,
) => {
  const { emulator } = handle.fixture;
  const ttl = signedTtl(signedCbor);
  const saved = {
    slot: emulator.slot,
    time: emulator.time,
    blockHeight: emulator.blockHeight,
  };
  expect(saved.slot).toBeGreaterThanOrEqual(ttl);
  emulator.slot = ttl - 1;
  emulator.time = saved.time - (saved.slot - emulator.slot) * 1000;
  let txHash: string;
  try {
    txHash = await emulator.submitTx(signedCbor.toString("hex"));
  } finally {
    emulator.slot = saved.slot;
    emulator.time = saved.time;
    emulator.blockHeight = saved.blockHeight;
  }
  expect(await handle.fixture.operatorLucid.awaitTx(txHash)).toBe(true);
  expect(
    (await handle.fixture.operatorLucid.transactionStatus(txHash)).status,
  ).toBe("confirmed");
  vi.setSystemTime(new Date(emulator.now()));
  return txHash;
};

/** The state-queue outputs a signed commit creates (its base's continuation
 * and its own node), as a chain that included it would serve them. */
export const signedCommitQueueOutputs = (
  h: Pick<Handle, "fixture">,
  journal: Pending.Record,
): LedgerSnapshotOutput[] => {
  const { policyId } = h.fixture.contracts.stateQueue;
  const tx = CML.Transaction.from_cbor_bytes(journal[C.SIGNED_TX_CBOR]!);
  const body = tx.body();
  const outputs = Array.from({ length: body.outputs().len() }, (_, index) =>
    coreToTxOutput(body.outputs().get(index)),
  );
  body.free();
  tx.free();
  const txHash = journal[C.INTENDED_TX_HASH]!.toString("hex");
  return outputs.flatMap((output, outputIndex) =>
    Object.keys(output.assets).some((unit) => unit.startsWith(policyId))
      ? [
          {
            txHash,
            outputIndex,
            address: output.address,
            assets: { ...output.assets },
            ...(output.datum == null ? {} : { datum: output.datum }),
            hasReferenceScript: output.scriptRef != null,
          },
        ]
      : [],
  );
};

/** The queue view of a chain on which `journal`'s signed commit took its
 * base's slot: the base output is replaced by the commit's queue outputs. */
export const landedCommitView = (
  h: Pick<Handle, "fixture">,
  journal: Pending.Record,
) => {
  const { policyId } = h.fixture.contracts.stateQueue;
  const created = signedCommitQueueOutputs(h, journal);
  expect(created).toHaveLength(2);
  const base = journal[C.BASE_TAIL_HEADER_HASH].toString("hex");
  const baseUnit = [
    policyId + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + base,
    policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME,
  ].find((unit) => created.some((output) => output.assets[unit] === 1n));
  if (baseUnit === undefined)
    throw new Error("The signed commit does not continue its base");
  return (outputs: readonly LedgerSnapshotOutput[]) => {
    const tails = outputs.filter((output) => output.assets[baseUnit] === 1n);
    expect(tails).toHaveLength(1);
    return [...outputs.filter((output) => output !== tails[0]), ...created];
  };
};

/**
 * A history transport whose served state-queue outputs a test may rewrite
 * for points not yet sealed: drives the authenticated view directly for a
 * chain the emulator cannot produce (a block this node has no journal for in
 * its base's slot). Every other output and every transaction is the
 * recorder's.
 */
export const makeRewritableQueueTransport = () => {
  let rewrite:
    | ((outputs: readonly LedgerSnapshotOutput[]) => LedgerSnapshotOutput[])
    | undefined;
  return {
    setRewrite: (next: typeof rewrite) => {
      rewrite = next;
    },
    transportFactory: (
      recorded: Parameters<typeof makeStreamingHistoryTransport>[0],
    ) => {
      let seen = recorded.batches.length;
      const transport = makeStreamingHistoryTransport(recorded);
      const append = transport.appendAccepted;
      transport.appendAccepted = () => {
        for (const batch of recorded.batches.slice(seen))
          if (rewrite !== undefined)
            (batch as { outputs: readonly LedgerSnapshotOutput[] }).outputs =
              rewrite(batch.outputs);
        seen = recorded.batches.length;
        return append();
      };
      return transport;
    },
  };
};

/** The emulator's current state queue as the correction observer sees it. */
export const readEmulatorQueue = async (
  h: Pick<Handle, "fixture">,
): Promise<SDK.StateQueueTransitionNode[]> =>
  Promise.all(
    (
      await Effect.runPromise(
        SDK.fetchSortedStateQueueUTxOsProgram(h.fixture.operatorLucid, {
          stateQueueAddress:
            h.fixture.contracts.stateQueue.spendingScriptAddress,
          stateQueuePolicyId: h.fixture.contracts.stateQueue.policyId,
        }),
      )
    ).map(async (node, index) => ({
      headerHash:
        index === 0
          ? null
          : await Effect.runPromise(SDK.headerHashFromStateQueueUTxO(node)),
      outRef: `${node.utxo.txHash}#${node.utxo.outputIndex}`,
    })),
  );

/** Bootstrap the production correction observer's cursor at `queue` (the
 * emulator's current queue by default), as the running node's correction
 * fiber does on its first tick: the observer row is replaced. */
export const seedCorrectionObserver = async (
  h: Handle,
  queue?: readonly SDK.StateQueueTransitionNode[],
) => {
  const start = queue ?? (await readEmulatorQueue(h));
  const identity = h.fixture.runtimeOverrides!.deploymentIdentity;
  const manifestId = identity.manifestId;
  if (manifestId === undefined)
    throw new Error("The fixture deployment must be manifest-bound");
  await read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DELETE FROM state_queue_terminal_observer_states`;
    }),
  );
  const unexpected = async () => {
    throw new Error("No transition is observed while seeding");
  };
  const exit = await Effect.runPromiseExit(
    reconcileStateQueueCorrections({
      source: {
        readQueue: async () => start,
        observeTransitions: unexpected,
        canonicalDepth: unexpected,
      },
      deploymentIdentityDigest: manifestId,
      stateQueuePolicyId: h.fixture.contracts.stateQueue.policyId,
      requiredFinalityDepth: BigInt(
        h.deployment.manifest.l1Finality.confirmationDepth,
      ),
      deploymentManifest: identity.manifest,
    }).pipe(
      Effect.provideService(Globals, h.globals),
      Effect.provide(Database.layer),
    ),
  );
  if (Exit.isFailure(exit)) throw new Error(Cause.pretty(exit.cause));
  expect(exit.value.status).toBe("bootstrapped");
  return start;
};
