import { Effect } from "effect";
import { expect, vi } from "vitest";

import * as Journal from "../../src/database/eventHistoryJournal.js";
import { MempoolLedgerDB } from "../../src/database/index.js";
import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { reconcileStateQueueCorrections } from "../../src/fibers/attestation-timeout-correction.js";
import { decodeBoundEventHistoryLedgerSnapshot } from "../../src/l1-event-history-source.js";
import { Database } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { makeLocalKupmiosStateQueueCorrectionSource } from "../../src/services/state-queue-correction-observer.make-local-kupmios-state-queue-correction-source.js";
import {
  type Lifecycle,
  read,
  readJournal,
  SYNCHRONIZE_BOUND_MS,
} from "./correction-rewind-scenario.js";
import {
  restoreEmulator,
  snapshotEmulator,
} from "./history-owner-rollback-journey.snapshot-emulator.js";
import {
  captureConfirmedHistoryObservations,
  historyOutputObservation,
} from "./history-projection-observations.js";
import { makeRollbackHistoryTransport } from "./history-rollback-transport.js";
import {
  nativeRoot,
  readEmulatorQueue,
  readImmutableCounts,
  signedTtl,
} from "./signed-intent-replacement.js";

/** Actual accepted emulator transactions and production native/SQL recovery.
 * Branch ancestry/rollback RPCs remain a controlled transport, not Cardano consensus. */
export const createSignedIntentReplacementFork = () => {
  let source: ReturnType<typeof makeRollbackHistoryTransport> | undefined;
  let ancestor:
    | { id: string; snapshot: ReturnType<typeof snapshotEmulator> }
    | undefined;
  let forkObserver:
    | ReturnType<typeof captureConfirmedHistoryObservations>
    | undefined;
  const transportFactory = (
    recorded: Parameters<typeof makeRollbackHistoryTransport>[0],
  ) => {
    source = makeRollbackHistoryTransport(recorded);
    return source;
  };
  const captureAncestor = (h: Lifecycle) => {
    if (source === undefined) throw new Error("Fork source is not open");
    expect(Object.keys(h.fixture.emulator.mempool)).toHaveLength(0);
    const point = source.points.at(-1)!.point;
    expect(point.slot).toBe(h.fixture.emulator.slot);
    ancestor = { id: point.id, snapshot: snapshotEmulator(h.fixture.emulator) };
  };
  const includeSigned = async (h: Lifecycle, journal: Pending.Record) => {
    const signed = journal.signed_tx_cbor!;
    const hash = journal.intended_tx_hash!.toString("hex");
    const ttl = signedTtl(signed);
    expect(h.fixture.emulator.slot + 20).toBeLessThan(ttl);
    expect(await h.fixture.emulator.submitTx(signed.toString("hex"))).toBe(
      hash,
    );
    expect(await h.fixture.operatorLucid.awaitTx(hash)).toBe(true);
    const status = await h.fixture.operatorLucid.transactionStatus(hash);
    expect(status.status).toBe("confirmed");
    if (status.status !== "confirmed")
      throw new Error("Signed fork commit must actually land");
    expect(status.confirmation.slot).toBeLessThan(ttl);
    return hash;
  };
  const includeReplacement = async (h: Lifecycle, journal: Pending.Record) => {
    const hash = await includeSigned(h, journal);
    await h.synchronize();
    const actual = h.batches
      .flatMap((batch) => batch.observations)
      .find((item) => item.transaction.txHash === hash);
    expect(actual?.signedCbor).toBe(journal.signed_tx_cbor!.toString("hex"));
  };
  const followOriginalFork = async (
    h: Lifecycle,
    journal: Pending.Record,
    replacement: Pending.Record,
  ) => {
    if (source === undefined || ancestor === undefined)
      throw new Error("Missing common fork ancestor");
    const replacementHash = replacement.intended_tx_hash!.toString("hex");
    h.observer.restore();
    restoreEmulator(h.fixture.emulator, ancestor.snapshot);
    h.fixture.operatorLucid.clearUTxOOverride();
    h.lucidService.api.clearUTxOOverride();
    expect(
      (await h.fixture.operatorLucid.transactionStatus(replacementHash)).status,
    ).toBe("not_found");
    // Restoring the owned L1 emulator must leave the prior local effects intact;
    // only the authenticated history recovery below may reverse them.
    expect(
      (await readJournal(replacement.header_hash.toString("hex"))).status,
    ).toBe(Pending.Status.LocallyApplied);
    expect(await nativeRoot(h)).toBe(replacement.expected_utxos_root);
    expect(
      await readImmutableCounts(
        replacement.mempoolTxIds.map((id) => id.toString("hex")),
      ),
    ).toEqual(
      Object.fromEntries(
        replacement.mempoolTxIds.map((id) => [id.toString("hex"), 1]),
      ),
    );
    vi.setSystemTime(h.fixture.emulator.now());
    source.rollbackTo(ancestor.id);
    const historyAddresses = [
      ...new Set([
        h.binding.hubAddress,
        ...Object.values(h.binding.deployments).flatMap(
          ({ address, retentionAddress }) => [address, retentionAddress],
        ),
      ]),
    ];
    const addresses = [
      ...historyAddresses,
      h.fixture.contracts.stateQueue.spendingScriptAddress,
    ];
    const outputs = async (selected = addresses) =>
      (
        await Promise.all(
          selected.map((address) => h.fixture.operatorLucid.utxosAt(address)),
        )
      )
        .flat()
        .map(historyOutputObservation);
    const batches: Parameters<typeof source.appendFork>[0][] = [];
    forkObserver = captureConfirmedHistoryObservations(
      h.fixture.operatorLucid,
      h.fixture.emulator,
      async (observations) => {
        batches.push({
          observations,
          observedSlot: h.fixture.emulator.slot,
          observedHeight: h.fixture.emulator.blockHeight,
          outputs: await outputs(),
        });
      },
    );
    const hash = await includeSigned(h, journal);
    expect(
      batches
        .flatMap((batch) => batch.observations)
        .find((item) => item.transaction.txHash === hash)?.signedCbor,
    ).toBe(journal.signed_tx_cbor!.toString("hex"));
    // This fixture starts the separate correction observer at its actual queue,
    // through the production first-tick API, without replacing any durable row.
    const identity = h.fixture.runtimeOverrides!.deploymentIdentity;
    if (identity.manifestId === undefined || identity.manifest === undefined)
      throw new Error("Canonical fork requires the deployment manifest");
    const contracts = h.fixture.contracts;
    const observed = await Effect.runPromise(
      reconcileStateQueueCorrections({
        source: makeLocalKupmiosStateQueueCorrectionSource({
          ...source.options,
          deploymentIdentityDigest: identity.manifestId,
          stateQueuePolicyId: contracts.stateQueue.policyId,
          stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
          hubOraclePolicyId: contracts.hubOracle.policyId,
          correctionLockAddress: contracts.correctionLock.spendingScriptAddress,
          fraudProofPolicyId: contracts.fraudProof.policyId,
          fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
          readQueue: () => readEmulatorQueue(h),
        }),
        deploymentIdentityDigest: identity.manifestId,
        stateQueuePolicyId: contracts.stateQueue.policyId,
        requiredFinalityDepth: BigInt(
          identity.manifest.l1Finality.confirmationDepth,
        ),
        deploymentManifest: identity.manifest,
      }).pipe(
        Effect.provideService(Globals, h.globals),
        Effect.provide(Database.layer),
      ),
    );
    expect(observed.status).toBe("bootstrapped");
    // Canonical displacement waits for this deployment's ordinary depth.
    for (
      let block = 0;
      block < identity.manifest.l1Finality.confirmationDepth;
      block += 1
    ) {
      h.fixture.emulator.awaitBlock(1);
      batches.push({
        observations: [],
        observedSlot: h.fixture.emulator.slot,
        observedHeight: h.fixture.emulator.blockHeight,
        outputs: await outputs(),
      });
    }
    const activeSource = source;
    const synchronizedCoverage = new Map<
      string,
      Parameters<typeof h.production.onCommitAttempt>[0]["coverage"]
    >();
    const onCommitAttempt: typeof h.production.onCommitAttempt = (receipt) => {
      expect(
        synchronizedCoverage.get(receipt.coverage.checkpointRevision),
      ).toEqual(receipt.coverage);
      expect(receipt.coverage.includedThroughMs).toBe(
        h.fixture.operatorLucid.slotToUnixTime(receipt.coverage.point.slot),
      );
      h.commitAttempts.push(structuredClone(receipt));
    };
    const synchronize = async () => {
      await forkObserver!.flush();
      expect(forkObserver!.pendingCount()).toBe(0);
      expect(Object.keys(h.fixture.emulator.mempool)).toHaveLength(0);
      for (const batch of batches.splice(0)) activeSource.appendFork(batch);
      if (h.fixture.emulator.slot > activeSource.points.at(-1)!.point.slot)
        activeSource.appendFork({
          observations: [],
          observedSlot: h.fixture.emulator.slot,
          observedHeight: h.fixture.emulator.blockHeight,
          outputs: await outputs(),
        });
      vi.setSystemTime(h.fixture.emulator.now());
      const tip = activeSource.points.at(-1)!.point;
      const coverage = await Effect.runPromise(
        h.production.owner
          .awaitReadyAt(tip)
          .pipe(Effect.timeout(`${SYNCHRONIZE_BOUND_MS} millis`)),
      );
      expect(coverage.point.id).toBe(tip.id);
      synchronizedCoverage.set(
        coverage.checkpointRevision,
        structuredClone(coverage),
      );
      const checkpoint = await read(Journal.load(h.binding));
      if (checkpoint === null)
        throw new Error("Ready fork owner has no checkpoint");
      const provider = await Effect.runPromise(
        decodeBoundEventHistoryLedgerSnapshot(
          {
            point: { id: tip.id, slot: tip.slot },
            addresses: historyAddresses,
            outputs: await outputs(historyAddresses),
          },
          h.binding,
        ),
      );
      expect(checkpoint.capture.snapshotDigest).toBe(provider.snapshotDigest);
      expect(coverage.snapshotDigest).toBe(provider.snapshotDigest);
      await read(
        h.production.cache.withPhaseBLock(
          Effect.gen(function* () {
            const cached = [...(yield* h.production.cache.currentState)]
              .map(([key, value]) => [key, value.toString("hex")])
              .sort();
            const durable = (yield* MempoolLedgerDB.retrieveSpendable)
              .map((row) => [
                row[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
                row[MempoolLedgerDB.Columns.OUTPUT].toString("hex"),
              ])
              .sort();
            expect(cached).toEqual(durable);
          }),
        ),
      );
    };
    await synchronize();
    expect(
      (await h.fixture.operatorLucid.transactionStatus(replacementHash)).status,
    ).toBe("not_found");
    expect(
      activeSource.points
        .flatMap((point) => point.transactions)
        .some((tx) => tx.id === replacementHash),
    ).toBe(false);
    expect(
      activeSource.points
        .flatMap((point) => point.transactions)
        .some((tx) => tx.id === hash),
    ).toBe(true);
    const command: typeof h.command = async (effect) => {
      const result = await h.runWithoutSynchronizing(effect);
      await synchronize();
      return result;
    };
    return {
      ...h,
      command,
      synchronize,
      production: { ...h.production, synchronize, onCommitAttempt },
    };
  };
  return {
    transportFactory,
    captureAncestor,
    includeReplacement,
    followOriginalFork,
    close: () => forkObserver?.restore(),
  };
};
