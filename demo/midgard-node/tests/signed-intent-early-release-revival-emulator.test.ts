import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { signedIntentReplacementDigest } from "../src/services/canonical-journal-recovery.js";
import {
  closeLifecycle,
  finalizeLocally,
  readJournal,
  readLocalFinalizationJob,
  readSqlLedgerRoot,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  attestBaseInPlace,
  C,
  finalizeBaseAndAdmitDeposit,
  loseCommitOnBase,
} from "./helpers/signed-intent-early-release.js";
import {
  expectReplaced,
  nativeRoot,
  nextPoint,
  readDepositHeader,
  readLeaseStatus,
  readPlans,
  synchronizeWithin,
} from "./helpers/signed-intent-replacement.js";
import {
  availableBlockAssetName,
  loseNextCommit,
  nodeAssetName,
  readGlobals,
} from "./signed-intent-replacement-revival-emulator.signed-commit-node.js";

/**
 * "Whichever lands wins" after an early replacement: E was replaced before
 * its TTL because the journaled canonical history showed D's output spent by
 * D's DA attestation, and NEW_E was built on the attested incarnation of D.
 * A rollback then takes the attestation back (D's output is unspent again)
 * and E lands inside its validity window. By the owner ruling of 2026-09-26
 * ("when the observed tail is an abandoned own block, revive that block and
 * abandon the other one"), E is revived and NEW_E, built on another output of
 * the same base node, is abandoned. Actual deployed validators, the
 * production history owner and emulator transactions. The emulator cannot
 * roll back its chain, so the rollback is its ledger returning to the state
 * before the attestation while its slot keeps moving forward; as in
 * signed-intent-replacement-revival-emulator.test.ts, the node's history
 * journal is not rolled back, only its authenticated view of the queue and
 * the canonical transactions it journals change.
 */

it("revives an early-replaced commit that lands after a rollback un-spends its base output, abandons the replacement built on the attested base, and locally finalizes the winner once", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { base, inclusion } = await finalizeBaseAndAdmitDeposit(h);
    const E = await loseCommitOnBase(h, base, inclusion);
    const depositId = E.journal.depositEventIds[0]!;
    const { emulator } = h.fixture;
    expect(Object.keys(emulator.mempool)).toHaveLength(0);
    // The ledger before the attestation: the chain a rollback returns to.
    const beforeAttestation = structuredClone(emulator.ledger);

    // Early replacement, well before E's TTL.
    const attested = await attestBaseInPlace(h, base);
    await synchronizeWithin(h);
    expect(h.batches.at(-1)!.observedSlot).toBeLessThan(E.ttl - 1);
    await expectReplaced(E.journal, { handle: h });

    // NEW_E, built on the attested incarnation of D, is handed to L1 and
    // lost. (Its scheduler alignment is skipped so E's reference inputs stay
    // those of the chain the rollback returns to; see the revival suite.)
    const N = await loseNextCommit(h, undefined, { alignScheduler: false });
    expect(N.journal[C.BASE_TAIL_OUT_REF]).toBe(attested.continued);
    expect(N.journal[C.BASE_TAIL_HEADER_HASH]).toEqual(
      E.journal[C.BASE_TAIL_HEADER_HASH],
    );
    expect(N.journal[C.BASE_UTXOS_ROOT]).toBe(E.journal[C.BASE_UTXOS_ROOT]);
    expect(N.journal.depositEventIds).toEqual(E.journal.depositEventIds);
    expect(await readDepositHeader(depositId)).toBeNull();

    // The rollback: D's output is unspent again, and E lands inside its
    // validity window. NEW_E can never land on this chain.
    expect(Object.keys(emulator.mempool)).toHaveLength(0);
    Object.assign(emulator, { ledger: structuredClone(beforeAttestation) });
    h.lucidService.api.clearUTxOOverride();
    h.fixture.operatorLucid.clearUTxOOverride();
    expect(emulator.slot).toBeLessThan(E.ttl);
    expect(await emulator.submitTx(E.signed.toString("hex"))).toBe(E.txHash);
    expect(await h.fixture.operatorLucid.awaitTx(E.txHash)).toBe(true);
    await expect(
      emulator.submitTx(N.journal[C.SIGNED_TX_CBOR]!.toString("hex")),
    ).rejects.toBeDefined();

    // The next point journals E's commit, a replaced sibling of NEW_E on the
    // same base node: E is revived and NEW_E abandoned. (Kills "match
    // siblings by base output only", which never links NEW_E on D's attested
    // output to E on D's original one.)
    await nextPoint(h);
    const revived = await readJournal(E.header);
    expect(revived[C.STATUS]).toBe(Pending.Status.ObservedWaitingStability);
    const abandoned = await readJournal(N.header);
    expect(abandoned[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(abandoned[C.CORRECTION_TRANSITION_DIGEST]).toBe(
      signedIntentReplacementDigest(N.journal),
    );
    expect(await readLocalFinalizationJob(N.header)).toBeUndefined();
    expect(
      await readLeaseStatus(N.journal[C.STATE_QUEUE_LEASE_TOKEN]),
    ).not.toBe("active");
    expect(await readDepositHeader(depositId)).toBe(E.header);
    expect((await readSqlLedgerRoot()).root_hex).toBe(
      E.journal[C.EXPECTED_UTXOS_ROOT],
    );
    expect(await nativeRoot(h)).toBe(E.journal[C.BASE_UTXOS_ROOT]);
    expect(readGlobals(h).localFinalizationPending).toBe(true);
    expect(availableBlockAssetName(h)).toBe(nodeAssetName(E.header));
    expect(
      (await readPlans())
        .filter(({ state }) => state !== "applied")
        .map(({ state }) => state),
    ).toEqual([]);

    // E is locally finalized once; its deposit is committed by E alone.
    await finalizeLocally(h, E.header);
    expect(await nativeRoot(h)).toBe(E.journal[C.EXPECTED_UTXOS_ROOT]);
    expect(await readDepositHeader(depositId)).toBe(E.header);
    expect((await readJournal(N.header))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
