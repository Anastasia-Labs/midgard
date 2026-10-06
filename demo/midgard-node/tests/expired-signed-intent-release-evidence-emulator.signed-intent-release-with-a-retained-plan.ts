import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/service.js";
import { advanceEmulatorPastLatestBlockEndTime } from "./deposit-flow-emulator-shared.js";
import {
  C,
  expectLandedAndFinalizedOnce,
} from "./expired-signed-intent-release-evidence-emulator.expect-landed-and-finalized-once.js";
import {
  closeLifecycle,
  finalizeLocally,
  openCorrectionRewindScenario,
  read,
  readDeposits,
  readJournal,
  readObserver,
  settleWithin,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  expectReplaced,
  type Handle,
  landSignedCommitAsFork,
  moveToExactSlot,
  nativeRoot,
  readPlans,
  resetSharedRows,
  signedTtl,
  synchronizeWithin,
  UNLANDED,
  updateJournal,
} from "./helpers/signed-intent-replacement.js";

const INJECTED_PLAN_FAILURE =
  "injected crash while marking the release applied";

const INJECTED_REPLAY_INTERRUPT = "injected crash after the native replay";

/** A database fault at the plan's final state change, inside the release's
 * own transaction. */
const refusePlanApplication = (refuse: boolean) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      if (!refuse) {
        yield* sql`DROP TRIGGER IF EXISTS midgard_test_refuse_plan_applied
          ON event_history_recovery_plans`;
        yield* sql`DROP FUNCTION IF EXISTS midgard_test_refuse_plan_applied()`;
        return;
      }
      yield* sql.unsafe(`CREATE OR REPLACE FUNCTION midgard_test_refuse_plan_applied()
        RETURNS trigger LANGUAGE plpgsql AS $$
        BEGIN RAISE EXCEPTION '${INJECTED_PLAN_FAILURE}'; END $$`);
      yield* sql.unsafe(`CREATE TRIGGER midgard_test_refuse_plan_applied
        BEFORE UPDATE ON event_history_recovery_plans FOR EACH ROW
        WHEN (NEW.state = 'applied' AND OLD.state = 'prepared')
        EXECUTE FUNCTION midgard_test_refuse_plan_applied()`);
    }),
  );

describe(
  "signed-intent release with a retained plan",
  { concurrent: false },
  () => {
    it("discards a retained release plan once the signed commit is seen landed, and locally finalizes it once without a crash loop", async () => {
      const initial = await openHistoryProductionOwnerLifecycle();
      let h: Handle & Pick<typeof initial, "close"> = initial;
      try {
        await resetSharedRows();
        await advanceEmulatorPastLatestBlockEndTime(initial.fixture);
        const inclusion = await submitDeposit(initial, 12_000_000n);
        const lost = await submitUnlandedBlock(initial, inclusion);
        const header = lost.submittedHeaderHash;
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        // The release is prepared and then crashes before it applies: its plan
        // is retained, nothing else is persisted.
        await refusePlanApplication(true);
        moveToExactSlot(initial, ttl);
        const failure = await settleWithin(initial.synchronize(), 240_000).then(
          () => undefined,
          (error: unknown) => inspect(error, { depth: 40 }),
        );
        expect(failure).toContain("Failed to apply history recovery plan");
        expect((await readPlans()).map(({ state }) => state)).toEqual([
          "prepared",
        ]);
        expect(UNLANDED).toContain((await readJournal(header))[C.STATUS]);
        // Meanwhile the chain followed included E inside its window. The
        // restarted owner sees E landed: it discards the retained plan and
        // records the observation instead of stopping on it at every start.
        // (Kills "fail on a retained plan when E landed".)
        await landSignedCommitAsFork(initial, journal[C.SIGNED_TX_CBOR]!);
        const restarted = await initial.restartRuntime({
          afterStop: () => refusePlanApplication(false),
        });
        h = restarted;
        // The restarted runtime hydrates the journal without its node, as any
        // startup does; E is on the emulator's own chain, so the confirmation
        // pass re-derives it before local finalization.
        await expectLandedAndFinalizedOnce(restarted, journal, finalizeLocally);
        expect(
          (await readDeposits()).map(({ projectedHeader }) => projectedHeader),
        ).toEqual([header]);
      } finally {
        await refusePlanApplication(false);
        await closeLifecycle(h);
      }
    }, 900_000);

    it("replays a landed, locally finalized block natively when it discards a retained plan whose native rewind already ran", async () => {
      const initial = await openHistoryProductionOwnerLifecycle();
      let h: Handle & Pick<typeof initial, "close"> = initial;
      try {
        await resetSharedRows();
        await advanceEmulatorPastLatestBlockEndTime(initial.fixture);
        const inclusion = await submitDeposit(initial, 12_000_000n);
        const lost = await submitUnlandedBlock(initial, inclusion);
        const header = lost.submittedHeaderHash;
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        // E's local finalization completed (markLocalFinalizationComplete):
        // recording E observed finalizes it without replaying it again.
        await updateJournal(header, {
          [C.STATUS]: Pending.Status.SubmittedUnconfirmed,
        });
        // Past E's TTL the release prepares its plan and runs the native rewind
        // to E's base; its SQL application then fails, so the plan is retained
        // with native state at the base.
        await refusePlanApplication(true);
        moveToExactSlot(initial, ttl);
        const failure = await settleWithin(
          initial.synchronize().then(
            () => undefined,
            (error: unknown) => inspect(error, { depth: 40 }),
          ),
          240_000,
        );
        expect(failure).toContain("Failed to apply history recovery plan");
        expect((await readPlans()).map(({ state }) => state)).toEqual([
          "prepared",
        ]);
        expect(await nativeRoot(initial)).toBe(journal[C.BASE_UTXOS_ROOT]);
        // E landed after all. The restarted owner discards the retained plan
        // and records E finalized before the native owner's startup would
        // replay E's journal, so it must replay E natively itself first. (Kills
        // "discard the retained plan without replaying E natively": native
        // state stays at E's base while E is recorded finalized.)
        await landSignedCommitAsFork(initial, journal[C.SIGNED_TX_CBOR]!);
        const restarted = await initial.restartRuntime({
          synchronize: false,
          afterStop: () => refusePlanApplication(false),
        });
        h = restarted;
        await synchronizeWithin(restarted);
        expect((await readJournal(header))[C.STATUS]).toBe(
          Pending.Status.Finalized,
        );
        expect(await readPlans()).toEqual([]);
        expect(await nativeRoot(h)).toBe(journal[C.EXPECTED_UTXOS_ROOT]);
      } finally {
        await refusePlanApplication(false);
        await closeLifecycle(h);
      }
    }, 900_000);

    it("resumes a retained release plan before an owed correction rewind of its base, and converges once", async () => {
      const scenario = await openCorrectionRewindScenario({
        blocks: 2,
        unlandedTail: true,
      });
      const { h: initial } = scenario;
      let h: Handle & Pick<typeof initial, "close"> = initial;
      try {
        const [base, header] = scenario.headers as [string, string];
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        // Past E's TTL with D still the tail, the release prepares E's
        // replacement and then fails to apply it: the plan is retained.
        await refusePlanApplication(true);
        moveToExactSlot(initial, ttl);
        const failure = await settleWithin(
          initial.synchronize().then(
            () => undefined,
            (error: unknown) => inspect(error, { depth: 40 }),
          ),
          240_000,
        );
        expect(failure).toContain("Failed to apply history recovery plan");
        expect((await readPlans()).map(({ state }) => state)).toEqual([
          "prepared",
        ]);
        // While the node is down, an attestation-timeout correction removes D
        // and becomes final; the correction fiber admits it.
        const removal = await scenario.removeTail(base, { observe: false });
        await scenario.awaitRemovalFinality(initial, { observe: false });
        const restarted = await initial.restartRuntime({
          synchronize: false,
          afterStop: () => refusePlanApplication(false),
        });
        h = restarted;
        expect(
          (await scenario.tick(restarted.globals)).admittedTransactionHashes,
        ).toEqual([removal.accepted.transaction.txHash]);
        // Bounded: the release resumes its own retained plan first (an owed
        // rewind waits for every retained plan), then the rewind abandons D
        // under the correction. (Kills "honor the owed rewind before the
        // retained release plan": each waits for the other and the owner never
        // becomes ready.)
        await synchronizeWithin(restarted);
        await scenario.nextSourceBlock(restarted);
        await expectReplaced(journal, {
          globalsReset: false,
          handle: restarted,
        });
        const digest = (await readObserver()).admitted[0]!.transitionDigest;
        const removed = await readJournal(base);
        expect(removed[C.STATUS]).toBe(Pending.Status.Abandoned);
        expect(removed[C.CORRECTION_TRANSITION_DIGEST]).toBe(digest);
        expect(
          (await readPlans()).filter(({ state }) => state !== "applied"),
        ).toEqual([]);
      } finally {
        await refusePlanApplication(false);
        await closeLifecycle(h);
      }
    }, 900_000);

    it("records an unpromoted signed commit landed over its retained base-to-base plan without replaying it natively, so an interrupted attempt stays resumable, and locally finalizes it once", async () => {
      const initial = await openHistoryProductionOwnerLifecycle();
      let h: Handle & Pick<typeof initial, "close"> = initial;
      const prototype = ProductionNativeMpfOwnerService.prototype;
      const recover = prototype.recover;
      let replayedWhileRetained = 0;
      try {
        await resetSharedRows();
        await advanceEmulatorPastLatestBlockEndTime(initial.fixture);
        const inclusion = await submitDeposit(initial, 12_000_000n);
        const lost = await submitUnlandedBlock(initial, inclusion);
        const header = lost.submittedHeaderHash;
        const journal = await readJournal(header);
        const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
        const base = journal[C.BASE_UTXOS_ROOT];
        const candidate = journal[C.EXPECTED_UTXOS_ROOT];
        // E was never promoted: its commit is signed but unacknowledged and
        // the native root is still at its base. The fixture worker always
        // promotes, so the state is set up directly.
        expect(await nativeRoot(initial)).toBe(candidate);
        await updateJournal(header, {
          [C.STATUS]: Pending.Status.PendingSubmission,
          [C.SUBMITTED_TX_HASH]: null,
        });
        const owner = await Effect.runPromise(
          Ref.get(initial.globals.NATIVE_MPF_OWNER),
        );
        if (owner === undefined) throw new Error("Native owner is not open");
        await owner.restoreCanonicalRoot({
          recoveryId: "0e".repeat(32),
          expectedRoot: candidate,
          targetRoot: base,
        });
        expect(await nativeRoot(initial)).toBe(base);
        // Past E's TTL the release prepares its plan from the base root, base
        // to base, and then fails to apply it: the plan is retained.
        await refusePlanApplication(true);
        moveToExactSlot(initial, ttl);
        const failure = await settleWithin(
          initial.synchronize().then(
            () => undefined,
            (error: unknown) => inspect(error, { depth: 40 }),
          ),
          240_000,
        );
        expect(failure).toContain("Failed to apply history recovery plan");
        const plans = await readPlans();
        expect(plans.map(({ state }) => state)).toEqual(["prepared"]);
        expect(
          (plans[0]!.intent as { expectedRoot?: unknown }).expectedRoot,
        ).toBe(base);
        expect(await nativeRoot(initial)).toBe(base);
        // E landed after all. The restarted owner discards the base-to-base
        // plan and records E landed without replaying it first; the observed
        // journal is replayed natively only after that (as any startup replays
        // an observed journal), with no plan retained. A native replay while
        // the plan is still retained is interrupted right after it ran, as a
        // crash before the discard would. (Kills "replay the landed block over
        // any retained plan": that replay moves the native root to E's
        // candidate, outside the plan's roots, and the interrupted attempt can
        // never resume.)
        await landSignedCommitAsFork(initial, journal[C.SIGNED_TX_CBOR]!);
        const restarted = await initial.restartRuntime({
          synchronize: false,
          afterStop: async () => {
            await refusePlanApplication(false);
            prototype.recover = async function (
              this: ProductionNativeMpfOwnerService,
              replay,
            ) {
              await recover.call(this, replay);
              if (
                (await readPlans()).some(({ state }) => state === "prepared")
              ) {
                replayedWhileRetained += 1;
                throw new Error(INJECTED_REPLAY_INTERRUPT);
              }
            };
          },
        });
        h = restarted;
        try {
          await synchronizeWithin(restarted);
        } finally {
          prototype.recover = recover;
        }
        expect(replayedWhileRetained).toBe(0);
        await expectLandedAndFinalizedOnce(restarted, journal, finalizeLocally);
        expect(
          (await readDeposits()).map(({ projectedHeader }) => projectedHeader),
        ).toEqual([header]);
      } finally {
        prototype.recover = recover;
        await refusePlanApplication(false);
        await closeLifecycle(h);
      }
    }, 900_000);
  },
);
