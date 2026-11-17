import { afterAll, beforeAll, beforeEach, it } from "vitest";

import { journeyFamilyDirectory } from "./artifacts.js";
import { selectJourneyFixtures } from "./catalogue.js";
import { selectJourneyDeploymentSource } from "./deployment-source.js";
import { journeyFamilyDeadline } from "./journey-family-deadline.js";
import { runAutonomousWatcherJourney } from "./journey-runner.js";
import {
  type AttachedJourneySession,
  type JourneySession,
  openAttachedJourneySession,
  openJourneySession,
} from "./journey-session.js";
import {
  readJourneyExecutionTiming,
  readStackJourneyTimeoutMs,
  stackRunTimeoutsMs,
} from "./journey-timing.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";
import { readStackJourneyTimingPaths } from "./stack-context.js";
import { fundStackLedgerOwner } from "./stack-ledger-owner.js";
import { verifiedPredecessorBeforeFamily } from "./stack-verified-predecessor.before-family.js";
import { checkStackVerifiedPredecessor } from "./stack-verified-predecessor.js";
import { withdrawalFundingShape } from "./withdrawal-owner-funding.js";
import { withdrawalOwnerTopUpBeforeFamily } from "./withdrawal-owner-top-up.js";

// Devnet mode (the default) runs from MIDGARD_WATCHER_JOURNEY_RUN_DIR and
// skips without it; stack mode attaches to a running e2e-stack.
const source = selectJourneyDeploymentSource(process.env);
const runDirectory =
  source?.mode === "devnet" ? source.runDirectory : undefined;
const fixtures = selectJourneyFixtures(
  process.env.MIDGARD_WATCHER_JOURNEY_CATEGORIES,
);
const stackTimingPaths =
  source?.mode === "stack"
    ? await readStackJourneyTimingPaths(source.configPath)
    : undefined;

let session: Promise<JourneySession | AttachedJourneySession> | undefined;
const openSession = () =>
  (session ??=
    source!.mode === "devnet"
      ? openJourneySession(source!.runDirectory)
      : source!.open().then((context) =>
          openAttachedJourneySession(context, {
            operator2: source!.operator2,
            // The session opens once, before every family of the run.
            families: fixtures,
          }),
        ));
afterAll(async () => {
  const opened = await session?.catch(() => undefined);
  await opened?.close();
}, 120_000);

// On a stack the run funds the drill's ledger owner before any family's
// timer starts: our node may absorb its deposit only after operator-2's
// whole shift and its committee's attestation, which no family's limit
// holds. Only the funding's own deadline bounds it, so the hook sets no
// Vitest timeout of its own; a failure refuses the run before any family.
beforeAll(async () => {
  if (source?.mode !== "stack" || fixtures.length === 0) return;
  const opened = await openSession();
  if (opened.mode !== "attached") return;
  await fundStackLedgerOwner(opened, {
    category: fixtures[0]!.category,
    onStage: (name) => console.info(`Ledger-owner funding: ${name}`),
  });
}, 0);

// Before every family, also outside its timer, the previous family's run
// must have unwound: a family stopped by its time limit closes the session
// only once its work has stopped, and the session refuses to start another
// family before then.
//
// On a stack, the block the family will build on must then be one the
// stack's watcher verified from its payload. A watcher that restarted after
// that block merged can no longer verify it, so the check heals it with one
// honest transfer through our node and waits, within the funding's
// absorption bound and the family's decision allowance, for the new block to
// be attested and verified.
//
// A withdrawal family then needs an output of the ledger owner's that funds
// what it withdraws. An earlier family, or the heal's transfer, may have
// spent or split them, so when none on the tail it builds on does, one
// deposit of exactly what the family needs tops the owner up and the check
// waits for it to be absorbed (`withdrawalOwnerTopUpBeforeFamily`). It runs
// last, so no transfer moves the output it found. Neither does anything once
// the session has closed, nor for a family resuming its staging.
const familyTitle = (category: string) =>
  `automatically proves '${category}', corrects exact economics, and processes an honest successor`;
beforeEach(async ({ task }) => {
  await (await session?.catch(() => undefined))?.familyUnwound();
  if (source === undefined) return;
  const fixture = fixtures.find(
    ({ category }) => familyTitle(category) === task.name,
  );
  if (fixture === undefined) return;
  const withdraws = withdrawalFundingShape(fixture.category) !== undefined;
  if (source.mode !== "stack" && !withdraws) return;
  const opened = await openSession();
  const onStage = (name: string) => {
    console.info(`Before the family: ${name}`);
  };
  await verifiedPredecessorBeforeFamily({
    familyDirectory: journeyFamilyDirectory(
      opened.context.runDirectory,
      fixture.category,
    ),
    isClosed: opened.isClosed,
    close: opened.close,
    checkPredecessor: async () => {
      if (opened.mode === "attached")
        await checkStackVerifiedPredecessor(opened, {
          category: fixture.category,
          onStage,
        });
    },
    checkFunding: () =>
      withdrawalOwnerTopUpBeforeFamily(opened, {
        category: fixture.category,
        onStage,
      }),
    onStage,
  });
}, 0);

// Every family is budgeted from the actual cadence and the depth its actions
// wait for: the generic plan, plus the single release-finality window of the
// finalized evidence stamp. The correction is not budgeted: the workflow
// journal's progress ends it, by the watcher's own rule, however many
// transactions it takes, so the family's limit pauses while it waits and
// then resumes with the replay of the blocks that arrived meanwhile
// (`journeyFamilyDeadline`). On a stack it also waits for operator-2's
// shift to hold the family's staging shift cover C: at worst the rest of one
// of its shifts, our node's whole shift, and C of the next; a family whose
// payload the committee refuses also waits out two attestation timeouts for
// its block to age out. The stack's limit has no replay term, so its pause
// adds nothing. The run's last family
// also resumes operator-2's node and waits for it to report ready. When a
// family's limit passes, its signal aborts: the family's waits stop, and the
// session closes once its work has stopped, starting operator-2 again only
// then. The family's own limit is the only one; Vitest sets none.
const options = { actionDepth: JOURNEY_ACTION_DEPTH };
const stackTimeoutsMs =
  stackTimingPaths !== undefined
    ? stackRunTimeoutsMs(
        await Promise.all(
          fixtures.map((fixture) =>
            readStackJourneyTimeoutMs(stackTimingPaths, fixture, options),
          ),
        ),
      )
    : undefined;

// All families share wallets and chain state. The package config admits one fork,
// and this sequential suite leaves deployment and service ownership with the runner.
for (const [index, fixture] of fixtures.entries()) {
  const limit =
    runDirectory !== undefined
      ? await (async () => {
          const execution = await readJourneyExecutionTiming(
            runDirectory,
            fixture.category,
            options,
          );
          return {
            limitMs: execution.journeyTimeoutMs,
            replayUtilization: execution.healthyReplay.replayUtilization,
          };
        })()
      : // A stack's limit; with no deployment selected the case is skipped.
        stackTimeoutsMs === undefined
        ? undefined
        : { limitMs: stackTimeoutsMs[index]!, replayUtilization: 0 };
  it.skipIf(source === undefined)(
    familyTitle(fixture.category),
    async ({ signal }) => {
      const deadline = journeyFamilyDeadline(limit!);
      try {
        const activeSession = await openSession();
        const last = fixture === fixtures.at(-1);
        await runAutonomousWatcherJourney(
          activeSession.mode === "devnet"
            ? runDirectory!
            : activeSession.context.runDirectory,
          fixture,
          {
            session: activeSession,
            waitForAnchors: last,
            limit: AbortSignal.any([signal, deadline.signal]),
            excludingLimit: deadline.excluding,
            // Operator-2's node stays held through every family's fault phase;
            // the completed run starts it again and proves it live.
            ...(activeSession.mode === "attached" && last
              ? { afterJourney: () => activeSession.operator2.resume() }
              : {}),
          },
        );
      } finally {
        deadline.dispose();
      }
    },
    0,
  );
}
