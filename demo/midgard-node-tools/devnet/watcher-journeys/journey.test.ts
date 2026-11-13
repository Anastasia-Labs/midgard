import { afterAll, beforeAll, beforeEach, it } from "vitest";

import { journeyFamilyDirectory } from "./artifacts.js";
import { selectJourneyFixtures } from "./catalogue.js";
import { selectJourneyDeploymentSource } from "./deployment-source.js";
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
import {
  checkStackVerifiedPredecessor,
  verifiedPredecessorBeforeFamily,
} from "./stack-verified-predecessor.js";

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
// be attested and verified. It does nothing once the session has closed, and
// nothing for a family resuming its staging.
const familyTitle = (category: string) =>
  `automatically proves '${category}', corrects exact economics, and processes an honest successor`;
beforeEach(async ({ task }) => {
  await (await session?.catch(() => undefined))?.familyUnwound();
  if (source?.mode !== "stack") return;
  const fixture = fixtures.find(
    ({ category }) => familyTitle(category) === task.name,
  );
  if (fixture === undefined) return;
  const opened = await openSession();
  if (opened.mode !== "attached") return;
  const onStage = (name: string) => {
    console.info(`Verified predecessor: ${name}`);
  };
  await verifiedPredecessorBeforeFamily({
    familyDirectory: journeyFamilyDirectory(
      opened.context.runDirectory,
      fixture.category,
    ),
    isClosed: opened.isClosed,
    close: opened.close,
    check: () =>
      checkStackVerifiedPredecessor(opened, {
        category: fixture.category,
        onStage,
      }),
    onStage,
  });
}, 0);

// Every family is budgeted from the actual cadence and the depth its actions
// wait for: the audited transitionTrace plan or the finite generic plan, plus
// the single release-finality window of the finalized evidence stamp. On a
// stack it also waits for operator-2's shift to hold the family's staging
// shift cover C: at worst the rest of one of its shifts, our node's whole
// shift, and C of the next. The run's last family also resumes operator-2's
// node and waits for it to report ready. When a family's limit passes,
// Vitest aborts its signal: the family's waits stop, and the session closes
// once its work has stopped, starting operator-2 again only then.
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
  const timeoutMs =
    runDirectory !== undefined
      ? (
          await readJourneyExecutionTiming(
            runDirectory,
            fixture.category,
            options,
          )
        ).journeyTimeoutMs
      : // A stack's limit; with no deployment selected the case is skipped.
        stackTimeoutsMs?.[index];
  it.skipIf(source === undefined)(
    familyTitle(fixture.category),
    async ({ signal }) => {
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
          limit: signal,
          // Operator-2's node stays held through every family's fault phase;
          // the completed run starts it again and proves it live.
          ...(activeSession.mode === "attached" && last
            ? { afterJourney: () => activeSession.operator2.resume() }
            : {}),
        },
      );
    },
    timeoutMs,
  );
}
