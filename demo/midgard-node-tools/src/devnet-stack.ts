#!/usr/bin/env node
import { isAbsolute, resolve } from "node:path";

import { Command } from "commander";

import { registerAcceptanceCommands } from "./devnet-stack/acceptance-payout-commands.js";
import {
  compose,
  ensureChainGenerated,
  startL1,
} from "./devnet-stack/chain.js";
import {
  drillCatalogue,
  journeyFinished,
  productionChaosDeps,
  runDrills,
  selectDrills,
} from "./devnet-stack/chaos.js";
import {
  composeDeps,
  finishOwedRestore,
} from "./devnet-stack/chaos-restore.js";
import {
  STABLE_WINDOW_MS,
  waitStablyReady,
} from "./devnet-stack/chaos-stable.js";
import {
  ensureCommitteeDatabases,
  ensureDaManifests,
} from "./devnet-stack/da.js";
import {
  type DeployContext,
  ensureArtifacts,
  ensureDeployed,
} from "./devnet-stack/deploy.js";
import { requireSuccess } from "./devnet-stack/exec.js";
import {
  holdRunForResume,
  type LoadedCode,
  registerUp,
  runLockPaths,
  startRunUser,
  upDeps,
} from "./devnet-stack/fresh-controller.js";
import type { FundingRecord } from "./devnet-stack/funding.js";
import { ensureFunded } from "./devnet-stack/funding.js";
import { loadIdentities, walletInfos } from "./devnet-stack/identities.js";
import { Journal } from "./devnet-stack/journal.js";
import { Journey, runScenario } from "./devnet-stack/journey.js";
import { type Layout, makeLayout, readRunEnv } from "./devnet-stack/layout.js";
import { acquireLock, lockOwner } from "./devnet-stack/lock.js";
import { provisionReserveFloat } from "./devnet-stack/reserve-float-chain.js";
import { registerServiceRecovery } from "./devnet-stack/service-recovery-cli.js";
import {
  enduranceReport,
  serviceSpecs,
  supervisorMaintainers,
  supervisorPaths,
} from "./devnet-stack/services.js";
import {
  bondStatus,
  ensureBonded,
  ensureSupervisor,
  recordedOneShot,
  recordSupervisorSpecs,
  runningSupervisor,
  serviceReports,
  startSupervisor,
  stopSupervisor,
  SUPERVISOR_STOP_MS,
  supervisorRuns,
} from "./devnet-stack/stack.js";
import { superviseWithMaintainers } from "./devnet-stack/supervisor.js";
import { ensureWatcherRelease } from "./devnet-stack/watcher.js";

const layoutFor = (runDir: string) => {
  if (!isAbsolute(runDir))
    throw new Error("--run-dir must be an absolute path");
  return makeLayout(resolve(runDir));
};

/** A whole number above zero: a mistyped bound must not run as 0 or NaN. */
const positiveInteger = (option: string, value: string) => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0)
    throw new Error(
      `${option} must be a whole number above 0, not ${JSON.stringify(value)}`,
    );
  return parsed;
};

/** `up` after the build: only ever run by a controller that did not build. */
const resumeUp = async (
  layout: Layout,
  readyTimeoutMs: number,
  code: LoadedCode,
) => {
  const run = await ensureChainGenerated(layout);
  holdRunForResume(layout, code);
  const journal = new Journal(layout.journal);
  const identities = loadIdentities(layout);
  const wallets = walletInfos(identities);
  // A drill run killed mid-outage left an L1 container stopped or paused; a
  // live drill run restores its own.
  if (lockOwner(runLockPaths(layout).drill) === undefined) {
    const owed = await finishOwedRestore(
      composeDeps(layout, run),
      layout.drillsLog,
    );
    if (owed !== undefined)
      console.log(
        `l1: ${owed.undo} ${owed.service}, owed by the interrupted drill ${owed.drill}`,
      );
  }
  console.log(`l1: starting ${run.composeProject}`);
  await startL1(layout, run);
  console.log("l1: ready");
  const funding = await ensureFunded(layout, run, journal, wallets);
  console.log(`funding: confirmed ${funding.txId}`);
  const artifacts = ensureArtifacts(layout);
  const context: DeployContext = { layout, run, identities, artifacts };
  const oneShot = await ensureDeployed(context);
  console.log(
    `deploy: complete (hub oracle ${oneShot.txHash}#${oneShot.outputIndex})`,
  );
  await provisionReserveFloat(layout, run, journal);
  await ensureCommitteeDatabases(layout, run);
  await ensureDaManifests(context, oneShot);
  await ensureWatcherRelease(context, oneShot);
  console.log(`watcher: release ready under ${layout.watcher}`);
  const specs = serviceSpecs(context, oneShot);
  const supervisor = await ensureSupervisor({
    running: () => runningSupervisor(layout),
    runs: () => supervisorRuns(layout, specs, code.stamp),
    stop: () => stopSupervisor(layout, SUPERVISOR_STOP_MS),
    start: async () => {
      const bond = await ensureBonded(context, oneShot);
      console.log(
        `da-bond: backed ${bond.backing} of ${bond.requiredBacking} lovelace`,
      );
      return startSupervisor(layout);
    },
  });
  // Ready and then stable: a service in a restart loop answers ready between restarts.
  await waitStablyReady(context, oneShot, readyTimeoutMs, {
    supervisorPid: supervisor,
  });
  const names = specs.map((spec) => spec.name);
  console.log(
    `services: ready, stable for ${STABLE_WINDOW_MS / 1000} s (${names.join(", ")})`,
  );
};

/** Everything a running stack needs, read from the run's records only. */
const runningContext = (layout: Layout) => {
  const run = readRunEnv(layout);
  const identities = loadIdentities(layout);
  const artifacts = ensureArtifacts(layout);
  const oneShot = recordedOneShot(layout);
  if (oneShot === undefined)
    throw new Error(`${layout.runDir} has no deployment yet; run up first`);
  return {
    context: { layout, run, identities, artifacts } as DeployContext,
    oneShot,
  };
};

/**
 * Grace for maintainer work that does not watch the stop signal (a
 * cardano-cli call in flight) once the services are stopped: `up` waits
 * SUPERVISOR_STOP_MS for this process to exit, and refuses if it does not.
 * Only a requested stop forces the exit: a supervisor that failed mid-run
 * keeps whatever of it still supervises, as it did before its maintainers.
 */
const SUPERVISOR_EXIT_GRACE_MS = 5_000;

const supervise = async (options: { runDir: string }) => {
  const layout = layoutFor(options.runDir);
  acquireLock(layout.supervisorPid);
  const { context, oneShot } = runningContext(layout);
  const specs = serviceSpecs(context, oneShot);
  recordSupervisorSpecs(layout, specs);
  const abort = new AbortController();
  for (const signal of ["SIGTERM", "SIGINT", "SIGHUP"] as const)
    process.on(signal, () => abort.abort());
  try {
    await superviseWithMaintainers(
      specs,
      supervisorPaths(context, specs),
      abort.signal,
      supervisorMaintainers(context),
    );
  } finally {
    if (abort.signal.aborted)
      setTimeout(() => process.exit(), SUPERVISOR_EXIT_GRACE_MS).unref();
  }
};

const status = async (options: { runDir: string }) => {
  const layout = layoutFor(options.runDir);
  const { context, oneShot } = runningContext(layout);
  const bond = await bondStatus(context, oneShot).catch((error: unknown) =>
    String(error),
  );
  const endurance = await enduranceReport(context);
  const services = await serviceReports(layout, serviceSpecs(context, oneShot));
  console.log(
    JSON.stringify(
      {
        supervisor: runningSupervisor(layout) ?? null,
        services,
        daBond: bond,
        endurance,
      },
      null,
      2,
    ),
  );
};

const down = async (options: { runDir: string; l1: boolean }) => {
  const layout = layoutFor(options.runDir);
  const stopped = await stopSupervisor(layout, SUPERVISOR_STOP_MS);
  console.log(stopped ? "services: stopped" : "services: not running");
  if (options.l1) {
    const run = readRunEnv(layout);
    // `stop`, never `down -v`: the chain and its databases are the run.
    requireSuccess(
      await compose(layout, run, ["stop"], "l1-stop"),
      "stopping the L1 containers",
    );
    console.log(`l1: stopped ${run.composeProject}`);
  }
};

const journey = async (options: { runDir: string; idle: string }) => {
  const layout = layoutFor(options.runDir);
  startRunUser(layout, "journey");
  const { context, oneShot } = runningContext(layout);
  const journal = new Journal(layout.journal);
  const funding = journal.get<FundingRecord>("funding");
  if (funding === undefined) throw new Error("the run has no funding record");
  // Before any withdrawal of this journey: a payout can strand without it.
  await provisionReserveFloat(layout, context.run, journal);
  await runScenario(new Journey(context, oneShot, funding.assets), {
    idleMs: Number(options.idle) * 1000,
  });
  console.log("journey: complete");
};

type DrillOptions = {
  runDir: string;
  drills?: string;
  list?: boolean;
  gap: string;
  rounds: string;
  recovery: string;
  stable: string;
  outage: string;
  pause: string;
  untilJourneyDone?: boolean;
};

const drill = async (options: DrillOptions) => {
  const layout = layoutFor(options.runDir);
  const { context, oneShot } = runningContext(layout);
  const catalogue = drillCatalogue(
    serviceSpecs(context, oneShot).map((service) => service.name),
    {
      outageMs: positiveInteger("--outage", options.outage) * 1000,
      pauseMs: positiveInteger("--pause", options.pause) * 1000,
    },
  );
  if (options.list === true) {
    console.log(catalogue.map((d) => d.name).join("\n"));
    return;
  }
  // Every drill makes the supervisor restart a service from its dist.
  startRunUser(layout, "drill");
  const drills =
    options.drills === undefined
      ? catalogue
      : selectDrills(catalogue, options.drills.split(","));
  const rounds =
    options.rounds === "forever"
      ? Number.POSITIVE_INFINITY
      : positiveInteger("--rounds", options.rounds);
  if (rounds === Number.POSITIVE_INFINITY && options.untilJourneyDone !== true)
    throw new Error("--rounds forever needs --until-journey-done");
  const abort = new AbortController();
  for (const signal of ["SIGTERM", "SIGINT", "SIGHUP"] as const)
    process.on(signal, () => abort.abort());
  const summary = await runDrills({
    drills,
    runDir: layout.runDir,
    pidDir: supervisorPaths(context).pidDir,
    drillsLog: layout.drillsLog,
    deps: productionChaosDeps(context, oneShot),
    gapMs: positiveInteger("--gap", options.gap) * 1000,
    rounds,
    recoveryMs: positiveInteger("--recovery", options.recovery) * 1000,
    stableMs: positiveInteger("--stable", options.stable) * 1000,
    ...(options.untilJourneyDone === true
      ? { shouldStop: () => journeyFinished(layout.journeyDir) }
      : {}),
    signal: abort.signal,
  });
  for (const record of summary.records)
    console.log(
      `drill ${record.drill} (${record.target}): ${!record.ok ? "FAILED" : record.skipped === true ? "skipped" : "recovered"} - ${record.detail}`,
    );
  const { recovered, skipped, failed } = summary.counts;
  console.log(
    `drills: ${recovered} recovered, ${skipped} skipped, ${failed} failed`,
  );
  if (summary.failures.length > 0) process.exitCode = 1;
};

const program = new Command()
  .name("midgard-devnet-stack")
  .description(
    "Create, resume and run a private Midgard devnet: chain, deployment, node, DA committee and watcher",
  );

registerUp(program, (options) => {
  const layout = layoutFor(options.runDir);
  return {
    deps: upDeps(layout, (code) =>
      resumeUp(layout, options.readyTimeoutMs, code),
    ),
    launch: {
      execPath: process.execPath,
      execArgv: process.execArgv,
      argv: process.argv,
      env: process.env,
      signals: process,
    },
  };
});

program
  .command("supervise")
  .description(
    "Run and keep alive every service of a deployed run (started by up)",
  )
  .requiredOption("--run-dir <path>", "Absolute run directory")
  .action(supervise);

program
  .command("status")
  .description(
    "Print the supervisor, each service's liveness and readiness, the DA bond and every endurance reason",
  )
  .requiredOption("--run-dir <path>", "Absolute run directory")
  .action(status);

program
  .command("journey")
  .description(
    "Run the user journey (deposits, L2 transfers, withdrawals to L1 payout) through the supported user interfaces; resumes from its journal",
  )
  .requiredOption("--run-dir <path>", "Absolute run directory")
  .option(
    "--idle <seconds>",
    "Length of the idle period with no activity",
    "300",
  )
  .action(journey);

registerAcceptanceCommands(program, (runDir) => {
  const layout = layoutFor(runDir);
  startRunUser(layout, "journey");
  startRunUser(layout, "drill");
  return runningContext(layout);
});

program
  .command("drill")
  .description(
    "Inject faults into the running stack one at a time (service kills, some at chosen pipeline moments, and L1 outages) and check each recovers with no manual step",
  )
  .requiredOption("--run-dir <path>", "Absolute run directory")
  .option(
    "--drills <names>",
    "Comma-separated drill names, in order (default: every drill)",
  )
  .option("--list", "Print the drills this run offers and exit")
  .option("--gap <seconds>", "Pause between two drills", "120")
  .option(
    "--rounds <count>",
    'Passes over the drills, or "forever" with --until-journey-done',
    "1",
  )
  .option(
    "--recovery <seconds>",
    "Bound on every service being ready again after a drill",
    "900",
  )
  .option(
    "--stable <seconds>",
    "How long every service must then stay ready, with no restart, to count as recovered",
    String(STABLE_WINDOW_MS / 1000),
  )
  .option("--outage <seconds>", "Length of a Kupo or Ogmios outage", "60")
  .option("--pause <seconds>", "Length of a Postgres pause", "30")
  .option("--until-journey-done", "Stop once the run's journey has finished")
  .action(drill);

program
  .command("down")
  .description(
    "Stop the services; the chain, deployment and all state are kept",
  )
  .requiredOption("--run-dir <path>", "Absolute run directory")
  .option("--l1", "Also stop the L1 containers (never removes their volumes)")
  .action(async (options: { runDir: string; l1?: boolean }) => {
    await down({ runDir: options.runDir, l1: options.l1 === true });
  });

registerServiceRecovery(program);

program.parseAsync().catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
});
