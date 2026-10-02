import { createHash } from "node:crypto";
import { join } from "node:path";

import {
  COMMITTEE_SIZE,
  committeeEnvironment,
  grantPublicReader,
  publicRetainedDaEnvironment,
} from "./da.js";
import type { DeployContext } from "./deploy.js";
import { recordedHistoryGenesisPin } from "./history-pin.js";
import { walletInfos } from "./identities.js";
import { servicePorts } from "./layout.js";
import { type HubOracleOneShot, nodeEnvironment } from "./node-env.js";
import {
  enduranceReasons,
  runEnduranceMaintainer,
} from "./reserve-float-chain.js";
import type {
  InProcessMaintainer,
  ServiceSpec,
  SupervisorPaths,
} from "./supervisor.js";
import { watcherServiceSpecs } from "./watcher.js";

/**
 * The node binds its HTTP server only once startup is done. Its default
 * startup budget: four provider steps (protocol status, DA provider
 * assertions, state-queue boundary seed, tx-order catch-up), each retried
 * STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS 120 x 5 s, plus the first-start
 * ledger scan's LEDGER_SCAN_TIMEOUT_MS of 15 min: 55 min, rounded up.
 */
export const NODE_START_GRACE_MS = 60 * 60_000;

export const supervisorPaths = (
  context: Pick<DeployContext, "layout">,
): SupervisorPaths => ({
  runDir: context.layout.runDir,
  pidDir: join(context.layout.state, "services"),
  events: context.layout.supervisorEvents,
  serviceLog: context.layout.serviceLog,
});

/**
 * Every long-running process of the stack. Each is a host process on this
 * controller's Node binary, reading only the run's own records.
 */
export const serviceSpecs = (
  context: DeployContext,
  oneShot: HubOracleOneShot,
): ServiceSpec[] => {
  const { layout, run, identities, artifacts } = context;
  const ports = servicePorts(run);
  const committees: ServiceSpec[] = Array.from(
    { length: COMMITTEE_SIZE },
    (_, index) => {
      const base = `http://127.0.0.1:${ports.committeeApi(index)}`;
      return {
        name: `da-committee-${index}`,
        command: process.execPath,
        args: [join(layout.daRoot, "dist/index.js")],
        cwd: layout.daRoot,
        env: committeeEnvironment({
          layout,
          run,
          identities,
          chainSyncBinary: artifacts.chainSyncBinary,
          index,
        }),
        healthUrl: `${base}/healthz`,
        readyUrl: `${base}/readyz`,
      };
    },
  );
  const node = `http://127.0.0.1:${ports.nodeHttp}`;
  return [
    ...committees,
    {
      name: "public-retained-da",
      command: process.execPath,
      args: [join(layout.daRoot, "dist/public-retained-da.js")],
      cwd: layout.daRoot,
      env: publicRetainedDaEnvironment({ layout, run, identities }),
      // Its tables exist once member 0 has migrated its store.
      prestart: () => grantPublicReader(layout, run, identities),
    },
    {
      name: "node",
      command: process.execPath,
      args: [join(layout.nodeRoot, "dist/index.js"), "listen"],
      cwd: layout.nodeRoot,
      env: nodeEnvironment({
        ...context,
        oneShot,
        historyGenesisPin: recordedHistoryGenesisPin(layout),
        role: "listen",
      }),
      healthUrl: `${node}/healthz`,
      readyUrl: `${node}/readyz`,
      startGraceMs: NODE_START_GRACE_MS,
    },
    ...watcherServiceSpecs(context, oneShot),
  ];
};

/** What the endurance maintainer and its status report read. */
const enduranceInputs = (context: DeployContext) => ({
  layout: context.layout,
  run: context.run,
  wallets: walletInfos(context.identities),
});

/**
 * The work the run's supervisor keeps in its own process beside its services:
 * the reserve float and the endurance reasons, for the run's life. It
 * restarts with the dists (specsDigest's code stamp covers the supervisor's
 * bundle), and a fault of it never stops a service.
 */
export const supervisorMaintainers = (
  context: DeployContext,
): InProcessMaintainer[] => [
  {
    name: "endurance",
    run: (signal) =>
      runEnduranceMaintainer({ ...enduranceInputs(context), signal }),
  },
];

/** The endurance reasons for status; a report that fails reads as its error. */
export const enduranceReport = (
  context: DeployContext,
): Promise<readonly string[] | string> =>
  Promise.resolve()
    .then(() => enduranceReasons(enduranceInputs(context)))
    .catch((error: unknown) => String(error));

/**
 * Everything about a service set a running supervisor fixed when it started,
 * and the code it runs: `code` is the stamp of the runtime dists
 * (dist-freshness.ts codeStamp) when it started them. `up` compares it with
 * the set it would run now on the dists on disk, so a rebuild that changed
 * them restarts the supervisor and every service onto the new code. A
 * prestart is a closure, so only its presence counts.
 */
export const specsDigest = (
  specs: readonly ServiceSpec[],
  code: string,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify([
        code,
        specs.map((spec) => [
          spec.name,
          spec.command,
          spec.args,
          spec.cwd,
          Object.entries(spec.env).sort(([a], [b]) =>
            a < b ? -1 : a > b ? 1 : 0,
          ),
          spec.healthUrl ?? null,
          spec.readyUrl ?? null,
          spec.startGraceMs ?? null,
          spec.prestart !== undefined,
        ]),
      ]),
    )
    .digest("hex");
