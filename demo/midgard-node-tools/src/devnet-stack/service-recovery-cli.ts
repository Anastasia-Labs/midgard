import { isAbsolute, resolve } from "node:path";

import type { Command } from "commander";

import {
  codeStamp,
  requireFreshDists,
  runtimeDistTargets,
} from "./dist-freshness.js";
import { refuseWhileBuilding, runLockPaths } from "./fresh-controller.js";
import { makeLayout } from "./layout.js";
import { acquireLock } from "./lock.js";
import { hasReadinessProbe } from "./service-readiness.js";
import { readServiceRecoveryContext } from "./service-recovery-context.js";
import { requestServiceRecovery } from "./service-refusal.js";
import { serviceSpecs, supervisorPaths } from "./services.js";
import { recordedOneShot, runningSupervisor, supervisorRuns } from "./stack.js";

/** Requests one retry on this supervisor's unchanged run/service set/code.
 * The explanation is operator input, never verified configuration evidence.
 * No deployment records, checkpoint, source binding or secret is rewritten. */
export const registerServiceRecovery = (program: Command): void => {
  program
    .command("recover-service")
    .description(
      "Request one validation attempt for a refused role after an explicit external correction",
    )
    .requiredOption("--run-dir <path>", "Absolute existing run directory")
    .requiredOption("--service <name>", "Refused role from status")
    .requiredOption("--refusal-id <id>", "Current refusal token from status")
    .requiredOption(
      "--note <text>",
      "Operator explanation of correction; not verified evidence",
    )
    .action(
      (options: {
        runDir: string;
        service: string;
        refusalId: string;
        note: string;
      }) => {
        if (!isAbsolute(options.runDir))
          throw new Error("--run-dir must be absolute");
        const layout = makeLayout(resolve(options.runDir));
        const stamp = codeStamp(runtimeDistTargets(layout));
        const release = acquireLock(layout.lock);
        try {
          refuseWhileBuilding(runLockPaths(layout).build, "recover-service");
          requireFreshDists(layout, "recover-service");
          if (codeStamp(runtimeDistTargets(layout)) !== stamp)
            throw new Error(
              "runtime code changed while recovery started; restart the command",
            );
          const oneShot = recordedOneShot(layout);
          if (oneShot === undefined)
            throw new Error("run has no completed deployment");
          const context = readServiceRecoveryContext(layout);
          const specs = serviceSpecs(context, oneShot);
          if (
            runningSupervisor(layout) === undefined ||
            !supervisorRuns(layout, specs, stamp)
          )
            throw new Error(
              "supervisor run/code/service set differs; use normal up validation first",
            );
          const service = specs.find((spec) => spec.name === options.service);
          if (service === undefined || !hasReadinessProbe(service))
            throw new Error(
              "role requires a configured validated readiness probe before scoped recovery",
            );
          const paths = supervisorPaths(context, specs);
          if (paths.deploymentBinding === undefined)
            throw new Error("run has no public deployment manifest");
          requestServiceRecovery(
            paths,
            service.name,
            options.refusalId,
            options.note,
          );
          console.log(
            `${service.name}: one validation attempt requested; refusal clears only after child readiness`,
          );
        } finally {
          release();
        }
      },
    );
};
