import type { Command } from "commander";

import { type DeployContext, nodeCli } from "./deploy.js";
import {
  codeStamp,
  requireFreshDists,
  runtimeDistTargets,
} from "./dist-freshness.js";
import { requireSuccess } from "./exec.js";
import {
  ensureFurtherNodeDatabase,
  furtherNode,
  furtherNodeContext,
  furtherNodeLayout,
  furtherNodeSpec,
  loadFurtherNodes,
} from "./further-nodes.js";
import type { Layout } from "./layout.js";
import type { HubOracleOneShot } from "./node-env.js";
import {
  ensureSupervisor,
  runningSupervisor,
  serviceReport,
  startSupervisor,
  stopSupervisor,
  SUPERVISOR_STOP_MS,
  supervisorRuns,
  waitForService,
} from "./stack.js";

/** A deployed run's context, read from its records (the controller's). */
type RunningContext = (layout: Layout) => {
  readonly context: DeployContext;
  readonly oneShot: HubOracleOneShot;
};

export const furtherNodeNumber = (value: string): number => {
  const node = Number(value);
  if (!Number.isSafeInteger(node) || node < 2)
    throw new Error(
      `--node must be a further node, counted from 2, not ${JSON.stringify(value)}`,
    );
  return node;
};

/** The context, service and paths of the run's further node `node`. */
export const furtherNodeService = async (
  running: RunningContext,
  stack: Layout,
  node: number,
) => {
  const { context, oneShot } = running(stack);
  const own = furtherNodeContext(context, furtherNode(stack, node));
  const spec = await furtherNodeSpec(
    context,
    oneShot,
    furtherNode(stack, node),
  );
  return { context, oneShot, own, spec };
};

/** Each further node's supervisor and service, for `status`. */
export const furtherNodeReports = (running: RunningContext, stack: Layout) =>
  Promise.all(
    loadFurtherNodes(stack).map(async ({ node }) => {
      const { own, spec } = await furtherNodeService(running, stack, node);
      return {
        supervisor: runningSupervisor(own.layout) ?? null,
        service: await serviceReport(own.layout, spec),
      };
    }),
  );

/** Stops every running further node of the run. */
export const stopFurtherNodes = async (stack: Layout) => {
  for (const { node } of loadFurtherNodes(stack))
    if (
      await stopSupervisor(furtherNodeLayout(stack, node), SUPERVISOR_STOP_MS)
    )
      console.log(`node-${node}: stopped`);
};

/**
 * Starts a further node and, unless told not to, registers its operator. A
 * node that starts without a ledger takes the confirmed one from the run's
 * other nodes, so it can join after blocks have merged.
 */
const joinNode = async (
  running: RunningContext,
  stack: Layout,
  options: { node: string; readyTimeout: string; register: boolean },
) => {
  const node = furtherNodeNumber(options.node);
  const readyTimeoutMs = Number(options.readyTimeout) * 1000;
  if (!(readyTimeoutMs > 0))
    throw new Error("--ready-timeout must be a number of seconds above 0");
  const { context, oneShot, own, spec } = await furtherNodeService(
    running,
    stack,
    node,
  );
  requireFreshDists(stack, "join-node");
  await ensureFurtherNodeDatabase(stack, context.run, node);
  const supervisor = await ensureSupervisor({
    running: () => runningSupervisor(own.layout),
    runs: () =>
      supervisorRuns(own.layout, [spec], codeStamp(runtimeDistTargets(stack))),
    stop: () => stopSupervisor(own.layout, SUPERVISOR_STOP_MS),
    start: async () => startSupervisor(own.layout, node),
  });
  await waitForService(own.layout, spec, supervisor, readyTimeoutMs);
  console.log(`${spec.name}: ready`);
  if (!options.register) return;
  const label = `${spec.name}-register-active-operator`;
  requireSuccess(
    await nodeCli(own, ["register-active-operator"], label, oneShot),
    label,
  );
  console.log(`${spec.name}: operator registered`);
};

export const registerJoinNode = (
  program: Command,
  layoutFor: (runDir: string) => Layout,
  running: RunningContext,
) =>
  program
    .command("join-node")
    .description(
      "Start a further operator's node of a run created with up --further-nodes, wait until it is ready and register its operator; a node without a ledger takes the confirmed one from the run's other nodes",
    )
    .requiredOption("--run-dir <path>", "Absolute run directory")
    .requiredOption("--node <number>", "The further node, counted from 2")
    .option(
      "--ready-timeout <seconds>",
      "How long to wait for the node to become ready",
      "1200",
    )
    .option("--no-register", "Start the node without registering its operator")
    .action(
      (options: {
        runDir: string;
        node: string;
        readyTimeout: string;
        register: boolean;
      }) => joinNode(running, layoutFor(options.runDir), options),
    );
