import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  armExitDeadline,
  ROLE_EXIT_DEADLINE_MS,
} from "@al-ft/midgard-l1-follower";

import type { WatcherAvailabilityStatusTransition } from "../availability/runtime.js";
import {
  WATCHER_STARTUP_FAILED,
  WatcherStartupHeldError,
} from "./startup-operations.js";

export const WATCHER_PACKAGE_NAME = "midgard-watcher";
export const WATCHER_COMMAND_FAILURE_EXIT_CODE = 70;
export type WatcherCommand = "replay" | "start";

export type WatcherCommandIo = Readonly<{
  writeOutput: (text: string) => void;
  writeError: (text: string) => void;
}>;

type WatcherCommandDependencies = Readonly<{
  runWatcher(
    configPath: string,
    onStartupProgress: (progress: WatcherStartupProgress) => void,
    onAvailabilityStatusTransition: (
      event: WatcherAvailabilityStatusTransition,
    ) => void,
  ): Promise<
    Readonly<{
      done: Promise<void>;
      caughtUp: Promise<void>;
      faultProofReadiness: readonly Readonly<{
        ready: true;
        category: string;
      }>[];
      recoveredFaultProofWorkflowCount: number;
      faultProofSupervisor: Readonly<{
        status(): Readonly<{
          phase: "accepting" | "blocked" | "closing" | "closed";
          recovered: boolean;
          deadlineHealth: "safe" | "at_risk" | "unsafe";
        }>;
      }>;
      close(): Promise<void>;
    }>
  >;
  waitForShutdown(stop?: AbortSignal): Promise<"SIGINT" | "SIGTERM">;
  /**
   * Exits the process. Without it, as in tests, a failing watcher's close
   * has no deadline.
   */
  exit?: (code: number) => void;
  /** The close deadline on a failure; the roles' shared one by default. */
  exitDeadlineMs?: number;
}>;

/**
 * The first SIGINT or SIGTERM. Its listeners are removed once one arrives
 * or `stop` aborts, so none is left to swallow a later signal.
 */
export const waitForShutdown = async (
  stop?: AbortSignal,
): Promise<"SIGINT" | "SIGTERM"> =>
  await new Promise((resolve) => {
    const onSigint = () => finish("SIGINT");
    const onSigterm = () => finish("SIGTERM");
    const remove = () => {
      process.off("SIGINT", onSigint);
      process.off("SIGTERM", onSigterm);
      stop?.removeEventListener("abort", remove);
    };
    const finish = (signal: "SIGINT" | "SIGTERM") => {
      remove();
      resolve(signal);
    };
    process.once("SIGINT", onSigint);
    process.once("SIGTERM", onSigterm);
    stop?.addEventListener("abort", remove, { once: true });
  });

const productionDependencies: WatcherCommandDependencies = Object.freeze({
  runWatcher: async (
    configPath,
    onStartupProgress,
    onAvailabilityStatusTransition,
  ) => {
    const [{ loadWatcherProcessConfigFile }, { createWatcherRuntime }] =
      await Promise.all([
        import("./process-config.js"),
        import("./watcher-runtime.js"),
      ]);
    return await createWatcherRuntime({
      config: await loadWatcherProcessConfigFile(configPath),
      onStartupProgress,
      onAvailabilityStatusTransition,
    });
  },
  waitForShutdown,
  exit: (code) => process.exit(code),
});

const commandStatus = (input: Readonly<Record<string, unknown>>): string =>
  `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, ...input })}\n`;

/**
 * A `start` whose startup failed and holds (`WatcherStartupHeldError`):
 * the process stays up, its operations server naming `startup_failed`,
 * until an operator stops it; it then exits non-zero, having never served.
 * A `replay` is one-shot and exits on it at once.
 */
const holdUntilShutdown = async (
  command: WatcherCommand,
  held: WatcherStartupHeldError,
  io: WatcherCommandIo,
  dependencies: WatcherCommandDependencies,
): Promise<number> => {
  if (command === "replay") {
    await held.release();
    throw held;
  }
  io.writeError(
    commandStatus({
      command,
      state: "startup_held",
      productionReady: false,
      reason: WATCHER_STARTUP_FAILED,
      error: held.message,
    }),
  );
  const signal = await dependencies.waitForShutdown();
  io.writeOutput(commandStatus({ command, state: "stopping", signal }));
  await held.release();
  return WATCHER_COMMAND_FAILURE_EXIT_CODE;
};

/**
 * Closes the runtime. On the way to a non-zero exit (`failing`) the close
 * has the roles' shared exit deadline (`armExitDeadline`): past it the
 * process names the deadline and exits 70, so a hung close cannot keep a
 * failed watcher up.
 */
const closeRuntime = async (
  runtime: Readonly<{ close(): Promise<void> }>,
  failing: boolean,
  command: WatcherCommand,
  io: WatcherCommandIo,
  dependencies: WatcherCommandDependencies,
): Promise<void> => {
  const exit = dependencies.exit;
  const deadlineMs = dependencies.exitDeadlineMs ?? ROLE_EXIT_DEADLINE_MS;
  const disarm =
    failing && exit !== undefined
      ? armExitDeadline({
          code: WATCHER_COMMAND_FAILURE_EXIT_CODE,
          deadlineMs,
          exit: (code) => {
            io.writeError(
              commandStatus({
                command,
                state: "close_deadline_exceeded",
                productionReady: false,
                deadlineMs,
              }),
            );
            exit(code);
          },
        })
      : undefined;
  try {
    await runtime.close();
  } finally {
    disarm?.();
  }
};

const execute = async (
  command: WatcherCommand,
  configPath: string,
  io: WatcherCommandIo,
  dependencies: WatcherCommandDependencies,
): Promise<number> => {
  const started = await dependencies
    .runWatcher(
      configPath,
      (progress) =>
        io.writeError(
          commandStatus({
            command,
            state: "starting",
            productionReady: false,
            ...progress,
          }),
        ),
      (event) =>
        io.writeError(
          commandStatus({
            command,
            state: "availability_status",
            productionReady: false,
            ...event,
          }),
        ),
    )
    .catch((error: unknown) => {
      if (error instanceof WatcherStartupHeldError) return error;
      throw error;
    });
  if (started instanceof WatcherStartupHeldError)
    return await holdUntilShutdown(command, started, io, dependencies);
  const runtime = started;
  const supervisor = runtime.faultProofSupervisor.status();
  // A proof deadline at risk or unsafe never stops the process: a restart
  // would meet the same deadline again. The record carries the health, and
  // readiness names it (deadline_at_risk, deadline_unsafe) while the
  // supervisor keeps proving.
  if (
    runtime.faultProofReadiness.length === 0 ||
    runtime.faultProofReadiness.some(({ ready }) => ready !== true) ||
    !Number.isSafeInteger(runtime.recoveredFaultProofWorkflowCount) ||
    runtime.recoveredFaultProofWorkflowCount < 0 ||
    supervisor.phase !== "accepting" ||
    supervisor.recovered !== true
  ) {
    await closeRuntime(runtime, true, command, io, dependencies);
    throw new Error("watcher production proof supervision is not ready");
  }
  io.writeOutput(
    commandStatus({
      command,
      state: "ready",
      productionReady: true,
      proofCategories: runtime.faultProofReadiness.map(
        ({ category }) => category,
      ),
      recoveredFaultProofWorkflowCount:
        runtime.recoveredFaultProofWorkflowCount,
      proofSupervisorState: supervisor.phase,
      proofDeadlineHealth: supervisor.deadlineHealth,
    }),
  );
  const stopWaiting = new AbortController();
  let failing = true;
  try {
    if (command === "replay") {
      const replayEnd = await Promise.race([
        runtime.caughtUp.then(() => "caught_up" as const),
        runtime.done.then(() => "runtime_stopped" as const),
      ]);
      if (replayEnd === "runtime_stopped") {
        throw new Error(
          "watcher production liveness ended before durable catch-up",
        );
      }
      io.writeOutput(commandStatus({ command, state: "caught_up" }));
      failing = false;
      return 0;
    }
    const stop = await Promise.race([
      runtime.done.then(() => ({ kind: "runtime_stopped" as const })),
      dependencies.waitForShutdown(stopWaiting.signal).then((signal) => ({
        kind: "operator_shutdown" as const,
        signal,
      })),
    ]);
    if (stop.kind === "runtime_stopped") {
      throw new Error("watcher production liveness ended before shutdown");
    }
    io.writeOutput(
      commandStatus({ command, state: "stopping", signal: stop.signal }),
    );
    failing = false;
    return 0;
  } finally {
    stopWaiting.abort();
    await closeRuntime(runtime, failing, command, io, dependencies);
  }
};

export const runWatcherCommand = async (
  command: WatcherCommand,
  configPath: string,
  io: WatcherCommandIo,
): Promise<number> => {
  // The watcher's replay verifies Ed25519 signatures with libsodium, which
  // answers only once it has initialised.
  await ledgerEd25519SignatureReady();
  return await execute(command, configPath, io, productionDependencies);
};

/** Test-only process seam; it cannot change production dependency selection. */
export const unsafeRunWatcherCommandForTest = async (
  command: WatcherCommand,
  configPath: string,
  io: WatcherCommandIo,
  dependencies: WatcherCommandDependencies,
): Promise<number> => await execute(command, configPath, io, dependencies);
import type { WatcherStartupProgress } from "./startup-progress.js";
