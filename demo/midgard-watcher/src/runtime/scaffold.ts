export const WATCHER_PACKAGE_NAME = "midgard-watcher";
export const WATCHER_COMMAND_FAILURE_EXIT_CODE = 70;
export type WatcherCommand = "authority" | "replay" | "start";

export type WatcherCommandIo = Readonly<{
  writeOutput: (text: string) => void;
  writeError: (text: string) => void;
}>;

type WatcherCommandDependencies = Readonly<{
  runAuthority(
    configPath: string,
  ): Promise<Readonly<{ close(): Promise<void> }>>;
  runWatcher(configPath: string): Promise<
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
  waitForShutdown(): Promise<"SIGINT" | "SIGTERM">;
}>;

const waitForShutdown = async (): Promise<"SIGINT" | "SIGTERM"> =>
  await new Promise((resolve) => {
    const onSigint = () => finish("SIGINT");
    const onSigterm = () => finish("SIGTERM");
    const finish = (signal: "SIGINT" | "SIGTERM") => {
      process.off("SIGINT", onSigint);
      process.off("SIGTERM", onSigterm);
      resolve(signal);
    };
    process.once("SIGINT", onSigint);
    process.once("SIGTERM", onSigterm);
  });

const productionDependencies: WatcherCommandDependencies = Object.freeze({
  runAuthority: async (configPath) => {
    const [
      { loadWatcherTrustedHeadAuthorityProcessConfigFile },
      { startWatcherTrustedHeadAuthorityProcess },
    ] = await Promise.all([
      import("./process-config.js"),
      import("./trusted-head-runtime.js"),
    ]);
    return await startWatcherTrustedHeadAuthorityProcess({
      config:
        await loadWatcherTrustedHeadAuthorityProcessConfigFile(configPath),
    });
  },
  runWatcher: async (configPath) => {
    const [{ loadWatcherProcessConfigFile }, { createWatcherRuntime }] =
      await Promise.all([
        import("./process-config.js"),
        import("./watcher-runtime.js"),
      ]);
    return await createWatcherRuntime({
      config: await loadWatcherProcessConfigFile(configPath),
    });
  },
  waitForShutdown,
});

const commandStatus = (input: Readonly<Record<string, unknown>>): string =>
  `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, ...input })}\n`;

const execute = async (
  command: WatcherCommand,
  configPath: string,
  io: WatcherCommandIo,
  dependencies: WatcherCommandDependencies,
): Promise<number> => {
  if (command === "authority") {
    const authority = await dependencies.runAuthority(configPath);
    io.writeOutput(commandStatus({ command, state: "ready" }));
    try {
      await dependencies.waitForShutdown();
    } finally {
      await authority.close();
    }
    return 0;
  }
  const runtime = await dependencies.runWatcher(configPath);
  const supervisor = runtime.faultProofSupervisor.status();
  if (
    runtime.faultProofReadiness.length === 0 ||
    runtime.faultProofReadiness.some(({ ready }) => ready !== true) ||
    !Number.isSafeInteger(runtime.recoveredFaultProofWorkflowCount) ||
    runtime.recoveredFaultProofWorkflowCount < 0 ||
    supervisor.phase !== "accepting" ||
    supervisor.recovered !== true ||
    supervisor.deadlineHealth !== "safe"
  ) {
    await runtime.close();
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
      return 0;
    }
    const stop = await Promise.race([
      runtime.done.then(() => ({ kind: "runtime_stopped" as const })),
      dependencies.waitForShutdown().then((signal) => ({
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
    return 0;
  } finally {
    await runtime.close();
  }
};

export const runWatcherCommand = async (
  command: WatcherCommand,
  configPath: string,
  io: WatcherCommandIo,
): Promise<number> =>
  await execute(command, configPath, io, productionDependencies);

/** Test-only process seam; it cannot change production dependency selection. */
export const unsafeRunWatcherCommandForTest = async (
  command: WatcherCommand,
  configPath: string,
  io: WatcherCommandIo,
  dependencies: WatcherCommandDependencies,
): Promise<number> => await execute(command, configPath, io, dependencies);
