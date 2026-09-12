import { describe, expect, it } from "vitest";

import { parseWatcherArguments } from "../../src/cli.js";
import { unsafeRunWatcherCommandForTest } from "../../src/runtime/scaffold.js";

const ready = (
  overrides: {
    readonly phase?: "accepting" | "blocked" | "closing" | "closed";
    readonly recovered?: boolean;
    readonly deadlineHealth?: "safe" | "at_risk" | "unsafe";
    readonly done?: Promise<void>;
    readonly caughtUp?: Promise<void>;
    readonly faultProofReadiness?: readonly Readonly<{
      ready: true;
      category: string;
    }>[];
    readonly recoveredFaultProofWorkflowCount?: number;
    readonly onClose?: () => void;
  } = {},
) => ({
  done: overrides.done ?? new Promise<void>(() => undefined),
  caughtUp: overrides.caughtUp ?? Promise.resolve(),
  faultProofReadiness: overrides.faultProofReadiness ?? [
    { ready: true as const, category: "doubleSpend" },
  ],
  recoveredFaultProofWorkflowCount:
    overrides.recoveredFaultProofWorkflowCount ?? 0,
  faultProofSupervisor: {
    status: () => ({
      phase: overrides.phase ?? ("accepting" as const),
      recovered: overrides.recovered ?? true,
      deadlineHealth: overrides.deadlineHealth ?? ("safe" as const),
    }),
  },
  close: async () => {
    overrides.onClose?.();
  },
});

describe("production watcher command arguments", () => {
  it.each([
    {
      name: "start",
      argv: ["start", "--config", "/etc/watcher.json"],
      command: "start",
    },
    {
      name: "replay",
      argv: ["replay", "--config", "/etc/watcher.json"],
      command: "replay",
    },
    {
      name: "authority",
      argv: ["authority", "--config", "/etc/authority.json"],
      command: "authority",
    },
  ])("parses $name with its explicit config path", ({ argv, command }) => {
    expect(parseWatcherArguments(argv)).toEqual({
      kind: "command",
      command,
      configPath: argv[2],
    });
  });

  it.each([
    { name: "no arguments at all", argv: [] },
    { name: "a command with no config flag", argv: ["start"] },
    { name: "a config flag with no path", argv: ["start", "--config"] },
    { name: "an empty config path", argv: ["start", "--config", ""] },
    {
      name: "an unknown command",
      argv: ["prove", "--config", "/etc/watcher.json"],
    },
    {
      name: "a misspelled config flag",
      argv: ["start", "--configuration", "/etc/watcher.json"],
    },
    {
      name: "a trailing extra argument",
      argv: ["start", "--config", "/etc/watcher.json", "--force"],
    },
    {
      name: "the config flag before the command",
      argv: ["--config", "/etc/watcher.json", "start"],
    },
  ])("refuses $name", ({ argv }) => {
    expect(parseWatcherArguments(argv)).toEqual({
      kind: "invalid",
      reason: "expected an explicit command and --config path",
    });
  });

  it.each([["--help"], ["-h"], ["help"]])("prints usage for %s", (flag) => {
    expect(parseWatcherArguments([flag])).toEqual({ kind: "help" });
  });

  it("does not treat a help flag as a command elsewhere in the line", () => {
    expect(parseWatcherArguments(["start", "--help"])).toMatchObject({
      kind: "invalid",
    });
  });
});

describe("production watcher commands", () => {
  const runCommand = async (
    command: "start" | "replay",
    runtime: ReturnType<typeof ready>,
    io: {
      writeOutput: (text: string) => void;
      writeError: (text: string) => void;
    } = { writeOutput: () => undefined, writeError: () => undefined },
  ) =>
    await unsafeRunWatcherCommandForTest(command, "/etc/watcher.json", io, {
      runAuthority: async () => ({ close: async () => undefined }),
      runWatcher: async () => runtime,
      waitForShutdown: async () => "SIGTERM" as const,
    });

  it("reports the admitted readiness record, then closes on catch-up and on shutdown", async () => {
    const lines: string[] = [];
    const io = {
      writeOutput: (text: string) => lines.push(text),
      writeError: (text: string) => lines.push(text),
    };
    let closes = 0;
    const runtimeFor = (command: "start" | "replay") =>
      ready({
        faultProofReadiness: [
          { ready: true as const, category: "doubleSpend" },
          { ready: true as const, category: "missingSignature" },
        ],
        recoveredFaultProofWorkflowCount: 3,
        ...(command === "replay" ? {} : {}),
        onClose: () => {
          closes += 1;
        },
      });

    await expect(runCommand("replay", runtimeFor("replay"), io)).resolves.toBe(
      0,
    );
    await expect(runCommand("start", runtimeFor("start"), io)).resolves.toBe(0);

    // Both commands close the runtime they opened.
    expect(closes).toBe(2);

    const records = lines.map(
      (line) => JSON.parse(line) as Record<string, unknown>,
    );
    expect(records).toEqual([
      {
        packageName: "midgard-watcher",
        command: "replay",
        state: "ready",
        productionReady: true,
        proofCategories: ["doubleSpend", "missingSignature"],
        recoveredFaultProofWorkflowCount: 3,
        proofSupervisorState: "accepting",
        proofDeadlineHealth: "safe",
      },
      {
        packageName: "midgard-watcher",
        command: "replay",
        state: "caught_up",
      },
      {
        packageName: "midgard-watcher",
        command: "start",
        state: "ready",
        productionReady: true,
        proofCategories: ["doubleSpend", "missingSignature"],
        recoveredFaultProofWorkflowCount: 3,
        proofSupervisorState: "accepting",
        proofDeadlineHealth: "safe",
      },
      {
        packageName: "midgard-watcher",
        command: "start",
        state: "stopping",
        signal: "SIGTERM",
      },
    ]);
  });

  it.each([
    {
      name: "journal recovery supervision is blocked",
      overrides: { phase: "blocked" as const, recovered: false },
    },
    {
      name: "the supervisor has not finished recovering",
      overrides: { recovered: false },
    },
    {
      name: "the supervisor is already closing",
      overrides: { phase: "closing" as const },
    },
    {
      name: "a proof deadline is at risk",
      overrides: { deadlineHealth: "at_risk" as const },
    },
    {
      name: "a proof deadline is already unsafe",
      overrides: { deadlineHealth: "unsafe" as const },
    },
    {
      name: "no fault-proof category reported readiness",
      overrides: { faultProofReadiness: [] },
    },
    {
      name: "the recovered workflow count is not a natural number",
      overrides: { recoveredFaultProofWorkflowCount: -1 },
    },
  ])(
    "refuses to advertise readiness when $name, and closes the runtime",
    async ({ overrides }) => {
      let closed = false;
      const lines: string[] = [];
      await expect(
        runCommand(
          "start",
          ready({ ...overrides, onClose: () => (closed = true) }),
          {
            writeOutput: (text: string) => lines.push(text),
            writeError: (text: string) => lines.push(text),
          },
        ),
      ).rejects.toThrow("watcher production proof supervision is not ready");
      expect(closed).toBe(true);
      // A refused start must not have advertised readiness first.
      expect(lines).toEqual([]);
    },
  );

  it("treats an unexpected clean runtime exit as a liveness failure and closes", async () => {
    let closed = false;
    await expect(
      unsafeRunWatcherCommandForTest(
        "start",
        "/etc/watcher.json",
        { writeOutput: () => undefined, writeError: () => undefined },
        {
          runAuthority: async () => ({ close: async () => undefined }),
          runWatcher: async () =>
            ready({
              done: Promise.resolve(),
              onClose: () => (closed = true),
            }),
          waitForShutdown: async () =>
            await new Promise<"SIGTERM">(() => undefined),
        },
      ),
    ).rejects.toThrow("watcher production liveness ended before shutdown");
    expect(closed).toBe(true);
  });

  it("fails replay instead of hanging when runtime liveness ends before catch-up", async () => {
    let closed = false;
    await expect(
      runCommand(
        "replay",
        ready({
          done: Promise.resolve(),
          caughtUp: new Promise<void>(() => undefined),
          onClose: () => (closed = true),
        }),
      ),
    ).rejects.toThrow(
      "watcher production liveness ended before durable catch-up",
    );
    expect(closed).toBe(true);
  });

  it("keeps the trusted-head authority process separate and closes it on signal", async () => {
    const events: string[] = [];
    await expect(
      unsafeRunWatcherCommandForTest(
        "authority",
        "/etc/authority.json",
        {
          writeOutput: (text) => events.push(text),
          writeError: (text) => events.push(text),
        },
        {
          runAuthority: async () => ({
            close: async () => {
              events.push("authority-closed");
            },
          }),
          runWatcher: async () => {
            throw new Error("watcher process must not be constructed");
          },
          waitForShutdown: async () => "SIGINT",
        },
      ),
    ).resolves.toBe(0);
    // The authority command advertises its own readiness record — never the
    // watcher's proof-supervision record — and closes only after shutdown.
    expect(events).toHaveLength(2);
    expect(JSON.parse(events[0]!)).toEqual({
      packageName: "midgard-watcher",
      command: "authority",
      state: "ready",
    });
    expect(events[1]).toBe("authority-closed");
  });

  it("retains availability failure timing when startup subsequently fails", async () => {
    const errors: string[] = [];
    const output: string[] = [];
    await expect(
      unsafeRunWatcherCommandForTest(
        "start",
        "/etc/watcher.json",
        {
          writeOutput: (text) => output.push(text),
          writeError: (text) => errors.push(text),
        },
        {
          runAuthority: async () => ({ close: async () => undefined }),
          runWatcher: async (_config, _startup, onAvailability) => {
            onAvailability({
              status: {
                phase: "blocked",
                pendingHeaders: ["ab".repeat(28)],
                detail: "authenticated snapshot unavailable",
              },
              observationDigest: "cd".repeat(32),
              nativePoint: {
                blockHash: "ef".repeat(32),
                parentBlockHash: null,
                slot: "24729",
                blockNo: "1273",
                chainPointId: "12".repeat(32),
                finalityDepth: "30",
              },
              elapsedMs: 1250,
              observedAt: "2026-09-11T07:00:00.000Z",
            });
            throw new Error("later startup failure");
          },
          waitForShutdown: async () => "SIGTERM",
        },
      ),
    ).rejects.toThrow("later startup failure");
    expect(output).toEqual([]);
    expect(errors).toHaveLength(1);
    expect(JSON.parse(errors[0]!)).toMatchObject({
      packageName: "midgard-watcher",
      command: "start",
      state: "availability_status",
      productionReady: false,
      status: {
        phase: "blocked",
        detail: "authenticated snapshot unavailable",
      },
      elapsedMs: 1250,
      nativePoint: { slot: "24729", blockNo: "1273" },
    });
  });
});
