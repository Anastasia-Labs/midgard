import { spawn } from "node:child_process";
import { createWriteStream, mkdirSync } from "node:fs";
import { join } from "node:path";
import { setTimeout as delay } from "node:timers/promises";

import type { DeployContext } from "./deploy.js";
import type { ExecResult } from "./exec.js";
import { recordedHistoryGenesisPin } from "./history-pin.js";
import {
  awaitEarlierAttempts,
  type CliRunner,
  FatalJourneyError,
  SUBMISSION_MARKER_ENV,
  SUBMISSION_RUN_ENV,
} from "./journey-cli.js";
import type { JourneyOptions } from "./journey-runtime.options.js";
import { type HubOracleOneShot, nodeEnvironment } from "./node-env.js";

export const assertAcceptanceActive = (signal: AbortSignal) => {
  if (signal.aborted)
    throw new FatalJourneyError(
      "acceptance cancelled; preserve submitted intents",
    );
};

/** An acceptance command owns only the process group it just spawned. */
export const acceptanceExec = (
  command: string,
  args: readonly string[],
  options: {
    readonly cwd: string;
    readonly env: NodeJS.ProcessEnv;
    readonly logDir: string;
    readonly label: string;
    readonly signal: AbortSignal;
    readonly timeoutMs: number;
    readonly killGraceMs?: number;
  },
): Promise<ExecResult> => {
  assertAcceptanceActive(options.signal);
  mkdirSync(options.logDir, { recursive: true, mode: 0o700 });
  const log = join(
    options.logDir,
    `${crypto.randomUUID()}-${options.label}.log`,
  );
  const transcript = createWriteStream(log, { mode: 0o600 });
  transcript.write(`$ ${command} ${args.join(" ")}\n`);
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      cwd: options.cwd,
      env: { PATH: process.env.PATH, HOME: process.env.HOME, ...options.env },
      detached: true,
      stdio: ["ignore", "pipe", "pipe"],
    });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk: Buffer) => {
      stdout += chunk.toString();
      transcript.write(chunk);
    });
    child.stderr.on("data", (chunk: Buffer) => {
      stderr += chunk.toString();
      transcript.write(chunk);
    });
    let killTimer: NodeJS.Timeout | undefined;
    let stopping = false;
    let timedOut = false;
    let transcriptFailure: Error | undefined;
    const signalGroup = (signal: NodeJS.Signals) => {
      if (child.pid === undefined) return;
      try {
        process.kill(-child.pid, signal);
      } catch (error) {
        if ((error as NodeJS.ErrnoException).code !== "ESRCH") throw error;
      }
    };
    const stop = () => {
      if (stopping) return;
      stopping = true;
      signalGroup("SIGTERM");
      killTimer = setTimeout(() => {
        signalGroup("SIGKILL");
      }, options.killGraceMs ?? 30_000);
    };
    const timer = setTimeout(() => {
      timedOut = true;
      stop();
    }, options.timeoutMs);
    transcript.once("error", (error) => {
      transcriptFailure = error;
      stop();
    });
    options.signal.addEventListener("abort", stop, { once: true });
    if (options.signal.aborted) stop();
    const cleanup = () => {
      clearTimeout(timer);
      clearTimeout(killTimer);
      options.signal.removeEventListener("abort", stop);
    };
    child.once("error", (error) => {
      cleanup();
      transcript.end();
      reject(error);
    });
    child.once("close", (code, signal) => {
      cleanup();
      transcript.end(`\n[exit code=${code} signal=${signal}]\n`, () => {
        if (transcriptFailure !== undefined) {
          reject(
            new Error(`acceptance transcript failed: ${log}`, {
              cause: transcriptFailure,
            }),
          );
          return;
        }
        if (timedOut) {
          reject(
            new Error(
              `acceptance command timed out after ${options.timeoutMs / 1000} s; transcript ${log}`,
            ),
          );
          return;
        }
        resolve({ code, signal, stdout, stderr, log });
      });
    });
  });
};

/** Options injection leaves the normal Journey runtime unchanged. */
export const acceptanceJourneyOwner = (
  context: DeployContext,
  oneShot: HubOracleOneShot,
  signal: AbortSignal,
) => {
  const active = new Set<Promise<unknown>>();
  const track = <T>(promise: Promise<T>): Promise<T> => {
    active.add(promise);
    void promise.then(
      () => active.delete(promise),
      () => active.delete(promise),
    );
    return promise;
  };
  const sleep = async (ms: number) => {
    assertAcceptanceActive(signal);
    try {
      await track(delay(ms, undefined, { signal }));
    } finally {
      assertAcceptanceActive(signal);
    }
  };
  const runCli: CliRunner = async (request) => {
    assertAcceptanceActive(signal);
    if (request.submissionId !== undefined)
      await awaitEarlierAttempts(
        context.layout.journeyDir,
        request.submissionId,
        request.timeoutMs,
        console.log,
        sleep,
      );
    assertAcceptanceActive(signal);
    const result = await track(
      acceptanceExec(process.execPath, ["dist/index.js", ...request.args], {
        cwd: context.layout.nodeRoot,
        env: {
          ...nodeEnvironment({
            ...context,
            oneShot,
            historyGenesisPin: recordedHistoryGenesisPin(context.layout),
            role: "command",
          }),
          ...(request.user === undefined
            ? {}
            : { USER_SEED_PHRASE: context.identities.seeds[request.user] }),
          ...(request.submissionId === undefined
            ? {}
            : {
                [SUBMISSION_RUN_ENV]: context.layout.journeyDir,
                [SUBMISSION_MARKER_ENV]: request.submissionId,
              }),
        },
        logDir: join(context.layout.journeyDir, "logs"),
        label: request.label,
        timeoutMs: request.timeoutMs,
        signal,
      }),
    );
    assertAcceptanceActive(signal);
    return result;
  };
  const options: JourneyOptions = {
    runCli,
    sleep,
    fetch: (input, init) => {
      assertAcceptanceActive(signal);
      return track(
        fetch(input, {
          ...init,
          signal: AbortSignal.any([
            signal,
            ...(init?.signal ? [init.signal] : []),
          ]),
        }),
      );
    },
  };
  return {
    options,
    drain: async () => {
      while (active.size > 0) await Promise.allSettled([...active]);
    },
  };
};
