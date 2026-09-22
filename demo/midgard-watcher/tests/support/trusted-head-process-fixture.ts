import { fork } from "node:child_process";
import { mkdtemp, rm } from "node:fs/promises";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { build } from "esbuild";

import type { startWatcherTrustedHeadAuthorityProcess } from "../../src/runtime/trusted-head-runtime.js";

type Input = Parameters<typeof startWatcherTrustedHeadAuthorityProcess>[0];
const supportDirectory = dirname(fileURLToPath(import.meta.url));

/** Runs the current authority source on its own event loop, as the deployed sidecar does. */
export const startWatcherTrustedHeadAuthorityChildForTest = async (
  input: Input,
): Promise<
  Readonly<{
    server: Readonly<{ endpoint: string }>;
    processId: number;
    close(): Promise<void>;
  }>
> => {
  const directory = await mkdtemp(
    join(supportDirectory, ".trusted-head-process-"),
  );
  const entry = join(directory, "entry.mjs");
  try {
    await build({
      entryPoints: [join(supportDirectory, "trusted-head-process-entry.ts")],
      outfile: entry,
      bundle: true,
      platform: "node",
      target: "node22",
      format: "esm",
      conditions: ["midgard-source"],
      logLevel: "silent",
      banner: {
        js: 'import { createRequire as __createRequire } from "node:module"; const require = __createRequire(import.meta.url);',
      },
      plugins: [
        {
          name: "external-npm-source-workspaces",
          setup(builder) {
            builder.onResolve({ filter: /^[^./]/u }, async (args) => {
              if (args.pluginData === "external-resolution") return undefined;
              if (
                args.path.startsWith("@al-ft/midgard-") ||
                args.path.startsWith("midgard-node") ||
                args.path.startsWith("da-committee-node")
              )
                return undefined;
              if (args.path.startsWith("node:"))
                return { path: args.path, external: true };
              const resolved = await builder.resolve(args.path, {
                importer: args.importer,
                resolveDir: args.resolveDir,
                kind: args.kind,
                pluginData: "external-resolution",
              });
              return { ...resolved, external: true };
            });
          },
        },
      ],
    });
  } catch (error) {
    await rm(directory, { recursive: true, force: true });
    throw error;
  }
  const child = fork(entry, [], {
    execArgv: [],
    stdio: ["ignore", "ignore", "ignore", "ipc"],
  });
  const killOnParentExit = (): void => {
    child.kill("SIGKILL");
  };
  process.once("exit", killOnParentExit);
  let exited = false;
  const exit = new Promise<void>((resolve) =>
    child.once("close", () => {
      exited = true;
      resolve();
    }),
  );
  let closing: Promise<void> | undefined;
  const close = (): Promise<void> => {
    closing ??= (async () => {
      if (!exited) {
        if (child.connected) child.send({ kind: "close" }, () => undefined);
        else child.kill("SIGTERM");
        const timer = setTimeout(() => child.kill("SIGKILL"), 5_000);
        try {
          await exit;
        } finally {
          clearTimeout(timer);
        }
      }
      process.removeListener("exit", killOnParentExit);
      await rm(directory, { recursive: true, force: true });
    })();
    return closing;
  };
  try {
    const ready = await new Promise<{ endpoint: string; processId: number }>(
      (resolve, reject) => {
        const timer = setTimeout(
          () => fail(new Error("trusted-head child startup timed out")),
          30_000,
        );
        const cleanup = (): void => {
          clearTimeout(timer);
          child.removeListener("message", onMessage);
          child.removeListener("error", fail);
          child.removeListener("exit", onExit);
        };
        const fail = (error: Error): void => {
          cleanup();
          reject(error);
        };
        const onExit = (): void =>
          fail(new Error("trusted-head child exited before readiness"));
        const onMessage = (message: unknown): void => {
          if (typeof message !== "object" || message === null) return;
          if (!("kind" in message)) return;
          if (message.kind === "error") {
            fail(new Error("trusted-head child startup failed"));
            return;
          }
          if (
            message.kind !== "ready" ||
            !("endpoint" in message) ||
            typeof message.endpoint !== "string" ||
            !("processId" in message) ||
            typeof message.processId !== "number" ||
            message.processId !== child.pid
          )
            return;
          cleanup();
          resolve({ endpoint: message.endpoint, processId: message.processId });
        };
        child.on("message", onMessage);
        child.once("error", fail);
        child.once("exit", onExit);
        child.send({ kind: "start", input }, (error) => {
          if (error) fail(error);
        });
      },
    );
    return Object.freeze({
      server: Object.freeze({ endpoint: ready.endpoint }),
      processId: ready.processId,
      close,
    });
  } catch (error) {
    await close();
    throw error;
  }
};
