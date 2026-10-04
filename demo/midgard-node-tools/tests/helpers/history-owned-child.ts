import { spawn } from "node:child_process";

/** Own one detached synthetic process group, bound output and always join it. */
export const historyOwnedChild = (
  command: string,
  args: readonly string[],
  options: {
    readonly cwd: string;
    readonly env?: NodeJS.ProcessEnv;
    readonly channel?: boolean;
  },
) => {
  const child = spawn(command, [...args], {
    cwd: options.cwd,
    env: options.env ?? process.env,
    detached: true,
    stdio: options.channel
      ? ["ignore", "pipe", "pipe", "pipe"]
      : ["ignore", "pipe", "pipe"],
  });
  let output = "";
  let joined = false;
  const signal = (kind: NodeJS.Signals) => {
    if (joined || child.pid === undefined) return;
    try {
      process.kill(-child.pid, kind);
    } catch {
      /* Already stopped. */
    }
  };
  const add = (bytes: Buffer) => {
    output += bytes.toString("utf8");
    if (output.length > 1048576) {
      output = output.slice(-1048576);
      signal("SIGTERM");
    }
  };
  child.stdout?.on("data", add);
  child.stderr?.on("data", add);
  const done = new Promise<{
    code: number | null;
    signal: NodeJS.Signals | null;
  }>((resolve, reject) => {
    child.once("error", reject);
    child.once("close", (code, stopped) => {
      joined = true;
      resolve({ code, signal: stopped });
    });
  });
  return {
    child,
    done,
    output: () => output,
    close: async () => {
      signal("SIGTERM");
      const kill = setTimeout(() => signal("SIGKILL"), 1000);
      try {
        await done;
      } finally {
        clearTimeout(kill);
      }
    },
  };
};
export type HistoryOwnedChild = ReturnType<typeof historyOwnedChild>;
