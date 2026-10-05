import { spawn } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";

const PASS = "PASS synthetic signed history public evidence";

/** Each directory keeps the 20 s bound one fixture process used to have. */
const DIRECTORY_DEADLINE_MS = 20_000;

/** Runs the compiled signed-history fixture once for `directories`, in its
 * own process group, and resolves after every directory reported success. */
const writeDirectories = (
  entry: string,
  cwd: string,
  directories: readonly string[],
) =>
  new Promise<void>((resolve, reject) => {
    const child = spawn(process.execPath, [entry, "--roots", ...directories], {
      cwd,
      detached: true,
      stdio: ["ignore", "pipe", "pipe"],
    });
    const pid = child.pid;
    const signal = (kind: NodeJS.Signals) => {
      if (pid === undefined) return;
      try {
        process.kill(-pid, kind);
      } catch {
        /* Already joined. */
      }
    };
    let output = "";
    let errors = "";
    let failure: Error | undefined;
    let killing: ReturnType<typeof setTimeout> | undefined;
    const stop = (reason: string) => {
      failure ??= Error(reason);
      signal("SIGTERM");
      killing ??= setTimeout(() => signal("SIGKILL"), 1000);
    };
    const timer = setTimeout(
      () => stop("owned fixture exceeded bounded deadline"),
      DIRECTORY_DEADLINE_MS * directories.length,
    );
    child.stdout.on("data", (bytes: Buffer) => {
      output += bytes.toString("utf8");
      if (output.length + errors.length > 1_048_576)
        stop("owned fixture output exceeded bound");
    });
    child.stderr.on("data", (bytes: Buffer) => {
      errors += bytes.toString("utf8");
      if (output.length + errors.length > 1_048_576)
        stop("owned fixture output exceeded bound");
    });
    child.once("error", (error) => {
      failure = error;
    });
    child.once("close", (code) => {
      clearTimeout(timer);
      clearTimeout(killing);
      signal("SIGKILL");
      const reported = new Set(output.split("\n"));
      if (failure !== undefined) reject(failure);
      else if (
        code !== 0 ||
        directories.some((directory) => !reported.has(`${PASS} ${directory}`))
      )
        reject(Error(`owned signed fixture failed: ${errors}`));
      else resolve();
    });
  });

/**
 * Fresh signed-history run directories, each written by the real compiled
 * fixture into its own new directory. Directories are written `batch` at a
 * time by one fixture process; every caller still receives a directory no
 * other caller has seen. `close` removes directories nobody took.
 */
export const makeHistoryBindingFixturePool = (input: {
  readonly entry: string;
  readonly cwd: string;
  readonly prefix: string;
  readonly batch: number;
}) => {
  const ready: string[] = [];
  let filling: Promise<void> | undefined;
  const fill = async () => {
    const directories = Array.from({ length: input.batch }, () =>
      mkdtempSync(input.prefix),
    );
    try {
      await writeDirectories(input.entry, input.cwd, directories);
    } catch (error) {
      for (const directory of directories)
        rmSync(directory, { recursive: true, force: true });
      throw error;
    }
    ready.push(...directories);
  };
  return {
    next: async (): Promise<string> => {
      while (ready.length === 0) {
        filling ??= fill().finally(() => {
          filling = undefined;
        });
        await filling;
      }
      return ready.shift()!;
    },
    close: () => {
      for (const directory of ready.splice(0))
        rmSync(directory, { recursive: true, force: true });
    },
  };
};
