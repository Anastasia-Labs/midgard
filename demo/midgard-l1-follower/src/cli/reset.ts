import type { SqlBackend } from "../sql/backend.js";
import { openPostgresBackend } from "../sql/postgres-backend.js";
import { openSqliteBackend } from "../sql/sqlite-backend.js";
import { resetToOrigin } from "../store/reset.js";

export const RESET_USAGE = `  midgard-l1-follower reset --to-origin
      (--postgres <connection string> | --sqlite <database file>)
`;

export const RESET_HELP = `reset deletes the follower's facts and every follower-owned derived row
(classes A, C, D-t and D-x, with the seeds) in one transaction, and keeps
class B. Stop the follower first: reset refuses while a follower holds the
store's writer lease. The next start initializes at the configured l1Origin
and replays from it. A password may come from PGPASSWORD instead of the
connection string.
`;

/** Exit code when a running follower holds the store's writer lease. */
export const EXIT_STORE_LOCKED = 4;

export type ResetArguments = Readonly<
  | { adapter: "postgres"; connectionString: string }
  | {
      adapter: "sqlite";
      path: string;
    }
>;

/** Returns the parsed arguments, or a usage message. */
export const parseReset = (
  args: readonly string[],
): ResetArguments | string => {
  const [mode, ...rest] = args;
  if (mode !== "--to-origin") return "reset needs --to-origin";
  const [flag, value, ...extra] = rest;
  if (extra.length > 0) return `unexpected ${extra.join(" ")}`;
  if (value === undefined || value === "")
    return "reset needs one of --postgres <connection string> or --sqlite <database file>";
  if (flag === "--postgres")
    return { adapter: "postgres", connectionString: value };
  if (flag === "--sqlite") return { adapter: "sqlite", path: value };
  return `unknown flag ${flag ?? ""}`;
};

export const runReset = async (
  parsed: ResetArguments,
  io: Readonly<{
    stdout: (text: string) => void;
    stderr: (text: string) => void;
  }>,
): Promise<number> => {
  const backend: SqlBackend =
    parsed.adapter === "postgres"
      ? openPostgresBackend({
          connectionString: parsed.connectionString,
          maxConnections: 1,
        })
      : openSqliteBackend(parsed.path);
  try {
    const result = await resetToOrigin(backend);
    if (result.kind === "store_locked") {
      io.stderr(`reset refused: ${result.detail}\n`);
      return EXIT_STORE_LOCKED;
    }
    io.stdout(
      `${JSON.stringify({
        reset: "to-origin",
        nextGeneration: result.nextGeneration,
        tables: result.tables,
      })}\n`,
    );
    return 0;
  } catch (error) {
    io.stderr(`reset failed: ${(error as Error).message}\n`);
    return 1;
  } finally {
    await backend.close();
  }
};
