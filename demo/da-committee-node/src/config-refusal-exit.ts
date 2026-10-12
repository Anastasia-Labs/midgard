/**
 * How the committee process exits on a failure that escapes `main`. The
 * configuration is read before any port is known, so no `/readyz` can name
 * a refusal made while reading it. A refusal there that no restart can
 * clear exits 78 (sysexits' EX_CONFIG, as the watcher's permanent refusals
 * do, `midgard-watcher/src/runtime/permanent-refusal.ts`) with one named
 * line, so a supervisor stops restarting the process instead of looping on
 * it (the devnet supervisor records exit 78 as a refusal). The refusal that
 * exits so is a role's non-follower L1 setting (`role_non_follower_l1_config`);
 * every other failure keeps exit 1 and its stack, as before.
 */
import { RoleL1AccessRefusedError } from "@al-ft/midgard-l1-follower";

/** EX_CONFIG: the configuration refuses; a restart cannot clear it. */
export const COMMITTEE_CONFIG_REFUSAL_EXIT_CODE = 78;

/** The log event of a configuration refusal that exits 78. */
export const COMMITTEE_CONFIG_REFUSED = "committee_config_refused";

export type CommitteeFailureExit = Readonly<{ code: number; line: string }>;

const roleL1Refusal = (
  error: unknown,
): RoleL1AccessRefusedError | undefined => {
  let current = error;
  for (let depth = 0; depth < 8 && current instanceof Error; depth += 1) {
    if (current instanceof RoleL1AccessRefusedError) return current;
    current = current.cause;
  }
  return undefined;
};

/** The exit code and the one line written for a failure that escaped `main`. */
export const committeeFailureExit = (error: unknown): CommitteeFailureExit => {
  const refusal = roleL1Refusal(error);
  if (refusal !== undefined)
    return {
      code: COMMITTEE_CONFIG_REFUSAL_EXIT_CODE,
      line: `${JSON.stringify({
        event: COMMITTEE_CONFIG_REFUSED,
        reason: refusal.reason,
        keys: refusal.keys,
        detail: refusal.message,
        outcome: "no restart clears it; the process exits 78 (EX_CONFIG)",
      })}\n`,
    };
  return {
    code: 1,
    line: `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
  };
};
