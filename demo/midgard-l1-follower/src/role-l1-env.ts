/**
 * Role processes fail closed on L1 access (option E, owner ruling): a
 * role (the node's `listen` and its workers, the DA committee node, the
 * watcher) reads L1 only through its follower, so a non-follower L1 setting
 * in its environment is a misconfiguration refused at start, never ignored.
 */

/**
 * The settings that select or configure a non-follower L1 access: the tool
 * selector, the tools' external-provider endpoints, and the node's deleted
 * provider choice (`L1_PROVIDER`).
 *
 * Allowed, though they name Blockfrost: `L1_BLOCKFROST_API_URL` and
 * `L1_BLOCKFROST_KEY` are the fault-proofs prover CLI's own fallbacks (owner
 * ruling: the prover CLI keeps Kupmios and Blockfrost). No role reads them,
 * and CI sets them in the workflow-wide environment every job and every
 * role test inherits (`.github/workflows/midgard-node-ci.yml`, the
 * top-level `env`), so refusing them would refuse every role started there.
 *
 * Refused: `L1_PROVIDER` was the node's own L1 provider choice, deleted by
 * option E. A role environment that still sets it was written for that
 * choice and expects a role to read L1 through Kupmios or Blockfrost, which
 * no role does any more; refusing it by name is the only way that
 * expectation surfaces. No shared environment sets it (the prover CLI takes
 * its provider from `--provider`, `L1_PROVIDER` only as a fallback).
 */
export const NON_FOLLOWER_L1_ENV_KEYS = [
  "L1_ACCESS",
  "L1_PROVIDER",
  "L1_KUPO_URL",
  "L1_OGMIOS_URL",
  "L1_BLOCKFROST_URL",
  "L1_BLOCKFROST_PROJECT_ID",
] as const;

export type NonFollowerL1EnvKey = (typeof NON_FOLLOWER_L1_ENV_KEYS)[number];

/** A role process was started with a non-follower L1 configuration. */
export class RoleL1AccessRefusedError extends Error {
  override readonly name = "RoleL1AccessRefusedError";
  readonly reason = "role_non_follower_l1_config";
  constructor(
    readonly role: string,
    readonly keys: readonly NonFollowerL1EnvKey[],
  ) {
    super(
      `${role} reads L1 only through its follower; refusing to start with a non-follower L1 configuration: ${keys.join(", ")} (unset ${keys.length === 1 ? "it" : "them"}; Kupmios, Blockfrost and --l1 are for tools)`,
    );
  }
}

/**
 * The non-follower L1 settings present in `env`. `L1_ACCESS=follower` is the
 * role's own access and allowed; an empty value counts as unset.
 */
export const nonFollowerL1EnvKeys = (
  env: Readonly<Record<string, string | undefined>>,
): NonFollowerL1EnvKey[] =>
  NON_FOLLOWER_L1_ENV_KEYS.filter((key) => {
    const value = env[key]?.trim();
    if (value === undefined || value === "") return false;
    return !(key === "L1_ACCESS" && value === "follower");
  });

/** Refuses, naming every offending setting, unless `env` is follower-only. */
export const assertRoleL1Env = (
  env: Readonly<Record<string, string | undefined>>,
  role: string,
): void => {
  const keys = nonFollowerL1EnvKeys(env);
  if (keys.length > 0) throw new RoleL1AccessRefusedError(role, keys);
};
