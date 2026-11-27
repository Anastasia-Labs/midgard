import { readdir } from "node:fs/promises";
import { join, resolve } from "node:path";

import {
  loadPublicMountedConfig,
  PUBLIC_NODE_SECRET_NAMES,
} from "midgard-node/commands/listen-public-http";

import { SIGNED_RELEASE_FILES } from "../sign-deployment-release.js";
import type { StackConfig } from "./config.js";
import {
  assertAllowListedOperatorWallets,
  assertPublicNodeEnvFile,
  assertPublicNodeLimits,
  inside,
  loadPublicNodeSecrets,
} from "./public-node.js";

/**
 * The release a public stack's second instance runs: the stack's signed
 * release, its role images and its public origin, whose TLS certificate the
 * node serves on loopback. The second instance takes them from the stack.
 */
const SHARED_RELEASE_SETTINGS = [
  "MIDGARD_PUBLIC_API_ORIGIN",
  "MIDGARD_PUBLIC_RELEASE_REVISION",
  "MIDGARD_PUBLIC_NODE_IMAGE",
  "MIDGARD_PUBLIC_POSTGRES_IMAGE",
  "MIDGARD_PUBLIC_INGRESS_IMAGE",
];

/**
 * A second instance mounts the stack's release directory read-only and
 * writes only its own directories and files, so none of them may lie in it,
 * and it may lie in none of them.
 */
export function assertOutsideStackRelease(config: StackConfig) {
  const release = config.public!.releaseDirectory;
  for (const [label, path, holds] of [
    ["nodeRoot", config.nodeRoot, true],
    ["runDirectory", config.runDirectory, true],
    ["nodeConfigFile", config.public!.nodeConfigFile, false],
  ] as const)
    if (inside(path, release) || (holds && inside(release, path)))
      throw new Error(
        `${label} ${path} overlaps ${release}, the stack's release directory, which a second instance only reads`,
      );
}

/** The stack's environment with its node secrets, which a public stack keeps in its node configuration. */
export function withStackNodeSecrets(
  primary: StackConfig,
  primaryEnv: Record<string, string>,
) {
  if (primary.public === undefined) return primaryEnv;
  const secrets = loadPublicMountedConfig(primary.public.nodeConfigFile);
  return {
    ...primaryEnv,
    ...Object.fromEntries(
      PUBLIC_NODE_SECRET_NAMES.map((name) => [name, secrets[name]!]),
    ),
  };
}

/**
 * A public stack's second instance: its own node secrets, from its own
 * private node configuration, moved into `env`; the stack's release
 * settings, which its .env may only repeat; the public node's limits; and
 * operator and merge wallets on the compiled allow-list.
 */
export async function checkAttachedPublicInputs(
  config: StackConfig,
  env: Record<string, string>,
  primaryEnv: Record<string, string>,
) {
  const { nodeConfigFile } = config.public!;
  assertPublicNodeEnvFile(config.envFile, env, "nodeConfigFile");
  const context = resolve(config.nodeRoot, "..");
  const ignored = join(config.nodeRoot, "logs");
  if (inside(nodeConfigFile, context) && !inside(nodeConfigFile, ignored))
    throw new Error(
      `nodeConfigFile is inside the Docker build context ${context}; use a path under ${ignored} or outside the checkout`,
    );
  await loadPublicNodeSecrets(
    "nodeConfigFile",
    nodeConfigFile,
    config.envFile,
    env,
  );
  for (const name of SHARED_RELEASE_SETTINGS) {
    const stack = primaryEnv[name];
    if (stack === undefined)
      throw new Error(
        `The stack this instance attaches to sets no ${name}; run it first`,
      );
    if (name in env && env[name] !== stack)
      throw new Error(
        `${name} in ${config.envFile} differs from the stack's: a second instance runs the stack's release`,
      );
    env[name] = stack;
  }
  assertPublicNodeLimits(env);
  assertAllowListedOperatorWallets("nodeConfigFile", env);
}

/**
 * The stack's signed release, complete. Docker creates a missing bind-mount
 * source, so a second instance never starts its node before the stack signed.
 */
export async function assertStackReleaseSigned(config: StackConfig) {
  const directory = config.public!.releaseDirectory;
  const present = await readdir(directory).catch(() => [] as string[]);
  if (
    Object.values(SIGNED_RELEASE_FILES).some((name) => !present.includes(name))
  )
    throw new Error(
      `The stack this instance attaches to has not signed its public release in ${directory} yet; run that stack first`,
    );
}
