import { readFile, realpath } from "node:fs/promises";

import {
  type LoadedWatcherRuleBundle,
  loadWatcherRuleBundle,
} from "../verification/rule-bundle.js";
import { parseWatcherStrictJsonValue } from "./config.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  verifyWatcherDeploymentIdentity,
} from "./deployment-identity.js";

type ReadDeploymentAuthorityFile = (path: string) => Promise<Uint8Array>;

export type VerifiedWatcherDeploymentAuthority = Readonly<{
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  ruleBundle: LoadedWatcherRuleBundle;
}>;

const admittedDeploymentAuthorities = new WeakSet<object>();

export const assertWatcherVerifiedDeploymentAuthority = (
  authority: VerifiedWatcherDeploymentAuthority,
): void => {
  if (!admittedDeploymentAuthorities.has(authority)) {
    throw new Error(
      "watcher deployment and rule-bundle authority is not admitted",
    );
  }
};

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    (Object.getPrototypeOf(value) !== Object.prototype &&
      Object.getPrototypeOf(value) !== null) ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error("watcher deployment authority is not an exact object");
  }
  const record = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(
      "watcher deployment authority has unknown or missing fields",
    );
  }
  return record;
};

const read: ReadDeploymentAuthorityFile = async (path) => {
  if ((await realpath(path)) !== path) {
    throw new Error("watcher deployment authority path traverses a symlink");
  }
  return await readFile(path);
};

const readAuthorityJson = async (
  path: string,
  label: string,
  readFile: ReadDeploymentAuthorityFile,
): Promise<unknown> => {
  const bytes = await readFile(path);
  if (bytes.byteLength === 0 || bytes.byteLength > 16 * 1024 * 1024) {
    throw new Error(`watcher ${label} file size is invalid`);
  }
  return parseWatcherStrictJsonValue(
    new TextDecoder("utf-8", { fatal: true }).decode(bytes),
  );
};

/**
 * Reopens the signed deployment authority and its separately supplied release
 * rule bundle. Both verifiers receive the same raw identity, policy, trust roots
 * and durable marker; the runtime never constructs a replacement rule bundle.
 * The resulting authority is process-local and must be freshly loaded on restart.
 */
export const loadWatcherVerifiedDeploymentAuthority = async (input: {
  readonly path: string;
  readonly ruleBundlePath: string;
  readonly unsafeReadFileForTest?: ReadDeploymentAuthorityFile;
}): Promise<VerifiedWatcherDeploymentAuthority> => {
  const readFile = input.unsafeReadFileForTest ?? read;
  const [parsed, ruleBundle] = await Promise.all([
    readAuthorityJson(input.path, "deployment authority", readFile),
    readAuthorityJson(input.ruleBundlePath, "release rule bundle", readFile),
  ]);
  const authority = exactRecord(parsed, [
    "signedIdentity",
    "policy",
    "trustRoots",
    "durableMarker",
  ]);
  if (!Array.isArray(authority.trustRoots)) {
    throw new Error("watcher deployment authority trust roots are invalid");
  }
  const verificationInput = {
    signedIdentity: authority.signedIdentity,
    policy: authority.policy as Parameters<
      typeof verifyWatcherDeploymentIdentity
    >[0]["policy"],
    trustRoots: authority.trustRoots as Parameters<
      typeof verifyWatcherDeploymentIdentity
    >[0]["trustRoots"],
    durableMarker: authority.durableMarker,
  };
  const deploymentIdentity = verifyWatcherDeploymentIdentity(verificationInput);
  const loadedRuleBundle = loadWatcherRuleBundle({
    ...verificationInput,
    ruleBundle,
  });
  const admitted = Object.freeze({
    deploymentIdentity,
    ruleBundle: loadedRuleBundle,
  });
  admittedDeploymentAuthorities.add(admitted);
  return admitted;
};
