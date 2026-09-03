import { readFile, realpath } from "node:fs/promises";

import { parseWatcherStrictJsonValue } from "./config.js";
import {
  type VerifiedWatcherDeploymentIdentity,
  verifyWatcherDeploymentIdentity,
} from "./deployment-identity.js";

type ReadDeploymentAuthorityFile = (path: string) => Promise<Uint8Array>;

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
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

/**
 * Loads the signed W02 authority as a single duplicate-key-rejecting file and
 * returns only the module-admitted opaque identity minted by its verifier.
 */
export const loadWatcherVerifiedDeploymentAuthority = async (input: {
  readonly path: string;
  readonly unsafeReadFileForTest?: ReadDeploymentAuthorityFile;
}): Promise<VerifiedWatcherDeploymentIdentity> => {
  const bytes = await (input.unsafeReadFileForTest ?? read)(input.path);
  if (bytes.byteLength === 0 || bytes.byteLength > 16 * 1024 * 1024) {
    throw new Error("watcher deployment authority file size is invalid");
  }
  const parsed = parseWatcherStrictJsonValue(
    new TextDecoder("utf-8", { fatal: true }).decode(bytes),
  );
  const authority = exactRecord(parsed, [
    "signedIdentity",
    "policy",
    "trustRoots",
    "durableMarker",
  ]);
  if (!Array.isArray(authority.trustRoots)) {
    throw new Error("watcher deployment authority trust roots are invalid");
  }
  return verifyWatcherDeploymentIdentity({
    signedIdentity: authority.signedIdentity,
    policy: authority.policy as Parameters<
      typeof verifyWatcherDeploymentIdentity
    >[0]["policy"],
    trustRoots: authority.trustRoots as Parameters<
      typeof verifyWatcherDeploymentIdentity
    >[0]["trustRoots"],
    durableMarker: authority.durableMarker,
  });
};
