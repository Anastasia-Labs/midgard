import { readFile, realpath } from "node:fs/promises";

import { parseWatcherStrictJsonValueV1 } from "./config.js";
import {
  type VerifiedWatcherDeploymentIdentityV1,
  verifyWatcherDeploymentIdentityV1,
} from "./deployment-identity.js";

type ReadDeploymentAuthorityFileV1 = (path: string) => Promise<Uint8Array>;

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

const productionRead: ReadDeploymentAuthorityFileV1 = async (path) => {
  if ((await realpath(path)) !== path) {
    throw new Error("watcher deployment authority path traverses a symlink");
  }
  return await readFile(path);
};

/**
 * Loads the signed W02 authority as a single duplicate-key-rejecting file and
 * returns only the module-admitted opaque identity minted by its verifier.
 */
export const loadWatcherVerifiedDeploymentAuthorityV1 = async (input: {
  readonly path: string;
  readonly unsafeReadFileForTest?: ReadDeploymentAuthorityFileV1;
}): Promise<VerifiedWatcherDeploymentIdentityV1> => {
  const bytes = await (input.unsafeReadFileForTest ?? productionRead)(
    input.path,
  );
  if (bytes.byteLength === 0 || bytes.byteLength > 16 * 1024 * 1024) {
    throw new Error("watcher deployment authority file size is invalid");
  }
  const parsed = parseWatcherStrictJsonValueV1(
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
  return verifyWatcherDeploymentIdentityV1({
    signedIdentity: authority.signedIdentity,
    policy: authority.policy as Parameters<
      typeof verifyWatcherDeploymentIdentityV1
    >[0]["policy"],
    trustRoots: authority.trustRoots as Parameters<
      typeof verifyWatcherDeploymentIdentityV1
    >[0]["trustRoots"],
    durableMarker: authority.durableMarker,
  });
};
