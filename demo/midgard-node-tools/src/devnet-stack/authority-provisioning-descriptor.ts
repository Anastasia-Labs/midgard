import { createHash, randomUUID } from "node:crypto";
import {
  closeSync,
  constants,
  existsSync,
  fstatSync,
  fsyncSync,
  linkSync,
  lstatSync,
  mkdirSync,
  openSync,
  readFileSync,
  realpathSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { dirname, isAbsolute, join, normalize } from "node:path";

export const FRESH_AUTHORITY_PROFILE = Object.freeze({
  profileId: "fresh-phase4-authority-k64-20261002",
  liveRecordLimit: 64,
  evidenceSha256:
    "2d89a1d61b6ada6e60b9bfef2b981d4b6f4856c16894e64fe78df342cbfacbfb",
});
export type AuthorityProvisioningBinding = Readonly<{
  directory: string;
  policyDigest: string;
  deploymentMarker: unknown;
  recordKeySourceIdentity: string;
  bearerSourceIdentity: string;
  secretPaths: readonly string[];
}>;
export type AuthorityProvisioningDescriptor = Readonly<{
  schemaVersion: "midgard-fresh-authority-provisioning-v1";
  profile: typeof FRESH_AUTHORITY_PROFILE;
  generation: string;
  binding: AuthorityProvisioningBinding;
}>;
const bytes = (value: unknown) => `${JSON.stringify(value)}\n`;
const canonicalPath = (path: string) => {
  if (!isAbsolute(path) || normalize(path) !== path)
    throw Error("authority provisioning path must be canonical and absolute");
  return path;
};
const regular = (path: string) => {
  const stat = lstatSync(path);
  if (
    !stat.isFile() ||
    stat.size < 1 ||
    stat.size > 32768 ||
    realpathSync(path) !== path
  )
    throw Error(
      "authority provisioning evidence is not a bounded canonical regular file",
    );
};
export const syncProvisioningPath = (path: string) => {
  const fd = openSync(path, constants.O_RDONLY | constants.O_NOFOLLOW);
  try {
    fsyncSync(fd);
  } finally {
    closeSync(fd);
  }
};
const syncPublication = (path: string) => {
  regular(path);
  syncProvisioningPath(path);
  syncProvisioningPath(dirname(path));
  syncProvisioningPath(dirname(dirname(path)));
};
const read = (path: string): unknown => {
  regular(path);
  const fd = openSync(path, constants.O_RDONLY | constants.O_NOFOLLOW);
  try {
    if (!fstatSync(fd).isFile())
      throw Error("authority provisioning evidence changed");
    const held = readFileSync(fd, "utf8"),
      value: unknown = JSON.parse(held);
    if (bytes(value) !== held)
      throw Error("authority provisioning evidence is noncanonical");
    return value;
  } finally {
    closeSync(fd);
  }
};
/** Caller owns this protected volume/namespace exclusively. Canonical evidence
 * is never replaced; acknowledgement-loss retries reassert publication fsync. */
const publish = (path: string, value: unknown) => {
  const directory = dirname(canonicalPath(path));
  if (!existsSync(directory)) mkdirSync(directory, { mode: 0o700 });
  if (realpathSync(directory) !== directory)
    throw Error("authority provisioning directory traverses symlink");
  if (existsSync(path)) {
    if (bytes(read(path)) !== bytes(value))
      throw Error("authority provisioning publication differs");
    syncPublication(path);
    return;
  }
  const temporary = join(
    directory,
    `.authority-provisioning-${randomUUID()}.tmp`,
  );
  const fd = openSync(temporary, "wx", 0o600);
  try {
    writeFileSync(fd, bytes(value));
    fsyncSync(fd);
  } finally {
    closeSync(fd);
  }
  try {
    linkSync(temporary, path);
  } finally {
    unlinkSync(temporary);
  }
  syncPublication(path);
};
export const provisioningDigest = (
  descriptor: AuthorityProvisioningDescriptor,
) => createHash("sha256").update(bytes(descriptor)).digest("hex");
const completion = (descriptor: AuthorityProvisioningDescriptor) => ({
  schemaVersion: "midgard-fresh-authority-provisioning-complete-v1",
  descriptorSha256: provisioningDigest(descriptor),
});
export const assertAuthorityProvisioningDescriptor = (
  path: string,
  descriptor: AuthorityProvisioningDescriptor,
) => {
  if (bytes(read(path)) !== bytes(descriptor))
    throw Error("authority provisioning intent changed");
  syncPublication(path);
};
export const completeAuthorityProvisioning = (
  path: string,
  descriptor: AuthorityProvisioningDescriptor,
) => publish(`${path}.completed`, completion(descriptor));
export const prepareAuthorityProvisioning = (input: {
  descriptorPath: string;
  binding: AuthorityProvisioningBinding;
  initialize: boolean;
  protectedPaths: readonly string[];
}) => {
  canonicalPath(input.descriptorPath);
  canonicalPath(input.binding.directory);
  if (
    input.descriptorPath === input.binding.directory ||
    input.descriptorPath.startsWith(`${input.binding.directory}/`)
  )
    throw Error(
      "provisioning descriptor must be outside selected authority namespace",
    );
  let descriptor: AuthorityProvisioningDescriptor;
  if (existsSync(input.descriptorPath)) {
    const value = read(input.descriptorPath) as AuthorityProvisioningDescriptor;
    if (
      value === null ||
      typeof value !== "object" ||
      !/^generation-[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/u.test(
        value.generation,
      ) ||
      bytes(value) !==
        bytes({
          schemaVersion: "midgard-fresh-authority-provisioning-v1",
          profile: FRESH_AUTHORITY_PROFILE,
          generation: value.generation,
          binding: input.binding,
        })
    )
      throw Error("authority provisioning descriptor identity differs");
    descriptor = value;
    syncPublication(input.descriptorPath);
  } else {
    if (
      !input.initialize ||
      existsSync(`${input.descriptorPath}.completed`) ||
      input.protectedPaths.some(existsSync) ||
      input.binding.secretPaths.some(existsSync) ||
      existsSync(input.binding.directory)
    )
      throw Error(
        "explicit fresh authority ownership is required; existing state is never initialized",
      );
    descriptor = {
      schemaVersion: "midgard-fresh-authority-provisioning-v1",
      profile: FRESH_AUTHORITY_PROFILE,
      generation: `generation-${randomUUID()}`,
      binding: input.binding,
    };
    publish(input.descriptorPath, descriptor);
  }
  const completed = existsSync(`${input.descriptorPath}.completed`);
  if (completed) {
    if (
      bytes(read(`${input.descriptorPath}.completed`)) !==
      bytes(completion(descriptor))
    )
      throw Error("authority provisioning completion differs");
    syncPublication(`${input.descriptorPath}.completed`);
  } else if (!input.initialize)
    throw Error(
      "pending authority provisioning requires explicit initialization retry",
    );
  const allowMissingSecrets =
    !completed &&
    !existsSync(input.binding.directory) &&
    !input.protectedPaths.some(existsSync);
  if (
    !allowMissingSecrets &&
    input.binding.secretPaths.some((path) => !existsSync(path))
  )
    throw Error(
      "established authority secret is missing; it is never regenerated",
    );
  if (
    completed &&
    !existsSync(join(input.binding.directory, "authority-backend.json"))
  )
    throw Error(
      "completed authority namespace is missing; initialization is forbidden",
    );
  return { descriptor, completed, allowMissingSecrets };
};
