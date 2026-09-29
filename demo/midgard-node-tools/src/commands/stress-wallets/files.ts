import { createHash, randomBytes } from "node:crypto";
import { access, lstat, mkdir, open, readFile, rm } from "node:fs/promises";
import { dirname, join } from "node:path";

import { formatJson } from "midgard-node/commands/command-utils";
import {
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
} from "midgard-node/files/atomic-write";

export const fileExists = async (path: string): Promise<boolean> =>
  access(path).then(
    () => true,
    () => false,
  );

export const sha256Bytes = (bytes: Buffer): string =>
  createHash("sha256").update(bytes).digest("hex");

export const readFileGeneration = async (
  path: string,
): Promise<string | undefined> => {
  if (!(await fileExists(path))) return undefined;
  return sha256Bytes(await readFile(path));
};

export const assertFileGeneration = async (
  path: string,
  expectedSha256: string | undefined,
): Promise<void> => {
  const observedSha256 = await readFileGeneration(path);
  if (observedSha256 !== expectedSha256) {
    throw new Error(
      `Consolidation state generation changed at ${path}; expected ${expectedSha256 ?? "<absent>"}, observed ${observedSha256 ?? "<absent>"}. Refusing to overwrite concurrent or manual changes.`,
    );
  }
};

export const withExclusiveConsolidationStateLock = async <A>(
  statePath: string,
  action: () => Promise<A>,
): Promise<A> => {
  const lockPath = `${statePath}.lock`;
  const lockToken = randomBytes(32).toString("hex");
  await mkdir(dirname(statePath), { recursive: true, mode: 0o700 });
  let handle: Awaited<ReturnType<typeof open>>;
  try {
    handle = await open(lockPath, "wx", 0o600);
  } catch (cause) {
    throw new Error(
      `Consolidation state is exclusively locked at ${lockPath}. A stale lock must be archived or removed only after proving its recorded process is not running.`,
      { cause },
    );
  }
  const heldIdentity = await handle.stat();
  try {
    await handle.writeFile(
      `${formatJson({
        token: lockToken,
        pid: process.pid,
        acquiredAt: new Date().toISOString(),
      })}\n`,
      "utf8",
    );
    await handle.sync();
    return await action();
  } finally {
    let ownsPublishedLock = false;
    try {
      const publishedIdentity = await lstat(lockPath);
      const published = JSON.parse(await readFile(lockPath, "utf8")) as {
        readonly token?: unknown;
      };
      ownsPublishedLock =
        publishedIdentity.dev === heldIdentity.dev &&
        publishedIdentity.ino === heldIdentity.ino &&
        published.token === lockToken;
    } catch {
      ownsPublishedLock = false;
    }
    await handle.close();
    if (!ownsPublishedLock) {
      // Lock replacement is the safety-critical failure and must supersede an
      // action error so callers cannot mistake the lock state for clean.
      // eslint-disable-next-line no-unsafe-finally
      throw new Error(
        `Consolidation lock ownership changed at ${lockPath}; refusing to remove a missing or replacement lock.`,
      );
    }
    await rm(lockPath);
  }
};

export const withExclusiveStressWalletFundsLock = async <A>(
  outDir: string,
  action: () => Promise<A>,
): Promise<A> => {
  const lockPath = join(outDir, "stress-wallet-funds.lock");
  const lockToken = randomBytes(32).toString("hex");
  await mkdir(outDir, { recursive: true, mode: 0o700 });
  let handle: Awaited<ReturnType<typeof open>>;
  try {
    handle = await open(lockPath, "wx", 0o600);
  } catch (cause) {
    throw new Error(
      "Stress-wallet funds operations are exclusively locked at " +
        lockPath +
        ". A stale lock must be archived or removed only after proving its recorded process is not running.",
      { cause },
    );
  }
  const heldIdentity = await handle.stat();
  try {
    await handle.writeFile(
      formatJson({
        token: lockToken,
        pid: process.pid,
        acquiredAt: new Date().toISOString(),
      }) + "\n",
      "utf8",
    );
    await handle.sync();
    return await action();
  } finally {
    let ownsPublishedLock = false;
    try {
      const publishedIdentity = await lstat(lockPath);
      const published = JSON.parse(await readFile(lockPath, "utf8")) as {
        readonly token?: unknown;
      };
      ownsPublishedLock =
        publishedIdentity.dev === heldIdentity.dev &&
        publishedIdentity.ino === heldIdentity.ino &&
        published.token === lockToken;
    } catch {
      ownsPublishedLock = false;
    }
    await handle.close();
    if (!ownsPublishedLock) {
      // Lock replacement is the safety-critical failure and must supersede an
      // action error so callers cannot mistake the lock state for clean.
      // eslint-disable-next-line no-unsafe-finally
      throw new Error(
        "Stress-wallet funds lock ownership changed at " +
          lockPath +
          "; refusing to remove a missing or replacement lock.",
      );
    }
    await rm(lockPath);
  }
};

export const writePrivateFileAtomic = async (
  path: string,
  contents: string,
): Promise<void> => writeTextFileAtomic(path, contents, { mode: 0o600 });

export const writePrivateFileAtomicNoReplace = async (
  path: string,
  contents: string,
): Promise<void> =>
  writeTextFileAtomicNoReplace(path, contents, { mode: 0o600 });
