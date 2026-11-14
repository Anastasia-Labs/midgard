import { createHash } from "node:crypto";
import { open, readdir, readFile, realpath } from "node:fs/promises";
import { basename, dirname, join, relative } from "node:path";
import { fileURLToPath } from "node:url";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";

/** Bounded local owner evidence; only its digest is returned to diagnostics. */
export const loadTrustedPromiseEvidence = async (
  path: string,
  expectedDigest: string,
  maxBytes = 4 * 1024 * 1024,
): Promise<unknown> => {
  if (
    !/^[0-9a-f]{64}$/u.test(expectedDigest) ||
    !Number.isSafeInteger(maxBytes) ||
    maxBytes <= 0
  )
    throw new Error("Promise adoption evidence pin is invalid");
  const file = await open(path, "r");
  let bytes: Buffer;
  try {
    if ((await file.stat()).size > maxBytes)
      throw new Error("Promise adoption evidence exceeds its byte limit");
    const chunks: Buffer[] = [];
    let total = 0;
    while (true) {
      const chunk = Buffer.alloc(Math.min(65536, maxBytes - total + 1));
      const { bytesRead } = await file.read(chunk, 0, chunk.byteLength, null);
      if (!bytesRead) {
        bytes = Buffer.concat(chunks, total);
        break;
      }
      total += bytesRead;
      if (total > maxBytes)
        throw new Error("Promise adoption evidence grew beyond its byte limit");
      chunks.push(chunk.subarray(0, bytesRead));
    }
  } finally {
    await file.close();
  }
  if (createHash("sha256").update(bytes).digest("hex") !== expectedDigest)
    throw new Error("Promise adoption evidence digest mismatch");
  return JSON.parse(bytes.toString("utf8")) as unknown;
};

/** Fingerprint actual running package JS and locked dependency declaration.
 * Package installation/integrity remains part of the adopted runtime trust. */
export const committeePromiseBundleDigest = async (input: {
  packageRoots: Readonly<Record<string, string>>;
  lockfilePath: string;
}): Promise<string> => {
  const entries: Record<string, string> = {};
  let files = 0;
  let total = 0;
  for (const [name, rootInput] of Object.entries(input.packageRoots)) {
    const root = await realpath(rootInput);
    const visit = async (directory: string): Promise<void> => {
      for (const entry of await readdir(directory, { withFileTypes: true })) {
        if (entry.isSymbolicLink())
          throw new Error("Running bundle contains a symbolic link");
        const path = join(directory, entry.name);
        if (entry.isDirectory()) await visit(path);
        else if (/\.(?:js|mjs|cjs)$/u.test(entry.name)) {
          if (++files > 8192)
            throw new Error("Running bundle exceeds its file domain");
          const handle = await open(path, "r");
          let bytes: Buffer;
          try {
            if ((await handle.stat()).size > 64 * 1024 * 1024 - total)
              throw new Error("Running bundle exceeds its byte domain");
            bytes = await handle.readFile();
          } finally {
            await handle.close();
          }
          total += bytes.byteLength;
          if (total > 64 * 1024 * 1024)
            throw new Error("Running bundle exceeds its byte domain");
          entries[`${name}/${relative(root, path)}`] = createHash("sha256")
            .update(bytes)
            .digest("hex");
        }
      }
    };
    const before = files;
    await visit(root);
    if (files === before)
      throw new Error("Running bundle has no JavaScript implementation");
  }
  const lock = await readFile(input.lockfilePath);
  entries["locked-dependencies"] = createHash("sha256")
    .update(lock)
    .digest("hex");
  return createHash("sha256")
    .update(canonicalJson(entries, "running promise runtime bundle"))
    .digest("hex");
};

/** Resolve the package's actual dist from either bundled or split module paths. */
export const committeeLoadedDistRoot = (entryPath: string): string => {
  let directory = dirname(entryPath);
  while (basename(directory) !== "dist") {
    const parent = dirname(directory);
    if (parent === directory)
      throw new Error(
        "Loaded promise runtime is outside a built dist directory",
      );
    directory = parent;
  }
  return directory;
};

/** Production roots come from loaded modules, never artifact-provided paths. */
export const committeeRunningPromiseBuildDigest = async (): Promise<string> => {
  if (!import.meta.url.endsWith(".js"))
    throw new Error("Promise adoption requires the built runtime package");
  const committeeRoot = committeeLoadedDistRoot(fileURLToPath(import.meta.url));
  const coreRoot = committeeLoadedDistRoot(
    fileURLToPath(import.meta.resolve("@al-ft/midgard-core")),
  );
  const sdkRoot = committeeLoadedDistRoot(
    fileURLToPath(import.meta.resolve("@al-ft/midgard-sdk")),
  );
  return committeePromiseBundleDigest({
    packageRoots: {
      committee: committeeRoot,
      core: coreRoot,
      sdk: sdkRoot,
    },
    lockfilePath: join(dirname(dirname(committeeRoot)), "pnpm-lock.yaml"),
  });
};
