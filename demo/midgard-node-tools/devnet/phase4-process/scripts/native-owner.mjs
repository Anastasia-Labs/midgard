import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";

/**
 * Every node runs the Architecture G native owner and refuses to start unless
 * the owner binary is pinned by path and SHA-256. Resolves the binary this
 * checkout built (or the one node.env names) and its hash, refusing a missing
 * binary or a node.env pin that does not match it. Bootstrap runs this before
 * any devnet or L1 work, and write-acceptance-env.sh pins its result, so both
 * refuse with the same message.
 */
export const resolveNativeOwnerBinary = (nodeValues, nodeRoot) => {
  const configuredOwnerPath = nodeValues.MPF_NATIVE_OWNER_BINARY_PATH?.trim();
  const path = resolve(
    nodeRoot,
    configuredOwnerPath ||
      "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
  );
  if (!existsSync(path)) {
    // Building fixes only the checkout default: a node.env path (for example
    // the image's /app/native/architecture-g-owner kept from .env.example)
    // overrides the build output, so name that path instead.
    throw new Error(
      configuredOwnerPath
        ? `node.env names MPF_NATIVE_OWNER_BINARY_PATH=${configuredOwnerPath}, which does not exist on this host; remove it to use this checkout's build, or point it at a host binary`
        : `native owner binary is missing at ${path}; build it with \`pnpm --dir ${nodeRoot} run native:mpf-owner:build\``,
    );
  }
  const sha256 = createHash("sha256").update(readFileSync(path)).digest("hex");
  const pinnedSha256 = nodeValues.MPF_NATIVE_OWNER_BINARY_SHA256?.trim();
  if (pinnedSha256 && pinnedSha256 !== sha256) {
    throw new Error(
      `node.env pins MPF_NATIVE_OWNER_BINARY_SHA256=${pinnedSha256}, but ${path} hashes to ${sha256}`,
    );
  }
  return { path, sha256 };
};
