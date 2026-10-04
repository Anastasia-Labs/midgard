import { createHash } from "node:crypto";
import { readFileSync, statSync } from "node:fs";
import { join } from "node:path";

import type { DeployContext } from "./deploy.js";
import { readJsonIfPresent } from "./durable.js";
import {
  type Identities,
  LIBP2P_IDENTITIES,
  WALLET_ROLES,
} from "./identities.js";
import { type Layout, readRunEnv } from "./layout.js";

/** Existing-run recovery cannot reconstruct deployment evidence or identities. */
export const readServiceRecoveryContext = (layout: Layout): DeployContext => {
  const nativeOwnerBinary = join(layout.bin, "architecture-g-owner");
  const chainSyncBinary = join(layout.bin, "midgard-chain-sync");
  for (const path of [
    layout.runEnv,
    layout.identities,
    layout.blueprint,
    `${layout.blueprint}.deployment.json`,
    nativeOwnerBinary,
    chainSyncBinary,
    layout.contractManifest,
  ]) {
    let file = false;
    try {
      file = statSync(path).isFile();
    } catch {
      /* Missing evidence is refused below. */
    }
    if (!file)
      throw new Error(
        `existing deployment state missing: ${path}; restore exact recorded state or use an explicitly authorized new run`,
      );
  }
  const identities = readJsonIfPresent<Identities>(layout.identities);
  const nonempty = (value: unknown) =>
    typeof value === "string" && value.length > 0;
  if (
    identities === undefined ||
    identities.schemaVersion !== "midgard-devnet-identities-v1" ||
    !WALLET_ROLES.every((role) => nonempty(identities.seeds?.[role])) ||
    !LIBP2P_IDENTITIES.every((role) => nonempty(identities.libp2p?.[role])) ||
    !nonempty(identities.adminApiKey) ||
    !nonempty(identities.publicReaderPassword)
  )
    throw new Error(
      "existing deployment identities are incomplete; recovery never generates replacements",
    );
  return {
    layout,
    run: readRunEnv(layout),
    identities,
    artifacts: {
      nativeOwnerBinary,
      nativeOwnerSha256: createHash("sha256")
        .update(readFileSync(nativeOwnerBinary))
        .digest("hex"),
      chainSyncBinary,
    },
  };
};
