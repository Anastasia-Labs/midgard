import { createHash } from "node:crypto";

import {
  type CardanoL1SourceConfig,
  type Env,
  type L1SourceConfig,
} from "./config.committee-config.js";
import {
  assertLocalQuerySurfacesShareAuthority,
  booleanEnv,
  boundedIdentity,
  isFixtureProviderUrl,
  localChainSyncUrl,
  optionalNonEmpty,
  requireEnv,
} from "./config.operational-provider-identity.js";

export const parseL1SourceConfig = (
  env: Env,
  cardanoProviderUrls: readonly string[],
): L1SourceConfig => {
  const sourceMode = requireEnv(env, "CARDANO_L1_SOURCE_MODE");
  const testMode = booleanEnv(env.CARDANO_L1_TEST_MODE, false);
  if (sourceMode !== "local_node") {
    throw new Error("CARDANO_L1_SOURCE_MODE must be local_node");
  }
  if (
    !testMode &&
    cardanoProviderUrls.some((url) => isFixtureProviderUrl(url))
  ) {
    throw new Error(
      "fixture:/file: Cardano providers require explicit CARDANO_L1_TEST_MODE=true",
    );
  }
  const chainSyncProviderUrl = localChainSyncUrl(
    requireEnv(env, "CARDANO_LOCAL_NODE_CHAIN_SYNC_URL"),
    testMode,
  );
  if (!testMode) {
    assertLocalQuerySurfacesShareAuthority(
      chainSyncProviderUrl,
      cardanoProviderUrls,
    );
  }
  return {
    sourceMode,
    authorityNodeId: boundedIdentity(
      requireEnv(env, "CARDANO_LOCAL_NODE_AUTHORITY_ID"),
      "CARDANO_LOCAL_NODE_AUTHORITY_ID",
    ),
    chainSyncProviderUrl,
    chainSyncCursorPath: requireEnv(
      env,
      "CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH",
    ),
    queryProviderUrls: cardanoProviderUrls,
  };
};

const CARDANO_NAMED_NETWORK_MAGIC = {
  Mainnet: 764_824_073,
  Preprod: 1,
  Preview: 2,
} as const;

const CARDANO_NETWORK_MAGIC_MAX = 4_294_967_295;

const CARDANO_AUTHORITY_ID = /^[a-zA-Z0-9][a-zA-Z0-9._-]{0,127}$/u;

export const cardanoL1SourceConfig = ({
  env,
  network,
  cardanoProviderUrls,
}: {
  readonly env: Env;
  readonly network: string;
  readonly cardanoProviderUrls: readonly string[];
}): CardanoL1SourceConfig => {
  const sourceMode = requireEnv(env, "CARDANO_L1_SOURCE_MODE");
  if (sourceMode !== "local_node") {
    throw new Error("CARDANO_L1_SOURCE_MODE must be local_node");
  }
  const networkMagic = cardanoNetworkMagic(env, network);
  if (
    cardanoProviderUrls.some(
      (url) =>
        !url.startsWith("kupmios:") &&
        !url.startsWith("fixture:") &&
        !url.startsWith("file:"),
    )
  ) {
    throw new Error(
      "local_node mode permits only same-node kupmios query surfaces or deterministic fixtures",
    );
  }
  const authorityNodeId = requireEnv(env, "CARDANO_LOCAL_NODE_AUTHORITY_ID");
  if (!CARDANO_AUTHORITY_ID.test(authorityNodeId)) {
    throw new Error(
      "CARDANO_LOCAL_NODE_AUTHORITY_ID must be a stable public identifier",
    );
  }
  const authorityDigest = cardanoAuthorityDigest({
    sourceMode,
    network,
    networkMagic,
    authorityNodeId,
    querySurfaces: [...cardanoProviderUrls].sort(),
  });
  return {
    sourceMode,
    authorityNodeId,
    authorityDigest,
    networkMagic,
  };
};

const cardanoNetworkMagic = (env: Env, network: string): number => {
  const configured = optionalNonEmpty(env.CARDANO_NETWORK_MAGIC);
  if (network === "Custom") {
    if (configured === undefined) {
      throw new Error("CARDANO_NETWORK_MAGIC is required for Custom network");
    }
    return networkMagicInteger(configured);
  }
  if (network === "Mainnet" || network === "Preprod" || network === "Preview") {
    if (configured !== undefined) {
      throw new Error(
        "CARDANO_NETWORK_MAGIC must be omitted for named Cardano networks",
      );
    }
    return CARDANO_NAMED_NETWORK_MAGIC[network];
  }
  throw new Error(
    "Cardano network must be Mainnet, Preprod, Preview, or Custom",
  );
};

const networkMagicInteger = (value: string): number => {
  if (!/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(
      "CARDANO_NETWORK_MAGIC must be a canonical unsigned 32-bit integer",
    );
  }
  const parsed = Number(value);
  if (
    !Number.isSafeInteger(parsed) ||
    parsed < 0 ||
    parsed > CARDANO_NETWORK_MAGIC_MAX
  ) {
    throw new Error(
      "CARDANO_NETWORK_MAGIC must be a canonical unsigned 32-bit integer",
    );
  }
  return parsed;
};

const cardanoAuthorityDigest = (identity: object): string =>
  createHash("sha256")
    .update(
      JSON.stringify({
        schemaVersion: "midgard-da-cardano-l1-authority-v1",
        ...identity,
      }),
    )
    .digest("hex");
