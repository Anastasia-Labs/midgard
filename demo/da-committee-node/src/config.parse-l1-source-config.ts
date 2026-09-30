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
  operationalProviderIdentity,
  optionalNonEmpty,
  requireEnv,
  splitList,
} from "./config.operational-provider-identity.js";

export const parseL1SourceConfig = (
  env: Env,
  cardanoProviderUrls: readonly string[],
): L1SourceConfig => {
  const sourceMode = requireEnv(env, "CARDANO_L1_SOURCE_MODE");
  const testMode = booleanEnv(env.CARDANO_L1_TEST_MODE, false);
  if (sourceMode !== "local_node" && sourceMode !== "external_providers") {
    throw new Error(
      "CARDANO_L1_SOURCE_MODE must be local_node or external_providers",
    );
  }
  if (
    !testMode &&
    cardanoProviderUrls.some((url) => isFixtureProviderUrl(url))
  ) {
    throw new Error(
      "fixture:/file: Cardano providers require explicit CARDANO_L1_TEST_MODE=true",
    );
  }
  if (sourceMode === "local_node") {
    if (
      optionalNonEmpty(env.CARDANO_EXTERNAL_PROVIDER_IDENTITIES) !== undefined
    ) {
      throw new Error(
        "CARDANO_EXTERNAL_PROVIDER_IDENTITIES is forbidden in local_node mode",
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
  }
  if (
    optionalNonEmpty(env.CARDANO_LOCAL_NODE_AUTHORITY_ID) !== undefined ||
    optionalNonEmpty(env.CARDANO_LOCAL_NODE_CHAIN_SYNC_URL) !== undefined ||
    optionalNonEmpty(env.CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH) !==
      undefined
  ) {
    throw new Error(
      "CARDANO_LOCAL_NODE_AUTHORITY_ID, CARDANO_LOCAL_NODE_CHAIN_SYNC_URL and CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH are forbidden in external_providers mode",
    );
  }
  if (cardanoProviderUrls.length < 2) {
    throw new Error(
      "external_providers mode requires at least two operationally independent CARDANO_PROVIDER_URLS entries",
    );
  }
  const identities = splitList(
    requireEnv(env, "CARDANO_EXTERNAL_PROVIDER_IDENTITIES"),
  ).map((identity) =>
    boundedIdentity(identity, "CARDANO_EXTERNAL_PROVIDER_IDENTITIES"),
  );
  if (identities.length !== cardanoProviderUrls.length) {
    throw new Error(
      "CARDANO_EXTERNAL_PROVIDER_IDENTITIES must contain one identity per CARDANO_PROVIDER_URLS entry",
    );
  }
  if (new Set(identities).size !== identities.length) {
    throw new Error(
      "external_providers mode requires distinct operational provider identities",
    );
  }
  const operationalIdentities = cardanoProviderUrls.map((url, index) =>
    operationalProviderIdentity(url, identities[index]!, testMode),
  );
  const endpointOwners = new Map<string, string>();
  for (const identity of operationalIdentities) {
    for (const endpoint of identity.normalizedEndpoints) {
      const existingOwner = endpointOwners.get(endpoint);
      if (existingOwner !== undefined) {
        throw new Error(
          `external_providers mode requires operationally independent backends; ${existingOwner} and ${identity.operatorId} share normalized endpoint ${endpoint}`,
        );
      }
      endpointOwners.set(endpoint, identity.operatorId);
    }
  }
  if (
    new Set(operationalIdentities.map(({ backendKey }) => backendKey)).size !==
    operationalIdentities.length
  ) {
    throw new Error(
      "external_providers mode requires distinct normalized provider backends",
    );
  }
  return {
    sourceMode,
    providers: cardanoProviderUrls.map((url, index) => ({
      identity: identities[index]!,
      url,
      operationalIdentity: operationalIdentities[index]!,
    })),
  };
};

export const isLiveLucidProviderUrl = (value: string): boolean =>
  value.startsWith("blockfrost:") || value.startsWith("kupmios:");

const CARDANO_NAMED_NETWORK_MAGIC = {
  Mainnet: 764_824_073,
  Preprod: 1,
  Preview: 2,
} as const;

const CARDANO_NETWORK_MAGIC_MAX = 4_294_967_295;

const CARDANO_AUTHORITY_ID = /^[a-zA-Z0-9][a-zA-Z0-9._-]{0,127}$/u;

export const LOWER_HEX_32 = /^[0-9a-f]{64}$/u;

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
  if (sourceMode !== "local_node" && sourceMode !== "external_providers") {
    throw new Error(
      "CARDANO_L1_SOURCE_MODE must be local_node or external_providers",
    );
  }
  const networkMagic = cardanoNetworkMagic(env, network);
  if (sourceMode === "local_node") {
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
    if (optionalNonEmpty(env.CARDANO_PROVIDER_AUTHORITY_IDS) !== undefined) {
      throw new Error(
        "CARDANO_PROVIDER_AUTHORITY_IDS must be omitted in local_node mode",
      );
    }
    const authorityDigest = cardanoAuthorityDigest({
      sourceMode,
      network,
      networkMagic,
      authorityNodeId,
      querySurfaces: cardanoProviderUrls.map(providerPublicIdentity).sort(),
    });
    return {
      sourceMode,
      authorityNodeId,
      authorityDigest,
      networkMagic,
    };
  }

  if (optionalNonEmpty(env.CARDANO_LOCAL_NODE_AUTHORITY_ID) !== undefined) {
    throw new Error(
      "CARDANO_LOCAL_NODE_AUTHORITY_ID must be omitted in external_providers mode",
    );
  }
  if (cardanoProviderUrls.length < 2) {
    throw new Error(
      "external_providers mode requires at least two Cardano provider URLs",
    );
  }
  if (cardanoProviderUrls.some((url) => !url.startsWith("kupmios:"))) {
    // The state queue is read through every provider, and only Kupmios
    // serves its authenticated ordered history.
    throw new Error(
      "external_providers mode requires kupmios providers: blockfrost has no authenticated ordered state-queue history source",
    );
  }
  const providerAuthorityIds = splitList(
    requireEnv(env, "CARDANO_PROVIDER_AUTHORITY_IDS"),
  );
  if (providerAuthorityIds.length !== cardanoProviderUrls.length) {
    throw new Error(
      "CARDANO_PROVIDER_AUTHORITY_IDS must contain exactly one identity per CARDANO_PROVIDER_URLS entry",
    );
  }
  if (providerAuthorityIds.some((identity) => !LOWER_HEX_32.test(identity))) {
    throw new Error(
      "CARDANO_PROVIDER_AUTHORITY_IDS entries must be lowercase SHA-256 identities",
    );
  }
  if (
    new Set(providerAuthorityIds).size !== providerAuthorityIds.length ||
    new Set(cardanoProviderUrls.map(providerPublicIdentity)).size !==
      cardanoProviderUrls.length
  ) {
    throw new Error(
      "external_providers mode requires operationally independent provider authorities and endpoints",
    );
  }
  const providers = cardanoProviderUrls
    .map((url, index) => ({
      authorityId: providerAuthorityIds[index]!,
      endpoint: providerPublicIdentity(url),
    }))
    .sort((left, right) => left.authorityId.localeCompare(right.authorityId));
  const authorityDigest = cardanoAuthorityDigest({
    sourceMode,
    network,
    networkMagic,
    providers,
  });
  return {
    sourceMode,
    providerAuthorityIds,
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

const providerPublicIdentity = (url: string): string => {
  if (url.startsWith("blockfrost:")) {
    const raw = url.slice("blockfrost:".length);
    const projectSeparator = raw.lastIndexOf("#");
    return `blockfrost:${projectSeparator < 0 ? raw : raw.slice(0, projectSeparator)}`;
  }
  return url;
};
