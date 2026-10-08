import { type Env } from "./config.committee-config.js";

export const requireEnv = (env: Env, name: string): string => {
  const value = env[name];
  if (value === undefined || value.trim() === "") {
    throw new Error(`${name} is required`);
  }
  return value.trim();
};

export const optionalNonEmpty = (
  value: string | undefined,
): string | undefined => {
  const trimmed = value?.trim();
  return trimmed === undefined || trimmed === "" ? undefined : trimmed;
};

export const optionalKeySource = (
  value: string | undefined,
  name: string,
): string | undefined => {
  const source = optionalNonEmpty(value);
  if (source === undefined) {
    return undefined;
  }
  validateKeySourceSyntax(source, name);
  return source;
};

const validateKeySourceSyntax = (source: string, name: string): void => {
  const prefixedSources = [
    "file:",
    "seed:",
    "mnemonic:",
    "private-key:",
    "privateKey:",
  ];
  for (const prefix of prefixedSources) {
    if (source === prefix) {
      throw new Error(`${name} must include a value after ${prefix}`);
    }
  }
};

export const splitList = (value: string): readonly string[] => {
  const values = value
    .split(",")
    .map((part) => part.trim())
    .filter((part) => part.length > 0);
  if (values.length === 0) {
    throw new Error("expected a non-empty comma-separated list");
  }
  return values;
};

export const optionalSplitList = (
  value: string | undefined,
): readonly string[] => {
  const trimmed = optionalNonEmpty(value);
  return trimmed === undefined ? [] : splitList(trimmed);
};

export const localChainSyncUrl = (value: string, testMode: boolean): string => {
  if (!value.startsWith("chain-sync:") || value === "chain-sync:") {
    throw new Error(
      "CARDANO_LOCAL_NODE_CHAIN_SYNC_URL must use the chain-sync:<provider> form",
    );
  }
  const provider = value.slice("chain-sync:".length);
  if (!testMode && isFixtureProviderUrl(provider)) {
    throw new Error(
      "fixture:/file: local chain-sync sources require explicit CARDANO_L1_TEST_MODE=true",
    );
  }
  if (
    !provider.startsWith("kupmios:") &&
    !provider.startsWith("ogmios:") &&
    !provider.startsWith("fixture:") &&
    !provider.startsWith("file:")
  ) {
    throw new Error(
      "CARDANO_LOCAL_NODE_CHAIN_SYNC_URL authority must be a local ogmios: or kupmios: surface (fixture:/file: only in tests)",
    );
  }
  return value;
};

export const assertLocalQuerySurfacesShareAuthority = (
  chainSyncProviderUrl: string,
  queryProviderUrls: readonly string[],
): void => {
  const authorityProvider = chainSyncProviderUrl.slice("chain-sync:".length);
  const authorityOgmiosUrl = authorityProvider.startsWith("ogmios:")
    ? authorityProvider.slice("ogmios:".length)
    : authorityProvider.startsWith("kupmios:")
      ? authorityProvider.slice("kupmios:".length).split("|")[1]
      : undefined;
  if (authorityOgmiosUrl === undefined) {
    throw new Error(
      "production local_node chain sync requires an Ogmios authority endpoint",
    );
  }
  const normalizedAuthority = normalizeOperationalEndpoint(
    authorityOgmiosUrl,
    "local authority Ogmios",
  );
  for (const [index, providerUrl] of queryProviderUrls.entries()) {
    if (!providerUrl.startsWith("kupmios:")) {
      throw new Error(
        `production local_node query surface ${index.toString()} must be kupmios: backed by the local authority`,
      );
    }
    const [, queryOgmiosUrl, extra] = providerUrl
      .slice("kupmios:".length)
      .split("|");
    if (queryOgmiosUrl === undefined || extra !== undefined) {
      throw new Error(
        "kupmios provider URL must be kupmios:<kupo-url>|<ogmios-url>",
      );
    }
    const normalizedQueryAuthority = normalizeOperationalEndpoint(
      queryOgmiosUrl,
      "query Ogmios",
    );
    if (normalizedQueryAuthority !== normalizedAuthority) {
      throw new Error(
        `production local_node query surface ${index.toString()} is not backed by the configured chain-sync authority`,
      );
    }
  }
};

export const isFixtureProviderUrl = (value: string): boolean =>
  value.startsWith("fixture:") || value.startsWith("file:");

const normalizeOperationalEndpoint = (value: string, label: string): string => {
  let parsed: URL;
  try {
    parsed = new URL(value);
  } catch {
    throw new Error(`${label} operational endpoint must be an absolute URL`);
  }
  if (
    parsed.protocol !== "https:" &&
    parsed.protocol !== "http:" &&
    parsed.protocol !== "wss:" &&
    parsed.protocol !== "ws:"
  ) {
    throw new Error(`${label} operational endpoint uses unsupported transport`);
  }
  if (parsed.username !== "" || parsed.password !== "") {
    throw new Error(
      `${label} operational endpoint must not embed credentials in its identity`,
    );
  }
  const canonicalProtocol =
    parsed.protocol === "wss:"
      ? "https:"
      : parsed.protocol === "ws:"
        ? "http:"
        : parsed.protocol;
  const defaultPort =
    canonicalProtocol === "https:" && parsed.port === "443"
      ? ""
      : canonicalProtocol === "http:" && parsed.port === "80"
        ? ""
        : parsed.port;
  const hostname = parsed.hostname.toLowerCase().replace(/\.$/u, "");
  return `${canonicalProtocol}//${hostname}${defaultPort === "" ? "" : `:${defaultPort}`}`;
};

export const boundedIdentity = (value: string, name: string): string => {
  if (!/^[a-z][a-z0-9-]{2,63}$/u.test(value)) {
    throw new Error(`${name} entries must be lowercase operational identities`);
  }
  return value;
};

export const booleanEnv = (
  value: string | undefined,
  defaultValue: boolean,
): boolean => {
  const normalized = value?.trim().toLowerCase();
  if (normalized === undefined || normalized === "") {
    return defaultValue;
  }
  if (["1", "true", "yes", "on"].includes(normalized)) {
    return true;
  }
  if (["0", "false", "no", "off"].includes(normalized)) {
    return false;
  }
  throw new Error("boolean environment values must be true or false");
};
