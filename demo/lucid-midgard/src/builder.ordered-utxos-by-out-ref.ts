import {
  encodeMidgardAddressText,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import { CML, type Network } from "@lucid-evolution/lucid";

import {
  type DatumOfOptions,
  normalizeUtxo,
  type UtxosByOutRefOptions,
} from "./builder.compose-states.js";
import type {
  LucidMidgardConfig,
  LucidMidgardConfigSnapshot,
  UtxoOverrideSnapshot,
} from "./builder/context.js";
import { normalizeHashHex } from "./builder/normalizers.js";
import { assertNoDuplicateStrings, cloneUtxo } from "./builder/state.js";
import { type AssetUnit } from "./core/assets.js";
import {
  BuilderInvariantError,
  ProviderPayloadError,
  SigningError,
} from "./core/errors.js";
import { normalizeOutRef, type OutRef, outRefLabel } from "./core/out-ref.js";
import { type Address, type MidgardUtxo } from "./core/types.js";
import type { MidgardProvider } from "./provider.js";
import {
  assertAddressNetwork,
  type MidgardWallet,
  paymentKeyHashFromAddress,
} from "./wallet.js";

const normalizeProviderUtxo = (
  utxo: MidgardUtxo,
  endpoint: string,
): MidgardUtxo => {
  try {
    return normalizeUtxo(utxo);
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "Provider returned invalid Midgard UTxO",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

export const normalizeProviderUtxos = (
  utxos: readonly MidgardUtxo[],
  endpoint: string,
): readonly MidgardUtxo[] => {
  if (!Array.isArray(utxos)) {
    throw new ProviderPayloadError(
      endpoint,
      "Provider UTxO result must be an array",
    );
  }
  return utxos.map((utxo) => normalizeProviderUtxo(utxo, endpoint));
};

export const cloneUtxoOverrideSnapshot = (
  snapshot: UtxoOverrideSnapshot | undefined,
): UtxoOverrideSnapshot | undefined =>
  snapshot === undefined
    ? undefined
    : {
        generation: snapshot.generation,
        utxos: snapshot.utxos.map(cloneUtxo),
      };

export const normalizeAssetUnit = (unit: AssetUnit): AssetUnit => {
  if (typeof unit !== "string") {
    throw new BuilderInvariantError("Asset unit must be a string");
  }
  if (unit.trim().toLowerCase() === "lovelace") {
    return "lovelace";
  }
  let normalized: string;
  try {
    normalized = normalizeHex(unit, { fieldName: "asset unit" });
  } catch {
    throw new BuilderInvariantError(
      "Asset unit must be lovelace or policy-id plus hex asset-name",
      unit,
    );
  }
  if (normalized.length < 56 || normalized.length > 120) {
    throw new BuilderInvariantError(
      "Asset unit must be lovelace or policy-id plus hex asset-name",
      unit,
    );
  }
  return normalized;
};

export const normalizeAddressQuery = (
  address: Address,
  expectedNetworkId: number | undefined,
  endpoint: string,
): Address => {
  if (typeof address !== "string") {
    throw new ProviderPayloadError(
      endpoint,
      "Midgard address queries require a bech32 address string",
    );
  }
  let normalized: Address;
  try {
    normalized = encodeMidgardAddressText(midgardAddressFromText(address));
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "address must be a valid Midgard bech32 address",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  assertAddressNetwork(normalized, expectedNetworkId);
  return normalized;
};

const normalizeMissingMode = (
  options: UtxosByOutRefOptions | undefined,
): "error" | "omit" => {
  const mode = options?.missing ?? "error";
  if (mode !== "error" && mode !== "omit") {
    throw new BuilderInvariantError(
      'utxosByOutRef missing option must be "error" or "omit"',
      String(mode),
    );
  }
  return mode;
};

const assertRequestedOutRefsUnique = (
  outRefs: readonly OutRef[],
  endpoint: string,
): readonly OutRef[] => {
  const normalized = outRefs.map(normalizeOutRef);
  assertNoDuplicateStrings(
    normalized.map(({ txHash, outputIndex }) => `${txHash}#${outputIndex}`),
    "Duplicate requested outref",
  );
  if (normalized.length === 0) {
    throw new BuilderInvariantError(
      "OutRef query must include at least one outref",
      endpoint,
    );
  }
  return normalized;
};

export const orderedUtxosByOutRef = async (
  provider: MidgardProvider,
  outRefs: readonly OutRef[],
  options?: UtxosByOutRefOptions,
): Promise<readonly MidgardUtxo[]> => {
  const endpoint = "/utxos?by-outrefs";
  const requested = assertRequestedOutRefsUnique(outRefs, endpoint);
  const missingMode = normalizeMissingMode(options);
  const raw =
    provider.getUtxosByOutRefs === undefined
      ? await Promise.all(
          requested.map(async (outRef) => provider.getUtxoByOutRef(outRef)),
        ).then((items) =>
          items.filter((item): item is MidgardUtxo => item !== undefined),
        )
      : await provider.getUtxosByOutRefs(requested);
  const requestedLabels = requested.map(
    ({ txHash, outputIndex }) => `${txHash}#${outputIndex}`,
  );
  const requestedLabelSet = new Set(requestedLabels);
  const byLabel = new Map<string, MidgardUtxo>();
  for (const utxo of normalizeProviderUtxos(raw, endpoint)) {
    const label = outRefLabel(utxo);
    if (!requestedLabelSet.has(label)) {
      throw new ProviderPayloadError(
        endpoint,
        "Provider returned an unrequested outref",
        label,
      );
    }
    if (byLabel.has(label)) {
      throw new ProviderPayloadError(
        endpoint,
        "Provider returned a duplicate outref",
        label,
      );
    }
    byLabel.set(label, utxo);
  }
  const ordered: MidgardUtxo[] = [];
  for (const label of requestedLabels) {
    const found = byLabel.get(label);
    if (found === undefined) {
      if (missingMode === "error") {
        throw new ProviderPayloadError(
          endpoint,
          "Missing requested UTxO",
          label,
        );
      }
      continue;
    }
    ordered.push(cloneUtxo(found));
  }
  return ordered;
};

const datumExpectedHash = (
  options: DatumOfOptions | undefined,
): string | undefined => {
  const expected =
    typeof options === "string" ? options : options?.expectedHash;
  return expected === undefined
    ? undefined
    : normalizeHashHex(expected, "datum hash", 32);
};

export const inlineDatumFromUtxo = (
  utxo: MidgardUtxo,
  options?: DatumOfOptions,
): Buffer => {
  const endpoint = "/datum";
  const normalized = normalizeProviderUtxo(utxo, endpoint);
  if (
    normalized.output.datum === undefined ||
    normalized.output.datum === null
  ) {
    throw new ProviderPayloadError(endpoint, "UTxO does not contain a datum");
  }
  const datumCbor = Buffer.from(normalized.output.datum.cbor, "hex");
  const datum = CML.PlutusData.from_cbor_bytes(datumCbor);
  const expectedHash = datumExpectedHash(options);
  const actualHash = CML.hash_plutus_data(datum).to_hex();
  if (expectedHash !== undefined && expectedHash !== actualHash) {
    throw new ProviderPayloadError(
      endpoint,
      "Datum hash mismatch",
      `expected=${expectedHash} actual=${actualHash}`,
    );
  }
  return datumCbor;
};

export const readOnlyWalletFromAddress = (
  address: Address,
  expectedNetworkId: number | undefined,
): MidgardWallet => {
  assertAddressNetwork(address, expectedNetworkId);
  const keyHash = paymentKeyHashFromAddress(address);
  return {
    address: async () => address,
    keyHash: async () => keyHash,
    signBodyHash: async () => {
      throw new SigningError(
        "Read-only Midgard wallet cannot sign body hashes",
      );
    },
  };
};

export const normalizeLucidMidgardConfig = (
  networkOrConfig?: Network | LucidMidgardConfig,
): LucidMidgardConfig =>
  typeof networkOrConfig === "string"
    ? { network: networkOrConfig }
    : { ...(networkOrConfig ?? {}) };

export const networkForSeedWallet = (network: string | undefined): Network => {
  if (network === "Mainnet" || network === "Preprod" || network === "Preview") {
    return network;
  }
  throw new BuilderInvariantError(
    "Selecting a seed wallet requires a known Cardano network",
    network ?? "undefined",
  );
};

export const validateSelectedWalletForConfig = async (
  wallet: MidgardWallet | undefined,
  config: LucidMidgardConfigSnapshot,
): Promise<void> => {
  if (wallet === undefined || config.networkId === undefined) {
    return;
  }
  try {
    assertAddressNetwork(await wallet.address(), config.networkId);
  } catch (cause) {
    if (
      cause instanceof SigningError &&
      cause.message.includes("does not expose an address")
    ) {
      return;
    }
    throw cause;
  }
};
