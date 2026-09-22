import { ProviderPayloadError } from "../core/errors.js";
import type {
  MidgardProtocolParameters,
  MidgardUtxo,
  OutRef,
} from "../core/index.js";
import { outRefLabel } from "../core/out-ref.js";
import { utxoAddress } from "../core/output.js";
import { cloneSupportedScriptLanguages } from "./payload.js";
import type { MidgardProtocolInfo } from "./types.js";

export const knownNetworkId = (network: string): bigint => {
  switch (network) {
    case "Mainnet":
      return 1n;
    case "Preprod":
    case "Preview":
      return 0n;
    default:
      throw new ProviderPayloadError(
        "/protocol-info",
        "unsupported protocol network",
        network,
      );
  }
};

export const protocolInfoToParameters = (
  info: MidgardProtocolInfo,
): MidgardProtocolParameters => ({
  apiVersion: info.apiVersion,
  network: info.network,
  midgardNativeTxVersion: info.midgardNativeTxVersion,
  currentSlot: info.currentSlot,
  supportedScriptLanguages: cloneSupportedScriptLanguages(
    info.supportedScriptLanguages,
  ),
  minFeeA: info.protocolFeeParameters.minFeeA,
  minFeeB: info.protocolFeeParameters.minFeeB,
  networkId: knownNetworkId(info.network),
  maxSubmitTxCborBytes: info.submissionLimits.maxSubmitTxCborBytes,
  strictnessProfile: info.validation.strictnessProfile,
  ...(info.deploymentMarker === undefined
    ? {}
    : { deploymentManifestId: info.deploymentMarker.manifestId }),
});

export const requireAddressQueryUtxos = (
  utxos: readonly MidgardUtxo[],
  requestedAddress: string,
  endpoint: string,
): readonly MidgardUtxo[] =>
  utxos.map((utxo) => {
    if (utxoAddress(utxo) !== requestedAddress) {
      throw new ProviderPayloadError(
        endpoint,
        "GET /utxos returned an output for a different address",
      );
    }
    return utxo;
  });

export const requireUtxoByOutRef = (
  utxo: MidgardUtxo,
  requestedOutRef: OutRef,
  endpoint: string,
): MidgardUtxo => {
  if (outRefLabel(utxo) !== outRefLabel(requestedOutRef)) {
    throw new ProviderPayloadError(
      endpoint,
      "GET /utxo returned a different outref than requested",
    );
  }
  return utxo;
};

export const requestedOutRefLabels = (
  outRefs: readonly OutRef[],
  endpoint: string,
): ReadonlySet<string> => {
  const requestedLabels = new Set(outRefs.map((outRef) => outRefLabel(outRef)));
  if (requestedLabels.size !== outRefs.length) {
    throw new ProviderPayloadError(endpoint, "duplicate requested outref");
  }
  return requestedLabels;
};

export const requireBatchUtxosByOutRefs = (
  utxos: readonly MidgardUtxo[],
  requestedLabels: ReadonlySet<string>,
  endpoint: string,
): readonly MidgardUtxo[] => {
  const seen = new Set<string>();
  return utxos.map((utxo) => {
    const label = outRefLabel(utxo);
    if (!requestedLabels.has(label)) {
      throw new ProviderPayloadError(
        endpoint,
        `POST /utxos?by-outrefs returned unrequested outref ${label}`,
      );
    }
    if (seen.has(label)) {
      throw new ProviderPayloadError(
        endpoint,
        `POST /utxos?by-outrefs returned duplicate outref ${label}`,
      );
    }
    seen.add(label);
    return utxo;
  });
};
