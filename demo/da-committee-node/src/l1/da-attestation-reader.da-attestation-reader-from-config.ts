import type { LoadedCommitteeConfig } from "../config.js";
import { LucidDaAttestationChainReader } from "./da-attestation-reader.lucid-da-attestation-chain-reader.js";
import {
  lucidFromProviderUrl,
  MultiDaAttestationChainReader,
} from "./da-attestation-reader.lucid-from-provider-url.js";
import { type DaAttestationChainReader } from "./da-attestation-reader.provenance-resolver.js";
import { localNodeChainAuthorityFromConfig } from "./provider.js";

export const daAttestationReaderFromConfig = async (
  config: LoadedCommitteeConfig,
): Promise<DaAttestationChainReader | undefined> => {
  const l1Source = config.l1Source;
  const providerDescriptors =
    l1Source.sourceMode === "local_node"
      ? l1Source.queryProviderUrls.map((url, index) => ({
          url,
          providerSource: `query:${l1Source.authorityNodeId}:${index.toString()}`,
        }))
      : l1Source.providers.map(({ url, identity, operationalIdentity }) => ({
          url,
          providerSource: [
            identity,
            `operator=${operationalIdentity.operatorId}`,
            `transport=${operationalIdentity.transport}`,
            `backend=${operationalIdentity.backendKey}`,
          ].join(";"),
        }));
  if (
    providerDescriptors[0]?.url.startsWith("fixture:") === true ||
    providerDescriptors[0]?.url.startsWith("file:") === true
  ) {
    return undefined;
  }
  const localAuthority =
    config.l1Source.sourceMode === "local_node"
      ? localNodeChainAuthorityFromConfig(config)
      : undefined;
  const readers = await Promise.all(
    providerDescriptors.map(async ({ url, providerSource }) => {
      const provider = await lucidFromProviderUrl(
        url,
        config.network,
        config.cardanoL1Source.networkMagic,
      );
      return new LucidDaAttestationChainReader({
        lucid: provider.lucid,
        config,
        providerSource,
        inclusionPointResolver: provider.inclusionPointResolver,
        queryPointResolver: provider.queryPointResolver,
        localAuthority,
      });
    }),
  );
  if (
    config.l1Source.sourceMode === "external_providers" &&
    readers.length < 2
  ) {
    throw new Error(
      "external_providers mode requires at least two DA attestation readers",
    );
  }
  // Local query surfaces share one chain authority and are not a provider
  // quorum, but conflicting views must still fail closed.
  return readers.length === 1
    ? readers[0]!
    : new MultiDaAttestationChainReader(readers);
};
