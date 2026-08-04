import {
  DaRequestResponseProtocol,
  type DaRequestResponseProtocol as DaRequestResponseProtocolName,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";

export const DA_REQUEST_RESPONSE_PROTOCOLS = Object.freeze(
  Object.values(DaRequestResponseProtocol),
) as readonly DaRequestResponseProtocolName[];

export type DaProtocolAllowlist = {
  readonly deploymentFingerprint: string;
  readonly protocolIds: readonly string[];
  readonly protocolIdByName: ReadonlyMap<DaRequestResponseProtocolName, string>;
  readonly protocolNameById: ReadonlyMap<string, DaRequestResponseProtocolName>;
  hasProtocolName(protocol: string): protocol is DaRequestResponseProtocolName;
  hasProtocolId(protocolId: string): boolean;
  requireProtocolId(protocolId: string): DaRequestResponseProtocolName;
};

export const createDaProtocolAllowlist = (
  deploymentFingerprint: string,
): DaProtocolAllowlist => {
  const protocolIdByName = new Map<DaRequestResponseProtocolName, string>();
  const protocolNameById = new Map<string, DaRequestResponseProtocolName>();
  for (const protocol of DA_REQUEST_RESPONSE_PROTOCOLS) {
    const protocolId = daRequestResponseProtocolId(
      deploymentFingerprint,
      protocol,
    );
    protocolIdByName.set(protocol, protocolId);
    protocolNameById.set(protocolId, protocol);
  }
  const protocolIds = Object.freeze([...protocolNameById.keys()]);
  return {
    deploymentFingerprint,
    protocolIds,
    protocolIdByName,
    protocolNameById,
    hasProtocolName(protocol): protocol is DaRequestResponseProtocolName {
      return protocolIdByName.has(protocol as DaRequestResponseProtocolName);
    },
    hasProtocolId(protocolId): boolean {
      return protocolNameById.has(protocolId);
    },
    requireProtocolId(protocolId): DaRequestResponseProtocolName {
      const protocol = protocolNameById.get(protocolId);
      if (protocol === undefined) {
        throw new Error(`unsupported DA libp2p protocol ${protocolId}`);
      }
      return protocol;
    },
  };
};
