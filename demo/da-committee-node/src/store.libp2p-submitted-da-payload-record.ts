import type { DaStoredPayloadRecord } from "./domain.js";

export const libp2pSubmittedDaPayloadRecord = (args: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly payloadSchemaVersion: 1;
  readonly payloadCbor: Uint8Array;
  readonly payloadSha256: string;
  readonly receivedAt: Date;
}): DaStoredPayloadRecord => ({
  deploymentFingerprint: args.deploymentFingerprint,
  headerHash: args.headerHash,
  payloadSchemaVersion: args.payloadSchemaVersion,
  payloadCborHex: Buffer.from(args.payloadCbor).toString("hex"),
  payloadSha256: args.payloadSha256,
  sourcePeerId: "libp2p:payload-submit",
  fetchedAt: args.receivedAt.toISOString(),
  payloadFetchStatus: "available",
  // A payload-submit ACK proves retention only.  The watcher must promote
  // this to "verified" after strict inner payload/header validation.
  validationStatus: "fetched",
});
