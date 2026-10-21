import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

/** Decodes a stored DA payload envelope; only the canonical V1 schema is accepted. */
export const decodeStoredPayload = ({
  payloadCbor,
  schemaVersion,
}: {
  readonly payloadCbor: Buffer;
  readonly schemaVersion: number;
}): Promise<SDK.DaPayload> =>
  schemaVersion !== Number(SDK.DA_PAYLOAD_VERSION)
    ? Promise.reject(
        new Error("Stored DA payload schema version must equal canonical V1"),
      )
    : unwrapDaPayload(payloadCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      }).then((unwrapped) => SDK.decodeDaPayload(unwrapped.innerBytes));
