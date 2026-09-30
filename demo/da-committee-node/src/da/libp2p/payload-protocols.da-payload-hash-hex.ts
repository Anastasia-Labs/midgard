import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";

import { bytesToHex } from "../../utils/hex.js";

export const daPayloadHashHex = (payloadBytes: Uint8Array): string =>
  bytesToHex(computeDaSha256Hash(payloadBytes));
