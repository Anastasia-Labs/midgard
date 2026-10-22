import { encodeCbor } from "@al-ft/midgard-core";

import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";

/** Builds local dispute material from a raw Flat fixture with one CBOR wrapper. */
export const buildCekProgramFromRawFlat = (flat: Uint8Array) => {
  const scriptBytes = encodeCbor(Buffer.from(flat));
  return { ...buildMidgardCanonicalCekProgram(scriptBytes), scriptBytes };
};
