import {
  encodeCbor,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";

import type {
  MidgardCanonicalScriptArtifact,
  MidgardCanonicalScriptArtifactInput,
} from "./cek-program.unwrap-canonical-cbor-byte-string.js";

/** Wraps raw Flat once, preserving Cardano's exact script identity. */
export const buildMidgardCanonicalScriptArtifact = ({
  language,
  sourceRawFlatProgramBytes,
}: MidgardCanonicalScriptArtifactInput): MidgardCanonicalScriptArtifact => {
  const scriptBytes = encodeCbor(Buffer.from(sourceRawFlatProgramBytes));
  const script = (): MidgardVersionedScript => ({
    language,
    scriptBytes: Buffer.from(scriptBytes),
  });
  return Object.freeze({
    get canonicalMidgardCredentialScript(): MidgardVersionedScript {
      return script();
    },
    canonicalMidgardCredentialScriptHash: hashMidgardVersionedScript(script()),
  });
};
