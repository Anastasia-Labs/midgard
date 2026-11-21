import {
  computeHash32,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core/codec";

import { BuilderInvariantError } from "../core/errors.js";
import { normalizeScriptRef } from "../core/output.js";
import type { TrustedReferenceScriptMetadata } from "../core/scripts.js";
import type { MidgardScript } from "../core/types.js";
import type { KnownScriptSource } from "./script-witnesses.known-script-source.js";

export const knownReferenceScriptSource = (
  script: MidgardScript,
  sourceId: string,
  metadata?: TrustedReferenceScriptMetadata,
): KnownScriptSource => {
  const versioned = normalizeScriptRef(script);
  const localLanguage = versioned.language;
  const localHash = hashMidgardVersionedScript(versioned);
  if (metadata !== undefined && metadata.language !== localLanguage) {
    throw new BuilderInvariantError(
      "Reference script metadata language does not match local script reference",
      `${sourceId} ${metadata.language}`,
    );
  }
  if (metadata !== undefined && localHash !== metadata.scriptHash) {
    throw new BuilderInvariantError(
      "Reference script metadata hash does not match local script reference",
      `${sourceId} ${metadata.scriptHash}`,
    );
  }
  if (metadata?.scriptCborHash !== undefined) {
    const localScriptCborHash = computeHash32(
      encodeMidgardVersionedScript(versioned),
    ).toString("hex");
    if (localScriptCborHash !== metadata.scriptCborHash) {
      throw new BuilderInvariantError(
        "Reference script metadata scriptCborHash does not match local script reference",
        `${sourceId} ${metadata.scriptCborHash}`,
      );
    }
  }
  return {
    sourceId,
    inline: false,
    witnessScript: undefined,
    hashes: new Map([[localLanguage, localHash]]),
  };
};

export const knownTrustedReferenceScriptMetadataSource = (
  metadata: TrustedReferenceScriptMetadata,
  sourceId: string,
): KnownScriptSource => ({
  sourceId,
  inline: false,
  witnessScript: undefined,
  hashes: new Map([[metadata.language, metadata.scriptHash]]),
});
