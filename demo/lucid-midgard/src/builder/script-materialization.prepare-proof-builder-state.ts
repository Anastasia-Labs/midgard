import {
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeHash32,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import { buildMidgardCanonicalCekProgram } from "@al-ft/midgard-validation/cek-program";

import { BuilderInvariantError } from "../core/errors.js";
import { outRefLabel } from "../core/out-ref.js";
import {
  decodeMidgardTxOutput,
  normalizeScriptRef,
  utxoOutputCbor,
} from "../core/output.js";
import type {
  ScriptSource,
  TrustedReferenceScriptMetadata,
} from "../core/scripts.js";
import type { MidgardScript } from "../core/types.js";
import type { BuilderState } from "./context.js";
import {
  type KnownScriptSource,
  knownScriptSource,
  type PreparedProofBuilderState,
} from "./script-materialization.known-script-source.js";

const insertProgramMaterial = (
  material: Map<string, MidgardCekProgramMaterialEntry>,
  entry: MidgardCekProgramMaterialEntry,
): void => {
  const root = Buffer.from(entry.root).toString("hex");
  const prior = material.get(root);
  if (
    prior !== undefined &&
    (prior.kind !== entry.kind ||
      !Buffer.from(prior.preimage).equals(entry.preimage))
  ) {
    throw new BuilderInvariantError(
      "CEK program material hash collision",
      root,
    );
  }
  material.set(root, entry);
};

/**
 * Revalidates, merges, and canonically sorts exact V1 material collections.
 * Equal roots deduplicate only when their typed preimages are byte-identical.
 */
export const mergeCanonicalProofProgramMaterial = (
  ...collections: readonly (readonly MidgardCekProgramMaterialEntry[])[]
): readonly MidgardCekProgramMaterialEntry[] => {
  const material = new Map<string, MidgardCekProgramMaterialEntry>();
  try {
    for (const entries of collections) {
      const canonical = decodeMidgardCekProgramMaterialSidecar(
        encodeMidgardCekProgramMaterialSidecar(entries),
      );
      for (const entry of canonical) {
        insertProgramMaterial(material, entry);
      }
    }
  } catch (cause) {
    if (cause instanceof BuilderInvariantError) throw cause;
    throw new BuilderInvariantError(
      "Invalid canonical CEK program material",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  return Object.freeze(
    [...material.values()].sort((left, right) =>
      Buffer.compare(Buffer.from(left.root), Buffer.from(right.root)),
    ),
  );
};

const canonicalProofProgram = (
  script: MidgardVersionedScript,
  material: Map<string, MidgardCekProgramMaterialEntry>,
): MidgardVersionedScript => {
  if (script.language === "NativeCardano") return script;
  try {
    decodeMidgardCekProgramEnvelope(script.scriptBytes);
    return script;
  } catch {
    const canonical = buildMidgardCanonicalCekProgram(script.scriptBytes);
    for (const entry of canonical.material.values()) {
      insertProgramMaterial(material, entry);
    }
    return {
      language: script.language,
      scriptBytes: canonical.envelopeCbor,
    };
  }
};

const proofProgramEnvelope = (
  script: MidgardVersionedScript,
  sourceId: string,
): MidgardCekProgramEnvelope | undefined => {
  if (script.language === "NativeCardano") return undefined;
  try {
    return decodeMidgardCekProgramEnvelope(script.scriptBytes);
  } catch (cause) {
    throw new BuilderInvariantError(
      "V1 reference script must contain a canonical CEK program envelope",
      `${sourceId}: ${cause instanceof Error ? cause.message : String(cause)}`,
    );
  }
};

export const assertMetadataOnlyReferenceScriptMaterial = (
  metadata: TrustedReferenceScriptMetadata | undefined,
  sourceId: string,
): void => {
  if (metadata === undefined || metadata.language === "NativeCardano") {
    return;
  }
  throw new BuilderInvariantError(
    "Metadata-only non-native reference scripts require a canonical local reference script envelope and exact CEK program material",
    `${sourceId} ${metadata.language}`,
  );
};

/**
 * Replaces proof-profile raw UPLC authoring inputs with their compact
 * consensus envelopes and retains the exact content-addressed graph sidecar.
 * Historical reference inputs cannot be rewritten, so every non-native
 * reference envelope must be accompanied by its exact explicit material.
 */
export const prepareProofBuilderState = (
  state: BuilderState,
  explicitProgramMaterial: readonly MidgardCekProgramMaterialEntry[] = [],
): PreparedProofBuilderState => {
  const material = new Map(
    mergeCanonicalProofProgramMaterial(explicitProgramMaterial).map(
      (entry) => [Buffer.from(entry.root).toString("hex"), entry] as const,
    ),
  );
  const scripts = state.scripts.scripts.map((source, index): ScriptSource => {
    if (source.kind === "native") return source;
    if (source.kind === "dual-plutus-v3-midgard-v1") {
      throw new BuilderInvariantError(
        "Dual PlutusV3/MidgardV1 script witnesses are not supported; attach explicit versioned scripts",
        `inline:${index.toString()}`,
      );
    }
    const known = knownScriptSource(source, `inline:${index.toString()}`, true);
    if (known.witnessScript === undefined) {
      throw new BuilderInvariantError(
        "Inline V1 script is missing witness bytes",
      );
    }
    const canonical = canonicalProofProgram(known.witnessScript, material);
    return canonical.language === "PlutusV3"
      ? {
          kind: "plutus-v3",
          language: "PlutusV3",
          script: Buffer.from(canonical.scriptBytes),
        }
      : {
          kind: "midgard-v1",
          language: "MidgardV1",
          script: Buffer.from(canonical.scriptBytes),
        };
  });
  const outputs = state.outputs.map((output) => {
    if (output.scriptRef === undefined) return output;
    const canonical = canonicalProofProgram(
      normalizeScriptRef(output.scriptRef),
      material,
    );
    if (canonical.language === "NativeCardano") return output;
    return {
      ...output,
      scriptRef: {
        type: canonical.language,
        script: Buffer.from(canonical.scriptBytes).toString("hex"),
      } as const,
    };
  });
  const envelopes: MidgardCekProgramEnvelope[] = [];
  for (const [index, source] of scripts.entries()) {
    if (source.kind === "native") continue;
    if (source.kind === "dual-plutus-v3-midgard-v1") {
      throw new BuilderInvariantError(
        "Dual PlutusV3/MidgardV1 script witnesses are not supported; attach explicit versioned scripts",
        `inline:${index.toString()}`,
      );
    }
    const known = knownScriptSource(source, `inline:${index.toString()}`, true);
    if (known.witnessScript === undefined) {
      throw new BuilderInvariantError(
        "Inline V1 script is missing witness bytes",
      );
    }
    const envelope = proofProgramEnvelope(
      known.witnessScript,
      `inline:${index.toString()}`,
    );
    if (envelope !== undefined) envelopes.push(envelope);
  }
  for (const [index, output] of outputs.entries()) {
    if (output.scriptRef === undefined) continue;
    const envelope = proofProgramEnvelope(
      normalizeScriptRef(output.scriptRef),
      `output:${index.toString()}`,
    );
    if (envelope !== undefined) envelopes.push(envelope);
  }
  for (const input of state.referenceInputs) {
    const label = outRefLabel(input);
    const scriptRef = decodeMidgardTxOutput(utxoOutputCbor(input)).txOutput
      .scriptRef;
    if (scriptRef === undefined || scriptRef === null) {
      assertMetadataOnlyReferenceScriptMaterial(
        state.scripts.referenceScriptMetadata.find(
          (metadata) => outRefLabel(metadata) === label,
        ),
        `reference:${label}`,
      );
      continue;
    }
    const envelope = proofProgramEnvelope(
      normalizeScriptRef(scriptRef),
      `reference:${label}`,
    );
    if (envelope !== undefined) envelopes.push(envelope);
  }
  const programMaterial = mergeCanonicalProofProgramMaterial([
    ...material.values(),
  ]);
  try {
    verifyMidgardCekProgramMaterialBundle(envelopes, programMaterial);
  } catch (cause) {
    throw new BuilderInvariantError(
      "Incomplete or mismatched CEK program material",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  return Object.freeze({
    state: {
      ...state,
      scripts: {
        ...state.scripts,
        scripts,
      },
      outputs,
    },
    programMaterial,
  });
};

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
