import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScriptListPreimage,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonical,
  mergeMidgardCekProgramMaterialSidecars,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";

import { buildMidgardCanonicalScriptArtifact } from "../../src/cek-program.js";
import { type RetainedDaCanonicalScriptProjection } from "./retained-da-boundary.make-retained-pair-payload.js";

/**
 * Builds the canonical Midgard schema projection used only for retained-DA
 * capability evidence when a Cardano-derived transaction carries one genuine
 * raw Flat spending program. The script-witness identity and script-integrity
 * commitment are replaced with the canonical CEK envelope identity, copied
 * vkey signatures are removed, and the source/raw hash remains audit metadata.
 *
 * This does not assert Cardano-ledger or Midgard Phase A/B validity.
 */
export const buildMidgardRetainedDaCanonicalScriptProjection = ({
  canonicalTransactionCbor,
}: {
  readonly canonicalTransactionCbor: Uint8Array;
}): RetainedDaCanonicalScriptProjection => {
  const source = decodeMidgardNativeTxFullFromCanonicalCbor(
    canonicalTransactionCbor,
  );
  if (
    !source.body.requiredObserversPreimageCbor.equals(Buffer.from([0x80])) ||
    !source.body.mintPreimageCbor.equals(Buffer.from([0x80]))
  ) {
    throw new Error(
      "retained-DA single-script projection does not remap observer or mint credentials",
    );
  }
  const scripts = decodeMidgardVersionedScriptListPreimage(
    source.witnessSet.scriptTxWitsPreimageCbor,
  );
  if (
    scripts.length !== 1 ||
    (scripts[0]!.language !== "PlutusV3" &&
      scripts[0]!.language !== "MidgardV1")
  ) {
    throw new Error(
      "retained-DA single-script projection requires exactly one genuine Flat program",
    );
  }
  const rawScript = scripts[0]!;
  const artifact = buildMidgardCanonicalScriptArtifact({
    language: rawScript.language,
    sourceRawFlatProgramBytes: rawScript.scriptBytes,
  });

  const projected = materializeMidgardNativeTxFromCanonical({
    version: source.version,
    validity: source.validity,
    body: {
      ...source.body,
      scriptIntegrityHash: computeScriptIntegrityHashForLanguages(
        midgardFieldCommitment(source.witnessSet.redeemerTxWitsPreimageCbor),
        [rawScript.language],
      ),
    },
    witnessSet: {
      ...source.witnessSet,
      addrTxWitsPreimageCbor: Buffer.from([0x80]),
      scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
        artifact.canonicalMidgardCredentialScript,
      ]),
    },
  });
  const material = mergeMidgardCekProgramMaterialSidecars([
    artifact.canonicalMaterialSidecarCbor,
  ]);
  return {
    canonicalTransactionCbor: encodeMidgardNativeTxCanonical(projected),
    canonicalMaterialSidecarCbor:
      encodeMidgardCekProgramMaterialSidecar(material),
    sourceRawScriptAuditHash: artifact.sourceRawScriptAuditHash,
  };
};
