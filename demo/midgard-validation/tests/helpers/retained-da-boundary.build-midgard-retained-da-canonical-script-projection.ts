import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScriptListPreimage,
  encodeMidgardNativeTxCanonical,
  hashMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";

import { type RetainedDaCanonicalScriptProjection } from "./retained-da-boundary.make-retained-pair-payload.js";

/**
 * Builds the canonical Midgard schema projection used only for retained-DA
 * capability evidence when a Cardano-derived transaction carries one genuine
 * stored Flat spending program. The script witness keeps its exact identity,
 * the script-integrity commitment uses the Midgard redeemer projection, and
 * copied vkey signatures are removed.
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
  const storedScript = scripts[0]!;

  const projected = materializeMidgardNativeTxFromCanonical({
    version: source.version,
    validity: source.validity,
    body: {
      ...source.body,
      scriptIntegrityHash: computeScriptIntegrityHashForLanguages(
        midgardFieldCommitment(source.witnessSet.redeemerTxWitsPreimageCbor),
        [storedScript.language],
      ),
    },
    witnessSet: {
      ...source.witnessSet,
      addrTxWitsPreimageCbor: Buffer.from([0x80]),
    },
  });
  return {
    canonicalTransactionCbor: encodeMidgardNativeTxCanonical(projected),
    sourceScriptHash: hashMidgardVersionedScript(storedScript),
  };
};
