import {
  AuthenticatedCanonicalDecodeItemDatum,
  ObservedCanonicalDecodeItemDatum,
  PreparedCanonicalDecodeItemDatum,
  PreparedValidationResolutionDatum,
  ValidationResolutionDatum,
  VerifiedCanonicalDecodeItemDatum,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { type ResolvedValidationTraceDisputeDeploymentContracts } from "../runtime.js";
import { validationSemanticResolverGlobalIndex } from "./submit/reference-scripts.js";

export const canonicalCheckpointRoles = [
  "prepare",
  "authenticate",
  "source",
  "observe",
  "proof",
  "settlement",
] as const;

/** Address chooses the exact authenticated schema; no permissive decoder fallbacks. */
export const readCanonicalCheckpoint = (
  utxo: UTxO,
  chain: ResolvedValidationTraceDisputeDeploymentContracts["contracts"]["validationTraceDispute"],
) => {
  const semantic =
    chain.semanticResolvers[validationSemanticResolverGlobalIndex(0, 1)]!;
  const role = canonicalCheckpointRoles.find(
    (role) =>
      utxo.address ===
      (role === "prepare"
        ? chain.canonicalDecodePrepare
        : role === "authenticate"
          ? semantic
          : chain.canonicalDecodeItemStages[role]
      ).spendingScriptAddress,
  );
  if (role === undefined) return undefined;
  if (utxo.datum == null) throw new Error("canonical checkpoint omitted datum");
  if (role === "prepare") {
    const decoded = Data.from(utxo.datum, ValidationResolutionDatum);
    if (decoded.data === null)
      throw new Error("canonical preparation carries null state");
    return {
      role,
      resolution: decoded.data,
      fraudProver: decoded.fraud_prover,
      prepared: undefined,
      transition: undefined,
    };
  }
  const decoded =
    role === "authenticate"
      ? Data.from(utxo.datum, PreparedValidationResolutionDatum)
      : role === "source"
        ? Data.from(utxo.datum, AuthenticatedCanonicalDecodeItemDatum)
        : role === "observe"
          ? Data.from(utxo.datum, PreparedCanonicalDecodeItemDatum)
          : role === "proof"
            ? Data.from(utxo.datum, ObservedCanonicalDecodeItemDatum)
            : Data.from(utxo.datum, VerifiedCanonicalDecodeItemDatum);
  if (decoded.data === null)
    throw new Error("canonical checkpoint carries null state");
  // Every later schema embeds the exact earlier state installed by its validator.
  const authenticated =
    role === "source"
      ? Data.from(utxo.datum, AuthenticatedCanonicalDecodeItemDatum).data!
      : role === "observe"
        ? Data.from(utxo.datum, PreparedCanonicalDecodeItemDatum).data!
            .authenticated
        : role === "proof"
          ? Data.from(utxo.datum, ObservedCanonicalDecodeItemDatum).data!
              .prepared.authenticated
          : role === "settlement"
            ? Data.from(utxo.datum, VerifiedCanonicalDecodeItemDatum).data!
                .observed.prepared.authenticated
            : undefined;
  const prepared =
    authenticated?.base ??
    Data.from(utxo.datum, PreparedValidationResolutionDatum).data!;
  return {
    role,
    prepared,
    resolution: prepared.resolution,
    fraudProver: decoded.fraud_prover,
    transition: authenticated?.transition,
  };
};
