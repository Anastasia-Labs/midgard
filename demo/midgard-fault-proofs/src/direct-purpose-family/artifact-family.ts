/** What a direct purpose family supplies for its journaled artifact: how a
 * finding is admitted and how its convicting proof is planned. Types only, so
 * every family's replay module can name its artifact without loading the
 * workflow modules that load it. */
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import type { DirectPurposeTransition } from "./custody.js";
import type { DirectPurposeContracts } from "./family.js";
import type { DirectPurposePaging } from "./paging.js";
import type { DirectPurposeSourceCoordinate } from "./replay-support.js";

/** A family's planned finding at one committed source. */
export type DirectPurposeReplay = Readonly<{
  coordinate: DirectPurposeSourceCoordinate;
  material: DirectScriptMaterial;
  detectionId: string;
}>;

export type DirectPurposeProof = Readonly<{
  transitions: readonly DirectPurposeTransition[];
}>;

export type DirectPurposeAdmitted<Prepared extends DirectPurposeReplay> =
  Prepared & Readonly<{ current: TransitionTraceReconstruction }>;

/** What admits a family's journaled artifact. */
export type DirectPurposeArtifactAdmission<
  Prepared extends DirectPurposeReplay,
> = Readonly<{
  /** The category, naming every refusal. */
  label: string;
  /** The journaled artifact's `schemaVersion`. */
  artifactVersion: string;
  /** The finding at one coordinate; `null` when it does not convict. */
  prepare: (input: {
    readonly current: TransitionTraceReconstruction;
    readonly predecessor?: CheckedLedger;
    readonly coordinate: DirectPurposeSourceCoordinate;
  }) => Promise<Prepared | null>;
  /** Every finding of one block. */
  detect: (input: {
    readonly block: CanonicalBlockEvidence;
    readonly predecessor?: CheckedLedger;
  }) => Promise<
    readonly Readonly<{
      detectionId: string;
      prepared: Readonly<{ coordinate: DirectPurposeSourceCoordinate }>;
    }>[]
  >;
}>;

export type DirectPurposeArtifactFamily<
  Prepared extends DirectPurposeReplay,
  Proof extends DirectPurposeProof,
> = DirectPurposeArtifactAdmission<Prepared> &
  Readonly<{
    /** The convicting proof of an admitted artifact. */
    prove: (
      admitted: DirectPurposeAdmitted<Prepared>,
      contracts: DirectPurposeContracts,
      prover: string,
    ) => Proof | Promise<Proof>;
    /** The custody as the kit fits it, `step01Datum` before the first
     * transition; the planned transitions paged by the kit otherwise. */
    paging?: (proof: Proof, step01Datum: string) => DirectPurposePaging;
  }>;

/** A family's artifact, its finding and proof types inferred. */
export const directPurposeArtifactFamily = <
  Prepared extends DirectPurposeReplay,
  Proof extends DirectPurposeProof,
>(
  family: DirectPurposeArtifactFamily<Prepared, Proof>,
) => Object.freeze(family);
