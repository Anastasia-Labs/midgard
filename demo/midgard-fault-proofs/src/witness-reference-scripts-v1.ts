/**
 * Reference-script carriage for the shared witness scripts fault-proof
 * transactions execute: the computation-thread and fraud-proof minting
 * policies, and the `phas.membership.withdraw`, chunked-verify and
 * `pexcludes.exclusion.withdraw` withdrawal verifiers.
 *
 * Fault proofs and their supporting scripts deploy as published reference
 * scripts, never inline-attached (owner ruling 2026-08-26). Submitters take
 * a bundle of published reference-script UTxOs. Every required entry is
 * hash-checked against the exact script the transaction executes and joins
 * the transaction's reference inputs. Missing entries fail closed: there is
 * no inline fallback for undeployed witness scripts.
 */
import {
  type Script,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

/**
 * Published reference-script UTxOs for the shared witness scripts, keyed by
 * role. Every entry is optional; a submitter only reads the roles its
 * transaction would otherwise inline-attach.
 */
export type FaultProofWitnessReferenceScriptsV1 = {
  /** The computation-thread token minting policy. */
  readonly computationThreadMint?: UTxO;
  /** The fraud-proof token minting policy. */
  readonly fraudProofMint?: UTxO;
  /** The `phas.membership.withdraw` membership verifier. */
  readonly phasMembershipWithdraw?: UTxO;
  /** The chunked-proof membership verifier (#545 published-chunk carriage). */
  readonly chunkedVerifyWithdraw?: UTxO;
  /** The `pexcludes.exclusion.withdraw` exclusion verifier. */
  readonly pexcludesWithdraw?: UTxO;
};

const witnessOutRefLabel = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * Fail-closed gate on a published witness reference script: the UTxO must
 * carry a reference script hashing to exactly the script the caller would
 * otherwise inline-attach.
 */
export const requireWitnessReferenceScriptUtxoV1 = ({
  utxo,
  script,
  label,
}: {
  readonly utxo: UTxO;
  readonly script: Script;
  readonly label: string;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw new Error(
      `${label} reference UTxO ${witnessOutRefLabel(utxo)} carries no reference script.`,
    );
  }
  const expected = validatorToScriptHash(script);
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expected) {
    throw new Error(
      `${label} reference script at ${witnessOutRefLabel(utxo)} hashes to ${actual}, not ${expected}.`,
    );
  }
  return utxo;
};

/**
 * How one witness script reaches its transaction: through the published
 * reference UTxO in `referenceInputs`.
 */
export type WitnessScriptCarriageV1 = {
  /**
   * The hash-checked published UTxO. Callers MUST splice this into the transaction's reference
   * inputs BEFORE deriving any chunk/field-opening reference indices, so the
   * canonical sorted set those derivations see is the complete built set.
   */
  readonly referenceInputs: readonly UTxO[];
  /** Retained as an identity composition seam for existing submitter chains. */
  readonly attach: (tx: TxBuilder) => TxBuilder;
};

const witnessScriptCarriageV1 = ({
  script,
  referenceUtxo,
  label,
}: {
  readonly script: Script;
  readonly referenceUtxo: UTxO | undefined;
  readonly label: string;
}): WitnessScriptCarriageV1 => {
  if (referenceUtxo === undefined) {
    throw new Error(`${label} requires a published reference script UTxO.`);
  }
  return {
    referenceInputs: [
      requireWitnessReferenceScriptUtxoV1({
        utxo: referenceUtxo,
        script,
        label,
      }),
    ],
    attach: (tx) => tx,
  };
};

/** Carriage for a minting-policy witness. */
export const witnessMintingPolicyCarriageV1 = (options: {
  readonly script: Script;
  readonly referenceUtxo: UTxO | undefined;
  readonly label: string;
}): WitnessScriptCarriageV1 => witnessScriptCarriageV1({ ...options });

/** Carriage for a withdrawal-validator witness. */
export const witnessWithdrawalValidatorCarriageV1 = (options: {
  readonly script: Script;
  readonly referenceUtxo: UTxO | undefined;
  readonly label: string;
}): WitnessScriptCarriageV1 => witnessScriptCarriageV1({ ...options });

/** Carriage for a spending-validator witness. */
export const witnessSpendingValidatorCarriageV1 = (options: {
  readonly script: Script;
  readonly referenceUtxo: UTxO | undefined;
  readonly label: string;
}): WitnessScriptCarriageV1 => witnessScriptCarriageV1({ ...options });
