/**
 * Reference-script carriage for the shared witness scripts fault-proof
 * transactions execute: the computation-thread and fraud-proof minting
 * policies, and the `phas.membership.withdraw`, chunked-verify and
 * `pexcludes.exclusion.withdraw` withdrawal verifiers.
 *
 * Fault proofs and their supporting scripts deploy as published reference
 * scripts, never inline-attached (owner ruling 2026-08-26). Submitters take
 * an optional bundle of published reference-script UTxOs: a present entry is
 * hash-checked against the exact script the submitter would otherwise
 * inline-attach, joins the transaction's reference inputs, and the inline
 * attachment is skipped. An absent entry falls back to the historical inline
 * attachment, mirroring the per-step `referenceScriptUtxo` parameter the
 * chain submitters already expose, so deployments that have not published a
 * given witness keep working unchanged.
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
 * reference UTxO in `referenceInputs`, or through `attach` falling back to
 * the inline witness when nothing is published.
 */
export type WitnessScriptCarriageV1 = {
  /**
   * Empty for the inline fallback; the hash-checked published UTxO
   * otherwise. Callers MUST splice this into the transaction's reference
   * inputs BEFORE deriving any chunk/field-opening reference indices, so the
   * canonical sorted set those derivations see is the complete built set.
   */
  readonly referenceInputs: readonly UTxO[];
  /** Applies the inline attachment unless the published reference stands in. */
  readonly attach: (tx: TxBuilder) => TxBuilder;
};

const witnessScriptCarriageV1 = (
  attachInline: (tx: TxBuilder, script: Script) => TxBuilder,
  {
    script,
    referenceUtxo,
    label,
  }: {
    readonly script: Script;
    readonly referenceUtxo: UTxO | undefined;
    readonly label: string;
  },
): WitnessScriptCarriageV1 =>
  referenceUtxo === undefined
    ? {
        referenceInputs: [],
        attach: (tx) => attachInline(tx, script),
      }
    : {
        referenceInputs: [
          requireWitnessReferenceScriptUtxoV1({
            utxo: referenceUtxo,
            script,
            label,
          }),
        ],
        attach: (tx) => tx,
      };

/** Carriage for a minting-policy witness. */
export const witnessMintingPolicyCarriageV1 = (options: {
  readonly script: Script;
  readonly referenceUtxo: UTxO | undefined;
  readonly label: string;
}): WitnessScriptCarriageV1 =>
  witnessScriptCarriageV1((tx, script) => tx.attach.MintingPolicy(script), {
    ...options,
  });

/** Carriage for a withdrawal-validator witness. */
export const witnessWithdrawalValidatorCarriageV1 = (options: {
  readonly script: Script;
  readonly referenceUtxo: UTxO | undefined;
  readonly label: string;
}): WitnessScriptCarriageV1 =>
  witnessScriptCarriageV1(
    (tx, script) => tx.attach.WithdrawalValidator(script),
    { ...options },
  );

/** Carriage for a spending-validator witness. */
export const witnessSpendingValidatorCarriageV1 = (options: {
  readonly script: Script;
  readonly referenceUtxo: UTxO | undefined;
  readonly label: string;
}): WitnessScriptCarriageV1 =>
  witnessScriptCarriageV1((tx, script) => tx.attach.SpendingValidator(script), {
    ...options,
  });
