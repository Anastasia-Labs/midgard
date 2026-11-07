import {
  CML,
  coreToTxOutput,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import * as Availability from "./availability-challenge.js";
import { fail } from "./availability-challenge-transactions.at.js";
import {
  cborTransaction,
  plutusDataHash,
  type RecoverDaAvailabilityCommitmentParams,
  type RecoveredDaAvailabilityCommitment,
} from "./availability-challenge-transactions.build-timeout-da-availability-challenge-tx-program.js";
import {
  DA_ATTESTATION_ASSET_NAME_PREFIX,
  DaAttestationDatum,
} from "./da-attestation.js";

/**
 * Recovers the full attested commitment an Open needs from the Apply
 * transaction that set the node's `Attested{commitment_hash}`: finds the
 * input holding the DAAT token that Apply burns, reads that output from its
 * producing transaction, and parses `DaAttestationDatum.availability_commitment`.
 * A hashed datum falls back to the Apply transaction's witness datums, then
 * to the provider's datum lookup.
 *
 * The spent DAAT UTxO is gone from every UTxO view once Apply lands, so the
 * producing transaction is the primary source. In the Lucid emulator (which
 * deletes spent UTxOs at each block and keeps no transaction bodies) only this
 * path works, and only when `fetchTransactionCbor` serves the CBOR the harness
 * submitted; its datum table holds hashed datums alone, so the provider
 * fallback never sees an inline DAAT datum.
 */
export const recoverDaAvailabilityCommitmentFromApplyTx = async (
  lucid: Pick<LucidEvolution, "config">,
  params: RecoverDaAvailabilityCommitmentParams,
): Promise<RecoveredDaAvailabilityCommitment> => {
  const apply =
    (await cborTransaction(params.fetchTransactionCbor, params.applyTxHash)) ??
    fail(
      `Apply transaction ${params.applyTxHash} is unknown`,
      "apply-commitment-unrecoverable",
    );
  try {
    const body = apply.body();
    const burned: string[] = [];
    const policyTokens = body
      .mint()
      ?.get_assets(CML.ScriptHash.from_hex(params.daAttestationPolicyId));
    const names = policyTokens?.keys();
    for (let i = 0; i < (names?.len() ?? 0); i++) {
      const name = names!.get(i);
      const hex = Buffer.from(name.to_raw_bytes()).toString("hex");
      if (
        hex.startsWith(DA_ATTESTATION_ASSET_NAME_PREFIX) &&
        policyTokens!.get(name) === -1n
      )
        burned.push(hex);
    }
    if (burned.length !== 1)
      fail(
        "Apply transaction must burn exactly one DAAT token",
        "apply-commitment-unrecoverable",
      );
    const unit = params.daAttestationPolicyId + burned[0]!;
    const headerHash = burned[0]!.slice(
      DA_ATTESTATION_ASSET_NAME_PREFIX.length,
    );
    const inputs = body.inputs();
    for (let i = 0; i < inputs.len(); i++) {
      const input = inputs.get(i);
      const outRef = {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
      const producer = await cborTransaction(
        params.fetchTransactionCbor,
        outRef.txHash,
      );
      if (producer === undefined) continue;
      let output: ReturnType<typeof coreToTxOutput> | undefined;
      try {
        const outputs = producer.body().outputs();
        if (outRef.outputIndex < outputs.len())
          output = coreToTxOutput(outputs.get(outRef.outputIndex));
      } finally {
        producer.free();
      }
      if (output === undefined || output.assets[unit] !== 1n) continue;
      let datumCbor = output.datum ?? undefined;
      let source: RecoveredDaAvailabilityCommitment["source"] = "inline-datum";
      if (datumCbor == null && output.datumHash != null) {
        const witnessDatums = apply.witness_set().plutus_datums();
        for (let j = 0; j < (witnessDatums?.len() ?? 0); j++) {
          const candidate = witnessDatums!.get(j);
          if (CML.hash_plutus_data(candidate).to_hex() === output.datumHash) {
            datumCbor = candidate.to_cbor_hex();
            source = "witness-datum";
          }
        }
      }
      if (datumCbor == null && output.datumHash != null) {
        const provider = lucid.config().provider;
        const resolved = provider
          ? await provider.getDatum(output.datumHash).catch(() => undefined)
          : undefined;
        if (
          resolved !== undefined &&
          plutusDataHash(resolved) === output.datumHash
        ) {
          datumCbor = resolved;
          source = "provider-datum";
        }
      }
      if (datumCbor == null)
        return fail(
          "Spent DAAT output carries no resolvable datum",
          "apply-commitment-unrecoverable",
        );
      const attestation = Data.from(datumCbor, DaAttestationDatum);
      const commitment = attestation.availability_commitment;
      Availability.assertCanonicalDaAvailabilityCommitment(commitment);
      if (
        attestation.header_hash !== headerHash ||
        commitment.header_hash !== headerHash
      )
        fail(
          "DAAT datum does not name the burned token's block",
          "apply-commitment-unrecoverable",
        );
      const commitmentHash =
        Availability.daAvailabilityCommitmentHash(commitment);
      if (
        params.expectedCommitmentHash !== undefined &&
        commitmentHash !== params.expectedCommitmentHash
      )
        fail(
          `Recovered commitment hash ${commitmentHash} does not match ${params.expectedCommitmentHash}`,
          "commitment-hash-mismatch",
        );
      return {
        commitment,
        commitmentHash,
        headerHash,
        attestationOutRef: outRef,
        source,
      };
    }
    return fail(
      "No producing transaction of the Apply inputs yields the DAAT output",
      "apply-commitment-unrecoverable",
    );
  } finally {
    apply.free();
  }
};

export type DaAvailabilitySnapshotUtxos = {
  readonly availabilityUtxos: readonly UTxO[];
  readonly stateQueueUtxos: readonly UTxO[];
  readonly correctionLockUtxos: readonly UTxO[];
  /** UTxOs at the DA bond pool address; the pool is omitted when absent. */
  readonly poolUtxos?: readonly UTxO[];
  readonly carrierUtxos?: readonly UTxO[];
};
