import { createHash } from "node:crypto";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { ForcedTransactionsDB } from "../../src/database/index.js";
import { deterministicFixtureOutputReferenceId } from "../utils.js";

// Two canonical empty-input forced transactions. Real Phase A classifies
// them invalid; they still require durable projection and inclusion leaves.
// F2 is projected on pass 0 beyond the fitting prefix's event window.
export const forcedProjectionEntry = (
  seconds: number,
  inclusionTime: Date,
): Effect.Effect<ForcedTransactionsDB.Entry> =>
  Effect.gen(function* () {
    const nativeTxCbor = encodeMidgardForcedTxCanonical(
      materializeMidgardForcedTxFromCanonical({
        version: 1n,
        body: {
          spendInputsPreimageCbor: EMPTY_CBOR_LIST,
          referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
          outputsPreimageCbor: EMPTY_CBOR_LIST,
          fee: 0n,
          validityIntervalStart: -1n,
          validityIntervalEnd: -1n,
          requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
          requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
          mintPreimageCbor: EMPTY_CBOR_LIST,
          scriptIntegrityHash: EMPTY_NULL_ROOT,
          auxiliaryDataHash: EMPTY_NULL_ROOT,
          networkId: 255n,
        },
        witnessSet: {
          addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
          scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
          redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        },
      }),
    );
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      verdict: "ForcedTxValid",
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    }).pipe(Effect.orDie);
    const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
    return {
      tx_order_id: deterministicFixtureOutputReferenceId(
        `step-down.f${seconds}`,
      ),
      tx_order_l1_tx_hash: Buffer.alloc(32, seconds),
      tx_order_l1_output_index: 0,
      asset_name: Buffer.alloc(32, seconds),
      raw_datum: Buffer.from("01", "hex"),
      tx_id: encoded.txId,
      tx_compact: encoded.txCompact,
      forced_inclusion_value: encoded.value,
      consensus_profile_id: MIDGARD_CONSENSUS_PROFILE.profileId,
      native_tx_cbor: nativeTxCbor,
      transaction_commitment: encoded.transactionCommitment,
      cek_program_material_sidecar_cbor: sidecar,
      cek_program_material_sidecar_sha256: createHash("sha256")
        .update(sidecar)
        .digest(),
      inclusion_time: inclusionTime,
      projected_header_hash: null,
      status: ForcedTransactionsDB.Status.Awaiting,
    };
  });
export const assertForcedProjectionRows = (
  entries: readonly Record<string, unknown>[],
) => {
  expect(entries).toHaveLength(2);
  for (const entry of entries) {
    expect(entry.status).toBe(ForcedTransactionsDB.Status.Projected);
    expect(entry.projected_header_hash).toBeNull();
    expect(
      Data.from(
        (entry.forced_inclusion_value as Buffer).toString("hex"),
        SDK.ForcedInclusionTxV1,
      ).verdict,
    ).not.toBe("ForcedTxValid");
  }
};
