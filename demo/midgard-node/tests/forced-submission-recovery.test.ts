import { createHash } from "node:crypto";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardForcedTxCanonical,
  encodeMidgardNativeTxCanonical,
  materializeMidgardForcedTxFromCanonical,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { ForcedTransactionsDB as DB } from "../src/database/index.js";
import { Database } from "../src/services/database.js";
import { provideDatabaseLayers } from "./utils.js";

const run = <A, E>(program: Effect.Effect<A, E, Database>) =>
  Effect.runPromise(Effect.scoped(provideDatabaseLayers(program)));
const submitted = materializeMidgardForcedTxFromCanonical({
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
});
const bytes = encodeMidgardForcedTxCanonical(submitted);
const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
const key = (label: number) =>
  Buffer.from(
    Data.to(
      {
        transactionId: Buffer.alloc(32, label).toString("hex"),
        outputIndex: 0n,
      },
      SDK.OutputReference,
    ),
    "hex",
  );
const rejected: SDK.OperatorVerdict = {
  ForcedTxInvalid: { reason: "EmptyInputs" },
};
const entry = async (label: number): Promise<DB.Entry> => {
  const encoded = await Effect.runPromise(
    DB.encodeForcedInclusionValueV1({
      nativeTxCbor: bytes,
      verdict: "ForcedTxValid",
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    }),
  );
  return {
    tx_order_id: key(label),
    tx_order_l1_tx_hash: Buffer.alloc(32, label),
    tx_order_l1_output_index: 0,
    asset_name: Buffer.alloc(28, label),
    raw_datum: Buffer.from("80", "hex"),
    tx_id: encoded.txId,
    tx_compact: encoded.txCompact,
    forced_inclusion_value: encoded.value,
    consensus_profile_id: MIDGARD_CONSENSUS_PROFILE.profileId,
    native_tx_cbor: bytes,
    transaction_commitment: encoded.transactionCommitment,
    cek_program_material_sidecar_cbor: sidecar,
    cek_program_material_sidecar_sha256: createHash("sha256")
      .update(sidecar)
      .digest(),
    inclusion_time: new Date(1_750_000_000_000),
    projected_header_hash: null,
    status: DB.Status.Awaiting,
  };
};
const read = async (id: Buffer) =>
  Option.getOrThrow(await run(DB.retrieveByTxOrderId(id)));
const classify = (id: Buffer) => ({
  txOrderId: id,
  verdict: rejected,
  programMaterialSidecarCbor: sidecar,
});

beforeEach(async () => {
  await run(DB.clear);
});

describe("forced submission persistence and recovery", () => {
  it("persists only the verdict and preserves submission identity during classification", async () => {
    const original = await entry(1);
    await run(DB.insertEntries([original]));
    await run(DB.setProofClassifications([classify(original.tx_order_id)]));
    const stored = await read(original.tx_order_id);
    expect(DB.operatorVerdictOfEntry(stored)).toEqual(rejected);
    expect(DB.operatorValidityOfEntry(stored)).toBe("TxIsInvalid");
    expect(stored.native_tx_cbor).toEqual(original.native_tx_cbor);
    expect(stored.tx_compact).toEqual(original.tx_compact);
    expect(stored.transaction_commitment).toEqual(
      original.transaction_commitment,
    );
    const columns = await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql<{
          column_name: string;
        }>`SELECT column_name FROM information_schema.columns WHERE table_name = ${DB.tableName}`;
      }),
    );
    expect(columns.map((column) => column.column_name)).not.toContain(
      "operator_validity",
    );
  });

  it("rolls back the entire classification batch if a later order is missing", async () => {
    const original = await entry(2);
    await run(DB.insertEntries([original]));
    await expect(
      run(
        DB.setProofClassifications([
          classify(original.tx_order_id),
          classify(key(99)),
        ]),
      ),
    ).rejects.toThrow();
    expect((await read(original.tx_order_id)).forced_inclusion_value).toEqual(
      original.forced_inclusion_value,
    );
  });

  it("reopens the same verdict and submission from a new connection and canonical journal", async () => {
    const original = await entry(3);
    await run(DB.insertEntries([original]));
    await run(DB.setProofClassifications([classify(original.tx_order_id)]));
    const stored = await read(original.tx_order_id);
    const journal = DB.encodeForcedTransactionJournalMember({
      sourceValueCbor: stored.forced_inclusion_value,
      canonicalTransactionCbor: stored.native_tx_cbor,
      programMaterialSidecarCbor: stored.cek_program_material_sidecar_cbor,
    });
    const reopened = DB.decodeForcedTransactionJournalMember(journal);
    expect(reopened.canonicalTransactionCbor).toEqual(bytes);
    expect(
      Data.from(
        reopened.sourceValueCbor.toString("hex"),
        SDK.ForcedInclusionTxV1,
      ).verdict,
    ).toEqual(rejected);
    expect((await read(original.tx_order_id)).forced_inclusion_value).toEqual(
      reopened.sourceValueCbor,
    );
  });

  it("reopens a corrected projected order without rewriting its submission or verdict", async () => {
    const original = await entry(4);
    const header = Buffer.alloc(28, 0x44);
    await run(DB.insertEntries([original]));
    await run(DB.setProofClassifications([classify(original.tx_order_id)]));
    await run(DB.markAwaitingAsProjected([original.tx_order_id]));
    await run(DB.markProjectedByEventIds([original.tx_order_id], header));
    await run(
      DB.reopenAfterStateQueueCorrectionByEventIds(
        [original.tx_order_id],
        header,
      ),
    );
    const reopened = await read(original.tx_order_id);
    expect(reopened.status).toBe(DB.Status.Projected);
    expect(reopened.projected_header_hash).toBeNull();
    expect(reopened.native_tx_cbor).toEqual(bytes);
    expect(reopened.transaction_commitment).toEqual(
      original.transaction_commitment,
    );
    expect(DB.operatorVerdictOfEntry(reopened)).toEqual(rejected);
  });

  it("refuses the old source encoding and old profile identity instead of inferring a migration", async () => {
    const oldBytes = encodeMidgardNativeTxCanonical(
      materializeMidgardNativeTxFromCanonical({
        ...submitted,
        validity: "TxIsValid",
      }),
    );
    await expect(
      Effect.runPromise(
        DB.encodeForcedInclusionValueV1({
          nativeTxCbor: oldBytes,
          verdict: "ForcedTxValid",
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
    ).rejects.toThrow();
    const { forcedTransactionSourceEncoding: removedEncoding, ...oldProfile } =
      MIDGARD_CONSENSUS_PROFILE;
    expect(removedEncoding).toBe("midgard-forced-submission-v1");
    expect(isMidgardConsensusProfile(oldProfile)).toBe(false);
    expect(await run(DB.retrieveAllEntries())).toEqual([]);
  });
});
