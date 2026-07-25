import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardCekTermNodeV1,
  hashMidgardCekProgramMaterialPreimageV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "@/database/index.js";
import { publishedProgramMaterialEntries } from "@/fibers/fetch-and-insert-tx-order-utxos.js";
import { buildDeterministicValidationTraceMembers } from "@/workers/utils/mpf.js";

const canonicalTransaction = (
  version: bigint = MIDGARD_NATIVE_TX_V1_VERSION,
): MidgardNativeTxCanonicalV1 => ({
  version,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: EMPTY_CBOR_LIST,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  },
});

const encodedTransaction = (version?: bigint): Buffer =>
  encodeMidgardNativeTxCanonicalV1(
    materializeMidgardNativeTxFromCanonicalV1(canonicalTransaction(version)),
  );

describe("V1 forced transaction material", () => {
  it("accepts only exact self-authenticating material from the immutable L1 address", () => {
    const preimage = encodeMidgardCekTermNodeV1({ kind: "error" });
    const root = hashMidgardCekProgramMaterialPreimageV1("term", preimage);
    const [publication] = SDK.deriveCekProgramMaterialPublicationsV1([
      { kind: "term", root, preimage },
    ]);
    const utxo = (datum: string | undefined, outputIndex: number): UTxO =>
      ({
        txHash: "12".repeat(32),
        outputIndex,
        address: "addr_test1wmaterial",
        assets: { lovelace: 2_000_000n },
        ...(datum === undefined ? {} : { datum }),
      }) as UTxO;
    const decoded = publishedProgramMaterialEntries([
      utxo(publication!.datumCbor, 0),
      utxo(
        Data.to(
          { ...publication!.datum, root: "ff".repeat(32) },
          SDK.CekProgramMaterialDatumV1,
        ),
        1,
      ),
      utxo(undefined, 2),
    ]);

    expect(decoded.entries).toEqual([publication!.entry]);
    expect(decoded.malformedCount).toBe(2);
  });

  it("binds the exact canonical transaction independently of its derived verdict", async () => {
    const nativeTxCbor = encodedTransaction();
    const accepted = await Effect.runPromise(
      ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor,
        operatorValidity: "TxIsValid",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      }),
    );
    const rejected = await Effect.runPromise(
      ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor,
        operatorValidity: "FeeTooLow",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      }),
    );
    const decoded = Data.from(
      accepted.value.toString("hex"),
      SDK.ForcedInclusionTxV1,
    ) as SDK.ForcedInclusionTxV1;

    expect(accepted.txId).toEqual(rejected.txId);
    expect(accepted.transactionCommitment).toEqual(
      rejected.transactionCommitment,
    );
    expect(accepted.txCompact).toEqual(rejected.txCompact);
    expect(accepted.value).not.toEqual(rejected.value);
    expect(decoded).toEqual({
      tx_id: accepted.txId.toString("hex"),
      transaction_commitment: accepted.transactionCommitment.toString("hex"),
      source: {
        compact_cbor: accepted.source.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          accepted.source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          accepted.source.fieldPreimageLengthsCbor.toString("hex"),
      },
      operator_validity: "TxIsValid",
    });
    const journalMember =
      ForcedTransactionsDB.encodeForcedTransactionJournalMemberV1({
        sourceValueCbor: accepted.value,
        canonicalTransactionCbor: nativeTxCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1(
          [],
        ),
      });
    expect(
      ForcedTransactionsDB.decodeForcedTransactionJournalMemberV1(
        journalMember,
      ),
    ).toEqual({
      sourceValueCbor: accepted.value,
      canonicalTransactionCbor: nativeTxCbor,
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([]),
    });
  });

  it("builds a deterministic forced rejection descriptor from the same Phase A/B replay", async () => {
    const nativeTxCbor = encodedTransaction();
    const txId = computeMidgardNativeTxIdV1(
      materializeMidgardNativeTxFromCanonicalV1(canonicalTransaction()),
    );
    const eventKey: SDK.EventKey = {
      ForcedTransactionEventKey: {
        tx_order_id: {
          transactionId: "44".repeat(32),
          outputIndex: 0n,
        },
      },
    };
    const members = await Effect.runPromise(
      buildDeterministicValidationTraceMembers({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
        blockEndTime: new Date("2026-07-23T12:00:00.000Z"),
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactions: [
          {
            eventKey,
            transactionId: txId,
            canonicalTransactionCbor: nativeTxCbor,
            programMaterialSidecarCbor:
              encodeMidgardCekProgramMaterialSidecarV1([]),
            sourceKind: "forced",
            priorUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            postUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            ledgerOps: [],
            ledgerWitnessEntries: [],
            ledgerMutationSteps: [],
            verdict: "rejected",
            rejectionCode: RejectCodes.EmptyInputs,
          },
        ],
      }),
    );

    expect(members).toHaveLength(1);
    expect(members[0]?.value).toMatchObject({
      schema_version: 1n,
      machine_version: 1n,
      verdict: "Rejected",
    });
    expect(
      Data.from(
        members[0]!.valueCbor.toString("hex"),
        SDK.ValidationTraceDescriptorV1,
      ),
    ).toEqual(members[0]!.value);
  });
});
