import {
  encodeMidgardCekProgramMaterialSidecar,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  decodeMidgardTxOutput,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  RejectCodes,
} from "@al-ft/midgard-validation";
import {
  encodeByteList,
  encodeRecomputedNativeTx,
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  outRefFromByte,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { computeDaPayloadRoots } from "da-committee-node/da/payload";
import { Effect } from "effect";
import { expect } from "vitest";

import { makePayloadFixture } from "../../da-committee-node/tests/helpers.js";
import * as ForcedTransactionsDB from "../src/database/forcedTransactions.js";
import { classifyForcedTransactions } from "../src/mpf/event-window.classify-forced-transactions.js";
import { buildDeterministicValidationTraceMembers } from "../src/mpf/validation-trace.js";
import { sha256 } from "../src/sha256.js";
// Capacity fixtures require funded, signed transactions with real ledger proofs;
// the neighbouring admission suite exercises small rejected EmptyInputs events.
export const fixture = async (
  signatureCount: number,
  outputBytes: number,
  observerCount = 0,
) => {
  const keys = Array.from({ length: signatureCount }, (_, index) =>
    CML.PrivateKey.from_normal_bytes(
      Buffer.concat([
        Buffer.alloc(28),
        Buffer.from([
          (index + 1) >>> 24,
          (index + 1) >>> 16,
          (index + 1) >>> 8,
          index + 1,
        ]),
      ]),
    ),
  );
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(keys[0]!.to_public().hash()),
  ).to_address();
  // Accepted capacity fixtures obey the declared per-output bound while
  // exercising a whole output field larger than one publication.
  const outputSizes =
    outputBytes > MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes
      ? [10_000, outputBytes - 10_000]
      : [outputBytes];
  const outputs = outputSizes.map((size) =>
    encodeMidgardTxOutput({
      ...decodeMidgardTxOutput(makeMinAdaFundedExactSizeOutputItem(size)),
      address: Buffer.from(address.to_raw_bytes()),
    }),
  );
  const funding = outputs.reduce(
    (total, item) => total + decodeMidgardTxOutput(item).value.lovelace,
    0n,
  );
  const output = encodeMidgardTxOutput({
    ...decodeMidgardTxOutput(makeMinAdaFundedExactSizeOutputItem(100)),
    address: Buffer.from(address.to_raw_bytes()),
    value: { lovelace: funding, assets: new Map() },
  });
  const spent = outRefFromByte(0x11);
  const original = makeNativeTx({
    privateKey: keys[0],
    version: 1n,
    spendInputs: [spent],
    outputs,
    networkId: 0n,
    requiredObserverItems: Array.from({ length: observerCount }, (_, index) => {
      const hash = Buffer.alloc(28);
      hash.writeUInt32BE(index === observerCount - 1 ? index : index + 1, 24);
      return hash;
    }),
  });
  // Signature-sort worker's genuine descending physical order fixture; seeds
  // retain distinct keys beyond index 255, and every signature signs this tx id.
  const physical = keys
    .map((key) => ({
      hash: Buffer.from(key.to_public().hash().to_raw_bytes()),
      bytes: Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(original.txId),
          key,
        ).to_cbor_bytes(),
      ),
    }))
    .sort((left, right) => Buffer.compare(right.hash, left.hash));
  const transaction = encodeRecomputedNativeTx({
    ...original.tx,
    witnessSet: {
      ...original.tx.witnessSet,
      addrTxWitsPreimageCbor: encodeByteList(
        physical.map((item) => item.bytes),
      ),
    },
  });
  const forcedBytes = encodeMidgardForcedTxCanonical(transaction.tx);
  const source = deriveMidgardForcedTxProofSource(transaction.tx);
  let ledgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((outputCbor, index) =>
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor,
      }),
    ),
  ];
  let ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output }],
    operations: ledgerOps,
  });
  const priorUtxosRoot = ledgerMutationSteps[0]!.preRoot.toString("hex");
  let postUtxosRoot = ledgerMutationSteps.at(-1)!.postRoot.toString("hex");
  let rejectionCode: typeof RejectCodes.InvalidFieldType | null = null;
  let verdict: SDK.OperatorVerdict = "ForcedTxValid";
  let classifiedEntry: ForcedTransactionsDB.Entry | undefined;
  if (observerCount > 0) {
    const encoded = await Effect.runPromise(
      ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor: forcedBytes,
        verdict,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }),
    );
    const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
    const entry: ForcedTransactionsDB.Entry = {
      tx_order_id: Buffer.from(
        Data.to(
          { transactionId: "5a".repeat(32), outputIndex: 0n },
          SDK.OutputReference,
        ),
        "hex",
      ),
      tx_order_l1_tx_hash: Buffer.alloc(32, 0x5a),
      tx_order_l1_output_index: 0,
      asset_name: Buffer.from([0x5a]),
      raw_datum: Buffer.from([0x5a]),
      tx_id: encoded.txId,
      tx_compact: encoded.txCompact,
      forced_inclusion_value: encoded.value,
      consensus_profile_id: MIDGARD_CONSENSUS_PROFILE.profileId,
      native_tx_cbor: forcedBytes,
      transaction_commitment: encoded.transactionCommitment,
      cek_program_material_sidecar_cbor: sidecar,
      cek_program_material_sidecar_sha256: sha256(sidecar),
      inclusion_time: new Date(0),
      projected_header_hash: null,
      status: ForcedTransactionsDB.Status.Awaiting,
    };
    const [classified] = await Effect.runPromise(
      classifyForcedTransactions({
        entries: [entry],
        initialState: new Map([[spent.toString("hex"), output]]),
        effectiveEndTime: new Date(1_750_000_000_000),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation: {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          slotForUnixTime: () => 100n,
        },
        resolveProgramMaterialSidecar: () => Effect.succeed(sidecar),
      }),
    );
    if (classified === undefined)
      throw new Error("node classified no forced event");
    expect(classified.rejectionCode).toBe(RejectCodes.InvalidFieldType);
    classifiedEntry = classified.entry;
    verdict = ForcedTransactionsDB.operatorVerdictOfEntry(classified.entry);
    expect(verdict).toEqual({
      ForcedTxInvalid: {
        reason: {
          ObserverOrderInvalid: { observer_index: BigInt(observerCount - 1) },
        },
      },
    });
    rejectionCode = RejectCodes.InvalidFieldType;
    ledgerOps = [];
    ledgerMutationSteps = [];
    postUtxosRoot = priorUtxosRoot;
  }
  const base = await makePayloadFixture(1, { prevUtxosRoot: priorUtxosRoot });
  base.header.endTime = 1_750_000_000_000n;
  base.header.blockSlot = 100n;
  const txOrderId: SDK.OutputReference = {
    transactionId: "5a".repeat(32),
    outputIndex: 0n,
  };
  const orderKey = Data.to(txOrderId, SDK.OutputReference);
  const eventKey: SDK.EventKey = {
    ForcedTransactionEventKey: { tx_order_id: txOrderId },
  };
  const eventCbor = Data.to(eventKey, SDK.EventKey);
  const [member] = await Effect.runPromise(
    buildDeterministicValidationTraceMembers({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      blockEndTime: new Date(Number(base.header.endTime)),
      expectedNetworkId: base.header.expectedNetworkId,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: base.header.blockSlot,
      transactions: [
        {
          eventKey,
          transactionId: transaction.txId,
          canonicalTransactionCbor: forcedBytes,
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
            [],
          ),
          sourceKind: "forced",
          priorUtxosRoot,
          postUtxosRoot,
          ledgerOps,
          ledgerWitnessEntries: [{ outRef: spent, output }],
          ledgerMutationSteps,
          verdict: observerCount > 0 ? "rejected" : "accepted",
          rejectionCode,
        },
      ],
    }),
  );
  if (member === undefined) throw new Error("node exported no trace");
  const counts = {
    ...base.payload.block_body.counts,
    l2TransactionCount: 0n,
    forcedTransactionCount: 1n,
  };
  const leaf: SDK.ForcedInclusionTxV1 = {
    tx_id: transaction.txId.toString("hex"),
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict,
  };
  const unhashed: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      counts,
      utxos:
        observerCount > 0
          ? [[spent.toString("hex"), output.toString("hex")]]
          : outputs.map((item, index) => [
              outRefFromTxId(transaction.txId, BigInt(index)).toString("hex"),
              item.toString("hex"),
            ]),
      transactions: [],
      transaction_preimages: [],
      forced_transactions: [[orderKey, Data.to(leaf, SDK.ForcedInclusionTxV1)]],
      forced_transaction_preimages: [[orderKey, forcedBytes.toString("hex")]],
      transition_trace: base.payload.block_body.transition_trace.map(
        ([key, value]) => [
          key,
          Data.to(
            {
              ...Data.from(value, SDK.TransitionStep),
              event_key: eventKey,
              phase: "ForcedTransaction",
              pre_utxos_root: priorUtxosRoot,
              post_utxos_root: postUtxosRoot,
            },
            SDK.TransitionStep,
          ),
        ],
      ),
      event_to_step: base.payload.block_body.event_to_step.map(([, value]) => [
        eventCbor,
        Data.to(
          {
            ...Data.from(value, SDK.EventToStepValue),
            phase: "ForcedTransaction",
          },
          SDK.EventToStepValue,
        ),
      ]),
      validation_traces: [
        [member.keyCbor.toString("hex"), member.valueCbor.toString("hex")],
      ],
      validation_trace_witnesses: [...member.witnesses].sort(([a], [b]) =>
        a < b ? -1 : a > b ? 1 : 0,
      ),
    },
  };
  const roots = await computeDaPayloadRoots(unhashed);
  const header = { ...unhashed.block_body.header, ...counts, ...roots };
  const headerHash = Effect.runSync(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...unhashed,
    block_body: { ...unhashed.block_body, header, header_hash: headerHash },
  };
  return {
    payload,
    header,
    headerHash,
    member,
    forcedBytes,
    classifiedEntry,
    spent,
    output,
    address: address.to_bech32(),
  };
};
