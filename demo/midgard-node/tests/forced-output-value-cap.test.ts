/**
 * A forced transaction whose output Value is one byte over the 5,000-byte
 * Cardano cap is rejected with `OutputNonCanonical` at that output, the
 * existing arm the output-canonicality family proves, and the block that
 * carries the rejection is built and attested. Its 5,000-byte twin is valid
 * and applied in the same block. The tx-order mint refuses such an order, so
 * an honest chain never carries one; the node still records a provable
 * verdict for it instead of stopping.
 *
 * Each transaction spends two held inputs that together carry the output's
 * exact value, so the cap is the only rule either one can fail.
 */
import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  encodeMidgardTxOutput,
  midgardValueToCmlValue,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core/consensus-validation";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { hashMidgardValidationRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import {
  MIDGARD_COINS_PER_UTXO_BYTE,
  minAdaLovelace,
  RejectCodes,
  replayValidationVerdictEvent,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { decodeDaPayloadStrict } from "da-committee-node/da/payload";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  LONG_ASSET_NAME_LOVELACE,
  type LongAssetNameTail,
  longAssetNameValue,
} from "../../midgard-fault-proofs/tests/support/long-asset-name-value.js";
import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  buildPlaceholderValidationTraceMembers,
  classifyForcedTransactions,
  computeUtxoPayloadRoot,
} from "../src/mpf/index.js";
import {
  forcedEntry,
  makeOutput,
  makeSignedEffectfulTransaction,
  outputReferenceFromHash,
  TEST_ADDRESS,
} from "./forced-transactions.make-signed-effectful-transaction.js";

const time = new Date("2026-07-23T12:01:00.000Z");
const validation = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  bucketConcurrency: 1,
  slotForUnixTime: () => 100n,
};

const CHANGE_LOVELACE = 2_000_000n;
const TAIL_HELD_LOVELACE = 4_000_000n;
const NAMES_HELD_LOVELACE =
  LONG_ASSET_NAME_LOVELACE + CHANGE_LOVELACE - TAIL_HELD_LOVELACE;

/**
 * A transaction paying a small output, then the long-name output at index 1,
 * from two held inputs: the 141 long names and the tail asset.
 */
const capTransaction = (tailQuantity: LongAssetNameTail, inputByte: number) => {
  const namesInput = outputReferenceFromHash(Buffer.alloc(32, inputByte), 0n);
  const tailInput = outputReferenceFromHash(Buffer.alloc(32, inputByte), 1n);
  const held = (lovelace: bigint, part: "names" | "tail") =>
    encodeMidgardTxOutput({
      address: TEST_ADDRESS,
      value: longAssetNameValue({ lovelace, tailQuantity, part }),
    });
  const longOutput = {
    address: TEST_ADDRESS,
    value: longAssetNameValue({
      lovelace: LONG_ASSET_NAME_LOVELACE,
      tailQuantity,
    }),
  };
  const transaction = makeSignedEffectfulTransaction(
    namesInput,
    makeOutput(CHANGE_LOVELACE),
    {
      additionalSpendInputs: [tailInput],
      additionalOutputs: [encodeMidgardTxOutput(longOutput)],
    },
  );
  return {
    transaction,
    longOutput,
    state: [
      [namesInput.toString("hex"), held(NAMES_HELD_LOVELACE, "names")],
      [tailInput.toString("hex"), held(TAIL_HELD_LOVELACE, "tail")],
    ] as const,
  };
};

const overCap = capTransaction(256n, 0x41);
const atCap = capTransaction(24n, 0x42);

/** The DA payload of a block whose only event is `entry`'s forced leaf. */
const singleForcedBlockPayload = async ({
  canonicalCbor,
  entry,
  rejectionReason,
  ledgerMutationSteps,
  priorRoot,
}: {
  readonly canonicalCbor: Buffer;
  readonly entry: ForcedTransactionsDB.Entry;
  readonly rejectionReason: SDK.RejectionReason | null;
  readonly ledgerMutationSteps: readonly { readonly postRoot: Buffer }[];
  readonly priorRoot: string;
}): Promise<SDK.DaPayload> => {
  const orderKey =
    entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex");
  const eventKey: SDK.EventKey = {
    ForcedTransactionEventKey: {
      tx_order_id: Data.from(orderKey, SDK.OutputReference),
    },
  };
  const postRoot =
    ledgerMutationSteps.at(-1)?.postRoot.toString("hex") ?? priorRoot;
  const [member] = await Effect.runPromise(
    buildPlaceholderValidationTraceMembers({
      transactions: [
        {
          eventKey,
          canonicalTransactionCbor: canonicalCbor,
          sourceKind: "forced",
          verdict: rejectionReason === null ? "accepted" : "rejected",
          rejectionReason,
        },
      ],
    }),
  );
  expect(member!.value.rejection_code_hash).toBe(
    rejectionReason === null
      ? "00".repeat(32)
      : hashMidgardValidationRejectionReasonV1(rejectionReason).toString("hex"),
  );
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  const header: SDK.Header = {
    blockSlot: 100n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: "00".repeat(28),
    operatorVkey: "00".repeat(28),
    startTime: BigInt(time.getTime() - 1),
    endTime: BigInt(time.getTime()),
    prevUtxosRoot: priorRoot,
    utxosRoot: postRoot,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    protocolVersion: 1n,
    ...counts,
  };
  return {
    version: 1n,
    block_body: {
      header,
      header_hash: await Effect.runPromise(SDK.hashBlockHeader(header)),
      counts,
      transactions: [],
      transaction_preimages: [],
      forced_transactions: [
        [
          orderKey,
          entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE].toString(
            "hex",
          ),
        ],
      ],
      forced_transaction_preimages: [[orderKey, canonicalCbor.toString("hex")]],
      deposits: [],
      withdrawals: [],
      transition_trace: [
        [
          Data.to(0n),
          Data.to(
            {
              schema_version: 1n,
              step_index: 0n,
              event_key: eventKey,
              phase: "ForcedTransaction",
              pre_utxos_root: priorRoot,
              post_utxos_root: postRoot,
            },
            SDK.TransitionStep,
          ),
        ],
      ],
      event_to_step: [
        [
          Data.to(eventKey, SDK.EventKey),
          Data.to(
            { step_index: 0n, phase: "ForcedTransaction" },
            SDK.EventToStepValue,
          ),
        ],
      ],
      validation_traces: [
        [member!.keyCbor.toString("hex"), member!.valueCbor.toString("hex")],
      ],
      validation_trace_witnesses: [],
    },
  };
};

describe("forced transaction output Value at the 5,000-byte cap", () => {
  it("measures the twins at 5,001 and 5,000 bytes, each above min-Ada, and the mint admits only the 5,000-byte one", () => {
    expect(
      midgardValueToCmlValue(overCap.longOutput.value).to_cbor_bytes(),
    ).toHaveLength(5_001);
    expect(
      midgardValueToCmlValue(atCap.longOutput.value).to_cbor_bytes(),
    ).toHaveLength(5_000);
    for (const { longOutput } of [overCap, atCap])
      expect(longOutput.value.lovelace).toBeGreaterThanOrEqual(
        minAdaLovelace(
          MIDGARD_COINS_PER_UTXO_BYTE,
          BigInt(encodeMidgardTxOutput(longOutput).length),
        ),
      );
    const admission = ({ transaction }: typeof overCap) =>
      screenMidgardForcedTxAdmission(
        deriveMidgardForcedTxFaultEvidenceMaterial(transaction.canonicalCbor)
          .canonical,
      );
    expect(admission(overCap)?.check).toBe("outputs_are_admissible(outputs)");
    expect(admission(atCap)).toBeNull();
  });

  it("rejects the 5,001-byte output with OutputNonCanonical at its index and applies the 5,000-byte twin after it in the same block", async () => {
    const initialState = new Map<string, Buffer>([
      ...overCap.state,
      ...atCap.state,
    ]);
    const classified = await Effect.runPromise(
      classifyForcedTransactions({
        entries: [
          await forcedEntry({ label: 31, transaction: overCap.transaction }),
          await forcedEntry({ label: 32, transaction: atCap.transaction }),
        ],
        initialState,
        blockTime: time,
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation,
      }),
    );
    expect(classified).toHaveLength(2);
    const [rejected, accepted] = classified;
    const verdictOf = (result: (typeof classified)[number]) =>
      Data.from(
        result.entry[
          ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
        ].toString("hex"),
        SDK.ForcedInclusionTxV1,
      ).verdict;
    expect(verdictOf(rejected!)).toEqual({
      ForcedTxInvalid: { reason: { OutputNonCanonical: { output_index: 1n } } },
    });
    expect(rejected!.rejectionCode).toBe(RejectCodes.InvalidOutput);
    expect(rejected!.ledgerOps).toEqual([]);
    expect(verdictOf(accepted!)).toBe("ForcedTxValid");
    expect(accepted!.rejectionCode).toBeNull();
    expect(accepted!.ledgerOps.length).toBeGreaterThan(0);
  });

  it.each([
    ["5,001-byte", overCap, RejectCodes.InvalidOutput],
    ["5,000-byte", atCap, null],
  ] as const)(
    "the shared verdict replay agrees on the %s output and the DA committee attests its block",
    async (_label, shape, expectedCode) => {
      const initialState = new Map<string, Buffer>(shape.state);
      const [result] = await Effect.runPromise(
        classifyForcedTransactions({
          entries: [
            await forcedEntry({ label: 33, transaction: shape.transaction }),
          ],
          initialState,
          blockTime: time,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          validation,
        }),
      );
      expect(result!.rejectionCode).toBe(expectedCode);
      const replay = await Effect.runPromise(
        replayValidationVerdictEvent({
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          canonicalTransactionCbor: shape.transaction.canonicalCbor,
          sourceKind: "forced",
          ledgerWitnessEntries: result!.ledgerWitnessEntries,
          blockTimeMs: time.getTime(),
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 100n,
        }),
      );
      expect(replay.verdict).toBe(
        expectedCode === null ? "accepted" : "rejected",
      );
      expect(replay.rejection?.code ?? null).toBe(expectedCode);

      const priorRoot = await Effect.runPromise(
        computeUtxoPayloadRoot(
          [...initialState.entries()].map(([key, value]) => ({
            outref: Buffer.from(key, "hex"),
            output: value,
          })),
        ),
      );
      const payload = await singleForcedBlockPayload({
        canonicalCbor: shape.transaction.canonicalCbor,
        entry: result!.entry,
        rejectionReason: result!.rejectionReason,
        ledgerMutationSteps: result!.ledgerMutationSteps,
        priorRoot,
      });
      const decoded = decodeDaPayloadStrict(SDK.encodeDaPayload(payload));
      expect(decoded.block_body.forced_transactions).toEqual(
        payload.block_body.forced_transactions,
      );
    },
  );
});
