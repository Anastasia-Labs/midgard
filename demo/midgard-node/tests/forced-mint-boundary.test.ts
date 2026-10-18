import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  MIDGARD_CONSENSUS_PROFILE,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { hashMidgardValidationRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import {
  RejectCodes,
  replayValidationVerdictEvent,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { decodeDaPayloadStrict } from "da-committee-node/da/payload";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  buildPlaceholderValidationTraceMembers,
  classifyForcedTransactions,
} from "../src/mpf/index.js";
import {
  MINT_ADMITTED_UNDECODABLE,
  MINT_REFUSED,
} from "./forced-mint-boundary.fixtures.js";
import {
  forcedEntry,
  type makeSignedEffectfulTransaction,
} from "./forced-transactions.make-signed-effectful-transaction.js";

const time = new Date("2026-07-23T12:01:00.000Z");

/**
 * The tx-order mint is the forced admission boundary. A transaction it
 * refuses never becomes an order; a transaction it admits always gets a
 * forced leaf, even when its items do not decode, and the DA committee
 * attests the block that carries that leaf.
 */
describe("forced admission at the tx-order mint", () => {
  it.each(MINT_REFUSED)(
    "the mint refuses a forced transaction with %s",
    (_name, check, bytes) => {
      const tx = bytes();
      expect(
        screenMidgardForcedTxAdmission(
          deriveMidgardForcedTxFaultEvidenceMaterial(tx).canonical,
        )?.check,
      ).toBe(check);
      expect(() =>
        SDK.deriveTxOrderMaterial({
          submittedTxCbor: tx,
          owner: Buffer.alloc(28),
        }),
      ).toThrow();
    },
  );

  it.each(MINT_ADMITTED_UNDECODABLE)(
    "an admitted order with %s is rejected at its first fault and attested",
    async (_name, reason, bytes) => {
      const tx = bytes();
      expect(
        screenMidgardForcedTxAdmission(
          deriveMidgardForcedTxFaultEvidenceMaterial(tx).canonical,
        ),
      ).toBeNull();
      expect(
        SDK.deriveTxOrderMaterial({
          submittedTxCbor: tx,
          owner: Buffer.alloc(28),
        }).transactionId,
      ).toBe(
        deriveMidgardForcedTxFaultEvidenceMaterial(tx).transactionId.toString(
          "hex",
        ),
      );

      const [classified] = await Effect.runPromise(
        classifyForcedTransactions({
          entries: [
            await forcedEntry({
              label: 21,
              transaction: { canonicalCbor: tx } as ReturnType<
                typeof makeSignedEffectfulTransaction
              >,
            }),
          ],
          initialState: new Map(),
          blockTime: time,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          validation: {
            l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
            expectedNetworkId: 0n,
            minFeeA: 0n,
            minFeeB: 0n,
            bucketConcurrency: 1,
            slotForUnixTime: () => 100n,
          },
        }),
      );
      const result = classified!;
      const leaf = Data.from(
        result.entry[
          ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
        ].toString("hex"),
        SDK.ForcedInclusionTxV1,
      );
      expect(leaf.verdict).toEqual({ ForcedTxInvalid: { reason } });
      const code =
        typeof reason === "object" && "InputNotFound" in reason
          ? RejectCodes.InputNotFound
          : RejectCodes.InvalidFieldType;
      expect(result.rejectionCode).toBe(code);
      expect(result.ledgerOps).toEqual([]);

      // The shared verdict replay (watcher and foreign import) agrees.
      const replay = await Effect.runPromise(
        replayValidationVerdictEvent({
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          canonicalTransactionCbor: tx,
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
            [],
          ),
          sourceKind: "forced",
          ledgerWitnessEntries: result.ledgerWitnessEntries,
          blockTimeMs: time.getTime(),
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 100n,
        }),
      );
      expect(replay.verdict).toBe("rejected");
      expect(replay.rejection?.code).toBe(code);

      const orderKey =
        result.entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex");
      const eventKey: SDK.EventKey = {
        ForcedTransactionEventKey: {
          tx_order_id: Data.from(orderKey, SDK.OutputReference),
        },
      };
      const [member] = await Effect.runPromise(
        buildPlaceholderValidationTraceMembers({
          transactions: [
            {
              eventKey,
              canonicalTransactionCbor: tx,
              sourceKind: "forced",
              verdict: "rejected",
              rejectionReason: result.rejectionReason,
            },
          ],
        }),
      );
      expect(member!.value.rejection_code_hash).toBe(
        hashMidgardValidationRejectionReasonV1(
          result.rejectionReason!,
        ).toString("hex"),
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
        prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
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
      const payload: SDK.DaPayload = {
        version: 1n,
        block_body: {
          header,
          header_hash: await Effect.runPromise(SDK.hashBlockHeader(header)),
          counts,
          transactions: [],
          transaction_preimages: [],
          forced_transactions: [
            [orderKey, Data.to(leaf, SDK.ForcedInclusionTxV1)],
          ],
          forced_transaction_preimages: [[orderKey, tx.toString("hex")]],
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
                  pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
                  post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
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
            [
              member!.keyCbor.toString("hex"),
              member!.valueCbor.toString("hex"),
            ],
          ],
          validation_trace_witnesses: [],
          cek_program_material: [],
        },
      };
      expect(
        decodeDaPayloadStrict(SDK.encodeDaPayload(payload)).block_body
          .forced_transactions,
      ).toEqual(payload.block_body.forced_transactions);
      // A ForcedTxValid claim for the same transaction is refused.
      payload.block_body.forced_transactions = [
        [
          orderKey,
          Data.to(
            { ...leaf, verdict: "ForcedTxValid" },
            SDK.ForcedInclusionTxV1,
          ),
        ],
      ];
      expect(() =>
        decodeDaPayloadStrict(SDK.encodeDaPayload(payload)),
      ).toThrow();
    },
  );
});
