import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import {
  encodeMidgardFieldPreimage,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { validationMachineLedgerRoot } from "@al-ft/midgard-validation";
import {
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  buildPlaceholderValidationTraceMembers,
  classifyForcedTransactions,
} from "../src/mpf/index.js";
import { encodeTransitionStepCbor } from "../src/mpf/transition-cbor.js";
import { forcedEntry } from "./forced-transactions.make-signed-effectful-transaction.js";
import { fixture, rebind, verdict } from "./foreign-block-import.fixture.js";

/**
 * A forced transaction carrying a Plutus item whose stored program does not
 * decode is refused at that item, of either kind: a field-6 witness (kind 0)
 * at the witness-script phase, an output reference script (kind 1) at the
 * script-source phase. The node commits `PlutusScriptUndecodable` for it,
 * under a placeholder descriptor, and a foreign node imports the block.
 *
 * The tx-order mint admits native items only, so such a transaction is built
 * here directly; the node must still commit, and import, its verdict.
 */

/** `[3, h'00']`: a PlutusV3 item whose script bytes are not a byte string. */
const UNDECODABLE_PLUTUS = Buffer.from("82034100", "hex");

/** The input both transactions spend, held by the fixture key. */
const SPENT = outRefFromByte(0x11);
const SPENT_OUTPUT = makeOutput(FUNDED_OUTPUT_LOVELACE);
const LEDGER = [{ outRef: SPENT, output: SPENT_OUTPUT }];

/** A funded output carrying `UNDECODABLE_PLUTUS` as its reference script. */
const fundedWithUndecodableRef = (() => {
  const funded = makeOutput(FUNDED_OUTPUT_LOVELACE);
  return Buffer.concat([
    Buffer.from([funded[0]! + 1]),
    funded.subarray(1),
    Buffer.from([0x03]),
    UNDECODABLE_PLUTUS,
  ]);
})();

/** A forced transaction spending `SPENT`, signed by its key. */
const forcedTransaction = (opts: {
  readonly outputs: readonly Buffer[];
  readonly scriptItems: readonly Buffer[];
}) => {
  const baseline = makeNativeTx({
    spendInputs: [SPENT],
    outputs: opts.outputs,
  });
  // The key signs the body, so the witness set can change under it.
  const native = encodeRecomputedNativeTx({
    ...baseline.tx,
    witnessSet: {
      ...baseline.tx.witnessSet,
      scriptTxWitsPreimageCbor: encodeMidgardFieldPreimage(opts.scriptItems),
    },
  });
  return {
    native,
    canonicalCbor: encodeMidgardForcedTxCanonical(
      materializeMidgardForcedTxFromCanonical(native.tx),
    ),
  };
};

const reasonOf = (itemKind: bigint): SDK.RejectionReason => ({
  PlutusScriptUndecodable: { item_kind: itemKind, item_index: 0n, class: 1n },
});

const SHAPES = [
  [
    "a Plutus witness",
    0n,
    forcedTransaction({
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
      scriptItems: [UNDECODABLE_PLUTUS],
    }),
  ],
  [
    "a Plutus reference script",
    1n,
    forcedTransaction({
      outputs: [fundedWithUndecodableRef],
      scriptItems: [],
    }),
  ],
] as const;

describe.each(SHAPES)(
  "a forced transaction with %s whose program does not decode",
  (_name, itemKind, transaction) => {
    it("is committed as PlutusScriptUndecodable under a placeholder descriptor", async () => {
      const [result] = await Effect.runPromise(
        classifyForcedTransactions({
          entries: [
            await forcedEntry({
              label: 21,
              transaction: {
                transaction: transaction.native.tx,
                transactionId: transaction.native.txId,
                canonicalCbor: transaction.canonicalCbor,
              },
            }),
          ],
          initialState: new Map(
            LEDGER.map((e) => [e.outRef.toString("hex"), e.output]),
          ),
          blockTime: new Date(1_750_000_000_000),
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
      const leaf = Data.from(
        result!.entry[
          ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
        ].toString("hex"),
        SDK.ForcedInclusionTxV1,
      );
      expect(leaf.verdict).toStrictEqual({
        ForcedTxInvalid: { reason: reasonOf(itemKind) },
      });
      const eventKey: SDK.EventKey = {
        ForcedTransactionEventKey: {
          tx_order_id: Data.from(
            result!.entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString(
              "hex",
            ),
            SDK.OutputReference,
          ),
        },
      };
      const [member] = await Effect.runPromise(
        buildPlaceholderValidationTraceMembers({
          transactions: [
            {
              eventKey,
              canonicalTransactionCbor: transaction.canonicalCbor,
              sourceKind: "forced",
              verdict: "rejected",
              rejectionReason: result!.rejectionReason,
            },
          ],
        }),
      );
      expect(member!.value.verdict).toBe("Rejected");
    }, 60_000);

    it("is imported by a foreign node at its committed verdict", async () => {
      const payload = await fixture({
        forced: { cbor: transaction.canonicalCbor, reason: reasonOf(itemKind) },
      });
      // The block spends from a ledger holding `SPENT`; a rejected forced
      // transaction leaves that ledger unchanged.
      const root = (await validationMachineLedgerRoot(LEDGER)).toString("hex");
      const body = payload.block_body;
      const step = Data.from(body.transition_trace[0]![1], SDK.TransitionStep);
      const funded = await rebind({
        ...payload,
        block_body: {
          ...body,
          header: { ...body.header, prevUtxosRoot: root, utxosRoot: root },
          transition_trace: [
            [
              body.transition_trace[0]![0],
              encodeTransitionStepCbor({
                ...step,
                pre_utxos_root: root,
                post_utxos_root: root,
              }).toString("hex"),
            ],
          ],
        },
      });
      const result = await verdict(funded, {
        parentUtxosRoot: root,
        parentEntries: [{ outref: SPENT, output: SPENT_OUTPUT }],
      });
      if (result._tag === "Left") throw result.left;
      expect(result.right).toMatchObject({
        headerHash: funded.block_body.header_hash,
        root,
      });
    }, 60_000);
  },
);
