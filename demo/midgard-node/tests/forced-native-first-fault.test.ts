import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import {
  nativeFaultContext,
  nativeFaultFixture,
} from "@al-ft/midgard-validation/tests/forced-native-first-fault-fixture";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import {
  buildPlaceholderValidationTraceMembers,
  classifyForcedTransactions,
} from "../src/mpf/index.js";
import { forcedEntry } from "./forced-transactions.make-signed-effectful-transaction.js";

it.each([
  "present",
  "missing",
  "empty",
  "signature",
  "earlierFalse",
  "missingKey",
  "invalidChildren",
  "invalidThresholdChildren",
  "exhaustedBoundary",
  "mint",
  "validKey",
] as const)(
  "classifies the authentic %s native first fault under a placeholder descriptor",
  async (shape) => {
    const fixture = await nativeFaultFixture(shape);
    const [result] = await Effect.runPromise(
      classifyForcedTransactions({
        entries: [
          await forcedEntry({
            label: 14,
            transaction: {
              transaction: fixture.native.tx,
              transactionId: fixture.native.txId,
              canonicalCbor: fixture.canonicalTransactionCbor,
            },
          }),
        ],
        initialState: new Map(
          fixture.entries.map((e) => [e.outRef.toString("hex"), e.output]),
        ),
        blockTime: new Date(nativeFaultContext.blockTimeMs),
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
    expect(result!.rejectionCode).toBe(fixture.trace.rejectionCode);
    const leaf = Data.from(
      result!.entry[
        ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
      ].toString("hex"),
      SDK.ForcedInclusionTxV1,
    );
    expect(leaf.verdict).toHaveProperty("ForcedTxInvalid");
    if (shape === "mint" || shape === "validKey")
      expect(leaf.verdict).toStrictEqual({
        ForcedTxInvalid: {
          reason:
            shape === "mint"
              ? { WitnessNativeScriptMalformed: { script_index: 0n } }
              : { WitnessNativeScriptFalse: { script_index: 0n } },
        },
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
            canonicalTransactionCbor: fixture.canonicalTransactionCbor,
            sourceKind: "forced",
            verdict: "rejected",
            rejectionCode: result!.rejectionCode,
          },
        ],
      }),
    );
    expect(member!.value.verdict).toBe("Rejected");
  },
  60000,
);
