import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
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
  buildDeterministicValidationTraceMembers,
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
  "classifies and retains the authentic %s native first fault",
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
        effectiveEndTime: new Date(nativeFaultContext.blockEndTimeMs),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation: {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          slotForUnixTime: () => 100n,
        },
        resolveProgramMaterialSidecar: () =>
          Effect.succeed(fixture.programMaterialSidecarCbor),
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
    const root = fixture.trace.states[0]!.priorLedgerRoot.toString("hex");
    const [retained] = await Effect.runPromise(
      buildDeterministicValidationTraceMembers({
        ...nativeFaultContext,
        blockEndTime: new Date(nativeFaultContext.blockEndTimeMs),
        transactions: [
          {
            eventKey,
            transactionId: fixture.native.txId,
            canonicalTransactionCbor: fixture.canonicalTransactionCbor,
            programMaterialSidecarCbor: fixture.programMaterialSidecarCbor,
            sourceKind: "forced",
            priorUtxosRoot: root,
            postUtxosRoot: root,
            ledgerOps: result!.ledgerOps,
            ledgerWitnessEntries: result!.ledgerWitnessEntries,
            ledgerMutationSteps: result!.ledgerMutationSteps,
            verdict: "rejected",
            rejectionCode: result!.rejectionCode,
            scriptEvaluations: result!.scriptEvaluations,
          },
        ],
      }),
    );
    expect(retained!.value.verdict).toBe("Rejected");
    expect(retained!.witnesses.length).toBeGreaterThan(
      fixture.trace.witnesses.length,
    );
    // Every exported witness must decode through the real retained wire schema.
    for (const [, value] of retained!.witnesses)
      expect(
        SDK.decodeRetainedValidationWitness(Buffer.from(value, "hex")),
      ).toBeDefined();
  },
  60000,
);
