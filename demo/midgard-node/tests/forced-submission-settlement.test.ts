import "./utils.js";

import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec";
import {
  deriveMidgardForcedTxProofSource,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { makeForcedOrderSettlementScenario } from "@al-ft/midgard-fault-proofs/test-support/forced-order-settlement";
import * as SDK from "@al-ft/midgard-sdk";
import {
  replayValidationMachineEvent,
  validationMachineLedgerRoot,
} from "@al-ft/midgard-validation";
import {
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

const submission = (invalid = false) =>
  materializeMidgardForcedTxFromCanonical(
    makeNativeTx({
      spendInputs: [outRefFromByte(0x31)],
      outputs: [makeOutput(10_000_000n)],
      ...(invalid ? { invalidVkeyWitness: true as const } : {}),
    }).tx,
  );
const assertRefusal = async (attempt: () => Promise<unknown>) => {
  await expect(attempt()).rejects.toThrow(
    /Script|script|eval|Evaluation|UPLC/u,
  );
};

describe("forced submission under real order and settlement validators", () => {
  it.each([true, false])(
    "settles independently verified accepted=$0 execution over the original submission",
    async (accepted) => {
      const verdict: SDK.OperatorVerdict = accepted
        ? "ForcedTxValid"
        : {
            ForcedTxInvalid: {
              reason: { AddressWitnessSignatureInvalid: { witness_index: 0n } },
            },
          };
      const s = await makeForcedOrderSettlementScenario({
        submitted: submission(!accepted),
        verdict,
      });
      const entries = [
        { outRef: outRefFromByte(0x31), output: makeOutput(10_000_000n) },
      ];
      const priorRoot = (await validationMachineLedgerRoot(entries)).toString(
        "hex",
      );
      const replay = await Effect.runPromise(
        replayValidationMachineEvent({
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          sourceKind: "forced",
          canonicalTransactionCbor: s.bytes,
          eventKeyCbor: Buffer.from(
            Data.to(
              { ForcedTransactionEventKey: { tx_order_id: s.datum.event.id } },
              SDK.EventKey,
            ),
            "hex",
          ),
          ledgerWitnessEntries: entries,
          priorUtxosRoot: priorRoot,
          blockEndTimeMs: 1_750_000_000_000,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          blockSlot: 100n,
        }),
      );
      expect(replay.trace.verdict).toBe(accepted ? "accepted" : "rejected");
      expect(replay.replayInput.expectedLedgerOps).toHaveLength(
        accepted ? 2 : 0,
      );
      if (!accepted) expect(replay.replayInput.postUtxosRoot).toBe(priorRoot);
      expect(s.datum.event.tx.submitted_source).toEqual(
        s.original.submitted_source,
      );
      await s.settle();
      expect(
        await s.h.proverLucid.utxosAt(
          s.h.contracts.txOrder.spendingScriptAddress,
        ),
      ).toHaveLength(0);
    },
    180_000,
  );

  it("keeps two actual orders for one body ID distinct, refusing cross-order membership", async () => {
    const s = await makeForcedOrderSettlementScenario({
      submitted: submission(),
      twoOrders: true,
    });
    const datums = s.orders.map((order) =>
      Data.from(order.datum!, SDK.TxOrderDatum),
    );
    expect(datums[0]!.event.id).not.toEqual(datums[1]!.event.id);
    expect(datums[0]!.event.tx).toEqual(datums[1]!.event.tx);
    await assertRefusal(() => s.settleOrder(0, 1));
    await s.settleOrder(0);
    await s.settleOrder(1);
    expect(
      await s.h.proverLucid.utxosAt(
        s.h.contracts.txOrder.spendingScriptAddress,
      ),
    ).toHaveLength(0);
  }, 180_000);

  it.each(["witness", "lengths", "body", "obsolete"] as const)(
    "refuses settled %s substitution against the immutable order",
    async (kind) => {
      const submitted = submission();
      const s = await makeForcedOrderSettlementScenario({
        submitted,
        changeLeaf: (original) => {
          const leaf = structuredClone(original);
          if (kind === "witness") {
            const changed = deriveMidgardForcedTxProofSource(submission(true));
            leaf.submitted_source = {
              compact_cbor: changed.compactCbor.toString("hex"),
              witness_set_compact_cbor:
                changed.witnessSetCompactCbor.toString("hex"),
              field_preimage_lengths_cbor:
                changed.fieldPreimageLengthsCbor.toString("hex"),
            };
          }
          if (kind === "lengths")
            leaf.submitted_source.field_preimage_lengths_cbor =
              "89010101010101010102";
          if (kind === "body") leaf.tx_id = "ab".repeat(32);
          if (kind === "obsolete") {
            const compact = decodeSingleCbor(
              Buffer.from(leaf.submitted_source.compact_cbor, "hex"),
            );
            if (!Array.isArray(compact))
              throw new Error("expected compact array");
            leaf.submitted_source.compact_cbor = encodeCbor([
              ...compact,
              0n,
            ]).toString("hex");
          }
          return leaf;
        },
      });
      await assertRefusal(() => s.settle());
      expect(
        await s.h.proverLucid.utxosAt(
          s.h.contracts.txOrder.spendingScriptAddress,
        ),
      ).toHaveLength(1);
    },
    180_000,
  );

  it("refuses a different verdict while permitting the committed verdict", async () => {
    const s = await makeForcedOrderSettlementScenario({
      submitted: submission(),
    });
    await assertRefusal(() =>
      s.settle({ ForcedTxInvalid: { reason: "FeeBelowMinimum" } }),
    );
    await s.settle();
  }, 180_000);
});
