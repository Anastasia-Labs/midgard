import { encodeCbor, MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { Effect, Exit } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerMutationSteps,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";

// A transaction that spends the ledger's only entry and produces nothing
// drains the ledger. A Midgard header commits the drained ledger as
// `EMPTY_MERKLE_TREE_ROOT`; the machine trace must end at that same root, or an
// accepted claim over the honest block would disagree with its `utxos_root`.
const buildDrainTrace = async (postUtxosRoot: string) => {
  const spent = outRefFromByte(0x31);
  const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs: [],
    fee: FUNDED_OUTPUT_LOVELACE,
  });
  const expectedLedgerOps = [{ type: "delete" as const, key: spent }];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output }],
    operations: expectedLedgerOps,
  });
  return await Effect.runPromiseExit(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x73)]),
      sourceKind: "forced",
      blockEndTimeMs: 1_750_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: transaction.txId,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(transaction.tx),
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot,
      ledgerWitnessEntries: [{ outRef: spent, output }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

describe("validation machine over a drained ledger", () => {
  it("accepts a zero-output drain that ends at the committed empty-ledger root", async () => {
    const exit = await buildDrainTrace(EMPTY_MERKLE_TREE_ROOT);
    expect(Exit.isSuccess(exit)).toBe(true);
    if (Exit.isSuccess(exit)) {
      expect(exit.value.verdict).toBe("accepted");
    }
  });

  it("refuses to name the drained ledger by the MPF null root", async () => {
    const exit = await buildDrainTrace("00".repeat(32));
    expect(Exit.isFailure(exit)).toBe(true);
  });
});
