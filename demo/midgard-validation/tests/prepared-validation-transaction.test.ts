/**
 * The prepared transaction is built once, before the walk: its input
 * resolution schedule is proven through the pre-state ledger port in one
 * call, and nothing writes it afterwards, which the compiler enforces.
 */
import { deriveMidgardValidationContextCborV1 } from "@al-ft/midgard-core";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { emptyMidgardInputResolutionSchedule } from "../src/validation-machine/input-resolution.js";
import type { PreStateLedger } from "../src/validation-machine/pre-state-ledger.js";
import {
  buildPreparedValidationTransaction,
  type PreparedValidationTransaction,
} from "../src/validation-machine/prepared-validation-transaction.js";
import type { ReplayedValidationTransaction } from "../src/validation-machine/trace-walk.js";
import { makeNativeTx, outRefFromByte } from "./validation-fixtures.js";
import { context } from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

const transaction = makeNativeTx({
  version: 1n,
  spendInputs: [outRefFromByte(0x13), outRefFromByte(0x11)],
  referenceInputs: [outRefFromByte(0x12)],
});

const replayed: ReplayedValidationTransaction = {
  input: {
    consensusProfile: context.consensusProfile,
    transactionId: transaction.txId,
    canonicalTransactionCbor: transaction.txCbor,
    sourceKind: "normal",
    ledgerWitnessEntries: [],
    expectedNetworkId: context.expectedNetworkId,
  },
  contextCbor: deriveMidgardValidationContextCborV1({
    endTimeMs: BigInt(context.blockTimeMs),
    blockSlot: context.blockSlot,
    expectedNetworkId: context.expectedNetworkId,
    minFeeA: context.minFeeA,
    minFeeB: context.minFeeB,
  }),
  phaseALedgerTx: null,
  scriptWitnesses: [],
  ledgerState: new Map(),
  ledgerDescriptorState: new Map(),
  scriptEvaluations: [],
};

/** A port that answers each key with a proof naming it, and records calls. */
const recordingLedger = (answer = (keys: readonly Buffer[]) => keys) => {
  const calls: (readonly Buffer[])[] = [];
  const ledger: PreStateLedger = {
    proveResolution: async (keys) => {
      calls.push(keys);
      return answer(keys).map((key) =>
        Buffer.concat([Buffer.from("proof:"), key]),
      );
    },
  };
  return { ledger, calls };
};

const build = (ledger: PreStateLedger) =>
  Effect.runPromise(
    Effect.either(buildPreparedValidationTransaction(replayed, ledger)),
  );

describe("the prepared validation transaction", () => {
  it("proves the whole resolution schedule through the port in one call", async () => {
    const { ledger, calls } = recordingLedger();
    const prepared = Either.getOrThrow(await build(ledger));
    const nodes = prepared.resolutionScheduleNodes;
    expect(calls).toEqual([nodes.map(({ key }) => key)]);
    // Both input sets, in key order.
    expect(nodes.map(({ sourceKind }) => sourceKind)).toEqual([
      "spend",
      "reference",
      "spend",
    ]);
    nodes.forEach((node, index) => {
      expect(node.proofCbor).toEqual(
        Buffer.concat([Buffer.from("proof:"), node.key]),
      );
      expect(node.nextScheduleHash).toEqual(
        nodes[index + 1]?.scheduleHash ?? emptyMidgardInputResolutionSchedule(),
      );
    });
    expect(prepared.resolutionScheduleHash).toEqual(nodes[0]!.scheduleHash);
  });

  it("refuses a port that proves a different number of keys", async () => {
    const { ledger } = recordingLedger((keys) => keys.slice(1));
    const result = await build(ledger);
    expect(Either.isLeft(result) && result.left.message).toBe(
      "the pre-state ledger proved the wrong number of keys",
    );
  });
});

/** Writes the compiler refuses, at every depth; type-checked, never run. */
export const refusedWrites = (prepared: PreparedValidationTransaction) => {
  // @ts-expect-error a field is readonly
  prepared.resolutionScheduleHash = Buffer.alloc(32);
  // @ts-expect-error an array is readonly
  prepared.canonicalSignerHashes[0] = Buffer.alloc(28);
  // @ts-expect-error an array element's field is readonly
  prepared.resolutionScheduleNodes[0]!.proofCbor = Buffer.alloc(0);
  // @ts-expect-error a nested array is readonly
  prepared.addressWitnessScanItems[0]!.item.chunkHashes.length = 0;
  // @ts-expect-error a decoded redeemer is readonly
  prepared.decodedProofRedeemers[0]!.dataCborHex = "";
  // @ts-expect-error a replay fact is readonly
  prepared.input.ledgerWitnessEntries.length = 0;
  // @ts-expect-error a map is readonly
  const descriptors: Map<string, Buffer> = prepared.ledgerDescriptorState;
  // @ts-expect-error a readonly array is not a writable one
  const nodes: { proofCbor: Buffer }[] = prepared.resolutionScheduleNodes;
  return [descriptors, nodes];
};
