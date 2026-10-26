import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  hashMidgardValidationRejectionReasonV1,
  locateMidgardTransactionFieldItemFault,
} from "@al-ft/midgard-core";
import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  decideValidationChallengeV1,
  openValidationTraceV1,
  RejectCodes,
} from "../src/index.js";
import {
  expectSinglePhaseAAcceptance,
  expectSinglePhaseARejection,
} from "./phase-a.outref-projection.js";
import {
  encodeByteList,
  encodeRecomputedNativeTx,
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
} from "./validation-fixtures.js";
import { context } from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

/**
 * CanonicalDecode refuses fields 0, 1 and 7 out of canonical order as
 * `FieldItemWidthIllegal` at the first item that breaks it: inputs ascend by
 * item bytes (a repeat passes on to `DuplicateInput`), address witnesses
 * ascend strictly by signer hash, then by item bytes. The node and the trace
 * builder both reach that verdict before any later check.
 */
const [low, high] = [outRefFromByte(0x31), outRefFromByte(0x32)];
const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
const outputs = [output];

const secondSigner = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x05));
const signed = makeNativeTx({
  version: 1n,
  spendInputs: [low],
  outputs,
  extraVkeyWitnessKeys: [secondSigner],
});
const witnesses = decodeMidgardFieldPreimage(
  signed.tx.witnessSet.addrTxWitsPreimageCbor,
);
const withWitnesses = (items: readonly Buffer[]) =>
  encodeRecomputedNativeTx({
    ...signed.tx,
    witnessSet: {
      ...signed.tx.witnessSet,
      addrTxWitsPreimageCbor: encodeByteList(items),
    },
  });

const orderRefusal = (fieldIndex: bigint) => ({
  arm: "FieldItemWidthIllegal",
  fieldIndex,
  itemIndex: 1n,
});

// FIWI's fixed-item legality names the same first refused item.
const fiwiFault = (txCbor: Buffer) =>
  locateMidgardTransactionFieldItemFault(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCbor),
  );

describe("CanonicalDecode field order at the node", () => {
  it("accepts ascending inputs and ascending address witnesses", async () => {
    expect(witnesses).toHaveLength(2);
    await expectSinglePhaseAAcceptance(signed);
    const ascending = makeNativeTx({ referenceInputs: [low, high] });
    await expectSinglePhaseAAcceptance(ascending);
    expect(fiwiFault(signed.txCbor)).toBeNull();
    expect(fiwiFault(ascending.txCbor)).toBeNull();
  });

  it.each([
    ["spend inputs", 0n, { spendInputs: [high, low] }],
    ["reference inputs", 1n, { referenceInputs: [high, low] }],
  ] as const)(
    "refuses descending %s at the second item",
    async (_name, fieldIndex, options) => {
      const fixture = makeNativeTx({ outputs, ...options });
      const rejection = await expectSinglePhaseARejection(
        fixture,
        RejectCodes.InvalidFieldType,
      );
      expect(rejection.subject).toStrictEqual(orderRefusal(fieldIndex));
      expect(fiwiFault(fixture.txCbor)).toEqual({
        fieldIndex: Number(fieldIndex),
        itemIndex: 1,
      });
    },
  );

  it.each([
    ["descending", () => [witnesses[1]!, witnesses[0]!]],
    ["repeated", () => [witnesses[0]!, witnesses[0]!]],
  ] as const)(
    "refuses %s address witnesses at the second item",
    async (_name, items) => {
      const fixture = withWitnesses(items());
      expect(fixture.txId).toStrictEqual(signed.txId);
      const rejection = await expectSinglePhaseARejection(
        fixture,
        RejectCodes.InvalidFieldType,
      );
      expect(rejection.subject).toStrictEqual(orderRefusal(7n));
      expect(fiwiFault(fixture.txCbor)).toEqual({
        fieldIndex: 7,
        itemIndex: 1,
      });
    },
  );
});

const commitForcedRejection = (
  transaction: ReturnType<typeof makeNativeTx>,
  spent: readonly Buffer[],
) =>
  Effect.runPromise(
    openValidationTraceV1({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      ...context,
      sourceKind: "forced",
      transactionId: transaction.txId,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(transaction.tx),
      priorUtxosRoot: "03".repeat(32),
      postUtxosRoot: "03".repeat(32),
      ledgerWitnessEntries: spent.map((outRef) => ({ outRef, output })),
      expectedLedgerOps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.InvalidFieldType,
    }),
  );

const fieldOrderReason = (fieldIndex: bigint) =>
  hashMidgardValidationRejectionReasonV1({
    FieldItemWidthIllegal: { field_index: fieldIndex, item_index: 1n },
  });

describe(
  "CanonicalDecode field order in the trace",
  { timeout: 120_000 },
  () => {
    it.each([
      [
        "spend inputs",
        0n,
        () => makeNativeTx({ version: 1n, spendInputs: [high, low], outputs }),
        [high, low],
      ],
      [
        "address witnesses",
        7n,
        () => withWitnesses([witnesses[1]!, witnesses[0]!]),
        [low],
      ],
    ] as const)(
      "commits the order refusal of descending %s and disputes any other",
      async (_name, fieldIndex, transaction, spent) => {
        const leaves = await commitForcedRejection(transaction(), spent);
        expect(leaves.descriptor.verdict).toBe("rejected");
        expect(
          Buffer.from(leaves.descriptor.rejectionCodeHash).equals(
            fieldOrderReason(fieldIndex),
          ),
        ).toBe(true);
        expect(
          decideValidationChallengeV1(leaves, {
            ...leaves.descriptor,
            rejectionCodeHash: fieldOrderReason(fieldIndex === 0n ? 7n : 0n),
          }),
        ).toEqual({
          kind: "terminalIncoherent",
          terminalStateIndex: leaves.stepCount,
        });
      },
    );
  },
);
