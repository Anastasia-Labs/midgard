import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import * as codec from "../src/index.js";
import {
  decodeMidgardValidationMachineState,
  deriveMidgardValidationInitialStateV1,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardValidationSourceV1,
} from "../src/index.js";
import {
  deriveValidationInitialStateVectorsV1,
  type ValidationInitialStateGoldenV1,
  validationSourceOf,
} from "./fixtures/validation-initial-state-v1.vectors.mjs";

/**
 * The checked-in golden. The Aiken module generated beside it asserts
 * `validation_claim_v1.derive_initial_state` on the same values, so this
 * suite recomputing it through `src/` is the TypeScript half of the channel.
 */
const golden = JSON.parse(
  readFileSync(
    new URL(
      "./fixtures/validation-initial-state-v1.generated.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as ValidationInitialStateGoldenV1;

const bytes = (hex: string): Buffer => Buffer.from(hex, "hex");

const headerOf = (label: string) => {
  const header = golden.headers.find((entry) => entry.label === label)!;
  return {
    endTimeMs: BigInt(header.endTimeMs),
    blockSlot: BigInt(header.blockSlot),
    expectedNetworkId: BigInt(header.expectedNetworkId),
    minFeeA: BigInt(header.minFeeA),
    minFeeB: BigInt(header.minFeeB),
  };
};

const transactionOf = (label: string) =>
  golden.transactions.find((entry) => entry.label === label)!;

const vectorOf = (label: string) =>
  golden.vectors.find((entry) => entry.label === label)!;

describe("validation-initial-state-v1 golden", () => {
  it("recomputes every vector through the source twin", () => {
    expect(deriveValidationInitialStateVectorsV1(codec)).toEqual(golden);
  });

  it("covers four transactions under two headers, one forced and one at the field-8 bound", () => {
    expect(golden.vectors).toHaveLength(8);
    expect(
      golden.transactions.map(
        ({ redeemersPreimageBytes }) => redeemersPreimageBytes,
      ),
    ).toContain(MIDGARD_CONSENSUS_LIMITS.maxRedeemersPreimageBytes);
    expect(golden.vectors.map(({ source }) => source)).toEqual([
      ...Array<string>(6).fill("normal"),
      "forced",
      "forced",
    ]);
    expect(new Set(golden.vectors.map(({ stateHash }) => stateHash)).size).toBe(
      8,
    );
  });

  it("starts a forced source in a Forced state, the one the claim derives", () => {
    const transaction = transactionOf("forced_spend");
    const vector = vectorOf("forced_spend_a");
    const state = deriveMidgardValidationInitialStateV1({
      ...headerOf("a"),
      eventKeyCbor: bytes(vector.eventKeyCbor),
      source: {
        forced: {
          canonicalTransactionCbor: bytes(transaction.canonicalTransactionCbor),
        },
      },
      preUtxosRoot: bytes(vector.preUtxosRoot),
    });
    expect(state.sourceKind).toBe("forced");
    expect(
      decodeMidgardValidationMachineState(bytes(vector.stateCbor)).sourceKind,
    ).toBe("forced");
    expect(hashMidgardValidationMachineState(state).toString("hex")).toBe(
      vector.stateHash,
    );
  });

  it("starts a normal source in a Normal state, the one the claim derives", () => {
    const transaction = transactionOf("minimal_spend");
    const vector = vectorOf("minimal_spend_a");
    const state = deriveMidgardValidationInitialStateV1({
      ...headerOf("a"),
      eventKeyCbor: bytes(vector.eventKeyCbor),
      source: {
        normal: {
          canonicalTransactionCbor: bytes(transaction.canonicalTransactionCbor),
        },
      },
      preUtxosRoot: bytes(vector.preUtxosRoot),
    });
    expect(state.sourceKind).toBe("normal");
    expect(
      decodeMidgardValidationMachineState(bytes(vector.stateCbor)).sourceKind,
    ).toBe("normal");
    expect(hashMidgardValidationMachineState(state).toString("hex")).toBe(
      vector.stateHash,
    );
  });

  it("refuses a source that is not exactly one variant", () => {
    const canonicalTransactionCbor = bytes(
      transactionOf("minimal_spend").canonicalTransactionCbor,
    );
    const vector = vectorOf("minimal_spend_a");
    for (const source of [
      {},
      {
        normal: { canonicalTransactionCbor },
        forced: { canonicalTransactionCbor },
      },
    ]) {
      expect(() =>
        deriveMidgardValidationInitialStateV1({
          ...headerOf("a"),
          eventKeyCbor: bytes(vector.eventKeyCbor),
          source: source as unknown as MidgardValidationSourceV1,
          preUtxosRoot: bytes(vector.preUtxosRoot),
        }),
      ).toThrow();
    }
  });

  it("pins the start state the twin derives for each vector", () => {
    for (const vector of golden.vectors) {
      const transaction = transactionOf(vector.transaction);
      const state = deriveMidgardValidationInitialStateV1({
        ...headerOf(vector.header),
        eventKeyCbor: bytes(vector.eventKeyCbor),
        source: validationSourceOf(
          transaction.source,
          bytes(transaction.canonicalTransactionCbor),
        ),
        preUtxosRoot: bytes(vector.preUtxosRoot),
      });
      expect(
        decodeMidgardValidationMachineState(bytes(vector.stateCbor)),
      ).toEqual(state);
      expect(hashMidgardValidationMachineState(state).toString("hex")).toBe(
        vector.stateHash,
      );
      expect(state).toMatchObject({
        phase: "canonicalDecode",
        programCounter: 0,
        executionCpu: 0n,
        executionMemory: 0n,
        verdict: "pending",
        sourceKind: transaction.source,
      });
      expect(state.transactionId.toString("hex")).toBe(
        transaction.transactionId,
      );
      expect(state.priorLedgerRoot.toString("hex")).toBe(vector.preUtxosRoot);
    }
  });
});
