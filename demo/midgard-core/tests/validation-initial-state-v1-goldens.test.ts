import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import * as codec from "../src/index.js";
import {
  decodeMidgardValidationMachineState,
  deriveMidgardValidationInitialStateV1,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_LIMITS,
} from "../src/index.js";
import {
  deriveValidationInitialStateVectorsV1,
  type ValidationInitialStateGoldenV1,
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

describe("validation-initial-state-v1 golden", () => {
  it("recomputes every vector through the source twin", () => {
    expect(deriveValidationInitialStateVectorsV1(codec)).toEqual(golden);
  });

  it("covers three transactions under two headers, one of them at the field-8 bound", () => {
    expect(golden.vectors).toHaveLength(6);
    expect(
      golden.transactions.map(
        ({ redeemersPreimageBytes }) => redeemersPreimageBytes,
      ),
    ).toContain(MIDGARD_CONSENSUS_LIMITS.maxRedeemersPreimageBytes);
    expect(new Set(golden.vectors.map(({ stateHash }) => stateHash)).size).toBe(
      6,
    );
  });

  it("pins the start state the twin derives for each vector", () => {
    for (const vector of golden.vectors) {
      const header = golden.headers.find(
        ({ label }) => label === vector.header,
      )!;
      const transaction = golden.transactions.find(
        ({ label }) => label === vector.transaction,
      )!;
      const state = deriveMidgardValidationInitialStateV1({
        endTimeMs: BigInt(header.endTimeMs),
        blockSlot: BigInt(header.blockSlot),
        expectedNetworkId: BigInt(header.expectedNetworkId),
        minFeeA: BigInt(header.minFeeA),
        minFeeB: BigInt(header.minFeeB),
        eventKeyCbor: bytes(vector.eventKeyCbor),
        canonicalTransactionCbor: bytes(transaction.canonicalTransactionCbor),
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
        sourceKind: "normal",
      });
      expect(state.transactionId.toString("hex")).toBe(
        transaction.transactionId,
      );
      expect(state.priorLedgerRoot.toString("hex")).toBe(vector.preUtxosRoot);
    }
  });
});
