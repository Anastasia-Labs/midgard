import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type InvalidOneStepTransitionWitness,
  InvalidOneStepTransitionWitnessSchema,
} from "@/fraud-proof/transition-trace.js";
import { ROOT_DOMAINS } from "@/transition-trace.js";

const h32 = (byte: string): string => byte.repeat(64);
const txId = h32("1");
const eventKey = { L2TransactionEventKey: { tx_id: txId } } as const;

const witness: InvalidOneStepTransitionWitness = {
  L2TransactionTransition: {
    trace_proof: {
      domain: ROOT_DOMAINS.transitionTrace,
      root: h32("4"),
      phas_root: h32("5"),
      count: 1n,
      key: 0n,
      value: {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: h32("2"),
        post_utxos_root: h32("3"),
      },
      proof: [],
    },
    event_to_step: {
      domain: ROOT_DOMAINS.eventToStep,
      root: h32("6"),
      phas_root: h32("7"),
      count: 1n,
      key: eventKey,
      value: { step_index: 0n, phase: "L2Transaction" },
      proof: [],
    },
    source_membership: {
      domain: ROOT_DOMAINS.transactionsV1,
      root: h32("8"),
      phas_root: h32("9"),
      count: 1n,
      key: txId,
      value: "80",
      proof: [],
    },
    spend_inputs_preimage: "80",
    outputs_preimage: "80",
    spent_utxos: [],
    produced_utxos: [],
  },
};

// Aiken InvalidOneStepTransitionWitness constructor 4, with fields in the
// exact proof.ak order. This literal is intentionally independent of the
// TypeScript encoder so constructor drift changes the test vector.
const AIKEN_L2_TRANSACTION_TRANSITION_CBOR =
  "d87d9fd8799fd87d8058204444444444444444444444444444444444444444444444444444444444444444582055555555555555555555555555555555555555555555555555555555555555550100d8799f0100d87b9f58201111111111111111111111111111111111111111111111111111111111111111ffd87b805820222222222222222222222222222222222222222222222222222222222222222258203333333333333333333333333333333333333333333333333333333333333333ff80ffd8799fd87e80582066666666666666666666666666666666666666666666666666666666666666665820777777777777777777777777777777777777777777777777777777777777777701d87b9f58201111111111111111111111111111111111111111111111111111111111111111ffd8799f00d87b80ff80ffd8799fd87b8058208888888888888888888888888888888888888888888888888888888888888888582099999999999999999999999999999999999999999999999999999999999999990158201111111111111111111111111111111111111111111111111111111111111111418080ff418041808080ff";

describe("L2TransactionTransition ABI", () => {
  it("matches the exact Aiken constructor-4 CBOR vector", () => {
    expect(
      Data.to(witness as never, InvalidOneStepTransitionWitnessSchema as never),
    ).toBe(AIKEN_L2_TRANSACTION_TRANSITION_CBOR);
    expect(
      Data.from(
        AIKEN_L2_TRANSACTION_TRANSITION_CBOR,
        InvalidOneStepTransitionWitnessSchema,
      ),
    ).toEqual(witness);
  });

  it("rejects adjacent-constructor and malformed mutations", () => {
    const validDepositTagMutation = `d87c${AIKEN_L2_TRANSACTION_TRANSITION_CBOR.slice(4)}`;
    const missingFinalField = AIKEN_L2_TRANSACTION_TRANSITION_CBOR.slice(0, -2);

    expect(() =>
      Data.from(validDepositTagMutation, InvalidOneStepTransitionWitnessSchema),
    ).toThrow();
    expect(() =>
      Data.from(missingFinalField, InvalidOneStepTransitionWitnessSchema),
    ).toThrow();
  });
});
