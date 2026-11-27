import { h32 } from "@al-ft/midgard-test-support/hex";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type InvalidOneStepTransitionWitness,
  InvalidOneStepTransitionWitnessSchema,
} from "@/fraud-proof/transition-trace.js";
import { ROOT_DOMAINS } from "@/transition-trace.js";

const txOrderId = { transactionId: h32(0x11), outputIndex: 0n };
const eventKey = {
  ForcedTransactionEventKey: { tx_order_id: txOrderId },
} as const;
// The forced leaf is opened over its raw key bytes: the Data encoding of the
// tx order id.
const rawKey = `d8799f5820${h32(0x11)}00ff`;

const witness: InvalidOneStepTransitionWitness = {
  ValidForcedTransactionTransition: {
    trace_proof: {
      domain: ROOT_DOMAINS.transitionTrace,
      root: h32(0x44),
      phas_root: h32(0x55),
      count: 1n,
      key: 0n,
      value: {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "ForcedTransaction",
        pre_utxos_root: h32(0x22),
        post_utxos_root: h32(0x33),
      },
      proof: [],
    },
    event_to_step: {
      domain: ROOT_DOMAINS.eventToStep,
      root: h32(0x66),
      phas_root: h32(0x77),
      count: 1n,
      key: eventKey,
      value: { step_index: 0n, phase: "ForcedTransaction" },
      proof: [],
    },
    source_membership: {
      domain: ROOT_DOMAINS.forcedTransactionsV1,
      root: h32(0x88),
      phas_root: h32(0x99),
      count: 1n,
      key: rawKey,
      value: "80",
      proof: [],
    },
  },
};

// Aiken InvalidOneStepTransitionWitness constructor 5, with fields in the
// exact proof.ak order. This literal is intentionally independent of the
// TypeScript encoder so constructor drift changes the test vector.
const AIKEN_VALID_FORCED_TRANSACTION_TRANSITION_CBOR =
  "d87e9fd8799fd87d8058204444444444444444444444444444444444444444444444444444444444444444582055555555555555555555555555555555555555555555555555555555555555550100d8799f0100d87a9fd8799f5820111111111111111111111111111111111111111111111111111111111111111100ffffd87a805820222222222222222222222222222222222222222222222222222222222222222258203333333333333333333333333333333333333333333333333333333333333333ff80ffd8799fd87e80582066666666666666666666666666666666666666666666666666666666666666665820777777777777777777777777777777777777777777777777777777777777777701d87a9fd8799f5820111111111111111111111111111111111111111111111111111111111111111100ffffd8799f00d87a80ff80ffd8799fd87a805820888888888888888888888888888888888888888888888888888888888888888858209999999999999999999999999999999999999999999999999999999999999999015827d8799f5820111111111111111111111111111111111111111111111111111111111111111100ff418080ffff";

describe("ValidForcedTransactionTransition ABI", () => {
  it("matches the exact Aiken constructor-5 CBOR vector", () => {
    expect(
      Data.to(witness as never, InvalidOneStepTransitionWitnessSchema as never),
    ).toBe(AIKEN_VALID_FORCED_TRANSACTION_TRANSITION_CBOR);
    expect(
      Data.from(
        AIKEN_VALID_FORCED_TRANSACTION_TRANSITION_CBOR,
        InvalidOneStepTransitionWitnessSchema,
      ),
    ).toEqual(witness);
  });

  it("rejects adjacent-constructor and malformed mutations", () => {
    const missingFinalField =
      AIKEN_VALID_FORCED_TRANSACTION_TRANSITION_CBOR.slice(0, -2);
    const nextTagMutation = `d87f${AIKEN_VALID_FORCED_TRANSACTION_TRANSITION_CBOR.slice(4)}`;

    expect(() =>
      Data.from(missingFinalField, InvalidOneStepTransitionWitnessSchema),
    ).toThrow();
    expect(() =>
      Data.from(nextTagMutation, InvalidOneStepTransitionWitnessSchema),
    ).toThrow();
  });
});
