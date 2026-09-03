import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  AdvancingApproach,
  NeglectedUserEventSchema,
  OperatorRemovalReasonSchema,
  SchedulerDatum,
  SchedulerMintRedeemer,
  SchedulerSpendRedeemer,
} from "../src/scheduler.js";

const expectCanonicalCbor = (
  value: unknown,
  schema: unknown,
  expected: string,
): void => {
  expect(Data.to(value as never, schema as never)).toBe(expected);
  expect(Data.from(expected, schema as never)).toEqual(value);
};

describe("canonical Scheduler V1 ABI", () => {
  it("matches every Aiken scheduler constructor tag and arity exactly", () => {
    const vectors: readonly (readonly [unknown, unknown, string])[] = [
      ["NoActiveOperators", SchedulerDatum, "d87980"],
      [
        { ActiveOperator: { operator: "aa", start_time: 1n } },
        SchedulerDatum,
        "d87a9f41aa01ff",
      ],
      ["Init", SchedulerMintRedeemer, "d87980"],
      ["Deinit", SchedulerMintRedeemer, "d87a80"],
      ["NoNeglectedUserEvent", NeglectedUserEventSchema, "d87980"],
      [
        { NeglectedDeposit: { deposit_ref_input_index: 1n } },
        NeglectedUserEventSchema,
        "d87a9f01ff",
      ],
      [
        { NeglectedWithdrawal: { withdrawal_ref_input_index: 2n } },
        NeglectedUserEventSchema,
        "d87b9f02ff",
      ],
      [
        { NeglectedTxOrder: { tx_order_ref_input_index: 3n } },
        NeglectedUserEventSchema,
        "d87c9f03ff",
      ],
      ["OperatorRetirement", OperatorRemovalReasonSchema, "d87980"],
      ["OperatorSlashing", OperatorRemovalReasonSchema, "d87a80"],
      [
        {
          GoToNextDueToEndOfShift: {
            new_shifts_operator_node_ref_input_index: 1n,
          },
        },
        AdvancingApproach,
        "d8799f01ff",
      ],
      [
        {
          RewindDueToEndOfShift: {
            active_operators_root_ref_input_index: 1n,
            active_operators_last_node_ref_input_index: 2n,
            registered_element_ref_input_index: 3n,
          },
        },
        AdvancingApproach,
        "d87a9f010203ff",
      ],
      [
        {
          GoToNextDueToSkippedOperator: {
            new_shifts_operator_node_ref_input_index: 1n,
            skipped_operator_node_input_index: 2n,
            active_operators_spend_redeemer_index: 3n,
            state_queue_ref_input_index: 4n,
            hub_oracle_ref_input_index: 5n,
            neglected_user_event: {
              NeglectedDeposit: { deposit_ref_input_index: 6n },
            },
          },
        },
        AdvancingApproach,
        "d87b9f0102030405d87a9f06ffff",
      ],
      [
        {
          RewindDueToSkippedOperator: {
            active_operators_root_ref_input_index: 1n,
            skipped_operator_node_input_index: 2n,
            active_operators_spend_redeemer_index: 3n,
            state_queue_ref_input_index: 4n,
            hub_oracle_ref_input_index: 5n,
            m_active_operators_last_node_ref_input_index: 6n,
            registered_element_ref_input_index: 7n,
            neglected_user_event: {
              NeglectedWithdrawal: { withdrawal_ref_input_index: 8n },
            },
          },
        },
        AdvancingApproach,
        "d87c9f0102030405d8799f06ff07d87b9f08ffff",
      ],
      [
        {
          GoToNextDueToOperatorRemoval: {
            active_operators_mint_redeemer_index: 1n,
            removal_reason: "OperatorRetirement",
          },
        },
        AdvancingApproach,
        "d87d9f01d87980ff",
      ],
      [
        {
          RewindDueToOperatorRemoval: {
            active_operators_mint_redeemer_index: 1n,
            m_active_operators_last_node_ref_input_index: null,
            removal_reason: "OperatorSlashing",
            registered_element_ref_input_index: 2n,
          },
        },
        AdvancingApproach,
        "d87e9f01d87a80d87a8002ff",
      ],
      [
        {
          AppointFirstOperator: {
            new_shifts_operator_node_ref_input_index: 1n,
            registered_element_ref_input_index: 2n,
          },
        },
        AdvancingApproach,
        "d87f9f0102ff",
      ],
      [
        {
          scheduler_input_index: 1n,
          scheduler_output_index: 2n,
          advancing_approach: {
            AppointFirstOperator: {
              new_shifts_operator_node_ref_input_index: 3n,
              registered_element_ref_input_index: 4n,
            },
          },
        },
        SchedulerSpendRedeemer,
        "d8799f0102d87f9f0304ffff",
      ],
    ];

    for (const [value, schema, cbor] of vectors) {
      expectCanonicalCbor(value, schema, cbor);
    }
  });

  it("rejects adjacent scheduler tags and wrong constructor arities", () => {
    const invalid: readonly (readonly [string, unknown])[] = [
      ["d87b80", SchedulerDatum],
      ["d87b80", SchedulerMintRedeemer],
      ["d87d80", NeglectedUserEventSchema],
      ["d87b80", OperatorRemovalReasonSchema],
      ["d9050080", AdvancingApproach],
      ["d87980", AdvancingApproach],
      ["d8799f0102ff", SchedulerSpendRedeemer],
    ];

    for (const [cbor, schema] of invalid) {
      expect(() => Data.from(cbor, schema as never)).toThrow();
    }
  });
});
