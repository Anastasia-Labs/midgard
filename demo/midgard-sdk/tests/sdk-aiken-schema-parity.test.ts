import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  decodeValidityCode,
  encodeValidityCode,
  type MidgardTxValidity as NativeMidgardTxValidity,
  MidgardTxValidityCodes,
} from "@al-ft/midgard-core/codec";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { parseBlueprintAbi, refines } from "../src/blueprint-abi/index.js";
import * as SDK from "../src/index.js";

const repoRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../../..",
);
/**
 * Compares against the tracked blueprint. Point `MIDGARD_REAL_BLUEPRINT_PATH`
 * at another build to check that one instead.
 */
const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  path.join(repoRoot, "onchain/aiken/plutus.json");
const blueprintAbi = parseBlueprintAbi(
  JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown,
);

type SdkExport = keyof typeof SDK;

/**
 * Each SDK schema that mirrors an Aiken definition, keyed by its export name,
 * so renaming either side breaks typecheck or this test. The event-history
 * order shapes are the ones the follower decodes an admitted order with, so
 * they must match the types `order_facts.admit` reads.
 */
const MIRRORS = {
  ProofStepSchema: "aiken/merkle_patricia_forestry/ProofStep",
  NeighborSchema: "aiken/merkle_patricia_forestry/Neighbor",
  HeaderSchema: "midgard/ledger_state/HeaderV1",
  TransitionStepSchema: "midgard/ledger_state/TransitionStep",
  EventKeySchema: "midgard/ledger_state/EventKey",
  EventToStepValueSchema: "midgard/ledger_state/EventToStepValue",
  ForcedInclusionTxV1Schema: "midgard/ledger_state/ForcedInclusionTxV1",
  L2TransactionSourceSchema: "midgard/ledger_state/L2TransactionSourceV1",
  NativeTxProofSourceSchema: "midgard/ledger_state/NativeTxProofSourceV1",
  TxOrderPayloadSchema: "midgard/ledger_state/TxOrderPayloadV1",
  TxOrderEventSchema: "midgard/ledger_state/TxOrderEventV1",
  TxOrderDatumSchema: "midgard/user_events/tx_order_v1/Datum",
  TxOrderSpendRedeemerSchema: "midgard/user_events/tx_order_v1/SpendRedeemer",
  TxOrderMintRedeemerSchema: "midgard/user_events/tx_order_v1/MintRedeemer",
  ValidationMachineStateSchema:
    "midgard/validation_trace_v1/ValidationMachineStateV1",
  ValidationTraceProofSchema:
    "midgard/validation_trace_v1/ValidationTraceProof",
  CaseSchema: "midgard/validation_case_v1/CaseV1",
  CaseThreadSchema: "midgard/validation_case_v1/CaseThreadV1",
  ThreadHeaderSchema: "midgard/validation_case_v1/ThreadHeaderV1",
  CaseRedeemerSchema: "midgard/validation_case_v1/CaseRedeemerV1",
  CaseThreadDatumSchema: "fraud_proofs/validation_trace/timeout_v1/Datum",
  ValidationTimeoutSpendRedeemerSchema:
    "fraud_proofs/validation_trace/timeout_v1/SpendRedeemer",
  ValidationDisputeTurnSchema: "midgard/validation_dispute_v1/DisputeTurn",
  DisputeWinnerSchema: "midgard/validation_dispute_v1/DisputeWinner",
  ReceiptRedeemerSchema: "midgard/validation_award_v1/ReceiptRedeemerV1",
  ValidationAwardSpendRedeemerSchema:
    "midgard/validation_award_v1/SpendRedeemer",
  SlashingReasonSchema: "midgard/operator_directory/SlashingReason",
  SlashingArgumentsSchema: "midgard/operator_directory/SlashingArguments",
  ValidationSourceMembershipSchema:
    "midgard/validation_claim_v1/ValidationSourceMembershipV1",
  ValidationClaimWitnessSchema:
    "midgard/validation_claim_v1/ValidationClaimWitnessV1",
  OperatorVerdictSchema: "midgard/rejection_reason_v1/OperatorVerdictV1",
  RejectionReasonSchema: "midgard/rejection_reason_v1/RejectionReasonV1",
  CommittedFieldClaimSchema:
    "midgard/fraud_proofs/canonical_decodability/rule/CommittedFieldClaimV1",
  CanonicalDecodabilityStep02StateSchema:
    "midgard/fraud_proofs/canonical_decodability/step_02/State",
  CommittedFieldShapeStep02StateSchema:
    "midgard/fraud_proofs/committed_field_shape/step_02/State",
  AddressSchema: "cardano/address/Address",
  DepositInfoSchema: "midgard/ledger_state/DepositInfo",
  WithdrawalBodySchema: "midgard/ledger_state/WithdrawalBody",
  EventHistoryPayloadSchema: "midgard/event_history/order_facts/Payload",
} as const satisfies Partial<Record<SdkExport, string>>;

/**
 * SDK wire types whose Aiken twin no validator names in its ABI, so the
 * blueprint publishes no definition to refine against. Each names what pins
 * its encoding instead: a golden channel, a constant in this file, or an Aiken
 * test. Shrink-only: an entry whose definition the blueprint gains fails until
 * it moves to `MIRRORS`.
 */
const NO_BLUEPRINT_DEFINITION = {
  ValidationTraceDescriptorSchema: {
    definition: "midgard/validation_trace_v1/ValidationTraceDescriptorV1",
    pinnedBy: "transition-trace-abi",
    reason: "no validator interface carries the descriptor",
  },
  MidgardTxValiditySchema: {
    definition: "midgard/ledger_state/MidgardTxValidity",
    pinnedBy: "VALIDITY_VECTORS",
    reason: "the compact wire carries validity as a plain Int",
  },
  CaseConfigSchema: {
    definition: "midgard/validation_case_config_v1/CaseConfigV1",
    pinnedBy: "onchain/aiken/lib/midgard/validation-case-v1.test.ak",
    reason: "an inline datum no validator names in its ABI",
  },
  LostDefenseReceiptSchema: {
    definition: "midgard/validation_case_v1/LostDefenseReceiptV1",
    pinnedBy: "onchain/aiken/validators/operator-lost-defense-slash.test.ak",
    reason: "an inline datum no validator names in its ABI",
  },
  ValidationAuxiliaryWitnessSchema: {
    definition:
      "midgard/validation_machine/machine_types/ValidationAuxiliaryWitnessV1",
    pinnedBy: "validation-auxiliary-witness-v1",
    reason: "crosses the wire as Data into the yield dispatchers' decodes",
  },
  ValidationProofItemDatumSchema: {
    definition:
      "midgard/validation_machine/machine_types/ValidationProofItemDatumV1",
    pinnedBy: "validation-one-step-cross-language",
    reason: "read as Data off an inline datum by the proof-item decoder",
  },
} as const satisfies Partial<
  Record<
    SdkExport,
    Readonly<{ definition: string; pinnedBy: string; reason: string }>
  >
>;

const GOLDEN_CHANNELS = new Set(
  (
    JSON.parse(
      readFileSync(
        path.join(
          repoRoot,
          ".agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json",
        ),
        "utf8",
      ),
    ) as { channels: readonly { id: string }[] }
  ).channels.map(({ id }) => id),
);

/** A copy of `schema` with constructor 0's fields `left` and `right` swapped. */
const withFieldsSwapped = (schema: unknown, left: number, right: number) => {
  const copy = structuredClone(schema) as { anyOf: [{ fields: unknown[] }] };
  const fields = copy.anyOf[0].fields;
  [fields[left], fields[right]] = [fields[right], fields[left]];
  return copy;
};

// The #640 format wave retired the five coarse rejection arms: the compact
// wire scalar is two-valued (0/1) and the rejection reason moved to the forced
// leaf's `OperatorVerdictV1`. Codes 2..5 are no longer decodable.
const VALIDITY_VECTORS = [
  ["TxIsValid", 0n, "d87980"],
  ["TxIsInvalid", 1n, "d87a80"],
] as const satisfies readonly [NativeMidgardTxValidity, bigint, string][];

describe("SDK/Aiken canonical V1 schema parity", () => {
  it("has no retired transaction-order validator, export, or parser", () => {
    const retiredName =
      /(?:TxOrder(?:Datum|Event|Payload)|ForcedInclusionTxV1)V(?:2|3)/u;
    expect(Object.keys(SDK).filter((name) => retiredName.test(name))).toEqual(
      [],
    );

    for (const relativePath of [
      "onchain/aiken/lib/midgard/user-events/tx-order-v2.ak",
      "onchain/aiken/lib/midgard/user-events/tx-order-v3.ak",
      "onchain/aiken/validators/user-events/tx-order-v2.ak",
      "onchain/aiken/validators/user-events/tx-order-v3.ak",
    ]) {
      expect(existsSync(path.join(repoRoot, relativePath)), relativePath).toBe(
        false,
      );
    }
  });

  it.each(Object.entries(MIRRORS))(
    "refines %s to %s recursively, field titles included",
    (name, definition) => {
      const schema: unknown = SDK[name as SdkExport];
      expect(schema, `missing SDK schema ${name}`).toBeDefined();
      expect(refines(schema, blueprintAbi.ref(definition))).toEqual([]);
    },
  );

  it("keeps the no-definition list from growing", () => {
    // Lower this with every entry deleted; never raise it.
    expect(Object.keys(NO_BLUEPRINT_DEFINITION)).toHaveLength(6);
  });

  it.each(Object.entries(NO_BLUEPRINT_DEFINITION))(
    "%s has no blueprint definition and a pin that exists",
    (name, { definition, pinnedBy }) => {
      expect(
        SDK[name as SdkExport],
        `missing SDK schema ${name}`,
      ).toBeDefined();
      expect(() => blueprintAbi.ref(definition)).toThrow(
        `Blueprint is missing definition "${definition}"`,
      );
      expect(
        pinnedBy === "VALIDITY_VECTORS" ||
          GOLDEN_CHANNELS.has(pinnedBy) ||
          existsSync(path.join(repoRoot, pinnedBy)),
        pinnedBy,
      ).toBe(true);
    },
  );

  it("reports two same-typed Int fields swapped in a TypeScript schema", () => {
    // withdrawal_count (9) and deposit_count (12) are both `Int`, so only
    // their titles tell the two encodings apart.
    expect(
      refines(
        withFieldsSwapped(SDK.HeaderSchema, 9, 12),
        blueprintAbi.ref("midgard/ledger_state/HeaderV1"),
      ),
    ).toEqual([
      {
        path: "$.HeaderV1.withdrawal_count",
        message:
          'field 9 is "withdrawal_count" in the blueprint, "depositCount" in TypeScript',
      },
      {
        path: "$.HeaderV1.deposit_count",
        message:
          'field 12 is "deposit_count" in the blueprint, "withdrawalCount" in TypeScript',
      },
    ]);
  });

  it.each(VALIDITY_VECTORS)(
    "binds raw native validity %s code %s to its Plutus constructor",
    (name, code, plutusDataCbor) => {
      expect(MidgardTxValidityCodes[name]).toBe(code);
      expect(encodeValidityCode(name)).toBe(code);
      expect(decodeValidityCode(code, "validity")).toBe(name);
      expect(Data.to(name as never, SDK.MidgardTxValiditySchema as never)).toBe(
        plutusDataCbor,
      );
      expect(Data.from(plutusDataCbor, SDK.MidgardTxValiditySchema)).toBe(name);
    },
  );

  it("rejects unknown raw and Plutus validity constructors", () => {
    expect(() => decodeValidityCode(2n, "validity")).toThrow(
      /Unsupported Midgard tx validity code/u,
    );
    expect(() => decodeValidityCode(6n, "validity")).toThrow(
      /Unsupported Midgard tx validity code/u,
    );
    expect(() => Data.from("d87b80", SDK.MidgardTxValiditySchema)).toThrow();
    expect(() => Data.from("d87f80", SDK.MidgardTxValiditySchema)).toThrow();
  });
});
