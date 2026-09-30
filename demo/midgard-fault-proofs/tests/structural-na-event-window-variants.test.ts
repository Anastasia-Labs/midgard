/**
 * Q47 structural-N/A executable twins (GOAL_SPEC.md §9.1).
 *
 * Q47 (omitted / out-of-window deposit, withdrawal and forced-event variants)
 * has no standalone proof family: all six violation variants are constructors
 * of the shared `transitionTrace` family. The on-chain half of the disposition
 * lives in
 * `onchain/aiken/lib/midgard/fraud-proofs/transition-trace/structural-na-q47-event-window-variants.test.ak`.
 *
 * These are the off-chain twins of those eight Aiken selectors. Each one
 * encodes a `TransitionFault` through the deployed SDK schema and asserts the
 * exact Plutus Data shape the Aiken constructors expect. The expected
 * constructor indices and field orders are read out of the compiled
 * blueprint's `definitions` section, so the SDK encoder is checked against the
 * compiler's own ABI rather than against indices a human copied from an Aiken
 * source comment. Two further twins pin the discriminators the Aiken negative
 * controls exercise -- the root domain and the event id -- by decoding both
 * encodings and requiring the difference to land on exactly that leaf.
 *
 * These tests assert encoding agreement, not file existence.
 */

import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "./support/blueprint-abi.js";
import "./structural-na-event-window-variants.expect-arm-matches-blueprint.js";
import "./structural-na-event-window-variants.omitted-withdrawal-fault.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ASSET_NAME,
  constructorRendering,
  encodedFault,
  EVENT_KEY_DEF,
  expectArmMatchesBlueprint,
  MEMBERSHIP_FIELDS,
  membershipProofDef,
  NON_MEMBERSHIP_FIELDS,
  nonMembershipProofDef,
  OMITTED_WITNESS_DEF,
  omittedDepositFault,
  OPERATOR_VERDICT_DEF,
  OUT_OF_WINDOW_WITNESS_DEF,
  outRef,
  REF_INDEX,
  renderData,
} from "./structural-na-event-window-variants.expect-arm-matches-blueprint.js";
import {
  omittedForcedFault,
  omittedWithdrawalFault,
  outOfWindowDepositFault,
  outOfWindowForcedFault,
  outOfWindowWithdrawalFault,
} from "./structural-na-event-window-variants.omitted-withdrawal-fault.js";
import { blueprintConstructor } from "./support/blueprint-abi.js";

describe("Q47 omitted / out-of-window event-window variants", () => {
  it("encodes the omitted-due deposit variant exactly as the Aiken constructor", () => {
    expectArmMatchesBlueprint({
      fault: omittedDepositFault(),
      outerTitle: "OmittedDueL1Event",
      witnessDefinition: OMITTED_WITNESS_DEF,
      witnessTitle: "OmittedDueDeposit",
      expectedFieldTitles: ["source_non_membership"],
      expectedScalars: {},
      proof: {
        definitionKey: nonMembershipProofDef("DepositId"),
        constructorTitle: "RootNonMembershipProof",
        expectedFieldTitles: NON_MEMBERSHIP_FIELDS,
        domainConstructorTitle: "DepositsRootDomain",
      },
    });
  });

  it("encodes the omitted-due withdrawal variant exactly as the Aiken constructor", () => {
    expectArmMatchesBlueprint({
      fault: omittedWithdrawalFault(),
      outerTitle: "OmittedDueL1Event",
      witnessDefinition: OMITTED_WITNESS_DEF,
      witnessTitle: "OmittedDueWithdrawal",
      expectedFieldTitles: ["source_non_membership"],
      expectedScalars: {},
      proof: {
        definitionKey: nonMembershipProofDef("WithdrawalId"),
        constructorTitle: "RootNonMembershipProof",
        expectedFieldTitles: NON_MEMBERSHIP_FIELDS,
        domainConstructorTitle: "WithdrawalsRootDomain",
      },
    });
  });

  it("encodes the omitted-due forced-transaction variant with its validity override", () => {
    expectArmMatchesBlueprint({
      fault: omittedForcedFault(),
      outerTitle: "OmittedDueL1Event",
      witnessDefinition: OMITTED_WITNESS_DEF,
      witnessTitle: "OmittedDueForcedTransaction",
      expectedFieldTitles: [
        "event_ref_input_index",
        "event_asset_name",
        "validity_override",
        "source_non_membership",
      ],
      expectedScalars: {
        event_ref_input_index: REF_INDEX,
        event_asset_name: ASSET_NAME,
        validity_override: constructorRendering(
          OPERATOR_VERDICT_DEF,
          "ForcedTxValid",
        ),
      },
      proof: {
        definitionKey: nonMembershipProofDef("TxOrderId"),
        constructorTitle: "RootNonMembershipProof",
        expectedFieldTitles: NON_MEMBERSHIP_FIELDS,
        domainConstructorTitle: "ForcedTransactionsV1RootDomain",
      },
    });
  });

  it("encodes the out-of-window deposit variant exactly as the Aiken constructor", () => {
    expectArmMatchesBlueprint({
      fault: outOfWindowDepositFault(),
      outerTitle: "OutOfWindowSourceEvent",
      witnessDefinition: OUT_OF_WINDOW_WITNESS_DEF,
      witnessTitle: "OutOfWindowDeposit",
      expectedFieldTitles: ["source_membership"],
      expectedScalars: {},
      proof: {
        definitionKey: membershipProofDef("DepositId", "DepositInfo"),
        constructorTitle: "RootMembershipProof",
        expectedFieldTitles: MEMBERSHIP_FIELDS,
        domainConstructorTitle: "DepositsRootDomain",
      },
    });
  });

  it("encodes the out-of-window withdrawal source leaf without a separate verdict override", () => {
    expectArmMatchesBlueprint({
      fault: outOfWindowWithdrawalFault(),
      outerTitle: "OutOfWindowSourceEvent",
      witnessDefinition: OUT_OF_WINDOW_WITNESS_DEF,
      witnessTitle: "OutOfWindowWithdrawal",
      expectedFieldTitles: ["source_membership"],
      expectedScalars: {},
      proof: {
        definitionKey: membershipProofDef("WithdrawalId", "WithdrawalInfo"),
        constructorTitle: "RootMembershipProof",
        expectedFieldTitles: MEMBERSHIP_FIELDS,
        domainConstructorTitle: "WithdrawalsRootDomain",
      },
    });
  });

  it("encodes the out-of-window forced-transaction variant with its validity override", () => {
    expectArmMatchesBlueprint({
      fault: outOfWindowForcedFault(),
      outerTitle: "OutOfWindowSourceEvent",
      witnessDefinition: OUT_OF_WINDOW_WITNESS_DEF,
      witnessTitle: "OutOfWindowForcedTransaction",
      expectedFieldTitles: [
        "event_ref_input_index",
        "event_asset_name",
        "validity_override",
        "source_membership",
      ],
      expectedScalars: {
        event_ref_input_index: REF_INDEX,
        event_asset_name: ASSET_NAME,
        validity_override: constructorRendering(
          OPERATOR_VERDICT_DEF,
          "ForcedTxValid",
        ),
      },
      proof: {
        definitionKey: membershipProofDef("TxOrderId", "ForcedInclusionTxV1"),
        constructorTitle: "RootMembershipProof",
        expectedFieldTitles: MEMBERSHIP_FIELDS,
        domainConstructorTitle: "ForcedTransactionsV1RootDomain",
      },
    });
  });

  it.each([
    ["omitted deposit", omittedDepositFault],
    ["omitted withdrawal", omittedWithdrawalFault],
    ["outside deposit", outOfWindowDepositFault],
    ["outside withdrawal", outOfWindowWithdrawalFault],
  ] as const)(
    "rejects obsolete pointer-bearing %s witness bytes",
    (_name, fixture) => {
      const raw = Data.from(Data.to(fixture(), SDK.TransitionFault));
      if (!(raw instanceof Constr) || !(raw.fields[0] instanceof Constr))
        throw new Error("Expected a timed fault and its witness constructor");
      const witness = raw.fields[0];
      expect(witness.fields).toHaveLength(1);
      witness.fields.unshift(999n, "00");
      if (raw.index === 7 && witness.index === 1)
        witness.fields.splice(2, 0, new Constr(0, []));
      expect(() => Data.from(Data.to(raw), SDK.TransitionFault)).toThrow();
    },
  );

  it("keeps the root domain a load-bearing discriminator off-chain", () => {
    // Twin of the Aiken selector q47_wrong_domain_rejects: only the domain
    // differs, the root and count are identical, yet the difference must land
    // on the blueprint's `domain` leaf of the non-membership proof so the
    // on-chain domain equality check cannot be bypassed.
    const proofFields = (fault: SDK.TransitionFault): readonly unknown[] => {
      const proof = encodedFault(fault).witnessFields[0];
      if (!(proof instanceof Constr)) {
        throw new Error("source_non_membership must encode as a constructor.");
      }
      return proof.fields;
    };
    const declared = blueprintConstructor(
      nonMembershipProofDef("WithdrawalId"),
      "RootNonMembershipProof",
    );
    expect(declared.fieldTitles).toEqual(NON_MEMBERSHIP_FIELDS);

    const correct = proofFields(omittedWithdrawalFault());
    const wrongDomain = proofFields(
      omittedWithdrawalFault({ domain: SDK.ROOT_DOMAINS.deposits }),
    );
    const differing = declared.fieldTitles.filter(
      (_title, position) =>
        renderData(correct[position]) !== renderData(wrongDomain[position]),
    );
    expect(differing).toEqual(["domain"]);
  });

  it("keeps the event id a load-bearing discriminator off-chain", () => {
    // Twin of the Aiken selector q47_wrong_event_id_rejects.
    const proofFields = (fault: SDK.TransitionFault): readonly unknown[] => {
      const proof = encodedFault(fault).witnessFields[0];
      if (!(proof instanceof Constr)) {
        throw new Error("source_non_membership must encode as a constructor.");
      }
      return proof.fields;
    };
    const declared = blueprintConstructor(
      nonMembershipProofDef("WithdrawalId"),
      "RootNonMembershipProof",
    );
    const correct = proofFields(omittedWithdrawalFault());
    const wrongId = proofFields(omittedWithdrawalFault({ key: outRef(7n) }));
    const differing = declared.fieldTitles.filter(
      (_title, position) =>
        renderData(correct[position]) !== renderData(wrongId[position]),
    );
    expect(differing).toEqual(["key"]);

    // The three event domains must also map to three distinct EventKey
    // constructors, so a withdrawal id can never be read as a deposit id.
    const keys = [
      { DepositEventKey: { deposit_id: outRef(0n) } },
      { WithdrawalEventKey: { withdrawal_id: outRef(0n) } },
      { ForcedTransactionEventKey: { tx_order_id: outRef(0n) } },
    ] as const satisfies readonly SDK.EventKey[];
    const encodedKeys = keys.map((key) => Data.to(key, SDK.EventKey));
    expect(new Set(encodedKeys).size).toBe(3);
    // Each EventKey constructor must sit at the index the compiled ABI
    // declares, so an on-chain reader cannot mistake one event kind for
    // another.
    for (const [position, title] of [
      "DepositEventKey",
      "WithdrawalEventKey",
      "ForcedTransactionEventKey",
    ].entries()) {
      const decoded = Data.from(encodedKeys[position]);
      expect(
        renderData(decoded).startsWith(
          `C${blueprintConstructor(EVENT_KEY_DEF, title).index.toString()}(`,
        ),
        `${title} constructor index`,
      ).toBe(true);
    }
  });
});
