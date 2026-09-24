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

import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { blueprintConstructor } from "./support/blueprint-abi.js";

const TRANSITION_FAULT_DEF =
  "midgard/fraud_proofs/transition_trace/proof/TransitionFault";
const OMITTED_WITNESS_DEF =
  "midgard/fraud_proofs/transition_trace/proof/OmittedDueL1EventWitness";
const OUT_OF_WINDOW_WITNESS_DEF =
  "midgard/fraud_proofs/transition_trace/proof/OutOfWindowSourceEventWitness";
const ROOT_DOMAIN_DEF = "midgard/transition_trace/RootDomain";
const OPERATOR_VERDICT_DEF = "midgard/rejection_reason_v1/OperatorVerdictV1";
const EVENT_KEY_DEF = "midgard/ledger_state/EventKey";
const nonMembershipProofDef = (idType: string): string =>
  `midgard/transition_trace/RootNonMembershipProof<midgard/ledger_state/${idType}>`;
const membershipProofDef = (idType: string, infoType: string): string =>
  `midgard/transition_trace/RootMembershipProof<midgard/ledger_state/${idType},midgard/ledger_state/${infoType}>`;
const NON_MEMBERSHIP_FIELDS = [
  "domain",
  "root",
  "phas_root",
  "count",
  "key",
  "proof",
] as const;
const MEMBERSHIP_FIELDS = [
  "domain",
  "root",
  "phas_root",
  "count",
  "key",
  "value",
  "proof",
] as const;
/** Rendering of a nullary enum constructor at a blueprint-declared index. */
const constructorRendering = (
  definitionKey: string,
  constructorTitle: string,
): string =>
  `C${blueprintConstructor(definitionKey, constructorTitle).index.toString()}()`;

const H32_A =
  "1111111111111111111111111111111111111111111111111111111111111111";
const H32_B =
  "2222222222222222222222222222222222222222222222222222222222222222";
const H64_A = "33".repeat(64);
const EMPTY_ROOT =
  "0000000000000000000000000000000000000000000000000000000000000000";
const EVENT_ASSET_NAME = "01";
const H28_A = "aa".repeat(28);
/**
 * Deliberately not 0: with a distinct value at `event_ref_input_index` a
 * transposition of the two leading scalar fields cannot encode identically.
 */
const EVENT_REF_INPUT_INDEX = 3n;

const outRef = (index: bigint): SDK.OutputReference => ({
  transactionId: H32_A,
  outputIndex: index,
});

const nonMembership = <K>({
  domain,
  key,
}: {
  readonly domain: SDK.RootDomain;
  readonly key: K;
}) => ({
  domain,
  root: EMPTY_ROOT,
  phas_root: EMPTY_ROOT,
  count: 0n,
  key,
  proof: [],
});

const membership = <K, V>({
  domain,
  key,
  value,
}: {
  readonly domain: SDK.RootDomain;
  readonly key: K;
  readonly value: V;
}) => ({
  domain,
  root: H32_B,
  phas_root: H32_B,
  count: 1n,
  key,
  value,
  proof: [],
});

const withdrawalInfo = (
  validity: SDK.WithdrawalValidity,
): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: outRef(9n),
    l2_owner: H28_A,
    l2_value: new Map(),
    l1_address: {
      paymentCredential: { ScriptCredential: [H28_A] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: [H32_A, H64_A],
  validity,
});

const depositInfo: SDK.DepositInfo = {
  l2_address: {
    paymentCredential: { ScriptCredential: [H28_A] },
    stakeCredential: null,
  },
  l2_network_id: 0n,
  l2_datum: null,
};

const forcedInclusionTx: SDK.ForcedInclusionTxV1 = {
  tx_id: H32_A,
  submitted_source: {
    compact_cbor: "80",
    witness_set_compact_cbor: "80",
    field_preimage_lengths_cbor: "80",
  },
  verdict: "ForcedTxValid",
};

/** Stable structural rendering of a decoded Plutus Data value. */
const renderData = (value: unknown): string => {
  if (value instanceof Constr) {
    return `C${value.index.toString()}(${value.fields.map(renderData).join(",")})`;
  }
  if (Array.isArray(value)) {
    return `[${value.map(renderData).join(",")}]`;
  }
  if (value instanceof Map) {
    return `{${[...value.entries()]
      .map(([key, entry]) => `${renderData(key)}:${renderData(entry)}`)
      .join(",")}}`;
  }
  if (typeof value === "bigint") {
    return `I${value.toString()}`;
  }
  return `B${String(value)}`;
};

/**
 * Encodes a fault through the deployed schema and decodes the raw Plutus Data
 * so constructor indices, field arity and field values can be asserted
 * directly rather than inferred from a hex prefix.
 */
const encodedFault = (
  fault: SDK.TransitionFault,
): {
  readonly cbor: string;
  readonly outerIndex: number;
  readonly witnessIndex: number;
  readonly witnessFields: readonly unknown[];
} => {
  const cbor = Data.to(fault, SDK.TransitionFault);
  const outer = Data.from(cbor);
  if (!(outer instanceof Constr) || outer.fields.length !== 1) {
    throw new Error(
      "TransitionFault must encode as a single-field constructor wrapping its witness record.",
    );
  }
  // The single-field `{ witness }` record is flattened by the enum encoding,
  // so the outer constructor's only field is the witness constructor itself.
  const witness = outer.fields[0];
  if (!(witness instanceof Constr)) {
    throw new Error("The witness must encode as a constructor.");
  }
  return {
    cbor,
    outerIndex: outer.index,
    witnessIndex: witness.index,
    witnessFields: witness.fields,
  };
};

/**
 * Asserts one arm against the compiled ABI: the outer `TransitionFault`
 * constructor index, the witness constructor index, the declared field order,
 * the value the SDK placed at each declared scalar field position, and the
 * shape of the (non-)membership sub-record in the trailing field.
 */
const expectArmMatchesBlueprint = ({
  fault,
  outerTitle,
  witnessDefinition,
  witnessTitle,
  expectedFieldTitles,
  expectedScalars,
  proof,
}: {
  readonly fault: SDK.TransitionFault;
  readonly outerTitle: string;
  readonly witnessDefinition: string;
  readonly witnessTitle: string;
  readonly expectedFieldTitles: readonly string[];
  readonly expectedScalars: Readonly<Record<string, string>>;
  readonly proof: {
    readonly definitionKey: string;
    readonly constructorTitle: string;
    readonly expectedFieldTitles: readonly string[];
    readonly domainConstructorTitle: string;
  };
}): void => {
  const outer = blueprintConstructor(TRANSITION_FAULT_DEF, outerTitle);
  const witness = blueprintConstructor(witnessDefinition, witnessTitle);
  const encoded = encodedFault(fault);

  expect(
    witness.fieldTitles,
    `${witnessTitle} field order declared by the compiled blueprint`,
  ).toEqual(expectedFieldTitles);
  expect(encoded.outerIndex, `${outerTitle} constructor index`).toBe(
    outer.index,
  );
  expect(encoded.witnessIndex, `${witnessTitle} constructor index`).toBe(
    witness.index,
  );
  expect(encoded.witnessFields, `${witnessTitle} field arity`).toHaveLength(
    witness.fieldTitles.length,
  );
  for (const [position, title] of witness.fieldTitles.entries()) {
    const expected = expectedScalars[title];
    if (expected === undefined) {
      continue;
    }
    expect(
      renderData(encoded.witnessFields[position]),
      `${witnessTitle}.${title} at field ${position.toString()}`,
    ).toBe(expected);
  }

  // The trailing field carries the (non-)membership proof. Its constructor
  // index, arity and `domain` leaf all come from the compiled ABI.
  const proofPosition = witness.fieldTitles.length - 1;
  const proofValue = encoded.witnessFields[proofPosition];
  const declaredProof = blueprintConstructor(
    proof.definitionKey,
    proof.constructorTitle,
  );
  const declaredDomain = blueprintConstructor(
    ROOT_DOMAIN_DEF,
    proof.domainConstructorTitle,
  );
  expect(declaredProof.fieldTitles).toEqual(proof.expectedFieldTitles);
  if (!(proofValue instanceof Constr)) {
    throw new Error(
      `${witnessTitle}.${witness.fieldTitles[proofPosition]} must encode as a constructor.`,
    );
  }
  expect(proofValue.index, `${proof.constructorTitle} constructor index`).toBe(
    declaredProof.index,
  );
  expect(
    proofValue.fields,
    `${proof.constructorTitle} field arity`,
  ).toHaveLength(declaredProof.fieldTitles.length);
  expect(
    renderData(proofValue.fields[declaredProof.fieldTitles.indexOf("domain")]),
    `${proof.constructorTitle}.domain`,
  ).toBe(`C${declaredDomain.index.toString()}()`);

  // The schema must also read back exactly what it wrote; this is a secondary
  // check, not the compatibility oracle above.
  expect(
    Data.to(Data.from(encoded.cbor, SDK.TransitionFault), SDK.TransitionFault),
  ).toBe(encoded.cbor);
};

const REF_INDEX = `I${EVENT_REF_INPUT_INDEX.toString()}`;
const ASSET_NAME = `B${EVENT_ASSET_NAME}`;

const omittedDepositFault = (): SDK.TransitionFault =>
  SDK.omittedDueL1EventFault({
    OmittedDueDeposit: {
      source_non_membership: nonMembership({
        domain: SDK.ROOT_DOMAINS.deposits,
        key: outRef(0n),
      }) as SDK.DepositSourceNonMembershipProof,
    },
  });

const omittedWithdrawalFault = ({
  domain = SDK.ROOT_DOMAINS.withdrawals,
  key = outRef(0n),
}: {
  readonly domain?: SDK.RootDomain;
  readonly key?: SDK.OutputReference;
} = {}): SDK.TransitionFault =>
  SDK.omittedDueL1EventFault({
    OmittedDueWithdrawal: {
      source_non_membership: nonMembership({
        domain,
        key,
      }) as SDK.WithdrawalSourceNonMembershipProof,
    },
  });

const omittedForcedFault = (): SDK.TransitionFault =>
  SDK.omittedDueL1EventFault({
    OmittedDueForcedTransaction: {
      event_ref_input_index: EVENT_REF_INPUT_INDEX,
      event_asset_name: EVENT_ASSET_NAME,
      validity_override: "ForcedTxValid",
      source_non_membership: nonMembership({
        domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
        key: outRef(0n),
      }) as SDK.ForcedTransactionSourceNonMembershipProof,
    },
  });

const outOfWindowDepositFault = (): SDK.TransitionFault =>
  SDK.outOfWindowSourceEventFault({
    OutOfWindowDeposit: {
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.deposits,
        key: outRef(0n),
        value: depositInfo,
      }) as SDK.DepositSourceMembershipProof,
    },
  });

const outOfWindowWithdrawalFault = (): SDK.TransitionFault =>
  SDK.outOfWindowSourceEventFault({
    OutOfWindowWithdrawal: {
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.withdrawals,
        key: outRef(0n),
        value: withdrawalInfo("WithdrawalIsValid"),
      }) as SDK.WithdrawalSourceMembershipProof,
    },
  });

const outOfWindowForcedFault = (): SDK.TransitionFault =>
  SDK.outOfWindowSourceEventFault({
    OutOfWindowForcedTransaction: {
      event_ref_input_index: EVENT_REF_INPUT_INDEX,
      event_asset_name: EVENT_ASSET_NAME,
      validity_override: "ForcedTxValid",
      source_membership: membership({
        domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
        key: outRef(0n),
        value: forcedInclusionTx,
      }) as SDK.ForcedTransactionSourceMembershipProof,
    },
  });

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
