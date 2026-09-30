import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { blueprintConstructor } from "./support/blueprint-abi.js";

const TRANSITION_FAULT_DEF =
  "midgard/fraud_proofs/transition_trace/proof/TransitionFault";

export const OMITTED_WITNESS_DEF =
  "midgard/fraud_proofs/transition_trace/proof/OmittedDueL1EventWitness";

export const OUT_OF_WINDOW_WITNESS_DEF =
  "midgard/fraud_proofs/transition_trace/proof/OutOfWindowSourceEventWitness";

const ROOT_DOMAIN_DEF = "midgard/transition_trace/RootDomain";

export const OPERATOR_VERDICT_DEF =
  "midgard/rejection_reason_v1/OperatorVerdictV1";

export const EVENT_KEY_DEF = "midgard/ledger_state/EventKey";

export const nonMembershipProofDef = (idType: string): string =>
  `midgard/transition_trace/RootNonMembershipProof<midgard/ledger_state/${idType}>`;

export const membershipProofDef = (idType: string, infoType: string): string =>
  `midgard/transition_trace/RootMembershipProof<midgard/ledger_state/${idType},midgard/ledger_state/${infoType}>`;

export const NON_MEMBERSHIP_FIELDS = [
  "domain",
  "root",
  "phas_root",
  "count",
  "key",
  "proof",
] as const;

export const MEMBERSHIP_FIELDS = [
  "domain",
  "root",
  "phas_root",
  "count",
  "key",
  "value",
  "proof",
] as const;

/** Rendering of a nullary enum constructor at a blueprint-declared index. */
export const constructorRendering = (
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

export const EVENT_ASSET_NAME = "01";

const H28_A = "aa".repeat(28);

/**
 * Deliberately not 0: with a distinct value at `event_ref_input_index` a
 * transposition of the two leading scalar fields cannot encode identically.
 */
export const EVENT_REF_INPUT_INDEX = 3n;

export const outRef = (index: bigint): SDK.OutputReference => ({
  transactionId: H32_A,
  outputIndex: index,
});

export const nonMembership = <K>({
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

export const membership = <K, V>({
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

export const withdrawalInfo = (
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

export const depositInfo: SDK.DepositInfo = {
  l2_address: {
    paymentCredential: { ScriptCredential: [H28_A] },
    stakeCredential: null,
  },
  l2_network_id: 0n,
  l2_datum: null,
};

export const forcedInclusionTx: SDK.ForcedInclusionTxV1 = {
  tx_id: H32_A,
  submitted_source: {
    compact_cbor: "80",
    witness_set_compact_cbor: "80",
    field_preimage_lengths_cbor: "80",
  },
  verdict: "ForcedTxValid",
};

/** Stable structural rendering of a decoded Plutus Data value. */
export const renderData = (value: unknown): string => {
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
export const encodedFault = (
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
export const expectArmMatchesBlueprint = ({
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

export const REF_INDEX = `I${EVENT_REF_INPUT_INDEX.toString()}`;

export const ASSET_NAME = `B${EVENT_ASSET_NAME}`;

export const omittedDepositFault = (): SDK.TransitionFault =>
  SDK.omittedDueL1EventFault({
    OmittedDueDeposit: {
      source_non_membership: nonMembership({
        domain: SDK.ROOT_DOMAINS.deposits,
        key: outRef(0n),
      }) as SDK.DepositSourceNonMembershipProof,
    },
  });
