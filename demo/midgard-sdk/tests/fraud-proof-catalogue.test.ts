import { Data, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  decodeFraudProofCatalogueDatum,
  encodeFraudProofCatalogueDatum,
  FRAUD_PROOF_CATALOGUE_NODE_ASSET_NAME_PREFIX,
  FRAUD_PROOF_CATALOGUE_ROOT_ASSET_NAME,
  fraudProofCatalogueInnerRootDatum,
  fraudProofCatalogueMetadata,
  FraudProofCatalogueMetadata,
  FraudProofCatalogueMintRedeemer,
  fraudProofCatalogueNodeAssetName,
  fraudProofCatalogueNodeKey,
  fraudProofCatalogueNodeUnit,
  fraudProofCatalogueRootDatum,
  fraudProofCatalogueRootUnit,
  FraudProofCatalogueSpendRedeemer,
  fraudProofCatalogueStepDatum,
  fraudProofKeyFromId,
  FraudProofCatalogueIsLocked,
  fraudProofStepKey,
} from "../src/fraud-proof/catalogue.js";

const FRAUD_PROOF_0_ID = 0;
const FRAUD_PROOF_1_ID = 1;
const FRAUD_PROOF_256_ID = 256;
const FRAUD_PROOF_MAX_ID = 0xffffffffn;
const FRAUD_PROOF_OVERFLOW_ID = 0x1_0000_0000n;

const STEP_0_ID = 0;
const STEP_1_ID = 1;
const STEP_MAX_ID = 0xffffffffn;
const STEP_OVERFLOW_ID = 0x1_0000_0000n;
const STEP_COUNT = 4;

const FRAUD_PROOF_0_KEY = "00000000";
const FRAUD_PROOF_1_KEY = "00000001";
const FRAUD_PROOF_2_KEY = "00000002";
const FRAUD_PROOF_256_KEY = "00000100";
const MAX_KEY = "ffffffff";
const FRAUD_PROOF_1_STEP_0_NODE_KEY = "0000000100000000";
const FRAUD_PROOF_1_STEP_1_NODE_KEY = "0000000100000001";

const POLICY_ID = "aa".repeat(28);
const OUTPUT_0_INDEX = 0n;
const OUTPUT_1_INDEX = 1n;
const OUTPUT_2_INDEX = 2n;
const UNSAFE_NUMBER = Number.MAX_SAFE_INTEGER + 1;
const EMPTY_STEP_DATA = "";
const LIST_STATE_TRANSITION = "ListStateTransition";

const INIT_REDEEMER = { Init: { output_index: OUTPUT_0_INDEX } };
const ADD_FRAUD_PROOF_REDEEMER = {
  AddFraudProof: {
    fraud_proof_id: BigInt(FRAUD_PROOF_1_ID),
    step_output_count: BigInt(STEP_COUNT),
    m_root_ref_input_index: null,
  },
};
const ADD_FRAUD_PROOF_STEP_REDEEMER = {
  AddFraudProofStep: {
    fraud_proof_id: BigInt(FRAUD_PROOF_1_ID),
    step_id: BigInt(STEP_0_ID),
    continued_anchor_output_index: OUTPUT_1_INDEX,
    step_output_index: OUTPUT_2_INDEX,
  },
};
const LOCK_CATALOGUE_REDEEMER = {
  LockCatalogue: {
    root_input_index: OUTPUT_0_INDEX,
    continued_root_output_index: OUTPUT_1_INDEX,
  },
};

const roundTrip = <A>(value: A, schema: Parameters<typeof Data.to>[1]): A =>
  Data.from(Data.to(value, schema), schema) as A;

const roundTripDatum = (
  datum: ReturnType<typeof fraudProofCatalogueRootDatum>,
): ReturnType<typeof fraudProofCatalogueRootDatum> =>
  decodeFraudProofCatalogueDatum(encodeFraudProofCatalogueDatum(datum));

describe("fraud-proof catalogue pure helpers", () => {
  it("encodes fraud-proof ids as fixed-width big-endian keys", () => {
    expect(fraudProofKeyFromId(FRAUD_PROOF_0_ID)).toBe(FRAUD_PROOF_0_KEY);
    expect(fraudProofKeyFromId(FRAUD_PROOF_1_ID)).toBe(FRAUD_PROOF_1_KEY);
    expect(fraudProofKeyFromId(FRAUD_PROOF_256_ID)).toBe(FRAUD_PROOF_256_KEY);
    expect(fraudProofKeyFromId(FRAUD_PROOF_MAX_ID)).toBe(MAX_KEY);

    expect(() => fraudProofKeyFromId(-1)).toThrow(/non-negative/);
    expect(() => fraudProofKeyFromId(FRAUD_PROOF_OVERFLOW_ID)).toThrow(
      /4 bytes/,
    );
    expect(() => fraudProofKeyFromId(UNSAFE_NUMBER)).toThrow(/safe integer/);
  });

  it("encodes step ids and catalogue node keys", () => {
    expect(fraudProofStepKey(STEP_0_ID)).toBe(FRAUD_PROOF_0_KEY);
    expect(fraudProofStepKey(STEP_1_ID)).toBe(FRAUD_PROOF_1_KEY);
    expect(fraudProofStepKey(STEP_MAX_ID)).toBe(MAX_KEY);
    expect(fraudProofCatalogueNodeKey(FRAUD_PROOF_1_KEY, STEP_0_ID)).toBe(
      FRAUD_PROOF_1_STEP_0_NODE_KEY,
    );
    expect(fraudProofCatalogueNodeKey(FRAUD_PROOF_1_KEY, STEP_1_ID)).toBe(
      FRAUD_PROOF_1_STEP_1_NODE_KEY,
    );

    expect(() => fraudProofStepKey(-1)).toThrow(/non-negative/);
    expect(() => fraudProofStepKey(STEP_OVERFLOW_ID)).toThrow(/4 bytes/);
  });

  it("builds catalogue asset names and units", () => {
    const validator = {
      policyId: POLICY_ID,
    } as Parameters<typeof fraudProofCatalogueRootUnit>[0];
    const nodeAssetName = `${FRAUD_PROOF_CATALOGUE_NODE_ASSET_NAME_PREFIX}${FRAUD_PROOF_1_STEP_1_NODE_KEY}`;

    expect(
      fraudProofCatalogueNodeAssetName(FRAUD_PROOF_1_STEP_1_NODE_KEY),
    ).toBe(nodeAssetName);
    expect(fraudProofCatalogueRootUnit(validator)).toBe(
      toUnit(POLICY_ID, FRAUD_PROOF_CATALOGUE_ROOT_ASSET_NAME),
    );
    expect(
      fraudProofCatalogueNodeUnit(validator, FRAUD_PROOF_1_STEP_1_NODE_KEY),
    ).toBe(toUnit(POLICY_ID, nodeAssetName));
  });

  it("constructs metadata with integer step counts", () => {
    expect(fraudProofCatalogueMetadata(STEP_COUNT)).toEqual({
      step_count: BigInt(STEP_COUNT),
    });
    expect(fraudProofCatalogueMetadata(BigInt(STEP_COUNT))).toEqual({
      step_count: BigInt(STEP_COUNT),
    });
    expect(() => fraudProofCatalogueMetadata(UNSAFE_NUMBER)).toThrow(
      /safe integer/,
    );
  });

  it("round-trips catalogue root datums", () => {
    const unlockedRoot = roundTripDatum(
      fraudProofCatalogueRootDatum(false, null),
    );
    if (!("Root" in unlockedRoot.data)) {
      throw new Error("Expected root datum");
    }
    expect(
      Data.castFrom(unlockedRoot.data.Root.data, FraudProofCatalogueIsLocked),
    ).toBe(false);
    expect(unlockedRoot.link).toBeNull();

    const lockedRoot = roundTripDatum(
      fraudProofCatalogueRootDatum(true, FRAUD_PROOF_1_KEY),
    );
    if (!("Root" in lockedRoot.data)) {
      throw new Error("Expected root datum");
    }
    expect(
      Data.castFrom(lockedRoot.data.Root.data, FraudProofCatalogueIsLocked),
    ).toBe(true);
    expect(lockedRoot.link).toBe(FRAUD_PROOF_1_KEY);
  });

  it("round-trips catalogue inner root and step datums", () => {
    const innerRoot = roundTripDatum(
      fraudProofCatalogueInnerRootDatum(
        STEP_COUNT,
        FRAUD_PROOF_1_STEP_0_NODE_KEY,
        FRAUD_PROOF_2_KEY,
      ),
    );

    if (!("InnerRoot" in innerRoot.data)) {
      throw new Error("Expected inner root datum");
    }
    expect(
      Data.castFrom(innerRoot.data.InnerRoot.data, FraudProofCatalogueMetadata),
    ).toEqual({ step_count: BigInt(STEP_COUNT) });
    expect(innerRoot.data.InnerRoot.child_link).toBe(
      FRAUD_PROOF_1_STEP_0_NODE_KEY,
    );
    expect(innerRoot.link).toBe(FRAUD_PROOF_2_KEY);

    const step = roundTripDatum(fraudProofCatalogueStepDatum(null));
    if (!("Node" in step.data)) {
      throw new Error("Expected step node datum");
    }
    expect(step.data.Node.data).toBe(EMPTY_STEP_DATA);
    expect(step.link).toBeNull();
  });

  it("round-trips catalogue mint and spend redeemers", () => {
    expect(roundTrip(INIT_REDEEMER, FraudProofCatalogueMintRedeemer)).toEqual(
      INIT_REDEEMER,
    );
    expect(
      roundTrip(ADD_FRAUD_PROOF_REDEEMER, FraudProofCatalogueMintRedeemer),
    ).toEqual(ADD_FRAUD_PROOF_REDEEMER);
    expect(
      roundTrip(ADD_FRAUD_PROOF_STEP_REDEEMER, FraudProofCatalogueMintRedeemer),
    ).toEqual(ADD_FRAUD_PROOF_STEP_REDEEMER);
    expect(
      roundTrip(LIST_STATE_TRANSITION, FraudProofCatalogueSpendRedeemer),
    ).toBe(LIST_STATE_TRANSITION);
    expect(
      roundTrip(LOCK_CATALOGUE_REDEEMER, FraudProofCatalogueSpendRedeemer),
    ).toEqual(LOCK_CATALOGUE_REDEEMER);
  });
});
