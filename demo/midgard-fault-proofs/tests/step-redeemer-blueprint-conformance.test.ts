import { existsSync, readdirSync } from "node:fs";
import { isDeepStrictEqual } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import {
  type BlueprintMismatch,
  refines,
} from "@al-ft/midgard-sdk/blueprint-abi";
import { describe, expect, it } from "vitest";

import * as FieldItemWidthIllegal from "../src/field-item-width-illegal/schemas.js";
import * as L2TxMistag from "../src/l2-tx-mistag/schemas.js";
import * as MintDeclaredAssetLimit from "../src/mint-declared-asset-limit/schemas.js";
import * as MintItemNonCanonical from "../src/mint-item-non-canonical/schemas.js";
import * as ObserverOrderInvalid from "../src/observer-order-invalid/schemas.js";
import * as ObserversForbiddenOnUntaggedNetwork from "../src/observers-forbidden-on-untagged-network/schemas.js";
import * as OutputReferenceScriptDecoding from "../src/output-reference-script-decoding/schemas.js";
import * as ProtectedOutputSignerMissing from "../src/protected-output-signer-missing/schemas.js";
import * as RedeemerCanonicity from "../src/redeemer-canonicity/schemas.js";
import * as ResolvedOutputNonCanonical from "../src/resolved-output-non-canonical/schemas.js";
import * as ScriptIntegrityHashMissing from "../src/script-integrity-hash-missing/schemas.js";
import * as SpendInputSignerMissing from "../src/spend-input-signer-missing/schemas.js";
import * as TransactionOutputNonCanonical from "../src/transaction-output-non-canonical/schemas.js";
import * as ValueNotPreserved from "../src/value-not-preserved/schemas.js";
import type { FamilyCategory } from "../src/workflow/family-definition.js";
import * as ZeroInput from "../src/zero-input/schemas.js";
import {
  STEP_REDEEMER_ALLOWANCES,
  UNPAIRED_STEP_REDEEMER_GROUPS,
} from "./step-redeemer-blueprint-conformance.allowances.js";
import {
  allowedMismatches,
  REGISTERED_STEP_VALIDATORS,
  type RegisteredStepValidator,
  unreachedAllowances,
} from "./support/registered-step-validators.js";

type Exports = Readonly<Record<string, unknown>>;

/**
 * Every fault-proofs family schema module (`src/<family>/schemas.ts`), by
 * Aiken family directory. A test below keeps it complete.
 */
const FAMILY_SCHEMA_MODULES: Readonly<Record<string, Exports>> = {
  field_item_width_illegal: FieldItemWidthIllegal,
  l2_tx_mistag: L2TxMistag,
  mint_declared_asset_limit: MintDeclaredAssetLimit,
  mint_item_non_canonical: MintItemNonCanonical,
  observer_order_invalid: ObserverOrderInvalid,
  observers_forbidden_on_untagged_network: ObserversForbiddenOnUntaggedNetwork,
  output_reference_script_decoding: OutputReferenceScriptDecoding,
  protected_output_signer_missing: ProtectedOutputSignerMissing,
  redeemer_canonicity: RedeemerCanonicity,
  resolved_output_non_canonical: ResolvedOutputNonCanonical,
  script_integrity_hash_missing: ScriptIntegrityHashMissing,
  spend_input_signer_missing: SpendInputSignerMissing,
  transaction_output_non_canonical: TransactionOutputNonCanonical,
  value_not_preserved: ValueNotPreserved,
  zero_input: ZeroInput,
};

type TwinSources = Readonly<{
  sdk: Exports;
  modules: Readonly<Record<string, Exports>>;
}>;

const SOURCES: TwinSources = { sdk: SDK, modules: FAMILY_SCHEMA_MODULES };

const pascal = (name: string): string =>
  name
    .split("_")
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join("");

type RedeemerTwin = Readonly<{ twin: string; schema: unknown }>;

/**
 * The convention twins of `fraud_proofs/<family>/<validator>.main.spend`:
 * the SDK's `<Family><Validator>SpendRedeemerSchema` or
 * `<Family><Validator>RedeemerSchema`, and any export of the family's
 * fault-proofs schema module named `<Prefix><Validator>SpendRedeemerSchema` or
 * `<Prefix><Validator>RedeemerSchema`.
 */
const conventionTwins = (
  title: string,
  { sdk, modules }: TwinSources = SOURCES,
): readonly RedeemerTwin[] => {
  const parts = /^fraud_proofs\/([a-z0-9_]+)\/([a-z0-9_]+)\.main\.spend$/u.exec(
    title,
  );
  if (parts === null) throw new Error(`${title} is not a step validator title`);
  const [, family, validator] = parts as unknown as [string, string, string];
  const named = pascal(validator);
  const sdkTwins = [
    `${pascal(family)}${named}SpendRedeemerSchema`,
    `${pascal(family)}${named}RedeemerSchema`,
  ].filter((twin) => sdk[twin] !== undefined);
  const moduleExports = modules[family] ?? {};
  const pattern = new RegExp(
    `^[A-Z][A-Za-z0-9]*${named}(?:Spend)?RedeemerSchema$`,
    "u",
  );
  const moduleTwins = Object.keys(moduleExports).filter((name) =>
    pattern.test(name),
  );
  return [
    ...sdkTwins.map((twin) => ({ twin, schema: sdk[twin] })),
    ...moduleTwins.map((twin) => ({ twin, schema: moduleExports[twin] })),
  ];
};

/**
 * Families whose step redeemers are one ordered array rather than named
 * exports: entry `i` is step `i`'s redeemer, as `stepDatumSchemas` is for
 * datums.
 */
const ORDERED_REDEEMER_TWINS: ReadonlyMap<
  FamilyCategory,
  Readonly<{ name: string; schemas: readonly unknown[] }>
> = new Map([
  [
    "scriptIntegrityHashMissing",
    {
      name: "ScriptIntegritySpendRedeemers",
      schemas: ScriptIntegrityHashMissing.ScriptIntegritySpendRedeemers,
    },
  ],
]);

const orderedTwins = ({
  category,
  step,
}: RegisteredStepValidator): readonly RedeemerTwin[] => {
  const ordered = ORDERED_REDEEMER_TWINS.get(category);
  if (ordered === undefined) return [];
  if (step >= ordered.schemas.length) {
    throw new Error(`${ordered.name} has no redeemer for step ${step}`);
  }
  return [
    {
      twin: `${ordered.name}[${step.toString()}]`,
      schema: ordered.schemas[step],
    },
  ];
};

const twinsOf = (
  validator: RegisteredStepValidator,
  sources?: TwinSources,
): readonly RedeemerTwin[] => [
  ...conventionTwins(validator.title, sources),
  ...orderedTwins(validator),
];

const TITLES = [
  ...new Set(REGISTERED_STEP_VALIDATORS.map(({ title }) => title)),
].sort();

const redeemerMismatches = (
  validator: RegisteredStepValidator,
  schema: unknown,
): readonly BlueprintMismatch[] => refines(schema, validator.abi.redeemer());

const allowed = (title: string, twin: string): readonly BlueprintMismatch[] =>
  allowedMismatches(STEP_REDEEMER_ALLOWANCES, { title, twin });

/** Each (title, twin) once, though several families may share a validator. */
const PAIRS = [
  ...new Map(
    REGISTERED_STEP_VALIDATORS.flatMap((validator) =>
      twinsOf(validator).map(
        ({ twin, schema }) =>
          [
            `${validator.title} ${twin}`,
            {
              title: validator.title,
              twin,
              mismatches: redeemerMismatches(validator, schema),
            },
          ] as const,
      ),
    ),
  ).values(),
];

const UNPAIRED_TITLES = UNPAIRED_STEP_REDEEMER_GROUPS.flatMap(
  ({ titles }) => titles,
);

/** Registered titles with no twin, under the given convention sources. */
const titlesWithoutTwins = (sources?: TwinSources): readonly string[] => {
  const paired = new Set(
    REGISTERED_STEP_VALIDATORS.filter(
      (validator) => twinsOf(validator, sources).length > 0,
    ).map(({ title }) => title),
  );
  return TITLES.filter((title) => !paired.has(title));
};

const validatorTitled = (title: string): RegisteredStepValidator =>
  REGISTERED_STEP_VALIDATORS.find((validator) => validator.title === title)!;

describe("registered step redeemer twins against the blueprint", () => {
  it("pairs a twin from the SDK and from fault-proofs", () => {
    expect(TITLES.length).toBeGreaterThan(200);
    const twins = PAIRS.map(({ twin }) => twin);
    expect(twins).toContain("ZeroInputStep01SpendRedeemerSchema");
    expect(twins).toContain("ZeroInputStep01RedeemerSchema");
    expect(twins).toContain("ScriptIntegritySpendRedeemers[0]");
    expect(PAIRS.length).toBeGreaterThan(130);
  });

  it("pairs every entry of an ordered redeemer array with a step", () => {
    for (const [category, { schemas }] of ORDERED_REDEEMER_TWINS) {
      const steps = REGISTERED_STEP_VALIDATORS.filter(
        (validator) => validator.category === category,
      );
      expect(schemas.length).toBe(steps.length);
    }
  });

  it.each(PAIRS.map((pair) => [pair.twin, pair.title, pair]))(
    "%s refines the redeemer of %s",
    (twin, title, { mismatches }) => {
      expect(mismatches).toEqual(allowed(title, twin));
    },
  );

  it("lists exactly the registered titles the convention leaves unpaired", () => {
    // A title that gains a twin fails here until its entry is deleted.
    expect([...UNPAIRED_TITLES].sort()).toEqual(titlesWithoutTwins());
  });

  it("keeps the allowance lists from growing", () => {
    // Lower these with every entry deleted; never raise them.
    expect(UNPAIRED_TITLES.length).toBe(91);
    expect(new Set(UNPAIRED_TITLES).size).toBe(UNPAIRED_TITLES.length);
    expect(STEP_REDEEMER_ALLOWANCES.size).toBe(6);
  });

  it("allows only reports a convention pair still reaches", () => {
    expect(unreachedAllowances(STEP_REDEEMER_ALLOWANCES, PAIRS)).toEqual([]);
  });

  it("searches every fault-proofs family schema module", () => {
    const sourceDirectory = new URL("../src/", import.meta.url);
    const families = readdirSync(sourceDirectory, { withFileTypes: true })
      .filter(
        (entry) =>
          entry.isDirectory() &&
          existsSync(new URL(`${entry.name}/schemas.ts`, sourceDirectory)),
      )
      .map(({ name }) => name.replaceAll("-", "_"));
    expect(families.sort()).toEqual(Object.keys(FAMILY_SCHEMA_MODULES).sort());
  });

  it("refuses an unpaired entry once its title gains a twin", () => {
    const title = "fraud_proofs/no_input/step_01.main.spend";
    expect(UNPAIRED_TITLES).toContain(title);
    const gained = titlesWithoutTwins({
      ...SOURCES,
      sdk: {
        ...SDK,
        NoInputStep01SpendRedeemerSchema:
          SDK.NonExistentInputStep01SpendRedeemerSchema,
      },
    });
    expect(gained).not.toContain(title);
    expect(gained.length).toBe(UNPAIRED_TITLES.length - 1);
  });

  it("refuses a twin paired with another step's redeemer", () => {
    const mismatches = redeemerMismatches(
      validatorTitled("fraud_proofs/zero_input/step_01.main.spend"),
      SDK.ZeroInputStep02SpendRedeemerSchema,
    );
    expect(mismatches).not.toEqual([]);
    expect(
      isDeepStrictEqual(
        mismatches,
        allowed(
          "fraud_proofs/zero_input/step_01.main.spend",
          "ZeroInputStep01SpendRedeemerSchema",
        ),
      ),
    ).toBe(false);
  });
});
