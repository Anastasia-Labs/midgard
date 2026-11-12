import { isDeepStrictEqual } from "node:util";

import {
  type DeploymentContract,
  deploymentValidatorTitle,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type BlueprintMismatch,
  parseBlueprintAbi,
  refines,
} from "@al-ft/midgard-sdk/blueprint-abi";
import { describe, expect, it } from "vitest";

import {
  type FamilyCategory,
  type FamilyDefinition,
  familyStepContractNames,
} from "../src/workflow/family-definition.js";
import { STEP_DATUM_ALLOWANCES } from "./step-datum-blueprint-conformance.allowances.js";
import {
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import { REGISTERED_FAMILY_DEFINITIONS } from "./support/registered-family-definitions.js";

const blueprintAbi = parseBlueprintAbi(readBlueprint(realBlueprintPath));

type StepDatumPair = Readonly<{
  category: FamilyCategory;
  step: number;
  title: string;
  mismatches: readonly BlueprintMismatch[];
}>;

/**
 * Each declared step datum schema against the datum of the validator its step
 * contract deploys: `stepDatumSchemas[i]` -> step contract `i` -> validator
 * title -> the validator's datum definition. Every lookup throws rather than
 * skipping a step.
 */
const stepDatumPairs = (
  definition: FamilyDefinition<FamilyCategory>,
): readonly StepDatumPair[] => {
  const contracts = familyStepContractNames(definition);
  const schemas = definition.stepDatumSchemas as readonly unknown[];
  if (schemas.length !== contracts.length) {
    throw new Error(`${definition.category} step datums and contracts differ`);
  }
  return schemas.map((schema, step) => {
    const contract = contracts[step]!;
    const title: unknown = deploymentValidatorTitle(
      contract as DeploymentContract,
    );
    if (typeof title !== "string") {
      throw new Error(`${contract} deploys no compiled validator`);
    }
    const datum = blueprintAbi.validator(title).datum?.$ref;
    if (datum === undefined) {
      throw new Error(`${title} declares no datum definition`);
    }
    return {
      category: definition.category,
      step,
      title,
      mismatches: refines(schema, blueprintAbi.ref(datum)),
    };
  });
};

const allowedMismatches = (title: string): readonly BlueprintMismatch[] =>
  STEP_DATUM_ALLOWANCES.get(title)?.mismatches ?? [];

const unexpected = ({ title, mismatches }: StepDatumPair): boolean =>
  !isDeepStrictEqual(mismatches, allowedMismatches(title));

const PAIRS = [...REGISTERED_FAMILY_DEFINITIONS.values()].flatMap(
  stepDatumPairs,
);

const minAda = REGISTERED_FAMILY_DEFINITIONS.get("minAda")!;

describe("registered step datum schemas against the blueprint", () => {
  it("pairs a step of every registered family", () => {
    expect(new Set(PAIRS.map(({ category }) => category))).toEqual(
      new Set(REGISTERED_FAMILY_DEFINITIONS.keys()),
    );
    expect(PAIRS.length).toBeGreaterThan(300);
  });

  it.each(PAIRS.map((pair) => [pair.category, pair.step, pair.title, pair]))(
    "%s step %i refines the datum of %s",
    (_category, _step, title, { mismatches }) => {
      expect(mismatches).toEqual(allowedMismatches(title));
    },
  );

  it("keeps the allowance list from growing", () => {
    // Lower this with every entry deleted; never raise it.
    expect(STEP_DATUM_ALLOWANCES.size).toBe(80);
  });

  it("allows only titles a registered step still reaches", () => {
    const reached = new Set(PAIRS.map(({ title }) => title));
    expect(
      [...STEP_DATUM_ALLOWANCES.keys()].filter((title) => !reached.has(title)),
    ).toEqual([]);
  });

  it("refuses a family whose step datums are declared out of order", () => {
    const [first, second, third, fourth] = minAda.stepDatumSchemas;
    const reordered = {
      ...minAda,
      stepDatumSchemas: [first, third, second, fourth],
    };
    const misplaced = stepDatumPairs(reordered as never).filter(unexpected);
    expect(misplaced.map(({ step }) => step)).toEqual([1, 2]);
  });

  it("refuses a step datum whose field is mis-titled", () => {
    const [first, ...rest] = minAda.stepDatumSchemas;
    const retitled = structuredClone(first) as {
      anyOf: [{ fields: { title: string }[] }];
    };
    retitled.anyOf[0].fields[0]!.title = "challenger";
    const [pair] = stepDatumPairs({
      ...minAda,
      stepDatumSchemas: [retitled, ...rest],
    } as never);
    expect(pair!.mismatches).toEqual([
      {
        path: "$.StepDatum.fraud_prover",
        message:
          'field 0 is "fraud_prover" in the blueprint, "challenger" in TypeScript',
      },
    ]);
  });
});
