/**
 * #610 PERMANENT GATE — arity refusal at the compiled-script LOADING boundary.
 *
 * `zz605-semantic-resolver-arity.test.ts` covers the deployment builders, whose
 * helpers apply parameters and can therefore compare a supplied list against a
 * declared one. The loaders guarded here are the other half of the same class:
 * they hand a caller `compiledCode` out of the blueprint, and until #610 they
 * did it without ever consulting the validator's declared parameter list.
 *
 * Loading a validator that declares parameters and deploying it unapplied is the
 * #605 shape: the unapplied `validator main(...)` parameters stay as lambdas, the
 * ledger's single Plutus V3 script-context application reduces to a lambda VALUE
 * instead of running the validator body, evaluation terminates without error,
 * and the ledger reads "no error" as SUCCESS — an unconditional always-succeeds
 * script standing where an authenticated one should be. Before this landing the
 * only symptom was an opaque `→ undefined` evaluation failure a few hundred
 * milliseconds into a submission, with nothing naming the cause.
 *
 * WHAT THIS GATE MUST NOT BECOME. It pins no script hash and no parameter count:
 * every number below is read from the real blueprint at run time, and the
 * parameterized title used for the positive controls is DISCOVERED in it rather
 * than named here, so the rows keep measuring the real declaration after any
 * regeneration. The positive controls drive the real exported loaders and
 * require the refusal to carry this landing's message, and the pass-through rows
 * require a correctly-arity'd load to be byte-identical to what the unguarded
 * loader returned — so neither leg can be satisfied by removing the guard, and
 * neither can be satisfied by a guard that refuses everything.
 */
import { applyParamsToScript } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { getCompiledScript as runtimeGetCompiledScript } from "../src/runtime.js";
import {
  alwaysSucceedsBlueprintPath,
  applyCompiledScript,
  type Blueprint,
  type BlueprintValidator,
  getCompiledScript as harnessGetCompiledScript,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const realBlueprint: Blueprint = readBlueprint(realBlueprintPath);
const alwaysBlueprint: Blueprint = readBlueprint(alwaysSucceedsBlueprintPath);

/** Absent `parameters` is the compiler's encoding of "declares none". */
const declaredParameters = (
  validator: BlueprintValidator,
): readonly { readonly title: string }[] => validator.parameters ?? [];

const requireValidator = (
  blueprint: Blueprint,
  title: string,
): BlueprintValidator => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`blueprint is missing ${title}`);
  }
  return found;
};

/**
 * The positive controls need a validator that really declares parameters. It is
 * found in the blueprint rather than named here so the row cannot rot into
 * asserting against a title that has since become nullary — which would make it
 * a gate that passes for the wrong reason.
 */
const parameterizedValidator = ((): BlueprintValidator => {
  const found = realBlueprint.validators.find(
    (validator) => declaredParameters(validator).length > 0,
  );
  if (found === undefined) {
    throw new Error(
      `${realBlueprintPath} declares no parameterized validator, so the #610 ` +
        "arity-refusal controls below would be vacuous",
    );
  }
  return found;
})();

const parameterizedArity = declaredParameters(parameterizedValidator).length;

/** Application is positional and untyped, so placeholders of the right COUNT
 * are exactly what these rows need: the boundary under test reads the count. */
const placeholders = (count: number): readonly string[] =>
  Array.from({ length: count }, (_unused, index) =>
    (index + 1).toString(16).padStart(2, "0").repeat(28),
  );

/**
 * The titles the two loaders actually deploy bare in production. Every one must
 * declare zero parameters — that is the standing precondition the guards turn
 * from an assumption into a checked one.
 */
const BARE_LOADED_TITLES = [
  "phas.membership.withdraw",
  "pexcludes.exclusion.withdraw",
  "mpf_chunked_verify.verify.withdraw",
  "fraud_proof_catalogue.spend.else",
] as const;

describe("#610 the fault-proofs runtime loader refuses an arity mismatch", () => {
  it("refuses a validator that declares parameters, naming the count", () => {
    expect(parameterizedArity).toBeGreaterThan(0);
    let message = "";
    try {
      runtimeGetCompiledScript(realBlueprint, parameterizedValidator.title);
    } catch (cause) {
      message = cause instanceof Error ? cause.message : String(cause);
    }
    expect(
      message,
      `runtime getCompiledScript accepted ${parameterizedValidator.title}, ` +
        `which declares ${parameterizedArity.toString()} parameter(s)`,
    ).toContain(
      `declares ${parameterizedArity.toString()} parameter(s) but this loader deploys compiledCode bare`,
    );
    expect(message).toContain(parameterizedValidator.title);
    expect(message).toContain("#610");
    // The declared names are reported, so the caller is told what to apply.
    for (const parameter of declaredParameters(parameterizedValidator)) {
      expect(message).toContain(parameter.title);
    }
  });

  it("returns the blueprint's compiledCode unchanged for the titles it deploys", () => {
    for (const title of BARE_LOADED_TITLES) {
      const validator = requireValidator(realBlueprint, title);
      expect(declaredParameters(validator), `${title} declared arity`).toEqual(
        [],
      );
      expect(runtimeGetCompiledScript(realBlueprint, title)).toBe(
        validator.compiledCode,
      );
    }
  });

  it("still reports an unknown title as not found", () => {
    expect(() =>
      runtimeGetCompiledScript(realBlueprint, "not.a.real.validator"),
    ).toThrow(/Validator with title "not\.a\.real\.validator" not found/u);
  });

  it("refuses duplicate validator titles instead of selecting one", () => {
    const duplicate = requireValidator(realBlueprint, BARE_LOADED_TITLES[0]);
    const doctored = {
      validators: [...realBlueprint.validators, duplicate],
    };
    expect(() =>
      runtimeGetCompiledScript(doctored, BARE_LOADED_TITLES[0]),
    ).toThrow(/must contain exactly one validator.*found 2/u);
  });

  it("refuses a malformed declared-parameter list rather than reading it as zero", () => {
    const doctored = {
      validators: realBlueprint.validators.map((validator) =>
        validator.title === BARE_LOADED_TITLES[0]
          ? { ...validator, parameters: "not-a-list" }
          : validator,
      ),
    };
    expect(() =>
      runtimeGetCompiledScript(doctored, BARE_LOADED_TITLES[0]),
    ).toThrow(/parameters must be an array when present/u);
  });
});

describe("#610 the emulator harness loader refuses an arity mismatch", () => {
  it("refuses a parameterized validator loaded bare, naming the count", () => {
    let message = "";
    try {
      harnessGetCompiledScript(realBlueprint, parameterizedValidator.title);
    } catch (cause) {
      message = cause instanceof Error ? cause.message : String(cause);
    }
    expect(
      message,
      `harness getCompiledScript accepted ${parameterizedValidator.title}, ` +
        `which declares ${parameterizedArity.toString()} parameter(s)`,
    ).toContain(
      `declares ${parameterizedArity.toString()} parameter(s) but this loader deploys compiledCode bare`,
    );
    expect(message).toContain("#610");
  });

  it("refuses an application one term SHORTER than the blueprint declares", () => {
    expect(() =>
      applyCompiledScript(
        realBlueprint,
        parameterizedValidator.title,
        placeholders(parameterizedArity - 1),
      ),
    ).toThrow(
      new RegExp(
        `declares ${parameterizedArity.toString()} parameter\\(s\\) but ` +
          `${(parameterizedArity - 1).toString()} were applied`,
        "u",
      ),
    );
  });

  it("refuses an application one term LONGER than the blueprint declares", () => {
    expect(() =>
      applyCompiledScript(
        realBlueprint,
        parameterizedValidator.title,
        placeholders(parameterizedArity + 1),
      ),
    ).toThrow(
      new RegExp(
        `declares ${parameterizedArity.toString()} parameter\\(s\\) but ` +
          `${(parameterizedArity + 1).toString()} were applied`,
        "u",
      ),
    );
  });

  it("refuses applying anything to a validator that declares nothing", () => {
    expect(() =>
      applyCompiledScript(realBlueprint, BARE_LOADED_TITLES[0], ["ff"]),
    ).toThrow(/declares 0 parameter\(s\) but 1 were applied/u);
  });

  it("passes a correctly applied script through unchanged", () => {
    const params = placeholders(parameterizedArity);
    expect(
      applyCompiledScript(realBlueprint, parameterizedValidator.title, params),
    ).toBe(
      applyParamsToScript(parameterizedValidator.compiledCode, [...params]),
    );
  });

  it("loads every always-succeeds validator bare, as before", () => {
    expect(alwaysBlueprint.validators.length).toBeGreaterThan(0);
    for (const validator of alwaysBlueprint.validators) {
      expect(
        declaredParameters(validator),
        `${validator.title} declared arity`,
      ).toEqual([]);
      expect(harnessGetCompiledScript(alwaysBlueprint, validator.title)).toBe(
        validator.compiledCode,
      );
    }
  });

  it("reads an absent parameters key as zero declared, not as unknown", () => {
    const synthetic: Blueprint = {
      validators: [
        { title: "synthetic.nullary", compiledCode: "49480100002221200101" },
      ],
    };
    expect(harnessGetCompiledScript(synthetic, "synthetic.nullary")).toBe(
      "49480100002221200101",
    );
    expect(() =>
      applyCompiledScript(synthetic, "synthetic.nullary", ["ff"]),
    ).toThrow(/declares 0 parameter\(s\) but 1 were applied/u);
  });

  it("still reports an unknown title as not found", () => {
    expect(() =>
      harnessGetCompiledScript(realBlueprint, "not.a.real.validator"),
    ).toThrow(/Validator with title "not\.a\.real\.validator" not found/u);
  });
});
