/**
 * `Q02` — family scaffold generator.
 *
 * Acceptance (GOAL_SPEC.md §9.2): "Shared generator may create boilerplate only;
 * generated families retain explicit schemas/tests and no dynamic 'accept any'
 * dispatch."
 *
 * The anti-goal is a generator whose output accepts anything not explicitly
 * rejected, so the coverage below is mostly negative:
 *
 * - the spec parser refuses every implicit/defaulted input, including a spec
 *   that omits any of the four mandatory test classes (§9.1 outputs 4-5, §3
 *   invariant 9);
 * - the permissive-dispatch scanner fires on every construct it names (control
 *   against a vacuous scanner) and produces zero findings on every shipped
 *   fraud-proof artifact (control against a noisy one);
 * - the generator refuses its own output when an injected emitter introduces a
 *   permissive construct, a vacuously-passing test, a dropped schema field, a
 *   dropped declared test, or a pre-claimed closure row; and
 * - the writer refuses to overwrite an implemented family.
 *
 * The one positive control is the mirror sweep at the end: the terminal-args
 * pattern the generator emits is compared against EVERY shipped family's
 * terminal step module (decision 0005 R7), so a generator that drifts from the
 * deployed families — or a family that drifts from the generator — is caught in
 * one place rather than per family.
 */
import { existsSync, globSync } from "node:fs";
import { mkdir, mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { main } from "../src/bin.js";
import {
  assertNoPermissiveDispatchV1,
  emitClosureChecklistV1,
  FAMILY_CLOSURE_OUTPUTS_V1,
  FAMILY_SCAFFOLD_EMITTERS_V1,
  familyScaffoldNamesV1,
  FamilyScaffoldRejectionV1,
  type FraudProofFamilyScaffoldSpecV1,
  generateFraudProofFamilyScaffoldV1,
  parseFraudProofFamilyScaffoldSpecV1,
  PERMISSIVE_DISPATCH_RULES_V1,
  PermissiveDispatchRejectionV1,
  type ScaffoldArtifactLanguageV1,
  type ScaffoldFieldV1,
  ScaffoldGuardRejectionV1,
  ScaffoldWriteRejectionV1,
  scanForPermissiveDispatchV1,
  writeFraudProofFamilyScaffoldV1,
} from "../src/family-scaffold/index.js";

const REPO_ROOT = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "..",
  "..",
  "..",
);

/**
 * A realistic two-step family: step one binds the native transaction under the
 * committed `transactions_root`, step two concludes on the carried state. This
 * is the shape of the deployed `zero-input` family.
 */
const validSpec = (): Record<string, unknown> => ({
  schemaVersion: "midgard-fraud-proof-family-scaffold-v1",
  family: "network-id",
  taskId: "Q35",
  violationId: "network-id-mismatch",
  catalogueCategory: "networkId",
  summary: "Proves a transaction in the block declares the wrong network id.",
  steps: [
    {
      index: 1,
      rule: "the bad transaction is committed under the header's counted transactions_root",
      inputState: null,
      outputState: [
        {
          name: "bad_tx_network_id",
          type: "int",
          doc: "Declared network id of the bad transaction.",
        },
      ],
      argsFields: null,
    },
    {
      index: 2,
      rule: "the declared network id differs from the protocol network id",
      inputState: [
        {
          name: "bad_tx_network_id",
          type: "int",
          doc: "Declared network id of the bad transaction.",
        },
      ],
      outputState: null,
      argsFields: [],
    },
  ],
  tests: {
    positive: ["network_id_step_01_binds_native_v1_block_fixture"],
    validBlockNegative: ["network_id_step_01_rejects_forged_transactions_root"],
    mutation: ["network_id_step_02_rejects_mutated_network_id"],
    maximumFit: ["network_id_maximum_fit_native_tx"],
  },
});

/** Applies one mutation to a deep-cloned valid spec. */
const mutatedSpec = (
  mutate: (spec: Record<string, unknown>) => void,
): Record<string, unknown> => {
  const spec = JSON.parse(JSON.stringify(validSpec())) as Record<
    string,
    unknown
  >;
  mutate(spec);
  return spec;
};

const threeStepSpec = (): Record<string, unknown> =>
  mutatedSpec((spec) => {
    const steps = spec.steps as Record<string, unknown>[];
    const nextField: ScaffoldFieldV1 = {
      name: "next_state_bytes",
      type: "bytes",
      doc: "State produced by the intermediate step.",
    };
    steps[1] = {
      ...steps[1],
      index: 2,
      rule: "the intermediate state is advanced",
      outputState: [nextField],
      argsFields: [],
    };
    steps.push({
      index: 3,
      rule: "the final state proves the violation",
      inputState: [nextField],
      outputState: null,
      argsFields: [],
    });
  });

const rejectionCode = (spec: unknown): string => {
  try {
    parseFraudProofFamilyScaffoldSpecV1(spec);
  } catch (error) {
    expect(error).toBeInstanceOf(FamilyScaffoldRejectionV1);
    return (error as FamilyScaffoldRejectionV1).code;
  }
  throw new Error("expected the spec parser to reject");
};

const artifact = (
  plan: ReturnType<typeof generateFraudProofFamilyScaffoldV1>,
  path: string,
) => {
  const found = plan.artifacts.find((entry) => entry.path === path);
  if (found === undefined) {
    throw new Error(`missing artifact ${path}`);
  }
  return found;
};

const FRAUD_PROOF_LIB_ROOT = join(
  REPO_ROOT,
  "onchain/aiken/lib/midgard/fraud-proofs",
);

/**
 * Field names of an Aiken record declaration, in declared order.
 *
 * Same idiom the Q11 family gate already parses with
 * (`demo/scripts/verify-canonical-v1-proof-family-q11.mjs`): `aiken fmt` — which
 * the pre-commit hook and CI both enforce — puts every field of a `pub type`
 * record at exactly two spaces of indentation, so a two-space `name:` line is a
 * field while a `///` doc line and the continuation lines of a multi-line
 * generic type are not.
 */
const aikenRecordFieldNames = (
  source: string,
  type: string,
): readonly string[] => {
  const header = `pub type ${type} {`;
  const start = source.indexOf(header);
  if (start < 0) {
    throw new Error(`source does not declare ${type}`);
  }
  const body = source.slice(start + header.length);
  const end = body.indexOf("\n}");
  if (end < 0) {
    throw new Error(`declaration of ${type} is unterminated`);
  }
  return [...body.slice(0, end).matchAll(/^ {2}([a-z_][a-z0-9_]*):/gmu)].map(
    (match) => match[1] as string,
  );
};

/** `<family>/step-NN.ak` of the last numbered step every shipped family ships. */
const shippedTerminalStepModules = (): readonly {
  readonly id: string;
  readonly family: string;
  readonly path: string;
}[] => {
  const lastPerFamily = new Map<string, string>();
  for (const absolute of globSync(
    `${FRAUD_PROOF_LIB_ROOT}/*/step-[0-9][0-9].ak`,
  ).sort()) {
    const relative = absolute.slice(FRAUD_PROOF_LIB_ROOT.length + 1);
    const family = relative.slice(0, relative.indexOf("/"));
    // The glob is sorted and step numbers are zero-padded, so the last entry
    // seen for a family is its terminal step.
    lastPerFamily.set(family, absolute);
  }
  return [...lastPerFamily.entries()].map(([family, path]) => ({
    id: `${family}/${path.slice(path.lastIndexOf("/") + 1)}`,
    family,
    path,
  }));
};

const startsWithPrefix = (
  fields: readonly string[],
  prefix: readonly string[],
): boolean =>
  fields.length >= prefix.length &&
  prefix.every((name, index) => fields[index] === name);

describe("Q02 spec parsing is strict and fail-closed", () => {
  it("parses a complete family specification", () => {
    const spec = parseFraudProofFamilyScaffoldSpecV1(validSpec());
    expect(spec.family).toBe("network-id");
    expect(spec.taskId).toBe("Q35");
    expect(spec.steps).toHaveLength(2);
    expect(spec.tests.validBlockNegative).toHaveLength(1);
    expect(
      spec.steps[0]?.outputState?.map(({ name, type }) => ({ name, type })),
    ).toEqual(
      spec.steps[1]?.inputState?.map(({ name, type }) => ({ name, type })),
    );
  });

  it("derives every name form from the kebab-case family id", () => {
    expect(familyScaffoldNamesV1("withdrawn-reference-input")).toEqual({
      family: "withdrawn-reference-input",
      aikenModule: "withdrawn_reference_input",
      pascal: "WithdrawnReferenceInput",
      camel: "withdrawnReferenceInput",
      screamingSnake: "WITHDRAWN_REFERENCE_INPUT",
    });
  });

  it("rejects unknown and missing top-level keys instead of defaulting them", () => {
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          spec.acceptAnyStep = true;
        }),
      ),
    ).toBe("unknown_key");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          delete spec.violationId;
        }),
      ),
    ).toBe("missing_key");
    expect(rejectionCode("not an object")).toBe("not_an_object");
    expect(rejectionCode([])).toBe("not_an_object");
  });

  it("rejects a wrong schema version, family id, task id, or category", () => {
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          spec.schemaVersion = "midgard-fraud-proof-family-scaffold-v2";
        }),
      ),
    ).toBe("invalid_schema_version");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          spec.family = "NetworkId";
        }),
      ),
    ).toBe("invalid_family_name");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          spec.taskId = "W20";
        }),
      ),
    ).toBe("invalid_task_id");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          spec.catalogueCategory = "network-id";
        }),
      ),
    ).toBe("invalid_identifier");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          spec.summary = "   ";
        }),
      ),
    ).toBe("invalid_text");
  });

  it("rejects renumbered or empty step sequences", () => {
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (spec.steps as { index: number }[])[1].index = 3;
        }),
      ),
    ).toBe("invalid_step_sequence");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          spec.steps = [];
        }),
      ),
    ).toBe("invalid_step_sequence");
  });

  it("rejects output state name, type, and order mismatches", () => {
    const mismatches: readonly ((spec: Record<string, unknown>) => void)[] = [
      (spec) => {
        const output = (spec.steps as Record<string, unknown>[])[0]
          .outputState as Record<string, unknown>[];
        output[0].name = "different_state_name";
      },
      (spec) => {
        const output = (spec.steps as Record<string, unknown>[])[0]
          .outputState as Record<string, unknown>[];
        output[0].type = "bytes";
      },
      (spec) => {
        const steps = spec.steps as Record<string, unknown>[];
        const first = {
          name: "first_state_field",
          type: "int",
          doc: "First state field.",
        };
        const second = {
          name: "second_state_field",
          type: "bytes",
          doc: "Second state field.",
        };
        steps[0].outputState = [first, second];
        steps[1].inputState = [second, first];
      },
    ];

    for (const mutate of mismatches) {
      expect(rejectionCode(mutatedSpec(mutate))).toBe("invalid_state_shape");
    }
  });

  it("accepts matching ordered state declarations despite doc wording", () => {
    const spec = mutatedSpec((value) => {
      const steps = value.steps as Record<string, unknown>[];
      const output = steps[0].outputState as Record<string, unknown>[];
      const input = steps[1].inputState as Record<string, unknown>[];
      output[0].doc = "Producer-side description.";
      input[0].doc = "Consumer-side description.";
    });
    expect(parseFraudProofFamilyScaffoldSpecV1(spec).steps).toHaveLength(2);
  });

  it("rejects a terminal-shaped one-step specification", () => {
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          const steps = spec.steps as Record<string, unknown>[];
          spec.steps = [{ ...steps[0], outputState: null }];
        }),
      ),
    ).toBe("invalid_step_sequence");
  });

  it("rejects every implicit state shape", () => {
    // Step one carries no input state and takes `NativeTxInclusionArgs`.
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (spec.steps as Record<string, unknown>[])[0].inputState = [];
        }),
      ),
    ).toBe("invalid_state_shape");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (spec.steps as Record<string, unknown>[])[0].argsFields = [];
        }),
      ),
    ).toBe("invalid_state_shape");
    // A later step must declare exactly what it consumes and its args.
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (spec.steps as Record<string, unknown>[])[1].inputState = null;
        }),
      ),
    ).toBe("invalid_state_shape");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (spec.steps as Record<string, unknown>[])[1].argsFields = null;
        }),
      ),
    ).toBe("invalid_state_shape");
    // Only the terminal step may omit an output state.
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (spec.steps as Record<string, unknown>[])[0].outputState = null;
        }),
      ),
    ).toBe("invalid_state_shape");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (spec.steps as Record<string, unknown>[])[1].outputState = [
            { name: "leftover", type: "int", doc: "unreachable" },
          ];
        }),
      ),
    ).toBe("invalid_state_shape");
  });

  it("rejects unnamed, mistyped, and duplicated fields", () => {
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (
            (spec.steps as Record<string, unknown>[])[1].inputState as Record<
              string,
              unknown
            >[]
          )[0].type = "anything";
        }),
      ),
    ).toBe("unknown_field_type");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (
            (spec.steps as Record<string, unknown>[])[1].inputState as Record<
              string,
              unknown
            >[]
          )[0].name = "badTxNetworkId";
        }),
      ),
    ).toBe("invalid_identifier");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          const state = (spec.steps as Record<string, unknown>[])[1]
            .inputState as Record<string, unknown>[];
          state.push({ ...state[0] });
        }),
      ),
    ).toBe("duplicate_field");
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (
            (spec.steps as Record<string, unknown>[])[1].inputState as Record<
              string,
              unknown
            >[]
          )[0] = { name: "x_field", type: "int" };
        }),
      ),
    ).toBe("missing_key");
  });

  it("refuses free text that could break out of an emitted comment", () => {
    // Every emitter copies `summary`, `violationId`, a step `rule`, and a field
    // `doc` verbatim into a comment. A line break or a comment delimiter would
    // close that comment and turn spec prose into emitted code — the one way an
    // accept-anything construct could reach a generated family without being
    // declared as a schema. A multi-line injected body spans lines, so the
    // line-based permissive-dispatch scanner alone would not stop it.
    const injections = [
      "ok\npub fn accept_any(_x: Data) -> Bool {\n  True\n}\n////",
      "ok */ injected: any, /*",
      "ok // trailing",
      "ok\tstill one line",
    ];
    for (const injection of injections) {
      expect(
        rejectionCode(
          mutatedSpec((spec) => {
            spec.summary = injection;
          }),
        ),
      ).toBe("unsafe_text");
      expect(
        rejectionCode(
          mutatedSpec((spec) => {
            spec.violationId = injection;
          }),
        ),
      ).toBe("unsafe_text");
      expect(
        rejectionCode(
          mutatedSpec((spec) => {
            (spec.steps as Record<string, unknown>[])[1].rule = injection;
          }),
        ),
      ).toBe("unsafe_text");
      expect(
        rejectionCode(
          mutatedSpec((spec) => {
            (
              (spec.steps as Record<string, unknown>[])[1].inputState as Record<
                string,
                unknown
              >[]
            )[0].doc = injection;
          }),
        ),
      ).toBe("unsafe_text");
    }
  });

  it("refuses a family that does not declare all four mandatory test classes", () => {
    for (const testClass of [
      "positive",
      "validBlockNegative",
      "mutation",
      "maximumFit",
    ]) {
      expect(
        rejectionCode(
          mutatedSpec((spec) => {
            (spec.tests as Record<string, unknown>)[testClass] = [];
          }),
        ),
      ).toBe("empty_test_class");
      expect(
        rejectionCode(
          mutatedSpec((spec) => {
            delete (spec.tests as Record<string, unknown>)[testClass];
          }),
        ),
      ).toBe("missing_key");
    }
    expect(
      rejectionCode(
        mutatedSpec((spec) => {
          (spec.tests as Record<string, string[]>).mutation = [
            "network_id_step_01_binds_native_v1_block_fixture",
          ];
        }),
      ),
    ).toBe("duplicate_test_name");
  });
});

describe("Q02 generated families retain explicit schemas and tests", () => {
  const plan = generateFraudProofFamilyScaffoldV1({ spec: validSpec() });

  it("emits exactly the per-step, off-chain, and closure artifacts", () => {
    expect(plan.artifacts.map((entry) => entry.path)).toEqual([
      "onchain/aiken/lib/midgard/fraud-proofs/network-id/step-01.ak",
      "onchain/aiken/validators/fraud-proofs/network-id/step-01.ak",
      "onchain/aiken/lib/midgard/fraud-proofs/network-id/step-02.ak",
      "onchain/aiken/validators/fraud-proofs/network-id/step-02.ak",
      "demo/midgard-sdk/src/fraud-proof/network-id.ts",
      "demo/midgard-sdk/tests/network-id-v1.test.ts",
      "docs/exec-plans/evidence/family-closure/network-id-closure-checklist-v1.json",
    ]);
  });

  it("spells the carried state out as an explicit record on both sides", () => {
    expect(
      artifact(
        plan,
        "onchain/aiken/lib/midgard/fraud-proofs/network-id/step-02.ak",
      ).contents,
    ).toContain("pub type State {\n  bad_tx_network_id: Int,\n}");
    const sdk = artifact(
      plan,
      "demo/midgard-sdk/src/fraud-proof/network-id.ts",
    ).contents;
    expect(sdk).toContain(
      "export const NetworkIdStep02StateSchema = Data.Object({",
    );
    expect(sdk).toContain("bad_tx_network_id: Data.Integer(),");
    // The positional redeemer args every step carries are explicit too.
    expect(sdk).toContain("fraud_proof_mint_redeemer_index: Data.Integer(),");
  });

  it("sources next-state declarations from the preceding output contract", () => {
    const parsed = parseFraudProofFamilyScaffoldSpecV1(validSpec());
    const outputField: ScaffoldFieldV1 = {
      name: "declared_output_bytes",
      type: "bytes",
      doc: "The preceding step's declared output.",
    };
    const inputField: ScaffoldFieldV1 = {
      name: "declared_input_integer",
      type: "int",
      doc: "An intentionally inconsistent direct-emitter input.",
    };
    const directSpec: FraudProofFamilyScaffoldSpecV1 = {
      ...parsed,
      steps: [
        { ...parsed.steps[0], outputState: [outputField] },
        { ...parsed.steps[1], inputState: [inputField] },
      ],
    };
    const generatedTypes = FAMILY_SCAFFOLD_EMITTERS_V1.emitAikenStepTypesModule(
      {
        spec: directSpec,
        step: directSpec.steps[1],
      },
    ).contents;
    const generatedSdk =
      FAMILY_SCAFFOLD_EMITTERS_V1.emitSdkFamilyModule(directSpec).contents;

    expect(generatedTypes).toContain("declared_output_bytes: ByteArray,");
    expect(generatedTypes).not.toContain("declared_input_integer: Int,");
    expect(generatedSdk).toContain("declared_output_bytes: Data.Bytes(),");
    expect(generatedSdk).not.toContain(
      "declared_input_integer: Data.Integer(),",
    );
  });

  it("binds output state visibly while keeping validators fail-loud", () => {
    const plan = generateFraudProofFamilyScaffoldV1({
      spec: threeStepSpec(),
    });
    for (const step of ["step-01", "step-02"]) {
      const validator = artifact(
        plan,
        `onchain/aiken/validators/fraud-proofs/network-id/${step}.ak`,
      ).contents;
      expect(validator).toMatch(/^\s+output_state_data,?\s*$/mu);
      expect(validator).not.toContain("_output_state_data");
      expect(validator).toContain(
        "`expect output_state_data == expected_output_state`",
      );
      expect(validator).toMatch(/todo\s+@"Q35 network_id/u);
    }
  });

  it("resolves step schemas through an exhaustive union with no fallback", () => {
    const sdk = artifact(
      plan,
      "demo/midgard-sdk/src/fraud-proof/network-id.ts",
    ).contents;
    expect(sdk).toContain(
      'export const NETWORK_ID_STEP_NAMES_V1 = ["step_01", "step_02"] as const;',
    );
    expect(sdk).toContain('case "step_01":');
    expect(sdk).toContain('case "step_02":');
    expect(sdk).not.toMatch(/^\s*default\s*:/mu);
  });

  it("carries every declared selector into the emitted step modules", () => {
    const aiken = plan.artifacts
      .filter((entry) => entry.language === "aiken")
      .map((entry) => entry.contents)
      .join("\n");
    for (const name of [
      ...plan.spec.tests.positive,
      ...plan.spec.tests.validBlockNegative,
      ...plan.spec.tests.mutation,
      ...plan.spec.tests.maximumFit,
    ]) {
      expect(aiken).toContain(`test ${name}(`);
    }
  });

  it("drives the step-one binding selectors through the shared native fixture", () => {
    const stepOne = artifact(
      plan,
      "onchain/aiken/validators/fraud-proofs/network-id/step-01.ak",
    ).contents;
    expect(stepOne).toContain(
      "use midgard/fraud_proofs/native_binding_fixture_v1 as fixture",
    );
    expect(stepOne).toContain("fixture.native_block_fixture_v1(");
    expect(stepOne).toContain("fixture.native_inclusion_carriage_v1(");
    // The valid-block negative claims a forged root under a valid header.
    expect(stepOne).toContain("header: valid_block.header");
    expect(stepOne).toContain("header_hash: valid_block.header_hash");
  });

  it("leaves every family-specific decision as a loud `todo`, never a default", () => {
    const validators = plan.artifacts.filter((candidate) =>
      candidate.path.includes("/validators/"),
    );
    expect(validators).toHaveLength(2);
    for (const entry of validators) {
      expect(entry.contents).toMatch(/todo\s+@"Q35 network_id/u);
      // A `fail` annotation on a `todo` body passes vacuously.
      expect(entry.contents).not.toMatch(/^\s*test\s+\w+\(\)\s*fail\b/mu);
    }
    // The pure type modules declare schemas only; there is nothing to stub.
    for (const entry of plan.artifacts.filter((candidate) =>
      candidate.path.includes("/lib/midgard/"),
    )) {
      expect(entry.contents).not.toMatch(/\btodo\b/u);
      expect(entry.contents).not.toMatch(/^\s*test\s/mu);
    }
  });

  it("emits off-chain tests that cannot report green", () => {
    const sdkTest = artifact(
      plan,
      "demo/midgard-sdk/tests/network-id-v1.test.ts",
    ).contents;
    const itCount = sdkTest.match(/^\s*it\(/gmu)?.length ?? 0;
    const throwCount = sdkTest.match(/SCAFFOLD_UNIMPLEMENTED/gu)?.length ?? 0;
    expect(itCount).toBeGreaterThan(0);
    expect(throwCount).toBeGreaterThanOrEqual(itCount);
    expect(sdkTest).not.toMatch(/\b(it|test|describe)\.skip\s*\(/u);
  });

  it("emits an unclaimed §9.1 closure checklist", () => {
    const checklist = JSON.parse(
      artifact(
        plan,
        "docs/exec-plans/evidence/family-closure/network-id-closure-checklist-v1.json",
      ).contents,
    ) as {
      closureOutputs: readonly { output: number; status: string }[];
      taskId: string;
    };
    expect(checklist.taskId).toBe("Q35");
    expect(checklist.closureOutputs).toHaveLength(
      FAMILY_CLOSURE_OUTPUTS_V1.length,
    );
    expect(checklist.closureOutputs.every((row) => row.status === "TODO")).toBe(
      true,
    );
  });
});

describe("Q02 permissive-dispatch scanner", () => {
  const SAMPLES: readonly {
    readonly ruleId: string;
    readonly language: ScaffoldArtifactLanguageV1;
    readonly line: string;
  }[] = [
    {
      ruleId: "ak_catch_all_arm_true",
      language: "aiken",
      line: "      _ -> True",
    },
    {
      ruleId: "ak_catch_all_arm",
      language: "aiken",
      line: "      _ -> accept_step(datum)",
    },
    {
      ruleId: "ak_always_true_predicate",
      language: "aiken",
      line: "fn check(x: Int) -> Bool { True }",
    },
    {
      ruleId: "ak_or_else_true",
      language: "aiken",
      line: "  let ok = maybe_valid |> or_else(True)",
    },
    {
      ruleId: "ak_expect_wildcard",
      language: "aiken",
      line: "        expect _ = decode_state(data)",
    },
    {
      ruleId: "ak_opaque_data_parameter",
      language: "aiken",
      line: "pub type State = FamilyState<Data>",
    },
    {
      ruleId: "ak_vacuous_test",
      language: "aiken",
      line: "test family_binds() { True }",
    },
    { ruleId: "ts_default_case", language: "typescript", line: "    default:" },
    {
      ruleId: "ts_catch_accepts",
      language: "typescript",
      line: "  } catch { return true; }",
    },
    {
      ruleId: "ts_nullish_true",
      language: "typescript",
      line: "  const ok = verified ?? true;",
    },
    {
      ruleId: "ts_dynamic_registry_dispatch",
      language: "typescript",
      line: "  const handler = registry[family] ?? fallbackHandler;",
    },
    {
      ruleId: "ts_any_type",
      language: "typescript",
      line: "  const state: any = decode(datum);",
    },
    {
      ruleId: "ts_data_any",
      language: "typescript",
      line: "  state: Data.Any(),",
    },
    {
      ruleId: "ts_skipped_test",
      language: "typescript",
      line: '  it.skip("binds the counted root", () => {});',
    },
    {
      ruleId: "ts_always_true_predicate",
      language: "typescript",
      line: "  const isValid = () => true;",
    },
  ];

  it("fires on a sample of every rule it declares", () => {
    for (const sample of SAMPLES) {
      const findings = scanForPermissiveDispatchV1({
        path: `sample.${sample.ruleId}`,
        language: sample.language,
        contents: sample.line,
      });
      expect(
        findings.map((finding) => finding.ruleId),
        `rule ${sample.ruleId} did not fire`,
      ).toContain(sample.ruleId);
    }
  });

  it("covers every declared rule with a sample", () => {
    expect(new Set(SAMPLES.map((sample) => sample.ruleId))).toEqual(
      new Set(PERMISSIVE_DISPATCH_RULES_V1.map((rule) => rule.ruleId)),
    );
  });

  it("produces no findings on any shipped fraud-proof artifact", async () => {
    const files = [
      ...globSync(`${REPO_ROOT}/onchain/aiken/validators/fraud-proofs/**/*.ak`),
      ...globSync(
        `${REPO_ROOT}/onchain/aiken/lib/midgard/fraud-proofs/**/*.ak`,
      ),
      ...globSync(`${REPO_ROOT}/demo/midgard-sdk/src/fraud-proof/*.ts`),
    ].filter((path) => !path.includes(".test."));
    expect(files.length).toBeGreaterThan(100);
    const findings = (
      await Promise.all(
        files.map(async (path) =>
          scanForPermissiveDispatchV1({
            path: path.slice(REPO_ROOT.length + 1),
            language: path.endsWith(".ak") ? "aiken" : "typescript",
            contents: await readFile(path, "utf8"),
          }),
        ),
      )
    ).flat();
    expect(
      findings.map(
        (finding) => `${finding.path}:${finding.line} ${finding.ruleId}`,
      ),
    ).toEqual([]);
  });

  it("ignores a forbidden construct that appears only in a comment", () => {
    expect(
      scanForPermissiveDispatchV1({
        path: "sample.ak",
        language: "aiken",
        contents: "// never write `_ -> True` here\nexpect ok == True",
      }),
    ).toEqual([]);
  });

  it("permits the step-datum phantom slot and rejects a widened schema", () => {
    expect(
      scanForPermissiveDispatchV1({
        path: "sample.ak",
        language: "aiken",
        contents: "pub type Datum =\n  ct.StepDatum<Data>",
      }),
    ).toEqual([]);
    expect(
      scanForPermissiveDispatchV1({
        path: "sample.ts",
        language: "typescript",
        contents:
          "export const S = faultProofStepDatumSchema(\n  Data.Any(),\n);",
      }),
    ).toEqual([]);
    expect(
      scanForPermissiveDispatchV1({
        path: "sample.ts",
        language: "typescript",
        contents: "export const S = Data.Object({ state: Data.Any() });",
      }).map((finding) => finding.ruleId),
    ).toEqual(["ts_data_any"]);
  });

  // The shipped field-8 arm of `native-binding-fixture-v1.ak` is a wildcard
  // whose block opens with `expect field_index == 8`, so it refuses every case
  // the explicit arms did not take. That is enumerate-and-refuse, not
  // default-accept (owner-adjudicated 2026-08-18). The narrowing turns on two
  // conditions, and the hostile controls below remove each in turn.
  it("permits a refusing catch-all arm and still catches every permissive one", () => {
    const ruleIds = (contents: string): readonly string[] =>
      scanForPermissiveDispatchV1({
        path: "sample.ak",
        language: "aiken",
        contents,
      }).map((finding) => finding.ruleId);

    // Benign: the arm's first statement refuses everything but field 8.
    expect(
      ruleIds(
        "    _ -> {\n      expect field_index == 8\n      Pair(body, x)\n    }\n",
      ),
    ).toEqual([]);

    // Hostile: a bare wildcard, with no refusal at all.
    expect(ruleIds("    _ -> accept_step(datum)\n")).toContain(
      "ak_catch_all_arm",
    );
    // Hostile: a block that opens on real work rather than a refusal.
    expect(ruleIds("    _ -> {\n      Pair(body, x)\n    }\n")).toContain(
      "ak_catch_all_arm",
    );
    // Hostile: the `expect` is reached only after other statements, so it does
    // not gate the arm.
    expect(
      ruleIds(
        "    _ -> {\n      let n = field_index\n      expect n == 8\n    }\n",
      ),
    ).toContain("ak_catch_all_arm");
    // Hostile: `expect <pattern> = ...` destructures without refusing.
    expect(
      ruleIds(
        "    _ -> {\n      expect Some(v) = lookup(x)\n      Pair(body, v)\n    }\n",
      ),
    ).toContain("ak_catch_all_arm");
    // Hostile: an `expect` that asserts nothing.
    expect(
      ruleIds("    _ -> {\n      expect True\n      Pair(body, x)\n    }\n"),
    ).toContain("ak_catch_all_arm");
    // Hostile: an `expect` that discards the shape check it appears to make.
    expect(
      ruleIds(
        "    _ -> {\n      expect _ = decode(d)\n      Pair(body, x)\n    }\n",
      ),
    ).toContain("ak_catch_all_arm");
  });

  // Two shipped `??` fallbacks carry no dispatch decision: a verdict *name*
  // used only as a human-readable diagnostic label, and an absent asset
  // quantity defaulted to zero under an equality test against one, where
  // absence can only fail. Both are owner-adjudicated benign (2026-08-18);
  // a fallback that feeds a verdict, a handler, or a widened acceptance is not.
  it("permits label-only and conservative fallbacks while catching dispatching ones", () => {
    const ruleIds = (contents: string): readonly string[] =>
      scanForPermissiveDispatchV1({
        path: "sample.ts",
        language: "typescript",
        contents,
      }).map((finding) => finding.ruleId);

    // Benign: a quoted string bound to a `*Name`-shaped diagnostic key.
    expect(
      ruleIds(
        '    verdictName: MIDGARD_ENVELOPE_VERDICT_NAMES_V1[verdict] ?? "unknown",\n',
      ),
    ).toEqual([]);
    // Benign: absent quantity defaults to zero and cannot satisfy `=== 1n`.
    expect(
      ruleIds(
        "      (utxo) => (utxo.assets[unit] ?? 0n) === 1n && ok(utxo),\n",
      ),
    ).toEqual([]);

    // Hostile: the fallback supplies a handler, i.e. a dispatch target.
    expect(
      ruleIds("  const handler = registry[family] ?? fallbackHandler;\n"),
    ).toContain("ts_dynamic_registry_dispatch");
    // Hostile: the same string-literal shape, but it feeds a verdict.
    expect(
      ruleIds('    verdict: MIDGARD_VERDICTS[key] ?? "accept",\n'),
    ).toContain("ts_dynamic_registry_dispatch");
    // Hostile: a label-shaped key whose fallback is computed, not a literal.
    expect(
      ruleIds("    verdictName: NAMES[verdict] ?? deriveName(verdict),\n"),
    ).toContain("ts_dynamic_registry_dispatch");
    // Hostile: a string-literal fallback bound to a key that is not a label.
    expect(ruleIds('    nextStep: STEPS[index] ?? "default",\n')).toContain(
      "ts_dynamic_registry_dispatch",
    );
    // Hostile: `?? 1n` widens acceptance — an absent token now matches.
    expect(
      ruleIds(
        "      (utxo) => (utxo.assets[unit] ?? 1n) === 1n && ok(utxo),\n",
      ),
    ).toContain("ts_dynamic_registry_dispatch");
    // Hostile: the default itself satisfies the comparison.
    expect(
      ruleIds("      (u) => (counts[key] ?? 0n) === 0n && ok(u),\n"),
    ).toContain("ts_dynamic_registry_dispatch");
    // Hostile: a truthy default.
    expect(
      ruleIds("      (u) => (flags[key] ?? true) === true && ok(u),\n"),
    ).toContain("ts_dynamic_registry_dispatch");
  });

  it("throws with every finding when asked to gate artifacts", () => {
    try {
      assertNoPermissiveDispatchV1([
        {
          path: "bad.ak",
          language: "aiken",
          contents: "      _ -> True",
        },
      ]);
    } catch (error) {
      expect(error).toBeInstanceOf(PermissiveDispatchRejectionV1);
      expect((error as PermissiveDispatchRejectionV1).findings).toHaveLength(2);
      return;
    }
    throw new Error("expected a permissive-dispatch rejection");
  });
});

describe("Q02 generator refuses its own permissive or vacuous output", () => {
  const guardCode = (
    emitters: Partial<typeof FAMILY_SCAFFOLD_EMITTERS_V1>,
  ): string => {
    try {
      generateFraudProofFamilyScaffoldV1({
        spec: validSpec(),
        emitters: { ...FAMILY_SCAFFOLD_EMITTERS_V1, ...emitters },
      });
    } catch (error) {
      if (error instanceof PermissiveDispatchRejectionV1) {
        return error.findings[0]?.ruleId ?? "permissive";
      }
      expect(error).toBeInstanceOf(ScaffoldGuardRejectionV1);
      return (error as ScaffoldGuardRejectionV1).code;
    }
    throw new Error("expected the generator to refuse its own output");
  };

  it("refuses an emitted Aiken catch-all that accepts every other case", () => {
    expect(
      guardCode({
        emitAikenStepValidator: (input) => {
          const emitted =
            FAMILY_SCAFFOLD_EMITTERS_V1.emitAikenStepValidator(input);
          return {
            ...emitted,
            contents: `${emitted.contents}\nfn accept(r: Redeemer) -> Bool {\n  when r is {\n    _ -> True\n  }\n}\n`,
          };
        },
      }),
    ).toBe("ak_catch_all_arm_true");
  });

  it("refuses an emitted off-chain default dispatch branch", () => {
    expect(
      guardCode({
        emitSdkFamilyModule: (spec) => {
          const emitted = FAMILY_SCAFFOLD_EMITTERS_V1.emitSdkFamilyModule(spec);
          return {
            ...emitted,
            contents: emitted.contents.replace(
              '    case "step_02":',
              '    default:\n    case "step_02":',
            ),
          };
        },
      }),
    ).toBe("ts_default_case");
  });

  it("refuses a `fail`-annotated Aiken test whose body is still `todo`", () => {
    expect(
      guardCode({
        emitAikenStepValidator: (input) => {
          const emitted =
            FAMILY_SCAFFOLD_EMITTERS_V1.emitAikenStepValidator(input);
          return {
            ...emitted,
            contents: emitted.contents.replace(
              `test ${input.spec.tests.validBlockNegative[0]}()`,
              `test ${input.spec.tests.validBlockNegative[0]}() fail`,
            ),
          };
        },
      }),
    ).toBe("vacuous_fail_test");
  });

  it("refuses an Aiken step module that could pass without being implemented", () => {
    expect(
      guardCode({
        emitAikenStepValidator: (input) => {
          const emitted =
            FAMILY_SCAFFOLD_EMITTERS_V1.emitAikenStepValidator(input);
          return {
            ...emitted,
            contents: emitted.contents.replace(/todo\s+@"[^"]*"/gu, "True"),
          };
        },
      }),
    ).toBe("test_can_report_green");
  });

  it("refuses an off-chain test module that does not throw on every case", () => {
    expect(
      guardCode({
        emitSdkFamilyTestModule: (spec) => {
          const emitted =
            FAMILY_SCAFFOLD_EMITTERS_V1.emitSdkFamilyTestModule(spec);
          return {
            ...emitted,
            contents: emitted.contents.replace(
              /SCAFFOLD_UNIMPLEMENTED/gu,
              "ok",
            ),
          };
        },
      }),
    ).toBe("test_can_report_green");
  });

  it("refuses to drop a declared schema field", () => {
    expect(
      guardCode({
        emitSdkFamilyModule: (spec) => {
          const emitted = FAMILY_SCAFFOLD_EMITTERS_V1.emitSdkFamilyModule(spec);
          return {
            ...emitted,
            contents: emitted.contents.replace(
              /bad_tx_network_id/gu,
              "state_blob",
            ),
          };
        },
      }),
    ).toBe("missing_declared_field");
  });

  it("refuses to drop a declared test selector", () => {
    expect(
      guardCode({
        emitAikenStepValidator: (input) => {
          const emitted =
            FAMILY_SCAFFOLD_EMITTERS_V1.emitAikenStepValidator(input);
          return {
            ...emitted,
            contents: emitted.contents.replace(
              new RegExp(input.spec.tests.maximumFit[0] ?? "", "gu"),
              "skipped_selector",
            ),
          };
        },
      }),
    ).toBe("missing_declared_test");
  });

  it("refuses a closure checklist that pre-claims a §9.1 output", () => {
    expect(
      guardCode({
        emitClosureChecklist: (spec) => {
          const emitted = emitClosureChecklistV1(spec);
          return {
            ...emitted,
            contents: emitted.contents.replace('"TODO"', '"LOCAL_PASS"'),
          };
        },
      }),
    ).toBe("claimed_closure_row");
  });
});

describe("Q02 scaffold writer is fail-closed", () => {
  const planFor = (
    family: string,
  ): ReturnType<typeof generateFraudProofFamilyScaffoldV1> =>
    generateFraudProofFamilyScaffoldV1({
      spec: mutatedSpec((spec) => {
        spec.family = family;
        spec.catalogueCategory = "networkId";
      }),
    });

  it("reports every target without touching the tree on a dry run", async () => {
    const root = await mkdtemp(join(tmpdir(), "q02-dry-"));
    const result = await writeFraudProofFamilyScaffoldV1({
      plan: planFor("network-id"),
      repoRoot: root,
      dryRun: true,
    });
    expect(result.dryRun).toBe(true);
    expect(result.written).toHaveLength(7);
    for (const path of result.written) {
      expect(existsSync(join(root, path))).toBe(false);
    }
  });

  it("writes every artifact under the given root", async () => {
    const root = await mkdtemp(join(tmpdir(), "q02-write-"));
    const plan = planFor("network-id");
    const result = await writeFraudProofFamilyScaffoldV1({
      plan,
      repoRoot: root,
    });
    expect(result.dryRun).toBe(false);
    for (const entry of plan.artifacts) {
      expect(await readFile(join(root, entry.path), "utf8")).toBe(
        entry.contents,
      );
    }
  });

  it("never overwrites an already-implemented family", async () => {
    const root = await mkdtemp(join(tmpdir(), "q02-existing-"));
    const plan = planFor("network-id");
    const target = join(
      root,
      "onchain/aiken/validators/fraud-proofs/network-id/step-02.ak",
    );
    await mkdir(dirname(target), { recursive: true });
    await writeFile(target, "// real implemented step\n", "utf8");
    await expect(
      writeFraudProofFamilyScaffoldV1({ plan, repoRoot: root }),
    ).rejects.toBeInstanceOf(ScaffoldWriteRejectionV1);
    // The refusal is atomic: nothing else was written either.
    expect(
      existsSync(
        join(
          root,
          "onchain/aiken/validators/fraud-proofs/network-id/step-01.ak",
        ),
      ),
    ).toBe(false);
    expect(await readFile(target, "utf8")).toBe("// real implemented step\n");
  });

  it("refuses an artifact path that escapes the repository root", async () => {
    const root = await mkdtemp(join(tmpdir(), "q02-escape-"));
    const plan = planFor("network-id");
    const escaping: typeof plan = {
      ...plan,
      artifacts: [
        { ...plan.artifacts[0], path: "../../etc/midgard-escape.ak" },
      ],
    };
    await expect(
      writeFraudProofFamilyScaffoldV1({ plan: escaping, repoRoot: root }),
    ).rejects.toMatchObject({ code: "path_escapes_root" });
    const absolute: typeof plan = {
      ...plan,
      artifacts: [{ ...plan.artifacts[0], path: "/tmp/midgard-escape.ak" }],
    };
    await expect(
      writeFraudProofFamilyScaffoldV1({ plan: absolute, repoRoot: root }),
    ).rejects.toMatchObject({ code: "path_escapes_root" });
  });
});

describe("Q02 scaffold CLI", () => {
  const runScaffoldCli = async (argv: readonly string[]): Promise<string[]> => {
    const previousArgv = process.argv;
    const lines: string[] = [];
    const previousLog = console.log;
    console.log = (value: unknown) => {
      lines.push(String(value));
    };
    process.argv = ["node", "midgard-fault-proofs", ...argv];
    try {
      await main();
    } finally {
      process.argv = previousArgv;
      console.log = previousLog;
    }
    return lines;
  };

  it("generates a family from a spec file and reports every written path", async () => {
    const root = await mkdtemp(join(tmpdir(), "q02-cli-"));
    const specPath = join(root, "spec.json");
    await writeFile(specPath, JSON.stringify(validSpec()), "utf8");
    const lines = await runScaffoldCli([
      "scaffold-family",
      "--scaffold-spec",
      specPath,
      "--repo-root",
      root,
    ]);
    const report = JSON.parse(lines.join("\n")) as {
      family: string;
      taskId: string;
      dryRun: boolean;
      written: string[];
    };
    expect(report.family).toBe("network-id");
    expect(report.taskId).toBe("Q35");
    expect(report.dryRun).toBe(false);
    expect(report.written).toHaveLength(7);
    expect(
      existsSync(
        join(
          root,
          "onchain/aiken/validators/fraud-proofs/network-id/step-01.ak",
        ),
      ),
    ).toBe(true);
  });

  it("refuses a spec file the strict parser rejects", async () => {
    const root = await mkdtemp(join(tmpdir(), "q02-cli-bad-"));
    const specPath = join(root, "spec.json");
    await writeFile(
      specPath,
      JSON.stringify(
        mutatedSpec((spec) => {
          (spec.tests as Record<string, unknown>).validBlockNegative = [];
        }),
      ),
      "utf8",
    );
    await expect(
      runScaffoldCli([
        "scaffold-family",
        "--scaffold-spec",
        specPath,
        "--repo-root",
        root,
      ]),
    ).rejects.toBeInstanceOf(FamilyScaffoldRejectionV1);
    expect(existsSync(join(root, "onchain"))).toBe(false);
  });

  it("requires the spec path", async () => {
    await expect(runScaffoldCli(["scaffold-family"])).rejects.toThrow(
      "Missing required --scaffold-spec <path>.",
    );
  });

  it("ships a worked example spec that the strict parser still accepts", async () => {
    const example = JSON.parse(
      await readFile(
        join(
          REPO_ROOT,
          "demo/midgard-fault-proofs/templates/family-scaffold-spec-v1.example.json",
        ),
        "utf8",
      ),
    );
    const plan = generateFraudProofFamilyScaffoldV1({ spec: example });
    expect(plan.spec.family).toBe("network-id");
    expect(plan.artifacts).toHaveLength(7);
  });
});

describe("Q02 generated shape matches the deployed families", () => {
  it("mirrors the shipped zero-input step modules", async () => {
    const spec: FraudProofFamilyScaffoldSpecV1 =
      parseFraudProofFamilyScaffoldSpecV1(
        mutatedSpec((raw) => {
          raw.family = "zero-input";
          raw.taskId = "Q14";
          raw.violationId = "zero-input";
          raw.catalogueCategory = "zeroInput";
          const steps = raw.steps as Record<string, unknown>[];
          // Post-#575: the thread carries the disputed transaction's §2.5
          // anchor (its id), not a per-field collection commitment.
          const anchorField = {
            name: "bad_tx_id",
            type: "midgard_tx_id",
            doc: "The disputed transaction's §2.5 anchor.",
          };
          steps[0].outputState = [anchorField];
          steps[1].inputState = [anchorField];
          // The terminal step opens field 0 through the §8 door instead of
          // comparing a forwarded commitment against the empty constant.
          steps[1].argsFields = [
            {
              name: "spend_inputs_opening",
              type: "field_opening_v1",
              doc: "The prover's chosen §8 carriage for field 0's preimage.",
            },
          ];
        }),
      );
    const plan = generateFraudProofFamilyScaffoldV1({ spec });

    const generatedTypes = artifact(
      plan,
      "onchain/aiken/lib/midgard/fraud-proofs/zero-input/step-02.ak",
    ).contents;
    const shippedTypes = await readFile(
      join(
        REPO_ROOT,
        "onchain/aiken/lib/midgard/fraud-proofs/zero-input/step-02.ak",
      ),
      "utf8",
    );
    // Same declarations, in the same order, as the family that already builds.
    // The shipped `Args` block carries a `///` doc comment between
    // `fraud_proof_mint_redeemer_index` and `spend_inputs_opening` that the
    // boilerplate emitter does not reproduce (family-specific prose is not
    // boilerplate), so the block is pinned field-line by field-line rather
    // than as one contiguous string.
    for (const declaration of [
      "use midgard/computation_thread as ct",
      "use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}",
      "use midgard/ledger_state.{MidgardTxId}",
      "pub type State {\n  bad_tx_id: MidgardTxId,\n}",
      "pub type Datum =\n  ct.StepDatum<State>",
      "  input_index: Int,",
      "  output_index: Int,",
      "  fraud_proof_mint_redeemer_index: Int,",
      "  spend_inputs_opening: FieldOpeningV1,",
      "pub type SpendRedeemer =\n  ct.StepRedeemer<Args>",
    ]) {
      expect(generatedTypes).toContain(declaration);
      expect(shippedTypes).toContain(declaration);
    }

    // The divergence this ticket exists to close is load-bearing at step one
    // too: the deployed step-01 consumes a `NativeTxInclusionCarriage`
    // (issue #545's published-chunk carriage), not the retired
    // `NativeTxInclusionArgs` the generator used to emit directly.
    const generatedStepOne = artifact(
      plan,
      "onchain/aiken/lib/midgard/fraud-proofs/zero-input/step-01.ak",
    ).contents;
    const shippedStepOne = await readFile(
      join(
        REPO_ROOT,
        "onchain/aiken/lib/midgard/fraud-proofs/zero-input/step-01.ak",
      ),
      "utf8",
    );
    for (const declaration of [
      "use midgard/computation_thread as ct",
      "use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}",
      "pub type Datum =\n  ct.StepDatum<Data>",
      "pub type Args =\n  NativeTxInclusionCarriage",
      "pub type SpendRedeemer =\n  ct.StepRedeemer<Args>",
    ]) {
      expect(generatedStepOne).toContain(declaration);
      expect(shippedStepOne).toContain(declaration);
    }

    // The off-chain twin rebinds the same way: the step-01 redeemer carries
    // the carriage schema and the step-02 args schema carries the opening,
    // in the same position as the shipped `ZeroInputStep02ArgsSchema`.
    const sdk = artifact(
      plan,
      "demo/midgard-sdk/src/fraud-proof/zero-input.ts",
    ).contents;
    expect(sdk).toContain(
      'import { FieldOpeningV1Schema } from "./field-opening-v1.js";',
    );
    expect(sdk).not.toContain("NativeTxInclusionArgsSchema");
    expect(sdk).toContain(
      "faultProofStepRedeemerSchema(NativeTxInclusionCarriageSchema)",
    );
    expect(sdk).toContain("  fraud_proof_mint_redeemer_index: Data.Integer(),");
    expect(sdk).toContain("  spend_inputs_opening: FieldOpeningV1Schema,");
    // Positional order matches the shipped `ZeroInputStep02ArgsSchema`: the
    // mint-redeemer index precedes the family-specific opening.
    expect(sdk.indexOf("fraud_proof_mint_redeemer_index")).toBeLessThan(
      sdk.indexOf("spend_inputs_opening"),
    );
  });

  it("mirrors the terminal-args pattern in every shipped family, not only zero-input", async () => {
    // The pattern is read back out of the generator's own emitted module rather
    // than restated here, so this sweep moves with the generator instead of
    // pinning a second, drifting copy of its rule. A sentinel family-specific
    // argument marks where the boilerplate prefix ends.
    const sentinel = "sentinel_family_argument";
    const sentinelSpec: FraudProofFamilyScaffoldSpecV1 =
      parseFraudProofFamilyScaffoldSpecV1(
        mutatedSpec((raw) => {
          const steps = raw.steps as Record<string, unknown>[];
          steps[1].argsFields = [
            { name: sentinel, type: "int", doc: "Sentinel family argument." },
          ];
        }),
      );
    const generated = aikenRecordFieldNames(
      artifact(
        generateFraudProofFamilyScaffoldV1({ spec: sentinelSpec }),
        `onchain/aiken/lib/midgard/fraud-proofs/${sentinelSpec.family}/step-02.ak`,
      ).contents,
      "Args",
    );
    expect(generated.at(-1)).toBe(sentinel);
    const prefix = generated.slice(0, -1);
    // The shipped standard: positional indices, then the mint-redeemer index,
    // then whatever the family itself needs.
    expect(prefix).toEqual([
      "input_index",
      "output_index",
      "fraud_proof_mint_redeemer_index",
    ]);

    const terminals = shippedTerminalStepModules();
    // Control against a sweep that measures nothing: every family that ships
    // numbered step modules must be reached, including the deep one.
    expect(terminals.length).toBeGreaterThanOrEqual(17);
    expect(terminals.map((terminal) => terminal.id)).toEqual(
      expect.arrayContaining([
        "zero-input/step-02.ak",
        "double-spend/step-04.ak",
        "no-input/step-04.ak",
        "missing-native-script-tx/step-06.ak",
        "invalid-signature/step-02.ak",
        "no-reference-input/step-04.ak",
        "withdrawn-reference-input/step-03.ak",
      ]),
    );

    const deviating: string[] = [];
    const conforming: string[] = [];
    for (const terminal of terminals) {
      const fields = aikenRecordFieldNames(
        await readFile(terminal.path, "utf8"),
        "Args",
      );
      // A parser that quietly stopped finding fields would report every module
      // conforming, so an empty field list is a failure, never a pass.
      expect(
        fields.length,
        `${terminal.id} declares no Args fields`,
      ).toBeGreaterThan(0);
      (startsWithPrefix(fields, prefix) ? conforming : deviating).push(
        terminal.id,
      );
    }

    // Issue #626 reordered the last three deviating terminals, so the generator
    // now covers every shipped family with no exception list at all.
    expect(deviating).toEqual([]);
    // Decision 0005 R7: `no-input/step-04` is normalized inside the #617 wave,
    // so the generator now covers it with no per-family special case.
    expect(conforming).toContain("no-input/step-04.ak");

    // Hostile control: the pre-normalization `no-input/step-04` order must
    // still be measured as a deviation. A checker that accepted it would call
    // this sweep green while the divergence R7 exists to close was still there.
    const preNormalizationNoInputStep04 = [
      "pub type Args {",
      "  input_index: Int,",
      "  output_index: Int,",
      "  /// The prover's chosen carriage for the transactions-root absence proof.",
      "  non_membership_in_txs: NonMembershipCarriage,",
      "  fraud_proof_mint_redeemer_index: Int,",
      "}",
    ].join("\n");
    expect(
      aikenRecordFieldNames(preNormalizationNoInputStep04, "Args"),
    ).toEqual([
      "input_index",
      "output_index",
      "non_membership_in_txs",
      "fraud_proof_mint_redeemer_index",
    ]);
    expect(
      startsWithPrefix(
        aikenRecordFieldNames(preNormalizationNoInputStep04, "Args"),
        prefix,
      ),
    ).toBe(false);

    // Control on the parser itself: a multi-line generic argument contributes
    // its own field name and none of its type arguments, so the family this
    // shape belongs to is judged on its fields rather than on line noise.
    expect(
      aikenRecordFieldNames(
        await readFile(
          join(FRAUD_PROOF_LIB_ROOT, "withdrawn-reference-input/step-03.ak"),
          "utf8",
        ),
        "Args",
      ),
    ).toEqual([
      "input_index",
      "output_index",
      "fraud_proof_mint_redeemer_index",
      "withdrawal_membership",
    ]);
  });
});
