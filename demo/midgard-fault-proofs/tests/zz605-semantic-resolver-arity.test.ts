/**
 * #605/#609 PERMANENT GATE — the under-applied always-succeeds class.
 *
 * The compiled blueprint declares, per validator, exactly how many
 * `validator main(...)` parameters must be applied before the script context.
 * `applyParamsToScript` applies parameters POSITIONALLY and performs no arity
 * check, so applying too few leaves a partially-applied program: the ledger's
 * single Plutus V3 script-context application then reduces to a lambda VALUE,
 * evaluation terminates without error, and the ledger reads "no error" as
 * SUCCESS. Such a validator is an always-succeeds script regardless of what its
 * Aiken source says. That is how ten validation-trace semantic resolvers were
 * deployed after #592 gave them `field_preimage_certificate_policy_id`.
 *
 * WHAT THIS GATE MUST NOT BECOME. Pinning the deployed hashes would make it
 * verify the deployment against itself — the shape `validation-resolver-applied-
 * hashes` had, which pinned the UNDER-APPLIED hashes and greened on them (the
 * program's sixth gate-that-cannot-fail). So this file pins NO hash. It asserts
 * three things that stay falsifiable no matter what the builder does:
 *
 *   1. Every deployed semantic resolver equals the FULL application of its own
 *      declared parameters, and equals NONE of its under-applied prefixes. The
 *      prefixes are constructed here on purpose: they are the always-succeeds
 *      scripts, and the assertion is that the deployment is not one of them.
 *   2. No production source outside the sanctioned helper calls
 *      `applyParamsToScript` directly — the helper is the only door, so the
 *      guarantee cannot be routed around by a new call site.
 *   3. The builder fails CLOSED, proven by driving the real public builder with
 *      a doctored blueprint: a hand-written parameter list one shorter than
 *      declared, one longer than declared, and a declared parameter name the
 *      name-keyed semantic loop has no value for.
 *
 * Leg 1 is measured against the builder's real output, legs 2 and 3 are
 * measured against production source and the real public builder, so none of
 * the three can be satisfied by the fix that made them pass.
 */
import { readdirSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  buildFaultProofContracts,
  CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1,
  FAULT_PROOF_SHARED_TITLES,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
} from "@al-ft/midgard-sdk";
import {
  applyParamsToScript,
  Data,
  mintingPolicyToId,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
const blueprintPath =
  process.env["MIDGARD_REAL_BLUEPRINT_PATH"] ??
  resolve(repoRoot, "onchain/aiken/plutus.json");

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  readonly parameters?: readonly { readonly title: string }[];
};

const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as {
  readonly validators: readonly BlueprintValidator[];
};

const byTitle = new Map(blueprint.validators.map((v) => [v.title, v]));

/** Absent `parameters` is the compiler's encoding of "declares none". */
const declaredParameters = (
  validator: BlueprintValidator,
): readonly { readonly title: string }[] => validator.parameters ?? [];

const requireValidator = (title: string): BlueprintValidator => {
  const validator = byTitle.get(title);
  if (validator === undefined) {
    throw new Error(`blueprint is missing ${title}`);
  }
  return validator;
};

/**
 * The deployment loop in `contracts.ts` iterates
 * `Object.values(VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics)`, so
 * reading the same object here measures the real deployment order rather than a
 * re-derived guess that could drift.
 */
const SEMANTIC_TITLES: readonly string[] = Object.values(
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
);

const PREPARE_TITLES: readonly string[] = Object.values(
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares,
);

const scriptHashOf = (script: string): string =>
  validatorToScriptHash({ type: "PlutusV3", script });

/**
 * The gate reads the raw `plutus.json` rather than routing through the SDK's
 * own parser, so the declared-arity side of every comparison below stays
 * independent of the code under test. Raw entries omit `parameters` entirely on
 * nullary validators, which is exactly the shape the builder must read as
 * "declares none" — so handing it the raw object is the contract being tested,
 * and this cast is only bridging that optionality.
 */
type BuilderBlueprint = Parameters<
  typeof buildFaultProofContracts
>[0]["blueprint"];

const buildContracts = async (
  overrideBlueprint: typeof blueprint = blueprint,
) =>
  Effect.runPromise(
    buildFaultProofContracts({
      blueprint: overrideBlueprint as unknown as BuilderBlueprint,
      network: "Preprod",
      hubOraclePolicyId: "bb".repeat(28),
      fraudProofCataloguePolicyId: "cc".repeat(28),
    }),
  );

/** Building the whole fault-proof set applies hundreds of scripts. */
const BUILD_TIMEOUT_MS = 180_000;

/** A deep copy of the blueprint with one validator's declared parameters replaced. */
const blueprintWithParameters = (
  title: string,
  parameters: readonly { readonly title: string }[],
): typeof blueprint => {
  const copy = JSON.parse(JSON.stringify(blueprint)) as {
    validators: BlueprintValidator[];
  };
  const target = copy.validators.find((validator) => validator.title === title);
  if (target === undefined) {
    throw new Error(`blueprint is missing ${title}`);
  }
  (target as { parameters: readonly { readonly title: string }[] }).parameters =
    parameters;
  return copy;
};

describe("zz605/zz609 validation-trace resolver parameter arity", () => {
  it(
    "deploys every semantic resolver fully applied, and as none of its under-applied prefixes",
    { timeout: BUILD_TIMEOUT_MS },
    async () => {
      const contracts = await buildContracts();
      const dispute = contracts.validationTraceDispute;

      /**
       * Re-derived here from the BUILT contract set, independently of the
       * builder's internal parameter table: if the builder silently changed which
       * value it feeds a named parameter, these values still come from the
       * deployed artifacts and the equality below would fail.
       */
      const parameterValues = new Map<string, Data>([
        ["award_script_hash", dispute.award.spendingScriptHash],
        ["computation_thread_policy_id", contracts.computationThread.policyId],
        [
          "field_preimage_certificate_policy_id",
          // The §8.6 certificate mint declares no parameters, so its policy id is
          // a pure function of the blueprint. Deriving it here rather than asking
          // the builder for it keeps this side of the comparison independent.
          mintingPolicyToId({
            type: "PlutusV3",
            script: requireValidator(
              FAULT_PROOF_SHARED_TITLES.fieldPreimageCertificateMint,
            ).compiledCode,
          }),
        ],
        [
          "source_binder_script_hash",
          dispute.canonicalDecodeItemStages.source.spendingScriptHash,
        ],
        ["proof_item_script_hash", dispute.proofItem.spendingScriptHash],
        [
          "cek_program_material_script_hash",
          // The immutable CEK program-material validator declares no
          // parameters either (R5 item 1 moved the parameter from the retired
          // cek direct resolver onto the execution-selection semantic), so its
          // hash is likewise a pure function of the blueprint.
          scriptHashOf(
            requireValidator(CEK_PROGRAM_MATERIAL_SPEND_TITLE_V1).compiledCode,
          ),
        ],
      ]);

      const rows: string[] = [];
      const underApplied: string[] = [];

      for (const [index, title] of SEMANTIC_TITLES.entries()) {
        const validator = requireValidator(title);
        const declared = declaredParameters(validator);
        const deployed = dispute.semanticResolvers[index]!.spendingScriptHash;

        const values = declared.map((parameter) => {
          const value = parameterValues.get(parameter.title);
          expect(
            value,
            `${title} declares unknown parameter "${parameter.title}"`,
          ).toBeDefined();
          return value!;
        });

        const fullyApplied = scriptHashOf(
          applyParamsToScript(validator.compiledCode, values),
        );
        if (deployed !== fullyApplied) {
          underApplied.push(
            `[${String(index)}] ${title} deployed=${deployed} fullyApplied=${fullyApplied}`,
          );
        }

        // Every proper prefix is an always-succeeds script. The deployment must
        // be none of them — this is the #605 exploit stated as an assertion.
        for (let count = 0; count < declared.length; count += 1) {
          const prefix = scriptHashOf(
            applyParamsToScript(validator.compiledCode, values.slice(0, count)),
          );
          expect(
            deployed,
            `${title} is deployed under-applied with ${String(count)} of ` +
              `${String(declared.length)} declared parameters — an always-succeeds script`,
          ).not.toBe(prefix);
        }

        rows.push(
          `[${String(index).padStart(2, "0")}] declared=${String(declared.length)} ` +
            `${deployed === fullyApplied ? "ok" : "MISMATCH"}  ` +
            `${title.replace("fraud_proofs/validation_trace/", "")}  ` +
            `(${declared.map((p) => p.title).join(", ")})`,
        );
      }

      console.log(
        `\n#609 validation-trace semantic resolver arity (blueprint ${blueprintPath})\n` +
          `semantic titles: ${String(SEMANTIC_TITLES.length)}\n${rows.join("\n")}\n`,
      );

      expect(underApplied, underApplied.join("\n")).toEqual([]);
    },
  );

  it(
    "deploys every prepare resolver fully applied, and not bare",
    { timeout: BUILD_TIMEOUT_MS },
    async () => {
      const contracts = await buildContracts();
      const dispute = contracts.validationTraceDispute;

      for (const [index, title] of PREPARE_TITLES.entries()) {
        const validator = requireValidator(title);
        const declared = declaredParameters(validator);
        expect(declared, `${title} declared arity`).toHaveLength(2);

        const deployed = dispute.prepareResolvers[index]!.spendingScriptHash;
        expect(
          deployed,
          `${title} is deployed with zero of its ${String(declared.length)} declared parameters`,
        ).not.toBe(scriptHashOf(validator.compiledCode));
      }
    },
  );

  it("reports every certificate-parameter validator's declared arity", () => {
    const rows = blueprint.validators
      .filter((validator) =>
        declaredParameters(validator).some((parameter) =>
          parameter.title.includes("field_preimage_certificate"),
        ),
      )
      .map(
        (validator) =>
          `  declared=${String(declaredParameters(validator).length)}  ${validator.title}\n` +
          `      (${declaredParameters(validator)
            .map((parameter) => parameter.title)
            .join(", ")})`,
      );

    console.log(
      `\n#609 certificate-parameter validators (${String(rows.length)} blueprint entries)\n` +
        `${rows.join("\n")}\n`,
    );
    expect(rows.length).toBeGreaterThan(0);
  });
});

describe("zz609 the arity check is the only door", () => {
  /**
   * The builder's guarantee is only as strong as its exclusivity: a new
   * `applyParamsToScript` call added next to the helper would reopen the class
   * silently. Production source may reach the raw function in exactly one
   * place per package — the helper itself.
   */
  const productionSources = [
    {
      path: "demo/midgard-sdk/src/fraud-proof/contracts.ts",
      allowed: 1,
      helper: "applyBlueprintParams",
    },
    {
      path: "demo/midgard-node/src/services/midgard-contracts.ts",
      allowed: 1,
      helper: "applyBlueprintDeclaredParams",
    },
    // The emulator harness deploys the real blueprint into a real ledger, so
    // an under-applied script there greens a test instead of a chain (#610).
    {
      path: "demo/midgard-fault-proofs/tests/support/emulator/blueprints.ts",
      allowed: 1,
      helper: "applyCompiledScript",
    },
  ] as const;

  it("routes every production deployment through the arity-checking helper", () => {
    for (const source of productionSources) {
      const text = readFileSync(resolve(repoRoot, source.path), "utf8");
      const calls = [...text.matchAll(/\bapplyParamsToScript\(/gu)].length;
      expect(
        calls,
        `${source.path} calls applyParamsToScript directly ${String(calls)} time(s); ` +
          `only ${String(source.allowed)} is allowed (inside ${source.helper}). ` +
          "Route new deployments through the helper so the declared arity is checked (#609).",
      ).toBe(source.allowed);
      expect(text).toContain(source.helper);
    }
  });

  it("confines bare compiledCode reads to the allowlisted loaders (#610)", () => {
    // The call-site count above only sees `applyParamsToScript` calls, which
    // a loader evades by never applying anything: deploying `compiledCode`
    // bare is invisible to it, and a validator title that later grows a
    // declared parameter would keep being deployed under-applied — the #605
    // always-succeeds shape with no failing gate. So every production source
    // that touches `compiledCode` must be one of the known doors.
    //
    // The scan matches FILE TEXT, comments included (#642 item 1): a prose
    // mention of `compiledCode` in an unallowlisted file trips this gate
    // exactly like a real read. That is deliberate — the gate stays a dumb
    // text scan so nothing syntactic can evade it — so word doc comments
    // around it (say "blueprint body") rather than allowlisting the file.
    const scanRoots = [
      "demo/midgard-sdk/src",
      "demo/midgard-node/src",
      // The two fault-proofs sites the #608 addendum classified and the #610
      // ruling (2026-08-18) put inside the door: the runtime loader in `src`,
      // and the emulator harness's own loader under `tests`. `tests` is scanned
      // as well as `src` because an emulator that deploys an always-succeeds
      // script where an authenticated one belongs is a test that cannot fail —
      // the same defect class, one package over.
      "demo/midgard-fault-proofs/src",
      "demo/midgard-fault-proofs/tests",
    ];
    const allowedReaders = [
      // The two arity-checking helpers' homes, already pinned above.
      "demo/midgard-node/src/services/midgard-contracts.ts",
      "demo/midgard-sdk/src/fraud-proof/contracts.ts",
      // Deploys bare behind a zero-declared-parameters assertion (#610).
      "demo/midgard-sdk/src/phas-membership.ts",
      // Devnet-only convenience loader: an intentional, explicit exception.
      "demo/midgard-node/src/services/always-succeeds.ts",
      // `getCompiledScript`, bare behind a zero-declared-parameters assertion,
      // pinned below (#610).
      "demo/midgard-fault-proofs/src/runtime.ts",
      // The emulator harness's `getCompiledScript` (bare, same assertion) plus
      // `applyCompiledScript`, its arity-checking applying door (#610) — moved
      // from `submit-init-emulator-shared.ts` when that file became a barrel
      // over `tests/support/emulator/`.
      "demo/midgard-fault-proofs/tests/support/emulator/blueprints.ts",
      // This gate itself: it reads `compiledCode` to CONSTRUCT the
      // under-applied prefixes it then asserts nothing is deployed as.
      "demo/midgard-fault-proofs/tests/zz605-semantic-resolver-arity.test.ts",
      // The loading-boundary gate: drives both fault-proofs loaders and
      // compares a correct load against the blueprint's own `compiledCode`.
      "demo/midgard-fault-proofs/tests/zz610-compiled-script-arity.test.ts",
    ].sort();
    const collectTypescriptSources = (directory: string): readonly string[] =>
      readdirSync(resolve(repoRoot, directory), {
        withFileTypes: true,
      }).flatMap((entry) =>
        entry.isDirectory()
          ? collectTypescriptSources(`${directory}/${entry.name}`)
          : entry.name.endsWith(".ts")
            ? [`${directory}/${entry.name}`]
            : [],
      );
    const mentionsCompiledCode = (path: string): boolean =>
      /\bcompiledCode\b/u.test(readFileSync(resolve(repoRoot, path), "utf8"));

    /**
     * A selector that collects nothing is the gate-that-cannot-fail shape this
     * whole file exists to prevent: a mistyped or moved root would silently
     * contribute no files and the set equality below would still pass. So every
     * root's yield is MEASURED — both the sources it collected and the readers
     * it matched — asserted nonzero, and reported.
     */
    const perRoot = scanRoots.map((root) => {
      const sources = collectTypescriptSources(root);
      return { root, sources, readers: sources.filter(mentionsCompiledCode) };
    });
    for (const { root, sources, readers } of perRoot) {
      expect(
        sources.length,
        `${root} collected 0 TypeScript sources: a scan root that yields ` +
          "nothing cannot fail (#610).",
      ).toBeGreaterThan(0);
      expect(
        readers.length,
        `${root} matched 0 compiledCode readers: the scan is not reaching ` +
          "this root's loaders (#610).",
      ).toBeGreaterThan(0);
    }
    console.log(
      "\n#610 bare-compiledCode scan roots\n" +
        perRoot
          .map(
            ({ root, sources, readers }) =>
              `  ${String(sources.length).padStart(4)} .ts sources, ` +
              `${String(readers.length).padStart(2)} compiledCode reader(s)  ${root}`,
          )
          .join("\n") +
        "\n",
    );

    const bareReaders = perRoot
      .flatMap(({ readers }) => readers)
      .slice()
      .sort();
    expect(bareReaders).toEqual(allowedReaders);
    // Allowlisting a bare loader is only sound while its zero-arity door
    // stands: each must refuse a declared parameter at load time. Removing an
    // assertion fails this row even though its file stays allowlisted.
    const zeroArityDoors = [
      "demo/midgard-sdk/src/phas-membership.ts",
      "demo/midgard-fault-proofs/src/runtime.ts",
      "demo/midgard-fault-proofs/tests/support/emulator/blueprints.ts",
    ];
    for (const door of zeroArityDoors) {
      expect(
        readFileSync(resolve(repoRoot, door), "utf8"),
        `${door} no longer refuses a declared parameter at its bare-load door (#610)`,
      ).toMatch(
        /declares \$\{declaredParameters\.length\} parameter\(s\) but this loader deploys compiledCode bare/u,
      );
    }
  });

  it(
    "refuses a hand-written parameter list that is one SHORTER than the blueprint declares",
    { timeout: BUILD_TIMEOUT_MS },
    async () => {
      // The prepare loop applies a fixed two-term list, so growing the declared
      // arity to three is an under-application: exactly the #605 shape.
      const doctored = blueprintWithParameters(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares.inputSets,
        [
          { title: "semantic_resolver_script_hashes" },
          { title: "computation_thread_policy_id" },
          { title: "field_preimage_certificate_policy_id" },
        ],
      );
      await expect(buildContracts(doctored)).rejects.toThrow(
        /declares 3 parameter\(s\).*but 2 were applied/su,
      );
    },
  );

  it(
    "refuses a hand-written parameter list that is one LONGER than the blueprint declares",
    { timeout: BUILD_TIMEOUT_MS },
    async () => {
      const doctored = blueprintWithParameters(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares.inputSets,
        [{ title: "semantic_resolver_script_hashes" }],
      );
      await expect(buildContracts(doctored)).rejects.toThrow(
        /declares 1 parameter\(s\).*but 2 were applied/su,
      );
    },
  );

  it(
    "refuses a declared parameter the semantic loop has no value for",
    { timeout: BUILD_TIMEOUT_MS },
    async () => {
      // The semantic loop resolves declared parameters BY NAME, so it adapts to
      // a resolver that grows one. The fail-closed edge is a name it does not
      // know: it must refuse rather than deploy something short.
      const doctored = blueprintWithParameters(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics.inputSetsEmpty,
        [
          { title: "award_script_hash" },
          { title: "computation_thread_policy_id" },
          { title: "a_parameter_this_builder_has_never_heard_of" },
        ],
      );
      await expect(buildContracts(doctored)).rejects.toThrow(
        /has no value for/u,
      );
    },
  );
});
