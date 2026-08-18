/**
 * Family-scaffold emitters (Goal task `Q02`).
 *
 * Boilerplate only. Every emitted artifact is modelled on the shipped, deployed
 * `zero-input` and `double-spend` families, so the shape a family task starts
 * from is the shape that already compiles and binds. Family-specific logic is
 * emitted as `todo`, never as a permissive default: an unimplemented scaffold
 * fails loudly instead of accepting anything.
 */
import type { ScaffoldArtifactV1 } from "./permissive-dispatch-v1.js";
import {
  familyScaffoldNamesV1,
  type FraudProofFamilyScaffoldSpecV1,
  type ScaffoldFieldTypeV1,
  type ScaffoldFieldV1,
  type ScaffoldStepV1,
  stepFileNameV1,
  stepModuleNameV1,
  stepPascalNameV1,
} from "./spec-v1.js";

type FieldBinding = {
  readonly aikenType: string;
  readonly aikenImport: string | null;
  readonly tsSchema: string;
  readonly tsImport: "common" | "fieldOpeningV1" | null;
};

const FIELD_BINDINGS: Readonly<Record<ScaffoldFieldTypeV1, FieldBinding>> = {
  hash32: {
    aikenType: "ByteArray",
    aikenImport: null,
    tsSchema: "H32Schema",
    tsImport: "common",
  },
  // Post-#575 shape: every rebound family thread carries the disputed
  // transaction's §2.5 anchor (its id), not a per-field collection
  // commitment. `midgard_inputs_hash` (`MidgardInputsHash`) is retired from
  // this vocabulary because no shipped fraud-proof consumer carries it any
  // more (issue #607).
  midgard_tx_id: {
    aikenType: "MidgardTxId",
    aikenImport: "use midgard/ledger_state.{MidgardTxId}",
    tsSchema: "H32Schema",
    tsImport: "common",
  },
  // The §8 door opening a downstream step re-opens one committed field
  // through, in place of a reproduced `..._preimage: List<…>` argument.
  field_opening_v1: {
    aikenType: "FieldOpeningV1",
    aikenImport: "use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}",
    tsSchema: "FieldOpeningV1Schema",
    tsImport: "fieldOpeningV1",
  },
  int: {
    aikenType: "Int",
    aikenImport: null,
    tsSchema: "Data.Integer()",
    tsImport: null,
  },
  bytes: {
    aikenType: "ByteArray",
    aikenImport: null,
    tsSchema: "Data.Bytes()",
    tsImport: null,
  },
};

const sortedUses = (uses: readonly string[]): string =>
  [...new Set(uses)].sort((left, right) => (left < right ? -1 : 1)).join("\n");

const aikenRecordBody = (fields: readonly ScaffoldFieldV1[]): string =>
  fields
    .map((field) => `  ${field.name}: ${FIELD_BINDINGS[field.type].aikenType},`)
    .join("\n");

/**
 * Every non-first step consumes the state declared by the preceding step's
 * output contract. The parser proves this is also the step's inputState, but
 * the emitter intentionally reads the preceding output so that this contract
 * cannot be silently dropped by a future schema change.
 */
const precedingOutputState = (
  spec: FraudProofFamilyScaffoldSpecV1,
  step: ScaffoldStepV1,
): readonly ScaffoldFieldV1[] => {
  if (step.index === 1) {
    return [];
  }
  const previous = spec.steps[step.index - 2];
  if (previous === undefined || previous.outputState === null) {
    throw new Error(
      `missing preceding output state for ${spec.family} step ${step.index.toString()}`,
    );
  }
  return previous.outputState;
};

const moduleHeader = (
  spec: FraudProofFamilyScaffoldSpecV1,
  title: string,
): string =>
  [
    `//// ## ${title}`,
    "////",
    `//// ${spec.summary}`,
    "////",
    `//// Violation: ${spec.violationId}`,
    `//// Catalogue category: ${spec.catalogueCategory}`,
    "////",
    "//// Generated boilerplate (Goal Q02 family scaffold generator). Every",
    "//// schema below is explicit and every family-specific decision is `todo`,",
    `//// so nothing here can pass before ${spec.taskId} implements it.`,
    "",
  ].join("\n");

const terminalArgsFields = (
  step: ScaffoldStepV1,
  isTerminal: boolean,
): readonly ScaffoldFieldV1[] => {
  const positional: ScaffoldFieldV1[] = [
    { name: "input_index", type: "int", doc: "Own input index." },
    { name: "output_index", type: "int", doc: "Produced output index." },
  ];
  // Shipped terminal steps (`zero-input/step-02.ak`, `double-spend/step-04.ak`)
  // carry the mint-redeemer index immediately after the two positional
  // indices, with every family-specific field trailing it.
  const mintRedeemerIndex: readonly ScaffoldFieldV1[] = isTerminal
    ? [
        {
          name: "fraud_proof_mint_redeemer_index",
          type: "int",
          doc: "Index of the fraud-proof mint redeemer.",
        },
      ]
    : [];
  return [...positional, ...mintRedeemerIndex, ...(step.argsFields ?? [])];
};

/** `lib/midgard/fraud-proofs/<family>/step-NN.ak` — explicit step types. */
export const emitAikenStepTypesModuleV1 = ({
  spec,
  step,
}: {
  readonly spec: FraudProofFamilyScaffoldSpecV1;
  readonly step: ScaffoldStepV1;
}): ScaffoldArtifactV1 => {
  const isFirst = step.index === 1;
  const isTerminal = step.index === spec.steps.length;
  const uses = ["use midgard/computation_thread as ct"];
  const blocks: string[] = [];

  if (isFirst) {
    uses.push("use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}");
    blocks.push("pub type Datum =\n  ct.StepDatum<Data>");
    blocks.push("pub type Args =\n  NativeTxInclusionCarriage");
  } else {
    const state = precedingOutputState(spec, step);
    for (const field of state) {
      const binding = FIELD_BINDINGS[field.type].aikenImport;
      if (binding !== null) {
        uses.push(binding);
      }
    }
    blocks.push(`pub type State {\n${aikenRecordBody(state)}\n}`);
    blocks.push("pub type Datum =\n  ct.StepDatum<State>");
    const args = terminalArgsFields(step, isTerminal);
    for (const field of args) {
      const binding = FIELD_BINDINGS[field.type].aikenImport;
      if (binding !== null) {
        uses.push(binding);
      }
    }
    blocks.push(`pub type Args {\n${aikenRecordBody(args)}\n}`);
  }
  blocks.push("pub type SpendRedeemer =\n  ct.StepRedeemer<Args>");

  const header = [
    `//// Generated step types for the ${spec.family} fraud-proof family`,
    `//// (${spec.taskId}, violation ${spec.violationId}).`,
    "",
  ].join("\n");

  return {
    path: `onchain/aiken/lib/midgard/fraud-proofs/${spec.family}/${stepFileNameV1(
      step.index,
    )}.ak`,
    language: "aiken",
    contents: `${header}${sortedUses(uses)}\n\n${blocks.join("\n\n")}\n`,
  };
};

const cancelArm = `      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )`;

const firstStepValidatorBody = (
  spec: FraudProofFamilyScaffoldSpecV1,
  step: ScaffoldStepV1,
): string => {
  const names = familyScaffoldNamesV1(spec.family);
  const nextModule = stepModuleNameV1(step.index + 1);
  return `      ct.Continue(tx_inclusion_args) -> {
        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            _m_input_state_data,
            output_script_hash,
            output_state_data,
            _header,
            _bad_tx_id,
            _bad_tx_view
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        // 2. The proof must continue at the next step's script hash.
        expect output_script_hash == ${nextModule}_validator_script_hash

        // 3. Rule: ${step.rule}
        //    TODO(${spec.taskId}): drop the leading underscore from
        //    \`_bad_tx_view\`, build the exact ${nextModule} input state from
        //    the verified native transaction, and keep the following equality as
        //    an executable check: \`expect output_state_data == expected_output_state\`.
        todo @"${spec.taskId} ${names.aikenModule} ${stepModuleNameV1(step.index)}: rule check unimplemented"
      }`;
};

const intermediateStepValidatorBody = (
  spec: FraudProofFamilyScaffoldSpecV1,
  step: ScaffoldStepV1,
): string => {
  const names = familyScaffoldNamesV1(spec.family);
  const nextModule = stepModuleNameV1(step.index + 1);
  const stateFields = precedingOutputState(spec, step)
    .map((field) => `              ${field.name},`)
    .join("\n");
  const argsFields = terminalArgsFields(step, false)
    .map((field) => `        ${field.name},`)
    .join("\n");
  return `      ct.Continue(Args {
${argsFields}
      }) -> {
        let Transaction { inputs, outputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. The carried computation-thread state must be authentic and
        //    reproduced at the next step.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )

        expect
            Some(State {
${stateFields}
            })
          = m_input_state_data

        expect output_script_hash == ${nextModule}_validator_script_hash

        // 2. Rule: ${step.rule}
        //    TODO(${spec.taskId}): advance the carried state and keep the
        //    following equality as an executable check:
        //    \`expect output_state_data == expected_output_state\`.
        todo @"${spec.taskId} ${names.aikenModule} ${stepModuleNameV1(step.index)}: rule check unimplemented"
      }`;
};

const terminalStepValidatorBody = (
  spec: FraudProofFamilyScaffoldSpecV1,
  step: ScaffoldStepV1,
): string => {
  const names = familyScaffoldNamesV1(spec.family);
  const stateFields = precedingOutputState(spec, step)
    .map((field) => `              ${field.name},`)
    .join("\n");
  const argsFields = terminalArgsFields(step, true)
    .map((field) => `        ${field.name},`)
    .join("\n");
  return `      ct.Continue(Args {
${argsFields}
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. The carried computation-thread state must be authentic and the
        //    permanent fraud-proof token must be minted to its address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )

        expect
            Some(State {
${stateFields}
            })
          = m_input_state_data

        // 2. Rule: ${step.rule}
        //    TODO(${spec.taskId}): assert the exact violation over the carried
        //    state. The proof must conclude only when the rule is broken.
        todo @"${spec.taskId} ${names.aikenModule} ${stepModuleNameV1(step.index)}: rule check unimplemented"
      }`;
};

/**
 * An unimplemented Aiken test body.
 *
 * A bare `todo` is rejected by the compiler (`aiken::check::illegal::test::
 * return`: tests must return `Bool` or `Void`), so the `todo` is bound at
 * `Bool` and returned. The test therefore *fails loudly* with the message until
 * the family task replaces it.
 */
const unimplementedAikenTest = (
  spec: FraudProofFamilyScaffoldSpecV1,
  aikenModule: string,
  name: string,
): string => `  let unimplemented: Bool =
    todo @"${spec.taskId} ${aikenModule}: ${name} unimplemented"
  unimplemented`;

const firstStepTests = (spec: FraudProofFamilyScaffoldSpecV1): string => {
  const names = familyScaffoldNamesV1(spec.family);
  const nextModule = stepModuleNameV1(2);
  const positive = spec.tests.positive[0] ?? `${names.aikenModule}_positive`;
  const negative =
    spec.tests.validBlockNegative[0] ?? `${names.aikenModule}_valid_block`;
  const extraTests = [
    ...spec.tests.positive.slice(1),
    ...spec.tests.validBlockNegative.slice(1),
    ...spec.tests.mutation,
    ...spec.tests.maximumFit,
  ]
    .map(
      (name) => `/// TODO(${spec.taskId}): implement this selector.
test ${name}() {
${unimplementedAikenTest(spec, names.aikenModule, name)}
}`,
    )
    .join("\n\n");

  return `/// The transaction that violates ${spec.violationId}.
///
/// TODO(${spec.taskId}): mutate the shared valid native-V1 transaction so it
/// breaks the rule, and nothing else.
fn ${names.aikenModule}_bad_tx() -> MidgardTransaction {
  todo @"${spec.taskId} ${names.aikenModule}: bad transaction fixture unimplemented"
}

/// The exact ${nextModule} input state this step must forward.
///
/// TODO(${spec.taskId}): drop the underscore from \`_block\` and derive the state
/// from the verified native transaction it carries.
fn ${names.aikenModule}_${nextModule}_state(_block: fixture.NativeBlockFixtureV1) -> Data {
  todo @"${spec.taskId} ${names.aikenModule}: forwarded state unimplemented"
}

/// Positive binding selector: the violating native-V1 transaction, genuinely
/// committed under the block's counted \`transactions_root\`, passes step one.
test ${positive}() {
  let block = fixture.native_block_fixture_v1(${names.aikenModule}_bad_tx())
  let expected_output_state: Data = ${names.aikenModule}_${nextModule}_state(block)
  let l1_tx = fixture.continue_step_l1_tx_v1(
    block,
    expected_output_state,
    block.transactions_phas_root,
    block.tx_id,
    block.compact_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(fixture.native_inclusion_carriage_v1(block)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Valid-block negative selector: a raw transactions root that the header's
/// counted \`transactions_root\` does not commit cannot bind, so fabricated
/// evidence against a valid block is rejected at step one.
///
/// TODO(${spec.taskId}): add the \`fail\` annotation once this selector runs and
/// is observed to reject. A \`fail\` test whose body still contains \`todo\`
/// passes vacuously and proves nothing.
test ${negative}() {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.native_block_fixture_v1(${names.aikenModule}_bad_tx())
  let expected_output_state: Data = ${names.aikenModule}_${nextModule}_state(forged_block)
  let claimed_block = fixture.NativeBlockFixtureV1 {
    ..forged_block,
    header: valid_block.header,
    header_hash: valid_block.header_hash,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    claimed_block,
    expected_output_state,
    claimed_block.transactions_phas_root,
    claimed_block.tx_id,
    claimed_block.compact_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(fixture.native_inclusion_carriage_v1(claimed_block)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

${extraTests}`;
};

const laterStepTests = (
  spec: FraudProofFamilyScaffoldSpecV1,
  step: ScaffoldStepV1,
): string => {
  const names = familyScaffoldNamesV1(spec.family);
  const stepModule = stepModuleNameV1(step.index);
  const selectors = [
    ...spec.tests.positive,
    ...spec.tests.validBlockNegative,
    ...spec.tests.mutation,
    ...spec.tests.maximumFit,
  ].map((name) => `${name}_${stepModule}`);
  return selectors
    .map(
      (
        name,
      ) => `/// TODO(${spec.taskId}): implement this ${stepModule} selector.
test ${name}() {
${unimplementedAikenTest(spec, names.aikenModule, name)}
}`,
    )
    .join("\n\n");
};

/** `validators/fraud-proofs/<family>/step-NN.ak` — step validator + selectors. */
export const emitAikenStepValidatorV1 = ({
  spec,
  step,
}: {
  readonly spec: FraudProofFamilyScaffoldSpecV1;
  readonly step: ScaffoldStepV1;
}): ScaffoldArtifactV1 => {
  const names = familyScaffoldNamesV1(spec.family);
  const stepModule = stepModuleNameV1(step.index);
  const isFirst = step.index === 1;
  const isTerminal = step.index === spec.steps.length;
  const nextModule = stepModuleNameV1(step.index + 1);

  // Only the imports the emitted body actually uses: the first step drives the
  // shared native-binding fixture, later steps get their imports when the
  // family task implements their selectors.
  const uses = [
    "use cardano/assets.{PolicyId}",
    "use cardano/transaction.{OutputReference, Transaction}",
    "use midgard/computation_thread as ct",
  ];
  let parameters: string;
  let body: string;
  let tests: string;

  if (isFirst) {
    uses.push(
      "use aiken/crypto.{ScriptHash}",
      "use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step_carried}",
      "use midgard/fraud_proofs/native_binding_fixture_v1 as fixture",
      "use midgard/fraud_proofs/native_tx/types.{MidgardTransaction}",
      `use midgard/fraud_proofs/${names.aikenModule}/${stepModule}.{Datum, SpendRedeemer}`,
    );
    parameters = `  ${nextModule}_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,`;
    body = firstStepValidatorBody(spec, step);
    tests = firstStepTests(spec);
  } else if (isTerminal) {
    uses.push(
      "use cardano/address.{Address}",
      "use midgard/fraud_proofs/common.{cancel, finalize}",
      `use midgard/fraud_proofs/${names.aikenModule}/${stepModule}.{Args, Datum, SpendRedeemer, State}`,
    );
    parameters = `  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,`;
    body = terminalStepValidatorBody(spec, step);
    tests = laterStepTests(spec, step);
  } else {
    uses.push(
      "use aiken/crypto.{ScriptHash}",
      "use midgard/fraud_proofs/common.{cancel, continue}",
      `use midgard/fraud_proofs/${names.aikenModule}/${stepModule}.{Args, Datum, SpendRedeemer, State}`,
    );
    parameters = `  ${nextModule}_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,`;
    body = intermediateStepValidatorBody(spec, step);
    tests = laterStepTests(spec, step);
  }

  const title = `${names.pascal} Fraud Proof — ${stepModule}`;
  const contents = `${moduleHeader(spec, title)}
${sortedUses(uses)}

validator main(
${parameters}
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
${cancelArm}
${body}
    }
  }

  else(_) {
    fail
  }
}

${tests}
`;

  return {
    path: `onchain/aiken/validators/fraud-proofs/${spec.family}/${stepFileNameV1(
      step.index,
    )}.ak`,
    language: "aiken",
    contents,
  };
};

const tsSchemaFields = (fields: readonly ScaffoldFieldV1[]): string =>
  fields
    .map(
      (field) =>
        `  /** ${field.doc} */\n  ${field.name}: ${FIELD_BINDINGS[field.type].tsSchema},`,
    )
    .join("\n");

const tsAlias = (name: string, schema: string): string =>
  `export const ${name}Schema = ${schema};
export type ${name} = Data.Static<typeof ${name}Schema>;
export const ${name} = ${name}Schema as unknown as ${name};`;

/** `demo/midgard-sdk/src/fraud-proof/<family>.ts` — explicit off-chain schemas. */
export const emitSdkFamilyModuleV1 = (
  spec: FraudProofFamilyScaffoldSpecV1,
): ScaffoldArtifactV1 => {
  const names = familyScaffoldNamesV1(spec.family);
  const usesCommonSchema = spec.steps.some((step) =>
    [...precedingOutputState(spec, step), ...(step.argsFields ?? [])].some(
      (field) => FIELD_BINDINGS[field.type].tsImport === "common",
    ),
  );
  const usesFieldOpeningV1Schema = spec.steps.some((step) =>
    [...precedingOutputState(spec, step), ...(step.argsFields ?? [])].some(
      (field) => FIELD_BINDINGS[field.type].tsImport === "fieldOpeningV1",
    ),
  );

  const blocks: string[] = [];
  for (const step of spec.steps) {
    const stepName = `${names.pascal}${stepPascalNameV1(step.index)}`;
    const isFirst = step.index === 1;
    const isTerminal = step.index === spec.steps.length;
    if (isFirst) {
      blocks.push(
        tsAlias(`${stepName}Datum`, "faultProofStepDatumSchema(Data.Any())"),
      );
      blocks.push(
        tsAlias(
          `${stepName}SpendRedeemer`,
          "faultProofStepRedeemerSchema(NativeTxInclusionCarriageSchema)",
        ),
      );
      continue;
    }
    const state = precedingOutputState(spec, step);
    blocks.push(
      tsAlias(
        `${stepName}State`,
        `Data.Object({\n${tsSchemaFields(state)}\n})`,
      ),
    );
    blocks.push(
      tsAlias(
        `${stepName}Datum`,
        `faultProofStepDatumSchema(${stepName}StateSchema)`,
      ),
    );
    blocks.push(
      tsAlias(
        `${stepName}Args`,
        `Data.Object({\n${tsSchemaFields(terminalArgsFields(step, isTerminal))}\n})`,
      ),
    );
    blocks.push(
      tsAlias(
        `${stepName}SpendRedeemer`,
        `faultProofStepRedeemerSchema(${stepName}ArgsSchema)`,
      ),
    );
  }

  const stepNames = spec.steps
    .map((step) => `"${stepModuleNameV1(step.index)}"`)
    .join(", ");
  const resolverCases = spec.steps
    .map(
      (step) =>
        `    case "${stepModuleNameV1(step.index)}":\n      return ${names.pascal}${stepPascalNameV1(
          step.index,
        )}DatumSchema;`,
    )
    .join("\n");

  // Import groups, in the order the shipped `#604`-rebound modules use them:
  // the external package, then the `@/`-aliased common schemas, then the
  // family-scaffold's own relative modules (grouped together, no blank line
  // between them).
  const aliasImportLines = usesCommonSchema
    ? ['import { H32Schema } from "@/common.js";']
    : [];
  const relativeImportLines = [
    ...(usesFieldOpeningV1Schema
      ? ['import { FieldOpeningV1Schema } from "./field-opening-v1.js";']
      : []),
    `import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";`,
  ];
  const importBlock = [
    'import { Data } from "@lucid-evolution/lucid";',
    ...(aliasImportLines.length > 0 ? ["", ...aliasImportLines] : []),
    "",
    ...relativeImportLines,
  ].join("\n");

  const contents = `/**
 * ${spec.summary}
 *
 * Violation: ${spec.violationId}
 * Catalogue category: ${spec.catalogueCategory}
 *
 * Generated boilerplate (Goal Q02 family scaffold generator). Schemas are
 * explicit and the step resolver is an exhaustive union with no fallback, so an
 * unknown step name is a compile error rather than a silently accepted value.
 */
${importBlock}

${blocks.join("\n\n")}

export const ${names.screamingSnake}_STEP_NAMES_V1 = [${stepNames}] as const;
export type ${names.pascal}StepNameV1 =
  (typeof ${names.screamingSnake}_STEP_NAMES_V1)[number];

/**
 * Explicit, exhaustive step-datum resolver. There is no default branch: adding
 * a step without adding its schema fails to compile.
 */
export const ${names.camel}StepDatumSchemaV1 = (
  step: ${names.pascal}StepNameV1,
) => {
  switch (step) {
${resolverCases}
  }
};
`;
  return {
    path: `demo/midgard-sdk/src/fraud-proof/${spec.family}.ts`,
    language: "typescript",
    contents,
  };
};

/** `demo/midgard-sdk/tests/<family>-v1.test.ts` — codec-agreement stubs. */
export const emitSdkFamilyTestModuleV1 = (
  spec: FraudProofFamilyScaffoldSpecV1,
): ScaffoldArtifactV1 => {
  const names = familyScaffoldNamesV1(spec.family);
  const roundTrips = spec.steps
    .filter((step) => step.index > 1)
    .map(
      (step) => `  it("${stepModuleNameV1(
        step.index,
      )} state round-trips through the canonical codec", () => {
    throw new Error(
      "SCAFFOLD_UNIMPLEMENTED ${spec.taskId} ${spec.family} ${stepModuleNameV1(
        step.index,
      )}: assert Data.to/Data.from agreement against the Aiken vector",
    );
  });`,
    )
    .join("\n\n");

  const contents = `/**
 * ${spec.taskId} — ${spec.family} off-chain codec agreement.
 *
 * Generated boilerplate (Goal Q02 family scaffold generator). Every test below
 * throws \`SCAFFOLD_UNIMPLEMENTED\` on purpose: a generated family must never
 * report a green suite before its closure implements the assertions.
 */
import { describe, it } from "vitest";

import { ${names.pascal}Step01DatumSchema } from "@/fraud-proof/${spec.family}.js";

describe("${spec.family} codec agreement (${spec.taskId})", () => {
  it("pins the step-01 datum schema against the deployed validator", () => {
    void ${names.pascal}Step01DatumSchema;
    throw new Error(
      "SCAFFOLD_UNIMPLEMENTED ${spec.taskId} ${spec.family} step_01: assert the schema against the Aiken blueprint",
    );
  });

${roundTrips}
});
`;
  return {
    path: `demo/midgard-sdk/tests/${spec.family}-v1.test.ts`,
    language: "typescript",
    contents,
  };
};

/**
 * The ten `GOAL_SPEC.md` §9.1 `LOCAL_PASS` outputs, verbatim, so a generated
 * family carries its own closure checklist instead of relying on the parent to
 * remember what a family owes.
 */
export const FAMILY_CLOSURE_OUTPUTS_V1 = [
  "Normative rule and violation identifier.",
  "Canonical evidence schema and strict TypeScript/Aiken codec agreement.",
  "Correct native counted-root/typed-commitment binding.",
  "Aiken proof steps with positive and valid-block negative tests.",
  "Maximum/adversarial proof-fit fixture.",
  "Catalogue identifier, first-step hash, membership proof, deployment and reference-script records.",
  "DA-first evidence builder using retained authenticated material.",
  "One resumable prepare/submit command, with no hidden manual state.",
  "Emulator lifecycle: init, every step, permanent proof token, fraudulent header/claim removal, correct slashing/reward.",
  "Coverage and catalogue rows changed to `LOCAL_PASS` at integration.",
] as const;

export const SCAFFOLD_CLOSURE_ROW_STATUS_V1 = "TODO" as const;

/** `<family>-closure-checklist-v1.json` — the §9.1 outputs, all unclaimed. */
export const emitClosureChecklistV1 = (
  spec: FraudProofFamilyScaffoldSpecV1,
): ScaffoldArtifactV1 => {
  const contents = `${JSON.stringify(
    {
      schemaVersion: "midgard-family-closure-checklist-v1",
      taskId: spec.taskId,
      family: spec.family,
      violationId: spec.violationId,
      catalogueCategory: spec.catalogueCategory,
      generatedBy: "Q02 family scaffold generator",
      note: "Generated boilerplate. Every row is unclaimed: only the owning family task may change a status, and only with the evidence GOAL_SPEC.md §9.1 requires.",
      steps: spec.steps.map((step) => ({
        index: step.index,
        module: stepModuleNameV1(step.index),
        rule: step.rule,
      })),
      declaredTests: spec.tests,
      closureOutputs: FAMILY_CLOSURE_OUTPUTS_V1.map((output, index) => ({
        output: index + 1,
        requirement: output,
        status: SCAFFOLD_CLOSURE_ROW_STATUS_V1,
      })),
    },
    null,
    2,
  )}\n`;
  return {
    path: `docs/exec-plans/evidence/family-closure/${spec.family}-closure-checklist-v1.json`,
    language: "json",
    contents,
  };
};
