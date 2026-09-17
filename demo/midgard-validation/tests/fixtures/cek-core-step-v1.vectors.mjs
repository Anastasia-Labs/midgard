/**
 * The **program definitions** behind the V1 CEK core-step golden vectors — one
 * source of truth, driven from two sides.
 *
 * The generator (`scripts/generate-cek-core-step-v1-goldens.mjs`) compiles
 * these through the built `dist/` twins to emit the JSON fixture and the Aiken
 * module; the vitest suite (`tests/cek-core-step-goldens.test.ts`) drives the
 * same definitions through `src/` and checks the result against the checked-in
 * fixture. Because both start from these definitions rather than from the
 * fixture's own bytes, a trace builder that drifts is caught on the TypeScript
 * side too — not only by `--check` and the Aiken verifier.
 *
 * This module holds **UPLC terms, script contexts and twist definitions
 * only**. It imports nothing from the package, so it can be loaded by either
 * side without deciding which build of the CEK is under test. The UPLC terms
 * are built with `@harmoniclabs/uplc` exactly as `tests/cek-executor.test.ts`
 * does, and compiled as `(program 1.1.0 term)` Flat bytes.
 *
 * Every program is small on purpose: the Aiken consumer decodes each step's
 * `CoreStepEvidenceV1` CBOR with the stdlib's pure-Aiken `cbor.deserialise`,
 * which is an order of magnitude more expensive than a builtin, so the set is
 * curated for **witness-arm coverage per step** rather than for size.
 */

import {
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  Case,
  Constr,
  constT,
  Delay,
  ErrorUPLC,
  Force,
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";

/** `(program 1.1.0 term)` as canonical Flat bytes, the V1 script payload. */
export const compileMidgardCekGoldenProgram = (term) =>
  Buffer.from(
    UPLCEncoder.compile(new UPLCProgram([1, 1, 0], term)).toBuffer().buffer,
  );

/** The empty script context every program without a `context` is applied to. */
export const EMPTY_CONTEXT = () => new DataConstr(0n, []);

/**
 * A payload wide enough that the map-conversion result exceeds the inline
 * constant bound, so the `startBuiltinMapConversion` result rides as a
 * semantic constant. The narrow programs below pin the inline-result form;
 * `tests/cek-executor.test.ts` covers the same widths.
 */
const WIDE_MAP_PAYLOAD_BYTES = 9_000;

const wideMap = () =>
  new DataMap([
    new DataPair(
      new DataI(1n),
      new DataB(Buffer.alloc(WIDE_MAP_PAYLOAD_BYTES, 0x2a)),
    ),
    new DataPair(new DataI(2n), new DataI(3n)),
  ]);

const narrowMap = () =>
  new DataMap([
    new DataPair(new DataI(1n), new DataB(Buffer.alloc(10, 0x2a))),
    new DataPair(new DataI(2n), new DataI(3n)),
  ]);

const mapRoundTrip = (source) =>
  new Lambda(
    new Application(
      Builtin.mapData,
      new Application(Builtin.unMapData, source),
    ),
  );

const g1Compressed = () =>
  UPLCConst.byteString(
    new DataB(
      Buffer.from(
        "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
        "hex",
      ),
    ).bytes,
  );

const g2Compressed = () =>
  UPLCConst.byteString(
    new DataB(
      Buffer.from(
        "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
        "hex",
      ),
    ).bytes,
  );

const millerLoop = () =>
  new Application(
    new Application(
      Builtin.bls12_381_millerLoop,
      new Application(Builtin.bls12_381_G1_uncompress, g1Compressed()),
    ),
    new Application(Builtin.bls12_381_G2_uncompress, g2Compressed()),
  );

const selfApplication = () =>
  new Lambda(new Application(new UPLCVar(0), new UPLCVar(0)));

/**
 * The curated programs. Each is applied to its script context by
 * `buildMidgardCekExecutionGraph`, so every trace opens with the same five
 * application steps before the program's own body runs; the `note` names the
 * witness arms the body is there to reach.
 */
export const CEK_CORE_STEP_PROGRAMS = [
  {
    label: "identity",
    note: "context application, variable lookup, empty-continuation halt",
    term: () => new Lambda(new UPLCVar(0)),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "force_delay",
    note: "computeForce, computeDelay, returnForceDelay, computeConstant",
    term: () => new Lambda(new Force(new Delay(UPLCConst.unit))),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "unbound_variable",
    note: "lookupEmptyEnvironment halting with error_unbound_variable",
    term: () => new Lambda(new UPLCVar(2)),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "explicit_error",
    note: "computeError halting with error_explicit",
    term: () => new Lambda(new ErrorUPLC()),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "nonconstant_halt",
    note: "a lambda value reaching the empty continuation: error_nonconstant_halt",
    term: () => new Lambda(new Lambda(new UPLCVar(0))),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "case_constr_empty",
    note: "computeCase, computeConstrEmpty, returnCaseConstr, selectCaseBranch",
    term: () => new Lambda(new Case(new Constr(0n, []), [UPLCConst.int(7)])),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "case_constr_lambda",
    note: "computeConstrNonempty, returnConstrNext/Done, applyCaseValue, returnApplyValueLambda",
    term: () =>
      new Lambda(
        new Case(new Constr(0n, [UPLCConst.int(42), UPLCConst.int(43)]), [
          new Lambda(new Lambda(new UPLCVar(1))),
        ]),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "case_constr_builtin",
    note: "applyCaseValue feeding a builtin: returnApplyValueBuiltin",
    term: () =>
      new Lambda(
        new Case(new Constr(0n, [UPLCConst.int(1), UPLCConst.int(2)]), [
          Builtin.addInteger,
        ]),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "case_value_invalid",
    note: "applyCaseValue against a constant branch: returnApplyValueInvalid",
    term: () =>
      new Lambda(
        new Case(new Constr(0n, [UPLCConst.int(1)]), [UPLCConst.int(5)]),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "case_branch_missing",
    note: "returnCaseConstr with no branch at the tag: error_case_branch_missing",
    term: () => new Lambda(new Case(new Constr(1n, []), [UPLCConst.int(7)])),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "case_scrutinee_invalid",
    note: "returnCaseInvalid: error_invalid_case_scrutinee",
    term: () => new Lambda(new Case(UPLCConst.int(1), [UPLCConst.int(7)])),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "add_integer",
    note: "computeBuiltin, returnApplyBuiltin, executeBuiltinDirect",
    term: () =>
      new Lambda(
        new Application(
          new Application(Builtin.addInteger, UPLCConst.int(41)),
          UPLCConst.int(1),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "if_then_else",
    note: "returnForceBuiltin on a forced builtin, three saturating applications",
    term: () =>
      new Lambda(
        new Application(
          new Application(
            new Application(Builtin.ifThenElse, UPLCConst.bool(true)),
            UPLCConst.int(1),
          ),
          UPLCConst.int(2),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "builtin_type_failure",
    note: "executeBuiltinTypeFailure without a builtin charge",
    term: () =>
      new Lambda(
        new Application(
          new Application(Builtin.addInteger, new UPLCVar(0)),
          UPLCConst.int(1),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "builtin_failure",
    note: "executeBuiltinFailure on quotientInteger by zero",
    term: () =>
      new Lambda(
        new Application(
          new Application(Builtin.quotientInteger, UPLCConst.int(1)),
          UPLCConst.int(0),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "apply_constant",
    note: "returnApplyInvalid: error_invalid_application",
    term: () => new Lambda(new Application(UPLCConst.int(1), UPLCConst.int(2))),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "force_constant",
    note: "returnForceInvalid: error_invalid_force",
    term: () => new Lambda(new Force(UPLCConst.unit)),
    context: EMPTY_CONTEXT,
    maxSteps: 32,
  },
  {
    label: "head_list_nested",
    note: "executeBuiltinSemantic with the nested-list memory witness",
    term: () =>
      new Lambda(
        new Application(
          Builtin.headList,
          UPLCConst.listOf(constT.listOf(constT.int))([[1n, 2n], [3n]]),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "un_constr_wrong_variant",
    note: "executeBuiltinSemanticFailure on a map where a constr is required",
    term: () =>
      new Lambda(new Application(Builtin.unConstrData, new UPLCVar(0))),
    context: () =>
      new DataMap([
        new DataPair(new DataI(1n), new DataB(Buffer.from([0x71]))),
      ]),
    maxSteps: 64,
  },
  {
    label: "map_round_trip",
    note: "startBuiltinMapConversion, stepBuiltinMapToList, stepBuiltinListToMap, finishBuiltinMapConversion with semantic-constant results",
    term: () => mapRoundTrip(new UPLCVar(0)),
    context: wideMap,
    maxSteps: 64,
  },
  {
    label: "map_round_trip_narrow",
    note: "the same four map-conversion arms with inline-constant results",
    term: () => mapRoundTrip(new UPLCVar(0)),
    context: narrowMap,
    maxSteps: 64,
  },
  {
    label: "map_round_trip_empty",
    note: "startBuiltinMapConversion straight into finishBuiltinMapConversion on the empty map, both directions",
    term: () => mapRoundTrip(new UPLCVar(0)),
    context: () => new DataMap([]),
    maxSteps: 64,
  },
  {
    label: "map_round_trip_constant",
    note: "map conversion from an inline program constant rather than the script context",
    term: () => mapRoundTrip(UPLCConst.data(narrowMap())),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "un_map_wrong_variant_constant",
    note: "executeBuiltinSemanticFailure on an inline list constant where a map is required",
    term: () =>
      new Lambda(
        new Application(
          Builtin.unMapData,
          UPLCConst.data(new DataList([new DataI(1n)])),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
  },
  {
    label: "bls_final_verify",
    note: "executeBuiltinBlsFinal over two Miller loops",
    term: () =>
      new Lambda(
        new Application(
          new Application(Builtin.bls12_381_finalVerify, millerLoop()),
          millerLoop(),
        ),
      ),
    context: EMPTY_CONTEXT,
    maxSteps: 256,
  },
  {
    label: "omega_budget_exceeded",
    note: "self-application stopped at the first transition over the budget",
    term: () =>
      new Lambda(new Application(selfApplication(), selfApplication())),
    context: EMPTY_CONTEXT,
    maxSteps: 64,
    executionBudget: { cpu: 100_000n, memory: 1_000n },
  },
];

/**
 * The state each program's initial state is expected to carry before its
 * first step, pinned so the fixture's `initialState` is not a tautology.
 */
export const INITIAL_MODE = "compute";

/**
 * The per-program negative twists. Each one takes a genuine step from the
 * program's trace and perturbs exactly one field of its post-state, so the
 * pinned CBOR is well-formed `CoreStepEvidenceV1` that both verifiers must
 * still refuse. `select` names which step of the trace is twisted.
 */
export const CEK_CORE_STEP_TWISTS = [
  {
    label: "post_cpu_minus_one",
    select: (steps) => steps.length - 1,
    apply: (step) => ({
      ...step,
      post: { ...step.post, cpu: step.post.cpu - 1n },
    }),
  },
  {
    label: "post_mode_flipped",
    select: () => 0,
    apply: (step) => ({
      ...step,
      post: {
        ...step.post,
        mode: step.post.mode === "compute" ? "return" : "compute",
      },
    }),
  },
];

/** The `MidgardCekCoreStepWitness` arms, in ABI constructor order. */
export const CEK_CORE_STEP_WITNESS_KINDS = [
  "computeVariable",
  "computeConstant",
  "computeLambda",
  "computeDelay",
  "computeApplication",
  "computeForce",
  "computeError",
  "computeBuiltin",
  "computeConstrEmpty",
  "computeConstrNonempty",
  "computeCase",
  "lookupEnvironment",
  "lookupEmptyEnvironment",
  "returnEmptyContinuation",
  "returnApplyArgument",
  "returnApplyLambda",
  "returnApplyBuiltin",
  "returnApplyInvalid",
  "returnApplyValueLambda",
  "returnApplyValueBuiltin",
  "returnApplyValueInvalid",
  "returnForceDelay",
  "returnForceBuiltin",
  "returnForceInvalid",
  "returnConstrNext",
  "returnConstrDone",
  "returnCaseConstr",
  "returnCaseInvalid",
  "selectCaseBranch",
  "applyCaseValue",
  "executeBuiltinDirect",
  "executeBuiltinSemantic",
  "startBuiltinMapConversion",
  "stepBuiltinListToMap",
  "stepBuiltinMapToList",
  "finishBuiltinMapConversion",
  "executeBuiltinSemanticFailure",
  "executeBuiltinBlsFinal",
  "executeBuiltinFailure",
  "executeBuiltinTypeFailure",
  "computeContextConstant",
];

/**
 * The arms the curated set deliberately leaves unreached, with the reason.
 * Pinned so a program later added for one of them has to remove it here, and
 * so a coverage regression (a program that stops emitting an arm) fails
 * rather than silently shrinking the set.
 */
export const CEK_CORE_STEP_WITNESS_KINDS_UNCOVERED = [];
