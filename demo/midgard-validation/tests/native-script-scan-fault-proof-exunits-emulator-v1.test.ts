import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { inspect } from "node:util";

import {
  buildMidgardLedgerOutputProofTraceV1,
  hashMidgardValidationMachineStateV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MidgardLedgerOutputProofStagesV1,
  type MidgardLedgerOutputProofTraceV1,
  MidgardNativeScriptStructureStagesV1,
} from "@al-ft/midgard-core";
import {
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  type MidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  buildValidationTraceDisputeFaultProofContracts,
  parseFaultProofBlueprint,
  PreparedValidationResolutionDatumV1,
  requireInputIndex,
  requireUniqueOutputIndex,
  validationMachineStateDataFromCore,
  type ValidationTraceDisputeFaultProofContracts,
  WinningValidationResolutionDatumV1,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { validationOneStepEvidenceHashV1 } from "../../midgard-fault-proofs/src/validation-dispute/submit.js";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgumentV1,
  type DeterministicValidationMachineTrace,
} from "../src/index.js";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "../src/ledger-output-descriptor.js";
import {
  fundingLovelaceForOutputsV1,
  makeMinAdaFundedExactSizeOutputItemV1,
  makeNativeTx,
  outRefFromByte,
  outRefFromTxId,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

/**
 * #633 baseline benchmark: ExUnits and transaction size of the LIVE fault-proof
 * transaction that advances the tag-0 native-script structural scan.
 *
 * The scan (`native_script_scan_v1`) is reachable on-chain through the staged
 * ledger-output proof (`ledger_output_proof_v1.step_v1`), which the validation
 * machine drives during input resolution: every resolved input's output CBOR is
 * proven canonical step by step, and a tag-0 reference script enters the
 * NativeScript stage where each step consumes ONE scan token (or pops one
 * frame). On L1 each such step is a one-step semantic fault proof: a
 * computation-thread UTxO at `resolve_inputs_membership_step_semantic_v1` spent
 * with `Continue(VerifyMembershipStep{...})` into a winning-resolution output at
 * the award validator.
 *
 * This suite builds that exact transaction in the lucid-evolution emulator
 * against the real compiled blueprint, for deep and wide canonical payloads at
 * curve node counts, and reports per-step ExUnits (mem/cpu), fee, and complete
 * signed size — the measured baseline the #633 direction-(d) staged-proof
 * design (and any scan optimization) is judged against. No on-chain code is
 * touched: this is measurement only.
 */

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
const blueprintJson = JSON.parse(readFileSync(blueprintPath, "utf8"));

const HUB_ORACLE_POLICY_ID = "11".repeat(28);
const FRAUD_PROOF_CATALOGUE_POLICY_ID = "22".repeat(28);
const THREAD_ASSET_NAME = "aa".repeat(32);

const SIGNING_KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));
const SIGNER_HASH = Buffer.from(
  SIGNING_KEY.to_public().hash().to_raw_bytes(),
).toString("hex");

/** The §3.3 per-transaction execution basis the #633 measurements are read against. */
const BASIS_MEMORY_UNITS = 13_200_000n;
const BASIS_CPU_UNITS = 8_000_000_000n;
const MAX_L1_PROOF_TX_BYTES =
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;

/**
 * The whole-output canonical CBOR cap (`ledger_output_v1.
 * max_output_canonical_cbor_bytes`). Sourced from the consensus limits rather
 * than restated; the max-fit curve point is derived against it.
 */
const MAX_OUTPUT_CBOR_BYTES = 16_384;

/** Flat semantic-resolver index of `resolve_inputs_membership_step_semantic_v1`. */
const RESOLVE_INPUTS_MEMBERSHIP_STEP_RESOLVER = 29;
/** Auxiliary constructor `LedgerOutputProofStepWitness` (index 32, one field). */
const LEDGER_OUTPUT_PROOF_STEP_AUX_SHAPE = [32, 1] as const;

const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  eventKeyCbor: Buffer.from("d8799f4100ff", "hex"),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

/** `[1, [x]]` nesting terminated by `[4, 0]`: the deep worst-case shape. */
const deepNativeScript = (nodes: number): MidgardNativeScript => {
  let script: MidgardNativeScript = { type: "after", slot: 0n };
  for (let index = 1; index < nodes; index += 1) {
    script = { type: "all", scripts: [script] };
  }
  return script;
};

/** `[1, [leaf × (n-1)]]`: the wide worst-case shape. */
const wideNativeScript = (nodes: number): MidgardNativeScript => ({
  type: "all",
  scripts: Array.from({ length: nodes - 1 }, () => ({
    type: "after" as const,
    slot: 0n,
  })),
});

type PayloadShape = "deep" | "wide";

const nativeScriptForShape = (
  shape: PayloadShape,
  nodes: number,
): MidgardNativeScript =>
  shape === "deep" ? deepNativeScript(nodes) : wideNativeScript(nodes);

/**
 * The resolved-input output whose reference script carries the payload:
 * key-hash address, coin-only value funding the produced output exactly, tag-0
 * native reference script.
 */
const makeReferenceScriptOutput = (
  lovelace: bigint,
  script: MidgardNativeScript,
): Buffer =>
  encodeMidgardTxOutput({
    address: TEST_ADDRESS_BYTES,
    value: { lovelace, assets: new Map() },
    script_ref: {
      language: "NativeCardano",
      scriptBytes: encodeMidgardNativeScript(script),
      nativeScript: script,
    },
  });

/**
 * Largest node count whose spent-output CBOR stays within the whole-output
 * cap, for the given shape and funding lovelace. Solved by direct measurement
 * (per-node growth is exactly 3 bytes on both shapes, but headers shift).
 */
const maxFitNodes = (shape: PayloadShape, lovelace: bigint): number => {
  const sizeAt = (nodes: number): number =>
    makeReferenceScriptOutput(lovelace, nativeScriptForShape(shape, nodes))
      .length;
  let low = 1;
  let high = 6_000;
  while (sizeAt(high) <= MAX_OUTPUT_CBOR_BYTES) {
    high *= 2;
  }
  while (low < high) {
    const middle = Math.ceil((low + high) / 2);
    if (sizeAt(middle) <= MAX_OUTPUT_CBOR_BYTES) {
      low = middle;
    } else {
      high = middle - 1;
    }
  }
  return low;
};

type BenchmarkCase = {
  readonly trace: DeterministicValidationMachineTrace;
  readonly proofTrace: MidgardLedgerOutputProofTraceV1;
  /** trace state index of the k-th ledger-output-proof step witness. */
  readonly stepStateIndices: readonly number[];
  readonly spentOutputBytes: number;
  readonly payloadBytes: number;
};

const buildBenchmarkCase = async (
  shape: PayloadShape,
  nodes: number,
): Promise<BenchmarkCase> => {
  const producedItem = makeMinAdaFundedExactSizeOutputItemV1(160);
  const funding = fundingLovelaceForOutputsV1([producedItem]);
  const script = nativeScriptForShape(shape, nodes);
  const payloadBytes = encodeMidgardNativeScript(script).length;
  const spentOutput = makeReferenceScriptOutput(funding, script);
  expect(spentOutput.length).toBeLessThanOrEqual(MAX_OUTPUT_CBOR_BYTES);

  const spent = outRefFromByte(0x11);
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs: [producedItem],
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    buildValidationMachineLedgerInsertOpV1({
      key: outRefFromTxId(transaction.txId, 0n),
      outputCbor: producedItem,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });

  console.log(`[bench] building ${shape} ${nodes.toString()}-node trace…`);
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      ...traceContext,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );

  const material = buildCanonicalMidgardLedgerEntryOutputMaterialV1({
    outRef: spent,
    outputCbor: spentOutput,
  });
  const proofTrace = buildMidgardLedgerOutputProofTraceV1({
    outputIndex: material.descriptor.outputIndex,
    outputCbor: spentOutput,
  });

  const stepStateIndices: number[] = [];
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    if (
      witness.phase === "resolveInputs" &&
      witness.auxiliary?.kind === "ledgerOutputProofStep"
    ) {
      stepStateIndices.push(index);
    }
  }
  if (stepStateIndices.length !== proofTrace.steps.length) {
    throw new Error(
      `trace carries ${stepStateIndices.length.toString()} ledger-output-proof step witnesses but the direct proof trace has ${proofTrace.steps.length.toString()}`,
    );
  }
  return {
    trace,
    proofTrace,
    stepStateIndices,
    spentOutputBytes: spentOutput.length,
    payloadBytes,
  };
};

type StepDescriptor = {
  readonly label: string;
  /** index into proofTrace.steps / stepStateIndices. */
  readonly stepIndex: number;
  readonly stage: number;
  readonly nativeStage: number | null;
  readonly cursor: number | null;
  readonly stackDepth: number | null;
  readonly nodeCount: number | null;
};

const describeStep = (
  benchmarkCase: BenchmarkCase,
  stepIndex: number,
  label: string,
): StepDescriptor => {
  const control = benchmarkCase.proofTrace.steps[stepIndex]!.control;
  return {
    label,
    stepIndex,
    stage: control.stage,
    nativeStage: control.nativeScript?.stage ?? null,
    cursor: control.nativeScript?.cursor ?? null,
    stackDepth: control.nativeScript?.stackDepth ?? null,
    nodeCount: control.nativeScript?.nodeCount ?? null,
  };
};

/**
 * The step sample: every stage crossed once for context, and the NativeScript
 * stage probed at its interesting extremes (first/middle/last token, the
 * deepest-stack token, one frame pop, the finalize step).
 */
const selectSteps = (benchmarkCase: BenchmarkCase): StepDescriptor[] => {
  const { proofTrace } = benchmarkCase;
  const stages = MidgardLedgerOutputProofStagesV1;
  const nativeStages = MidgardNativeScriptStructureStagesV1;
  const selected: StepDescriptor[] = [];
  const firstOfStage = new Map<number, number>();
  proofTrace.steps.forEach((step, index) => {
    if (!firstOfStage.has(step.control.stage)) {
      firstOfStage.set(step.control.stage, index);
    }
  });
  for (const [stageName, stage] of Object.entries(stages)) {
    if (stage === stages.NativeScript) continue;
    const index = firstOfStage.get(stage);
    if (index !== undefined) {
      selected.push(describeStep(benchmarkCase, index, `stage:${stageName}`));
    }
  }
  const nativeSteps = proofTrace.steps
    .map((step, index) => ({ step, index }))
    .filter(({ step }) => step.control.stage === stages.NativeScript);
  const tokens = nativeSteps.filter(
    ({ step }) => step.control.nativeScript?.stage === nativeStages.Token,
  );
  const frames = nativeSteps.filter(
    ({ step }) => step.control.nativeScript?.stage === nativeStages.Frame,
  );
  const finalize = nativeSteps.filter(
    ({ step }) => step.control.nativeScript?.stage === nativeStages.Finalize,
  );
  if (tokens.length > 0) {
    selected.push(
      describeStep(benchmarkCase, tokens[0]!.index, "native:firstToken"),
    );
    const middle = tokens[Math.floor(tokens.length / 2)]!;
    selected.push(describeStep(benchmarkCase, middle.index, "native:midToken"));
    const deepest = tokens.reduce((left, right) =>
      (left.step.control.nativeScript?.stackDepth ?? 0) >=
      (right.step.control.nativeScript?.stackDepth ?? 0)
        ? left
        : right,
    );
    selected.push(
      describeStep(benchmarkCase, deepest.index, "native:maxStackToken"),
    );
    selected.push(
      describeStep(
        benchmarkCase,
        tokens[tokens.length - 1]!.index,
        "native:lastToken",
      ),
    );
  }
  if (frames.length > 0) {
    selected.push(
      describeStep(benchmarkCase, frames[0]!.index, "native:firstFrame"),
    );
    selected.push(
      describeStep(
        benchmarkCase,
        frames[frames.length - 1]!.index,
        "native:lastFrame",
      ),
    );
  }
  if (finalize.length > 0) {
    selected.push(
      describeStep(benchmarkCase, finalize[0]!.index, "native:finalize"),
    );
  }
  const seen = new Set<number>();
  return selected.filter((descriptor) => {
    if (seen.has(descriptor.stepIndex)) return false;
    seen.add(descriptor.stepIndex);
    return true;
  });
};

type StepMeasurement = {
  readonly label: string;
  readonly stepIndex: number;
  readonly stage: number;
  readonly nativeStage: number | null;
  readonly stackDepth: number | null;
  readonly mem: bigint;
  readonly cpu: bigint;
  readonly fee: bigint;
  readonly completeSignedBytes: number;
  readonly l1ByteMargin: number;
};

let cachedContracts: ValidationTraceDisputeFaultProofContracts | undefined;
const loadContracts =
  async (): Promise<ValidationTraceDisputeFaultProofContracts> => {
    cachedContracts ??= await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parseFaultProofBlueprint(blueprintJson),
        network: "Custom",
        hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
        fraudProofCataloguePolicyId: FRAUD_PROOF_CATALOGUE_POLICY_ID,
      }),
    ).catch((error: unknown) => {
      console.log(
        "[bench] contracts build failed:",
        inspect(error, { depth: 12 }),
      );
      throw error;
    });
    return cachedContracts;
  };

const preparedThreadDatumCbor = (
  benchmarkCase: BenchmarkCase,
  stateIndex: number,
  evidenceHash: string,
): string => {
  const successorHash = hashMidgardValidationMachineStateV1(
    benchmarkCase.trace.states[stateIndex + 1]!,
  ).toString("hex");
  // A well-formed prepared resolution requires the two successor hashes to
  // differ (a dispute); the membership-step resolver only reads pre_state, so
  // any distinct 32-byte operator hash works.
  const operatorSuccessorHash =
    (successorHash.startsWith("00") ? "ff" : "00") + successorHash.slice(2);
  return Data.to(
    {
      fraud_prover: SIGNER_HASH,
      data: {
        version: 1n,
        resolution: {
          version: 1n,
          pre_state: validationMachineStateDataFromCore(
            benchmarkCase.trace.states[stateIndex]!,
          ),
          operator_successor_hash: operatorSuccessorHash,
          challenger_successor_hash: successorHash,
        },
        evidence_hash: evidenceHash,
      },
    },
    PreparedValidationResolutionDatumV1,
  );
};

const winningDatumCbor = (): string =>
  Data.to(
    { fraud_prover: SIGNER_HASH, data: { version: 1n } },
    WinningValidationResolutionDatumV1,
  );

const measureStep = async (
  benchmarkCase: BenchmarkCase,
  descriptor: StepDescriptor,
): Promise<StepMeasurement> => {
  const contracts = await loadContracts();
  const resolver =
    contracts.validationTraceDispute.semanticResolvers[
      RESOLVE_INPUTS_MEMBERSHIP_STEP_RESOLVER
    ];
  if (resolver === undefined) {
    throw new Error("resolve-inputs membership-step resolver is missing");
  }
  const award = contracts.validationTraceDispute.award;
  const stateIndex = benchmarkCase.stepStateIndices[descriptor.stepIndex]!;
  const argument = buildValidationOneStepArgumentV1({
    trace: benchmarkCase.trace,
    stateIndex,
  });
  if (argument.resolverIndex !== 7 || argument.semanticResolverIndex !== 3) {
    throw new Error(
      `step ${descriptor.label} selected resolver ${argument.resolverIndex.toString()}/${argument.semanticResolverIndex.toString()}, expected ResolveInputs membership step (7/3)`,
    );
  }
  const transitionData = Data.from(argument.transitionCbor.toString("hex"));
  const auxiliary = Data.from(argument.auxiliaryCbor.toString("hex"));
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== LEDGER_OUTPUT_PROOF_STEP_AUX_SHAPE[0] ||
    auxiliary.fields.length !== LEDGER_OUTPUT_PROOF_STEP_AUX_SHAPE[1]
  ) {
    throw new Error("ledger-output-proof step auxiliary has unexpected shape");
  }
  const proofWitnessData = auxiliary.fields[0]!;
  const evidenceHash = validationOneStepEvidenceHashV1({
    transitionCbor: argument.transitionCbor,
    auxiliaryCbor: argument.auxiliaryCbor,
  });

  const walletAddress = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(SIGNING_KEY.to_public().hash()),
  )
    .to_address()
    .to_bech32();
  const threadUnit = toUnit(
    contracts.computationThread.policyId,
    THREAD_ASSET_NAME,
  );
  const threadDatum = preparedThreadDatumCbor(
    benchmarkCase,
    stateIndex,
    evidenceHash,
  );
  const emulator = new Emulator(
    [
      {
        seedPhrase: "",
        privateKey: SIGNING_KEY.to_bech32(),
        address: walletAddress,
        assets: { lovelace: 100_000_000_000n },
      },
      {
        seedPhrase: "",
        privateKey: "",
        address: resolver.spendingScriptAddress,
        assets: { lovelace: 60_000_000n, [threadUnit]: 1n },
        outputData: { inline: threadDatum },
      },
    ],
    // The parking transaction below carries the resolver script itself and
    // rides a raised ceiling; the measured transaction is compared against the
    // real 16,384-byte L1 cap explicitly in the report. The ExUnits ceilings
    // are raised so the harness records actual consumption instead of
    // aborting; every step is compared against the 13.2M/8B basis in the
    // report, not against these emulator caps.
    // The mem ceiling must stay small enough that a divergent or runaway
    // evaluation exhausts the BUDGET (a graceful over-budget error) before it
    // exhausts wasm32 linear memory (an opaque `unreachable` trap).
    {
      ...PROTOCOL_PARAMETERS_DEFAULT,
      maxTxSize: 262_144,
      maxTxExMem: 100_000_000n,
      maxTxExSteps: 200_000_000_000n,
    },
  );
  const lucid: LucidEvolution = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromPrivateKey(SIGNING_KEY.to_bech32());

  // Park the applied resolver as a reference script, the way the deployed
  // route publishes it once and reads it thereafter. Not part of the
  // measurement.
  const parkAddress = credentialToAddress(
    "Custom",
    scriptHashToCredential("2f".repeat(28)),
  );
  const parkUnsigned = await lucid
    .newTx()
    .pay.ToAddressWithData(
      parkAddress,
      undefined,
      { lovelace: 60_000_000n },
      resolver.spendingScript,
    )
    .complete();
  const parkSigned = await parkUnsigned.sign.withWallet().complete();
  await lucid.awaitTx(await parkSigned.submit());
  const validatorReferenceUtxo = (await lucid.utxosAt(parkAddress)).find(
    (utxo) => utxo.scriptRef != null,
  );
  if (validatorReferenceUtxo === undefined) {
    throw new Error("semantic resolver reference script failed to park");
  }

  const threadUtxos: UTxO[] = await lucid.utxosAt(
    resolver.spendingScriptAddress,
  );
  if (threadUtxos.length !== 1) {
    throw new Error("emulator thread seeding mismatch");
  }
  const threadUtxo = threadUtxos[0]!;
  const outputDatum = winningDatumCbor();

  const makeRedeemer: BuildTxWithRedeemer = (ctx) => {
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "scan-step semantic proof",
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === award.spendingScriptAddress &&
        output.datum != null &&
        output.assets[threadUnit] === 1n,
      "scan-step semantic proof",
    );
    return Data.to(
      new Constr(1, [
        new Constr(0, [
          inputIndex,
          outputIndex,
          transitionData,
          proofWitnessData,
        ]),
      ]),
    );
  };

  const walletUtxos = (await lucid.wallet().getUtxos()).filter(
    (utxo) => utxo.assets[threadUnit] === undefined,
  );
  const feeInput = walletUtxos.reduce((left, right) =>
    (left.assets.lovelace ?? 0n) >= (right.assets.lovelace ?? 0n)
      ? left
      : right,
  );
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], makeRedeemer)
    .readFrom([validatorReferenceUtxo])
    .pay.ToContract(
      award.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadUnit]: 1n,
      },
    )
    .addSignerKey(SIGNER_HASH)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);

  const transaction = CML.Transaction.from_cbor_hex(signedCbor);
  const redeemers = transaction
    .witness_set()
    .redeemers()
    ?.as_arr_legacy_redeemer();
  let mem = 0n;
  let cpu = 0n;
  if (redeemers !== undefined) {
    for (let index = 0; index < redeemers.len(); index += 1) {
      const units = redeemers.get(index).ex_units();
      mem += units.mem();
      cpu += units.steps();
    }
  }
  const completeSignedBytes = signedCbor.length / 2;
  const measurement: StepMeasurement = {
    label: descriptor.label,
    stepIndex: descriptor.stepIndex,
    stage: descriptor.stage,
    nativeStage: descriptor.nativeStage,
    stackDepth: descriptor.stackDepth,
    mem,
    cpu,
    fee: transaction.body().fee(),
    completeSignedBytes,
    l1ByteMargin: MAX_L1_PROOF_TX_BYTES - completeSignedBytes,
  };
  // Logged immediately so a later step's failure cannot lose measured rows.

  console.log(
    `[bench] measured ${descriptor.label}: mem=${mem.toString()} cpu=${cpu.toString()} txBytes=${completeSignedBytes.toString()} fee=${measurement.fee.toString()}`,
  );
  return measurement;
};

type CurvePointReport = {
  readonly shape: PayloadShape;
  readonly nodes: number;
  readonly payloadBytes: number;
  readonly spentOutputBytes: number;
  readonly proofStepCounts: {
    readonly total: number;
    readonly nativeToken: number;
    readonly nativeFrame: number;
    readonly nativeFinalize: number;
  };
  readonly measurements: readonly StepMeasurement[];
};

const runCurvePoint = async (
  shape: PayloadShape,
  nodes: number,
): Promise<CurvePointReport> => {
  const benchmarkCase = await buildBenchmarkCase(shape, nodes);
  const stages = MidgardLedgerOutputProofStagesV1;
  const nativeStages = MidgardNativeScriptStructureStagesV1;
  const nativeSteps = benchmarkCase.proofTrace.steps.filter(
    (step) => step.control.stage === stages.NativeScript,
  );
  const countNativeStage = (stage: number): number =>
    nativeSteps.filter((step) => step.control.nativeScript?.stage === stage)
      .length;
  const stepFilter = (process.env.MIDGARD_SCAN_BENCH_STEPS ?? "")
    .split(",")
    .filter((part) => part !== "");
  const descriptors = selectSteps(benchmarkCase).filter(
    (descriptor) =>
      stepFilter.length === 0 || stepFilter.includes(descriptor.label),
  );
  const measurements: StepMeasurement[] = [];
  for (const descriptor of descriptors) {
    console.log(
      `[bench] measuring ${shape} ${nodes.toString()} ${descriptor.label}`,
    );
    try {
      measurements.push(await measureStep(benchmarkCase, descriptor));
    } catch (error) {
      console.log(
        `[bench] step ${descriptor.label} failed:`,
        inspect(error, { depth: 14 }),
      );
      throw error;
    }
  }
  return {
    shape,
    nodes,
    payloadBytes: benchmarkCase.payloadBytes,
    spentOutputBytes: benchmarkCase.spentOutputBytes,
    proofStepCounts: {
      total: benchmarkCase.proofTrace.steps.length,
      nativeToken: countNativeStage(nativeStages.Token),
      nativeFrame: countNativeStage(nativeStages.Frame),
      nativeFinalize: countNativeStage(nativeStages.Finalize),
    },
    measurements,
  };
};

const formatReport = (reports: readonly CurvePointReport[]): string => {
  const lines: string[] = [];
  for (const report of reports) {
    lines.push(
      `\n== ${report.shape} ${report.nodes.toString()} nodes | payload ${report.payloadBytes.toString()} B | spent output ${report.spentOutputBytes.toString()} B | proof steps total ${report.proofStepCounts.total.toString()} (native token ${report.proofStepCounts.nativeToken.toString()}, frame ${report.proofStepCounts.nativeFrame.toString()}, finalize ${report.proofStepCounts.nativeFinalize.toString()}) ==`,
    );
    lines.push(
      "label                      | mem       | cpu           | %mem-basis | tx bytes | L1 margin | fee",
    );
    for (const row of report.measurements) {
      const basisShare = Number((row.mem * 10_000n) / BASIS_MEMORY_UNITS) / 100;
      lines.push(
        `${row.label.padEnd(26)} | ${row.mem.toString().padStart(9)} | ${row.cpu
          .toString()
          .padStart(
            13,
          )} | ${basisShare.toFixed(2).padStart(10)} | ${row.completeSignedBytes
          .toString()
          .padStart(
            8,
          )} | ${row.l1ByteMargin.toString().padStart(9)} | ${row.fee.toString()}`,
      );
    }
  }
  return lines.join("\n");
};

/**
 * Node-count tokens: integers, plus the literal "maxfit" for the largest
 * payload the whole-output cap admits (resolved per shape at run time).
 */
const CURVE_NODE_TOKENS = (
  process.env.MIDGARD_SCAN_BENCH_NODES ?? "65,257,1025"
)
  .split(",")
  .filter((part) => part !== "");
const INCLUDE_MAX_FIT = process.env.MIDGARD_SCAN_BENCH_MAXFIT !== "0";
const SHAPES: readonly PayloadShape[] = (
  process.env.MIDGARD_SCAN_BENCH_SHAPES ?? "deep,wide"
)
  .split(",")
  .filter((part): part is PayloadShape => part === "deep" || part === "wide");

describe("native-script scan fault-proof step ExUnits baseline (#633)", () => {
  it(
    "measures the live one-token-per-transaction staged scan",
    { timeout: 14_400_000 },
    async () => {
      const reports: CurvePointReport[] = [];
      for (const shape of SHAPES) {
        const producedItem = makeMinAdaFundedExactSizeOutputItemV1(160);
        const funding = fundingLovelaceForOutputsV1([producedItem]);
        const tokens =
          INCLUDE_MAX_FIT && !CURVE_NODE_TOKENS.includes("maxfit")
            ? [...CURVE_NODE_TOKENS, "maxfit"]
            : [...CURVE_NODE_TOKENS];
        const points = tokens.map((token) =>
          token === "maxfit"
            ? maxFitNodes(shape, funding)
            : Number.parseInt(token, 10),
        );
        for (const nodes of points) {
          const report = await runCurvePoint(shape, nodes);
          reports.push(report);

          console.log(formatReport([report]));
        }
      }

      console.log(formatReport(reports));
      const outPath = process.env.MIDGARD_SCAN_BENCH_OUT;
      if (outPath !== undefined && outPath !== "") {
        mkdirSync(dirname(outPath), { recursive: true });
        writeFileSync(
          outPath,
          `${JSON.stringify(
            reports,
            (_key, value: unknown) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          )}\n`,
        );
      }
      // This is a baseline measurement harness: within-basis / L1-margin
      // comparisons are REPORTED per row above, never gated, because the
      // point of the baseline is to record where the live step stands
      // relative to the basis — including any step that exceeds it. Only
      // sanity-check that every measured step actually evaluated.
      for (const report of reports) {
        for (const row of report.measurements) {
          expect(row.mem).toBeGreaterThan(0n);
          expect(row.cpu).toBeGreaterThan(0n);
        }
      }
    },
  );
});
