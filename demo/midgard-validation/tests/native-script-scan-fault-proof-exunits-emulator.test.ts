import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { inspect } from "node:util";

import {
  buildMidgardLedgerOutputProofTrace,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
  MidgardLedgerOutputProofStages,
  type MidgardLedgerOutputProofTrace,
  MidgardNativeScriptStructureStages,
} from "@al-ft/midgard-core";
import {
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  type MidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { validationOneStepEvidenceHash } from "@al-ft/midgard-fault-proofs";
import {
  buildValidationTraceDisputeFaultProofContracts,
  parseFaultProofBlueprint,
  PreparedValidationResolutionDatum,
  requireInputIndex,
  requireUniqueOutputIndex,
  validationMachineStateDataFromCore,
  type ValidationTraceDisputeFaultProofContracts,
  WinningValidationResolutionDatum,
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

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgument,
  type DeterministicValidationMachineTrace,
} from "../src/index.js";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "../src/ledger-output-descriptor.js";
import {
  checkScanBenchLedger,
  readScanBenchLedger,
  SCAN_BENCH_EXECUTION_BASIS,
  SCAN_BENCH_LEDGER_PATH,
  scanBenchFiltersInEffect,
  writeScanBenchLedger,
} from "./helpers/native-script-scan-exunits-ledger.js";
import {
  fundingLovelaceForOutputs,
  makeMinAdaFundedExactSizeOutputItem,
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
 * curve node counts, and measures per-step ExUnits (mem/cpu), fee, and complete
 * signed size — the baseline the #633 direction-(d) staged-proof design (and
 * any scan optimization) is judged against. No on-chain code is touched.
 *
 * **This is no longer measurement only, and it no longer runs by default.**
 *
 * The suite costs about 858 seconds. It used to run inside the default
 * `pnpm test` and assert only that each reading was positive, writing an
 * artifact solely when `MIDGARD_SCAN_BENCH_OUT` was set — 14 minutes of
 * measurement discarded on every run, behind a gate that no cost at any
 * multiple of the §3.3 basis could fail. Two changes, together:
 *
 *   1. The whole suite is gated behind `MIDGARD_VALIDATION_EVIDENCE`, following
 *      `demo/midgard-fault-proofs/tests/resolver-proof-fit-sweep-generate.test.ts`'s
 *      `describe.skipIf(!REGENERATE)`. The default `pnpm test` shows it SKIPPED —
 *      present and named, never silently absent — and
 *      `pnpm run test:evidence` runs it.
 *   2. Every measured row is now pinned against the COMMITTED artifact
 *      `evidence/native-script-scan-fault-proof-exunits-v1.json`, exactly, and
 *      each row's position relative to the §3.3 basis and the L1 transaction cap
 *      is a recorded judgement that movement in EITHER direction fails. So the
 *      cheaper lane gates strictly MORE than the old default run did, not less.
 *      See `tests/helpers/native-script-scan-exunits-ledger.ts` for why exact
 *      equality is the right comparison (emulator ExUnits are deterministic),
 *      what counts as re-takeable drift, and how
 *      `MIDGARD_SCAN_BENCH_UPDATE=1` re-records readings without laundering a
 *      judgement.
 *
 * `MIDGARD_SCAN_BENCH_OUT` is unchanged and independent: it still dumps the raw
 * per-curve reports to an arbitrary path, which is a scratch dump for reading,
 * not the pinned ledger. Nothing about the measured rows, shapes, node counts or
 * step selection changed — the same curve points and the same step sample are
 * measured as before.
 */

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
/**
 * Read lazily. `onchain/aiken/plutus.json` is gitignored and generated, so
 * reading it at module scope would throw during COLLECTION — turning the
 * `skipIf` above into a hard error on any tree that has not built the
 * blueprint, which is precisely the "skipped, not absent" property the lane
 * split depends on.
 */
let cachedBlueprintJson: unknown;
const loadBlueprintJson = (): unknown =>
  (cachedBlueprintJson ??= JSON.parse(readFileSync(blueprintPath, "utf8")));

const HUB_ORACLE_POLICY_ID = "11".repeat(28);
const FRAUD_PROOF_CATALOGUE_POLICY_ID = "22".repeat(28);
const THREAD_ASSET_NAME = "aa".repeat(32);

const SIGNING_KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));
const SIGNER_HASH = Buffer.from(
  SIGNING_KEY.to_public().hash().to_raw_bytes(),
).toString("hex");

/**
 * The §3.3 per-transaction execution basis the #633 measurements are read
 * against. Both halves participate: `memoryUnits` in the per-row `%mem-basis`
 * column of the report AND in the ledger's recomputed
 * `memBasisShareBasisPoints`, `cpuUnits` in the ledger's `basisFit` judgement,
 * which a row may not cross in either direction without going red.
 */
const BASIS_MEMORY_UNITS = SCAN_BENCH_EXECUTION_BASIS.memoryUnits;
const MAX_L1_PROOF_TX_BYTES = MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

/**
 * The whole-output canonical CBOR cap (`ledger_output_v1.
 * max_output_canonical_cbor_bytes`). Sourced from the consensus limits rather
 * than restated; the max-fit curve point is derived against it.
 */
const MAX_OUTPUT_CBOR_BYTES = 16_384;

/**
 * The basis the committed ledger must declare, assembled here from the
 * constants above and handed to the verifier — never read back out of the
 * ledger whose verdicts it decides.
 */
const SCAN_BENCH_LEDGER_BASIS = {
  memoryUnits: BASIS_MEMORY_UNITS,
  cpuUnits: SCAN_BENCH_EXECUTION_BASIS.cpuUnits,
  maxL1ProofTxBytes: MAX_L1_PROOF_TX_BYTES,
  maxOutputCanonicalCborBytes: MAX_OUTPUT_CBOR_BYTES,
  source: SCAN_BENCH_EXECUTION_BASIS.source,
} as const;

/** Flat semantic-resolver index of `resolve_inputs_membership_step_semantic_v1`. */
const RESOLVE_INPUTS_MEMBERSHIP_STEP_RESOLVER = 29;
/** Auxiliary constructor `LedgerOutputProofStepWitness` (index 32, one field). */
const LEDGER_OUTPUT_PROOF_STEP_AUX_SHAPE = [32, 1] as const;

const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
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
  readonly proofTrace: MidgardLedgerOutputProofTrace;
  /** trace state index of the k-th ledger-output-proof step witness. */
  readonly stepStateIndices: readonly number[];
  readonly spentOutputBytes: number;
  readonly payloadBytes: number;
};

const buildBenchmarkCase = async (
  shape: PayloadShape,
  nodes: number,
): Promise<BenchmarkCase> => {
  const producedItem = makeMinAdaFundedExactSizeOutputItem(160);
  const funding = fundingLovelaceForOutputs([producedItem]);
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
    buildValidationMachineLedgerInsertOp({
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

  const material = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: spent,
    outputCbor: spentOutput,
  });
  const proofTrace = buildMidgardLedgerOutputProofTrace({
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
  const stages = MidgardLedgerOutputProofStages;
  const nativeStages = MidgardNativeScriptStructureStages;
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
        blueprint: parseFaultProofBlueprint(loadBlueprintJson()),
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
  const successorHash = hashMidgardValidationMachineState(
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
    PreparedValidationResolutionDatum,
  );
};

const winningDatumCbor = (): string =>
  Data.to(
    { fraud_prover: SIGNER_HASH, data: { version: 1n } },
    WinningValidationResolutionDatum,
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
  const argument = buildValidationOneStepArgument({
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
  const evidenceHash = validationOneStepEvidenceHash({
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
  const stages = MidgardLedgerOutputProofStages;
  const nativeStages = MidgardNativeScriptStructureStages;
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

/**
 * The evidence lane. Same idiom as
 * `demo/midgard-fault-proofs/tests/resolver-proof-fit-sweep-generate.test.ts:746`:
 * `describe.skipIf` keeps the suite COLLECTED and reported as skipped under the
 * default `pnpm test`, so nobody has to remember it exists.
 */
const EVIDENCE = process.env.MIDGARD_VALIDATION_EVIDENCE === "1";
/** Re-record the raw readings into the committed artifact. Never a bypass. */
const UPDATE_LEDGER = process.env.MIDGARD_SCAN_BENCH_UPDATE === "1";

/**
 * Orchestration modes (#642 item 5). The `@lucid-evolution/uplc` wasm module
 * leaks linear memory across evaluations and wasm32 cannot shrink, so a
 * single process measuring every step dies with an opaque `unreachable` trap
 * — reproduced twice at exactly `deep 65 native:finalize`, both before and
 * after the parameter-application memoization. The fix is process-level:
 * `scripts/run-scan-bench-evidence-v1.mjs` runs this file once per
 * MEASUREMENT in a fresh process and once more to judge the merged readings.
 *
 *   - ""        — the historical single-process run (kept as a direct knob;
 *                 known to trap at deep-65 finalize on full coverage).
 *   - "list"    — build the configured curve points, write
 *                 `[{shape, nodes, labels}]` to MIDGARD_SCAN_BENCH_OUT, no
 *                 measurement and no ledger judgement.
 *   - "measure" — measure the (env-narrowed) points/steps and write the
 *                 reports to MIDGARD_SCAN_BENCH_OUT; positivity is asserted
 *                 here, the ledger judgement is NOT (a narrowed child must
 *                 not judge — that is the orchestrator's final phase).
 *   - "check"   — no measurement: read the orchestrator's merged readings
 *                 from MIDGARD_SCAN_BENCH_MERGED and run the full ledger
 *                 judgement (and update/bootstrap path) over them.
 */
const BENCH_MODE = process.env.MIDGARD_SCAN_BENCH_MODE ?? "";

/** Revives a merged-readings JSON row (bigints travel as strings). */
const reviveCurveReading = (curve: {
  readonly shape: PayloadShape;
  readonly nodes: number;
  readonly payloadBytes: number;
  readonly spentOutputBytes: number;
  readonly proofStepCounts: CurvePointReport["proofStepCounts"];
  readonly measurements: readonly (Omit<
    StepMeasurement,
    "mem" | "cpu" | "fee"
  > & { mem: string; cpu: string; fee: string })[];
}): CurvePointReport => ({
  ...curve,
  measurements: curve.measurements.map((row) => ({
    ...row,
    mem: BigInt(row.mem),
    cpu: BigInt(row.cpu),
    fee: BigInt(row.fee),
  })),
});

describe.skipIf(!EVIDENCE)(
  "native-script scan fault-proof step ExUnits baseline (#633)",
  () => {
    it(
      "pins the live one-token-per-transaction staged scan against the committed ExUnits ledger",
      { timeout: 14_400_000 },
      async () => {
        const resolveCurvePoints = (shape: PayloadShape): number[] => {
          const producedItem = makeMinAdaFundedExactSizeOutputItem(160);
          const funding = fundingLovelaceForOutputs([producedItem]);
          const tokens =
            INCLUDE_MAX_FIT && !CURVE_NODE_TOKENS.includes("maxfit")
              ? [...CURVE_NODE_TOKENS, "maxfit"]
              : [...CURVE_NODE_TOKENS];
          return tokens.map((token) =>
            token === "maxfit"
              ? maxFitNodes(shape, funding)
              : Number.parseInt(token, 10),
          );
        };

        if (BENCH_MODE === "list") {
          const listing: {
            shape: PayloadShape;
            nodes: number;
            labels: string[];
          }[] = [];
          for (const shape of SHAPES) {
            for (const nodes of resolveCurvePoints(shape)) {
              const benchmarkCase = await buildBenchmarkCase(shape, nodes);
              listing.push({
                shape,
                nodes,
                labels: selectSteps(benchmarkCase).map(
                  (descriptor) => descriptor.label,
                ),
              });
            }
          }
          const outPath = process.env.MIDGARD_SCAN_BENCH_OUT;
          if (outPath === undefined || outPath === "") {
            throw new Error("list mode requires MIDGARD_SCAN_BENCH_OUT");
          }
          mkdirSync(dirname(outPath), { recursive: true });
          writeFileSync(outPath, `${JSON.stringify(listing, null, 2)}\n`);
          expect(listing.length).toBeGreaterThan(0);
          for (const point of listing) {
            expect(point.labels.length).toBeGreaterThan(0);
          }
          return;
        }

        const reports: CurvePointReport[] = [];
        if (BENCH_MODE === "check") {
          const mergedPath = process.env.MIDGARD_SCAN_BENCH_MERGED;
          if (mergedPath === undefined || mergedPath === "") {
            throw new Error("check mode requires MIDGARD_SCAN_BENCH_MERGED");
          }
          const merged = JSON.parse(
            readFileSync(mergedPath, "utf8"),
          ) as Parameters<typeof reviveCurveReading>[0][];
          reports.push(...merged.map(reviveCurveReading));
        } else {
          for (const shape of SHAPES) {
            for (const nodes of resolveCurvePoints(shape)) {
              const report = await runCurvePoint(shape, nodes);
              reports.push(report);

              console.log(formatReport([report]));
            }
          }
        }

        console.log(formatReport(reports));

        // Unchanged and independent of the ledger below: a scratch dump of the
        // raw reports to an arbitrary path, for reading. The pinned evidence is
        // the committed artifact, which is written only by the update lane.
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

        // Every measured step actually evaluated. Kept from the original
        // harness: it is cheap, and it separates "the emulator returned zero"
        // from "the reading moved", which the ledger comparison below would
        // otherwise report as an ordinary drift.
        for (const report of reports) {
          for (const row of report.measurements) {
            expect(row.mem).toBeGreaterThan(0n);
            expect(row.cpu).toBeGreaterThan(0n);
          }
        }

        // A measure-mode child is env-narrowed by construction, and a narrowed
        // run must never judge (or bootstrap) the ledger — that is exactly the
        // truncated-pin shape checkScanBenchLedger refuses. Its readings are
        // judged once, merged, by the orchestrator's check phase.
        if (BENCH_MODE === "measure") {
          expect(reports.length).toBeGreaterThan(0);
          return;
        }

        // The pin. Exact per-row equality against the committed artifact, with
        // each row's side of the §3.3 basis and of the L1 cap recorded as a
        // judgement that fails on movement in EITHER direction.
        const verdict = checkScanBenchLedger({
          ledger: readScanBenchLedger(SCAN_BENCH_LEDGER_PATH),
          readings: reports,
          basis: SCAN_BENCH_LEDGER_BASIS,
          update: UPDATE_LEDGER,
          filtersInEffect: scanBenchFiltersInEffect(),
        });

        // A re-take over an existing ledger follows the aiken verifier exactly:
        // a structural failure writes NOTHING, because none of those is a
        // re-takeable number. A BOOTSTRAP is the one divergence — there is no
        // ledger to protect, the readings cost ~858 seconds, and the only thing
        // a fresh bootstrap can be missing is `infeasibility`/`ruling` prose a
        // human owes. So it writes the measured numbers and still fails red.
        if (
          UPDATE_LEDGER &&
          verdict.updated !== null &&
          (verdict.bootstrapped || verdict.failures.length === 0)
        ) {
          writeScanBenchLedger(SCAN_BENCH_LEDGER_PATH, verdict.updated);
          console.log(
            `[bench] ${verdict.bootstrapped ? "bootstrapped" : "updated"} ${SCAN_BENCH_LEDGER_PATH} ` +
              `with ${verdict.rowCount.toString()} row(s)`,
          );
        }

        if (verdict.failures.length > 0) {
          throw new Error(
            `native-script scan ExUnits ledger: ${verdict.failures.length.toString()} structural failure(s).\n` +
              `${verdict.failures.map((failure) => `  - ${failure}`).join("\n")}\n` +
              (UPDATE_LEDGER
                ? "MIDGARD_SCAN_BENCH_UPDATE absorbs measurement drift and nothing else."
                : "These are not re-takeable numbers; resolve them in the source or in the ledger."),
          );
        }
        if (verdict.drifts.length > 0) {
          throw new Error(
            `native-script scan ExUnits ledger: ${verdict.drifts.length.toString()} drift(s).\n` +
              `${verdict.drifts.map((drift) => `  - ${drift}`).join("\n")}\n` +
              "Emulator ExUnits are deterministic, so a moved reading means the " +
              "blueprint, the trace builder, the transaction shape or the cost " +
              "model moved. If the re-take is legitimate, re-run the update lane " +
              "(MIDGARD_SCAN_BENCH_UPDATE=1) and commit the artifact.",
          );
        }
        expect(verdict.rowCount).toBeGreaterThan(0);
      },
    );
  },
);
