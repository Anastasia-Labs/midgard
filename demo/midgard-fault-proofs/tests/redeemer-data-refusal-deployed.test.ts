import {
  buildMidgardRedeemerItemProofTrace,
  hashMidgardValidationMachineState,
  encodeCbor,
  isMidgardRedeemerDataHeadRejection,
  nextMidgardRedeemerItemProofSpan,
  readMidgardRedeemerItemProofSource,
} from "@al-ft/midgard-core";
import {
  buildScriptSourcesRedeemerItemStages,
  parseFaultProofBlueprint,
  PreparedValidationResolutionState,
  requireInputIndex,
  validationMachineStateDataFromCore,
  ValidationOneStepWitness,
} from "@al-ft/midgard-sdk";
import {
  redeemerItemControlData,
  validationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { deriveScriptSourcesRedeemerItemPlan } from "../src/redeemer-item-plan.js";
import { validationOneStepEvidenceHashFromData } from "../src/validation-dispute/submit/evidence.js";
import { buildDataRefusalTrace } from "./redeemer-data-refusal-deployed.build-trace.js";
import {
  applyCompiledScript,
  getCompiledScript,
} from "./support/emulator/blueprints.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  expectOnchainRefusal,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const values = [
  ["1801", "01", 0],
  ["d8799f1801ff", "d8799f01ff", 3],
  ["d8799f011801ff", "d8799f0101ff", 4],
  ["bf0101ff", "a10101", 0],
  ["d8668218808101", "d8668218809f01ff", 5],
  ["d8798101", "d8799f01ff", 0],
  ["d8799f8101ff", "d8799f9f01ffff", 3],
  ["d8799f01810102ff", "d8799f019f01ff02ff", 4],
  ["d8799fa20101810102ff", "d8799fa201019f01ff02ff", 6],
  ["580100", "4100", 0],
  ["59000100", "4100", 0],
  ["5a0000000100", "4100", 0],
  ["5b000000000000000100", "4100", 0],
  ["d8799f580100ff", "d8799f4100ff", 3],
  ["d8799f01580100ff", "d8799f014100ff", 4],
  ["590018" + "ab".repeat(24), "5818" + "ab".repeat(24), 0],
  ["b800", "a0", 0],
  ["b8010101", "a10101", 0],
  ["d9007980", "d87980", 0],
  ["580100", "4100", 0, true],
  ["d8799f01580100ff", "d8799f014100ff", 4, true],
] as const;
const policy = "72".repeat(28),
  deploymentId = "71".repeat(32),
  awardHash = "73".repeat(28);
const makeStages = () =>
  buildScriptSourcesRedeemerItemStages({
    blueprint: parseFaultProofBlueprint(readBlueprint(realBlueprintPath)),
    network,
    computationThreadPolicyId: policy,
    deploymentId,
    awardScriptHash: awardHash,
  });
const exactData = (value: unknown) => Data.from(Data.to<unknown>(value));

it("requires the invalid Data executor's exact deployment parameters", () => {
  const blueprint = readBlueprint(realBlueprintPath);
  const title =
    "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_data_executor.main.spend";
  const entry = blueprint.validators.find(
    (validator) => validator.title === title,
  )!;
  expect(entry.parameters!.map((parameter) => parameter.title)).toEqual([
    "deployment_id",
    "computation_thread_policy_id",
  ]);
  expect(applyCompiledScript(blueprint, title, [deploymentId, policy])).toMatch(
    /^[0-9a-f]+$/u,
  );
  for (const params of [[], [deploymentId], [deploymentId, policy, policy]]) {
    expect(() => applyCompiledScript(blueprint, title, params)).toThrow(
      /declares 2 parameter/u,
    );
  }
  expect(() => getCompiledScript(blueprint, title)).toThrow(
    /declares 2 parameter/u,
  );
  expect(() =>
    applyCompiledScript(blueprint, title, [deploymentId, "ab"]),
  ).toThrow(/28-byte hash/u);
});

it("settles authenticated Data refusals through the deployed ScriptSources item chain", async () => {
  const stages = makeStages();
  const account = generateEmulatorAccount({ lovelace: 100_000_000_000n });
  const prover = getAddressDetails(account.address).paymentCredential!.hash;
  const datum = (state: Data) =>
    Data.to(new Constr(0, [prover, new Constr(0, [state])]));
  const cases = await Promise.all(
    values.map(async ([data, , offset, missingScript = false], index) => {
      const { trace } = await buildDataRefusalTrace(
        data,
        undefined,
        missingScript,
      ).catch((cause: unknown) => {
        throw new Error(
          `Data refusal trace failed for ${data}: ${String(cause)}`,
        );
      });
      const coordinate = trace.witnesses.length - 2;
      const witness = trace.witnesses[coordinate]!;
      expect(witness.auxiliary).toMatchObject({
        kind: "redeemerItemStep",
        control: { traversal: { offset } },
        witness: { action: { kind: "traverseData", action: null } },
      });
      const transition = Data.from(
        Data.to(
          {
            work_witness_cbor: witness.cbor.toString("hex"),
            claimed_successor: validationMachineStateDataFromCore(
              trace.states[coordinate + 1]!,
            ),
          },
          ValidationOneStepWitness,
        ),
      );
      const auxiliary = exactData(
        validationAuxiliaryWitnessData(witness.auxiliary),
      );
      const prepared = Data.from(
        Data.to(
          {
            version: 1n,
            resolution: {
              version: 1n,
              pre_state: validationMachineStateDataFromCore(
                trace.states[coordinate]!,
              ),
              operator_successor_hash: "ff".repeat(32),
              challenger_successor_hash: hashMidgardValidationMachineState(
                trace.states[coordinate + 1]!,
              ).toString("hex"),
            },
            evidence_hash: validationOneStepEvidenceHashFromData(
              transition,
              auxiliary,
            ),
          },
          PreparedValidationResolutionState,
        ),
      );
      const plan = deriveScriptSourcesRedeemerItemPlan({
        preparedResolution: prepared,
        transition,
        auxiliary,
        stages,
        deploymentId,
      });
      expect(plan.map((stage) => stage.key)).toEqual([
        "entry",
        "normalize_current_traversal",
        "normalize_current_outer",
        "authenticate_source",
        "execute",
        "settle",
      ]);
      expect(plan[4]!.validator).toEqual(stages.executors[19]);
      return {
        data,
        prepared,
        plan,
        unit: policy + index.toString(16).padStart(64, "0"),
      };
    }),
  );
  const emulator = new Emulator(
    [
      account,
      ...cases.map((c) => ({
        ...account,
        address: stages.entry.spendingScriptAddress,
        assets: { lovelace: 30_000_000n, [c.unit]: 1n },
        outputData: { inline: datum(c.prepared) },
      })),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const refs = new Map<string, UTxO>();
  for (const role of [
    stages.entry,
    stages.traversalNormalizer,
    stages.outerNormalizer,
    stages.sourceAuthenticator,
    stages.executors[19]!,
    stages.settlement,
  ]) {
    const publication = await publishPlainReferenceScriptUtxo({
      lucid,
      script: role.spendingScript,
      label: "Data refusal",
    });
    expect(
      publication.publicationMeasurement.completeSignedBytes,
    ).toBeLessThanOrEqual(15872);
    refs.set(role.spendingScriptHash, publication.utxo);
    console.log(
      "Data refusal publication",
      role.spendingScriptHash,
      publication.publicationMeasurement,
    );
  }
  for (const c of cases) {
    let thread = (await lucid.utxosAt(stages.entry.spendingScriptAddress)).find(
      (u) => u.assets[c.unit] === 1n,
    )!;
    for (let index = 0; index < c.plan.length; index++) {
      const stage = c.plan[index]!;
      const nextAddress =
        c.plan[index + 1]?.validator.spendingScriptAddress ??
        credentialToAddress(network, { type: "Script", hash: awardHash });
      const spend = ((ctx) =>
        Data.to(
          stage.spendRedeemer(
            requireInputIndex(ctx, thread, "Data refusal"),
            0n,
          ),
        )) satisfies BuildTxWithRedeemer;
      const build = (output: Data) =>
        lucid
          .newTx()
          .collectFrom([thread], spend)
          .readFrom([refs.get(stage.validator.spendingScriptHash)!])
          .pay.ToContract(
            nextAddress,
            { kind: "inline", value: datum(output) },
            thread.assets,
          )
          .addSignerKey(prover)
          .complete({ localUPLCEval: true });
      // The identical accepted control below isolates the output binding at every stage.
      await expectOnchainRefusal(() => build(new Constr(0, [0n])));
      if (stage.key === "authenticate_source") {
        // Missing proof must fail in authentication, never be routed as semantic invalidity.
        const spendMissing = ((ctx) => {
          const redeemer = stage.spendRedeemer(
            requireInputIndex(ctx, thread, "Data refusal"),
            0n,
          ) as Constr<Data>;
          const action = redeemer.fields[0] as Constr<Data>;
          const witness = action.fields[2] as Constr<Data>;
          return Data.to(
            new Constr(1, [
              new Constr(0, [
                action.fields[0]!,
                action.fields[1]!,
                new Constr(0, [
                  witness.fields[0]!,
                  new Constr(1, []),
                  new Constr(1, []),
                ]),
              ]),
            ]),
          );
        }) satisfies BuildTxWithRedeemer;
        await expectOnchainRefusal(() =>
          lucid
            .newTx()
            .collectFrom([thread], spendMissing)
            .readFrom([refs.get(stage.validator.spendingScriptHash)!])
            .pay.ToContract(
              nextAddress,
              { kind: "inline", value: datum(stage.outputState) },
              thread.assets,
            )
            .addSignerKey(prover)
            .complete({ localUPLCEval: true }),
        );
      }
      const signed = await (
        await build(stage.outputState).catch((cause: unknown) => {
          throw new Error(`${c.data} ${stage.key}: ${String(cause)}`);
        })
      ).sign
        .withWallet()
        .complete();
      const measured = measureCompleteSignedTransaction(signed.toCBOR());
      expect(measured.completeSignedBytes).toBeLessThanOrEqual(15872);
      expect(measured.executionMemory).toBeLessThanOrEqual(13_200_000n);
      expect(measured.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
      console.log("Data refusal stage", c.data, stage.key, measured);
      const hash = await signed.submit();
      await lucid.awaitTx(hash);
      thread = (
        await lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
      )[0]!;
      expect(thread.datum).toBe(datum(stage.outputState));
    }
    expect(Data.from(thread.datum!)).toEqual(
      Data.from(datum(new Constr(0, [1n]))),
    );
  }
}, 1_800_000);

it("refuses canonical Data at the deployed invalid executor", async () => {
  const stages = makeStages(),
    executor = stages.executors[19]!;
  const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = getAddressDetails(account.address).paymentCredential!.hash;
  const datum = (state: Data) =>
    Data.to(new Constr(0, [prover, new Constr(0, [state])]));
  const attestation = new Constr(0, [1n]);
  const cases = values.map(([, canonical, offset], index) => {
    const trace = buildMidgardRedeemerItemProofTrace({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: encodeCbor([
        0n,
        0n,
        Buffer.from(canonical, "hex"),
        [10n, 20n],
      ]),
      mode: 1,
    });
    const step = trace.steps.find(
      (s) =>
        s.control.stage === 2 &&
        s.control.traversal?.offset === offset &&
        [0, 4, 5].includes(s.control.traversal.stage),
    )!;
    expect(step).toBeDefined();
    const witness = {
      ...step.witness,
      action: { kind: "traverseData" as const, action: null },
    };
    expect(isMidgardRedeemerDataHeadRejection(step.control, witness)).toBe(
      false,
    );
    const source = readMidgardRedeemerItemProofSource({
      control: step.control,
      witness,
    });
    expect(source).not.toBeNull();
    expect(nextMidgardRedeemerItemProofSpan(step.control)).not.toBeNull();
    const current = exactData(redeemerItemControlData(step.control));
    const state = new Constr(0, [
      new Constr(0, [
        deploymentId,
        9n,
        executor.spendingScriptHash,
        stages.settlement.spendingScriptHash,
        attestation,
      ]),
      current,
      current,
      new Constr(2, [new Constr(0, [])]),
      new Constr(0, [Buffer.from(source!.sourceBytes!).toString("hex")]),
    ]);
    return { state, unit: policy + index.toString(16).padStart(64, "0") };
  });
  const emulator = new Emulator(
    [
      account,
      ...cases.map((c) => ({
        ...account,
        address: executor.spendingScriptAddress,
        assets: { lovelace: 30_000_000n, [c.unit]: 1n },
        outputData: { inline: datum(c.state) },
      })),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const publication = await publishPlainReferenceScriptUtxo({
    lucid,
    script: executor.spendingScript,
    label: "Data refusal negative",
  });
  for (const c of cases) {
    const thread = (await lucid.utxosAt(executor.spendingScriptAddress)).find(
      (u) => u.assets[c.unit] === 1n,
    )!;
    const spend = ((ctx) =>
      Data.to(
        new Constr(1, [
          new Constr(0, [requireInputIndex(ctx, thread, "canonical Data"), 0n]),
        ]),
      )) satisfies BuildTxWithRedeemer;
    await expectOnchainRefusal(
      () =>
        lucid
          .newTx()
          .collectFrom([thread], spend)
          .readFrom([publication.utxo])
          .pay.ToContract(
            stages.settlement.spendingScriptAddress,
            { kind: "inline", value: datum(attestation) },
            thread.assets,
          )
          .addSignerKey(prover)
          .complete({ localUPLCEval: true }),
      {
        refusedBy:
          "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_data_executor",
        check: /semantics.invalid_data\(state\)/u,
      },
    );
  }
}, 240_000);
