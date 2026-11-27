import { computeHash28, encodeMidgardTxOutput } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import {
  credentialToAddress,
  Data,
  Emulator,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { createDirectFieldPlan } from "../../src/direct-script-derivation/field-plan.js";
import { prepareDirectScriptMaterial } from "../../src/direct-script-derivation/material.js";
import { buildDirectReceiveEvidence } from "../../src/direct-script-derivation/receive-evidence.js";
import { faultProofFieldOpening } from "../../src/field-opening.publish-fault-proof-field-carriage.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.reconstruct-da-payload.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "../helpers/canonical-block-evidence-fixture.js";
import { createDirectComponentStepper } from "./direct-component-stepper.js";
import { directHandoffContext } from "./direct-handoff-fixture.js";
import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./emulator/blueprints.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./emulator/protocol-parameters.js";
import { publishPlainReferenceScriptUtxo } from "./emulator/reference-scripts.publish-operator-lifecycle-reference-scripts.js";

export const driveDirectDeclarationRegression = async (
  attack: "wrong-index" | "unsigned-correct",
) => {
  const hash = "22".repeat(28);
  const output = (protectedAddress: boolean) =>
    encodeMidgardTxOutput({
      address: Buffer.from((protectedAddress ? "78" : "70") + hash, "hex"),
      value: { lovelace: 5_000_000n, assets: new Map() },
    });
  const outputs = [output(true)];
  const tx = buildFixtureTransaction({ spendInputs: [], outputs, fee: 0n });
  const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: tx.txId } };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [tx],
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
  });
  const material = await prepareDirectScriptMaterial({
    current: await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    }),
    eventKey,
  });
  const evidence = await buildDirectReceiveEvidence(outputs);
  const wrong = await buildDirectReceiveEvidence([output(false), output(true)]);
  expect(evidence.entries).toHaveLength(1);
  expect(evidence.entries[0]!.first_output_index).toBe(0n);
  expect(wrong.entries[0]!.first_output_index).toBe(1n);
  expect(wrong.outputRoot).not.toBe(evidence.outputRoot);
  const {
    owner,
    stranger,
    prover,
    strangerHash,
    receive,
    policy,
    certificate,
    nextHash,
  } = await directHandoffContext(readBlueprint(realBlueprintPath));
  expect(strangerHash).not.toBe(prover);
  const plan = createDirectFieldPlan(material, 2, prover);
  let state: SDKFraudProof.DirectReceiveOutputState = {
    cursor: plan.position("Certifying", 0).cursor,
    source_index: { root: SDK.EMPTY_MERKLE_TREE_ROOT, count: 0n },
    credential_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    credential_count: 0n,
    complete: false,
  };
  const outputDatum = (data: SDKFraudProof.DirectReceiveOutputState) =>
    Data.to(
      { fraud_prover: prover, data },
      SDKFraudProof.DirectReceiveOutputDatum,
    );
  const unit =
    policy +
    "0000002b" +
    computeHash28(SDK.encodeHeaderCbor(material.sourceBound.header)).toString(
      "hex",
    );
  const emulator = new Emulator(
    [
      owner,
      stranger,
      {
        ...owner,
        address: receive.start.spendingScriptAddress,
        assets: { lovelace: 30_000_000n, [unit]: 1n },
        outputData: {
          inline: Data.to(
            {
              fraud_prover: prover,
              data: { bound: material.bound, source_index: state.source_index },
            },
            SDKFraudProof.DirectReceiveStartDatum,
          ),
        },
      },
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const ownerLucid = await Lucid(emulator, network);
  ownerLucid.selectWallet.fromSeed(owner.seedPhrase);
  const strangerLucid = await Lucid(emulator, network);
  strangerLucid.selectWallet.fromSeed(stranger.seedPhrase);
  const references = new Map<string, UTxO>();
  for (const role of Object.values(receive))
    references.set(
      role.spendingScriptHash,
      (
        await publishPlainReferenceScriptUtxo({
          lucid: ownerLucid,
          script: role.spendingScript,
          label: "declaration regression",
        })
      ).utxo,
    );
  const initialThread = (
    await ownerLucid.utxosAt(receive.start.spendingScriptAddress)
  ).find((item) => item.assets[unit] === 1n)!;
  const run = createDirectComponentStepper({
    initialThread,
    unit,
    references,
    opening: (_, referenceInputs) =>
      faultProofFieldOpening({
        planned: plan.openingPlan,
        referenceInputs,
        certificatePolicyId: certificate,
        label: "declaration",
      }),
  });
  await run.accept(
    {
      label: "receive initialization from completed source receipt",
      role: receive.start,
      destination: receive.grammar.spendingScriptAddress,
      datum: outputDatum(state),
      redeemer: (indices, open) =>
        Data.to(
          { Continue: [{ ...indices, outputs_opening: open(2) }] },
          SDKFraudProof.DirectReceiveStartRedeemer,
        ),
    },
    ownerLucid,
  );
  state = { ...state, cursor: plan.position("Walking", 0).cursor };
  await run.accept(
    {
      label: "actual singleton grammar",
      role: receive.grammar,
      destination: receive.outputs.spendingScriptAddress,
      datum: outputDatum(state),
      redeemer: (indices, open) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                outputs_opening: open(2),
                checkpoint_bytes: plan.position("Certifying", 0)
                  .checkpointBytes,
                budget: 1n,
              },
            ],
          },
          SDKFraudProof.DirectReceiveGrammarRedeemer,
        ),
    },
    ownerLucid,
  );
  state = {
    ...state,
    cursor: plan.position("Walking", 1).cursor,
    credential_root: evidence.outputRoot,
    credential_count: 1n,
    complete: true,
  };
  await run.accept(
    {
      label: "actual singleton credential H to zero",
      role: receive.outputs,
      destination: receive.outputs.spendingScriptAddress,
      datum: outputDatum(state),
      redeemer: (indices, open) =>
        Data.to(
          {
            Continue: [
              {
                Scan: {
                  ...indices,
                  outputs_opening: open(2),
                  checkpoint_bytes: plan.position("Walking", 0).checkpointBytes,
                  witnesses: evidence.witnesses,
                },
              },
            ],
          },
          SDKFraudProof.DirectReceiveOutputRedeemer,
        ),
    },
    ownerLucid,
  );
  await run.accept(
    {
      label: "authentic exhaustion handoff",
      role: receive.outputs,
      destination: receive.finish.spendingScriptAddress,
      datum: outputDatum(state),
      redeemer: (indices) =>
        Data.to(
          { Continue: [{ Finish: indices }] },
          SDKFraudProof.DirectReceiveOutputRedeemer,
        ),
    },
    ownerLucid,
  );
  const declaration: SDKFraudProof.DirectReceiveDeclarationState = {
    bound: material.bound,
    source_index: state.source_index,
    output_root: evidence.outputRoot,
    expected_count: 1n,
    declared_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    declared_count: 0n,
    previous_credential: "",
    target_ordinal: 0n,
    selected: null,
  };
  const declarationDatum = (
    data: SDKFraudProof.DirectReceiveDeclarationState,
  ) =>
    Data.to(
      { fraud_prover: prover, data },
      SDKFraudProof.DirectReceiveDeclarationDatum,
    );
  await run.accept(
    {
      label: "owner selects actual receive target",
      role: receive.finish,
      destination: receive.declaration.spendingScriptAddress,
      datum: declarationDatum(declaration),
      redeemer: (indices, open) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                outputs_opening: open(2),
                checkpoint_bytes: plan.position("Walking", 1).checkpointBytes,
                target_ordinal: 0n,
              },
            ],
          },
          SDKFraudProof.DirectReceiveFinishRedeemer,
        ),
    },
    ownerLucid,
  );
  const advanced = (
    poison: boolean,
  ): SDKFraudProof.DirectReceiveDeclarationState => ({
    ...declaration,
    declared_root: poison ? wrong.outputRoot : evidence.outputRoot,
    declared_count: 1n,
    previous_credential: hash,
    selected: {
      purpose: {
        coordinate: { kind: 3n, index: 0n },
        script_hash: hash,
        subject: hash,
      },
      first_output_index: poison ? 1n : 0n,
    },
  });
  const scan = (poison: boolean) => ({
    label: poison
      ? "unrelated singleton H to one poison"
      : "actual singleton H to zero declaration",
    role: receive.declaration,
    destination: receive.declaration.spendingScriptAddress,
    datum: declarationDatum(advanced(poison)),
    redeemer: (indices: { input_index: bigint; output_index: bigint }) =>
      Data.to(
        {
          Continue: [
            {
              Scan: {
                ...indices,
                entries: poison ? wrong.entries : evidence.entries,
              },
            },
          ],
        },
        SDKFraudProof.DirectReceiveDeclarationRedeemer,
      ),
  });
  await run.refuse(scan(attack === "wrong-index"), strangerLucid);
  await run.accept(scan(false), ownerLucid);
  const finish = (label: string) => ({
    label,
    role: receive.declaration,
    destination: credentialToAddress(network, {
      type: "Script",
      hash: nextHash,
    }),
    datum: Data.to(
      {
        fraud_prover: prover,
        data: {
          bound: material.bound,
          source_index: state.source_index,
          coordinate: { kind: 3n, index: 0n },
          purpose: advanced(false).selected!.purpose,
        },
      },
      SDKFraudProof.DirectPurposeSelectionDatum,
    ),
    redeemer: (indices: { input_index: bigint; output_index: bigint }) =>
      Data.to(
        { Continue: [{ Finish: indices }] },
        SDKFraudProof.DirectReceiveDeclarationRedeemer,
      ),
  });
  await run.refuse(
    finish("unrelated key finishes honest sorted declaration"),
    strangerLucid,
  );
  await run.accept(
    finish("owner finishes honest sorted declaration"),
    ownerLucid,
  );
  console.log(
    "actual declaration authorization",
    JSON.stringify({
      attack,
      prover,
      strangerHash,
      measurements: run.measurements,
    }),
  );
  return run.measurements;
};
