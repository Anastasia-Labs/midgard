import { computeHash28 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  buildDirectReceiveEvidence,
  buildDirectSourceIndexEvidence,
  createDirectFieldPlan,
} from "../../src/direct-script-derivation/index.js";
import { publishDirectReceiveFieldCarriage } from "./direct-receive-field-carriage.js";
import { directReceiveLifecycleMaterial } from "./direct-receive-max-fixture.js";
import { createDirectReceiveStepper } from "./direct-receive-stepper.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./submit-init-emulator-shared.js";

export const driveDirectReceiveLifecycle = async (
  options: {
    readonly outputs?: readonly Buffer[];
    readonly batchSize?: number;
    readonly targetOrdinal?: number;
    readonly walletAddressType?: "Base" | "Enterprise";
  } = {},
) => {
  const { outputs, scriptWitnesses, material } =
    await directReceiveLifecycleMaterial(options.outputs);
  const batchSize = options.batchSize ?? 2;
  const targetOrdinal = options.targetOrdinal ?? 1;
  const batches = (count: number) =>
    Array.from(
      { length: Math.ceil(count / batchSize) },
      (_, index) =>
        [index * batchSize, Math.min((index + 1) * batchSize, count)] as const,
    );
  const generatedAccount = generateEmulatorAccount({
    lovelace: 40_000_000_000n,
  });
  const prover = getAddressDetails(generatedAccount.address).paymentCredential!
    .hash;
  const walletAddressType = options.walletAddressType ?? "Base";
  const account = {
    ...generatedAccount,
    address:
      walletAddressType === "Enterprise"
        ? credentialToAddress(network, { type: "Key", hash: prover })
        : generatedAccount.address,
  };
  const plan = createDirectFieldPlan(material, 2, prover);
  const sourcePlan = createDirectFieldPlan(material, 6, prover);
  const sourceEvidence = await buildDirectSourceIndexEvidence(material);
  const sourceReceipt: SDK.DirectSourceBoundIndex = {
    bound: material.bound,
    source_index: sourceEvidence.index,
  };
  expect(sourceReceipt.source_index.count).toBe(BigInt(scriptWitnesses.length));
  const substitutedIndexes = [
    { ...sourceReceipt.source_index, root: "55".repeat(32) },
    {
      ...sourceReceipt.source_index,
      count: sourceReceipt.source_index.count + 1n,
    },
  ];
  const evidence = await buildDirectReceiveEvidence(outputs);
  const blueprint = readBlueprint(realBlueprintPath),
    policy = "cc".repeat(28),
    returnHash = "ee".repeat(28);
  const certificate = SDK.makeMintingPolicy(
    SDK.getUnappliedScript(
      SDK.parseFaultProofBlueprint(blueprint),
      "field_preimage_certificate.field_preimage_certificate.mint",
    ),
  );
  const roles = await Effect.runPromise(
    SDK.buildDirectReceiveContracts({
      blueprint,
      network,
      computationThreadPolicyId: policy,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      nextStepScriptHash: returnHash,
    }),
  );
  const sourceRoles = await Effect.runPromise(
    SDK.buildDirectSourceContracts({
      blueprint,
      network,
      computationThreadPolicyId: policy,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      nextStepScriptHash: roles.start.spendingScriptHash,
    }),
  );
  const unit =
    policy +
    "0000002b" +
    computeHash28(SDK.encodeHeaderCbor(material.sourceBound.header)).toString(
      "hex",
    );
  let state: SDK.DirectReceiveOutputState = {
    cursor: plan.position("Certifying", 0).cursor,
    source_index: sourceReceipt.source_index,
    credential_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    credential_count: 0n,
    complete: false,
  };
  const datum = (value: SDK.DirectReceiveOutputState) =>
    Data.to(
      { fraud_prover: prover, data: value },
      SDK.DirectReceiveOutputDatum,
    );
  const emulator = new Emulator(
    [
      account,
      {
        ...account,
        // Seed only the completed source prefix. Source Finish and the new
        // receive initializer are real physical transitions below; this is
        // still not CT Init, family adjudication, proof mint or SQ acceptance.
        address: sourceRoles.finish.spendingScriptAddress,
        assets: { lovelace: 30_000_000n, [unit]: 1n },
        outputData: {
          inline: Data.to(
            {
              fraud_prover: prover,
              data: {
                cursor: sourcePlan.position("Walking", scriptWitnesses.length)
                  .cursor,
                source_index: sourceReceipt.source_index,
                complete: true,
              },
            },
            SDK.DirectSourceScanDatum,
          ),
        },
      },
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(account.seedPhrase, {
    addressType: walletAddressType,
  });
  const references = new Map<string, UTxO>();
  for (const [name, role] of Object.entries({
    ...roles,
    sourceFinish: sourceRoles.finish,
  }))
    references.set(
      role.spendingScriptHash,
      (
        await publishPlainReferenceScriptUtxo({
          lucid,
          script: role.spendingScript,
          label: `N6 receive ${name}`,
        })
      ).utxo,
    );
  const carriageReferences = await publishDirectReceiveFieldCarriage({
    lucid,
    emulator,
    account,
    prover,
    plan: plan.openingPlan,
    certificate,
    compactCbor: material.bound.compact_cbor,
    witnessSetCompactCbor:
      material.material.proofSource.witnessSetCompactCbor.toString("hex"),
    walletAddressType,
  });
  const { step, measurements } = createDirectReceiveStepper({
    lucid,
    prover,
    unit,
    initialThread: (
      await lucid.utxosAt(sourceRoles.finish.spendingScriptAddress)
    ).find((u) => u.assets[unit] === 1n)!,
    references,
    carriageReferences,
    plan,
    sourcePlan,
    certificatePolicyId: certificate.policyId,
  });
  await step(
    "complete source receipt into receive initializer",
    sourceRoles.finish,
    roles.start.spendingScriptAddress,
    Data.to(
      { fraud_prover: prover, data: sourceReceipt },
      SDK.DirectReceiveStartDatum,
    ),
    ({ opening, ...indices }) =>
      Data.to(
        {
          Continue: [
            {
              ...indices,
              opening,
              checkpoint: sourcePlan.position("Walking", scriptWitnesses.length)
                .checkpointBytes,
            },
          ],
        },
        SDK.DirectSourceFinishRedeemer,
      ),
    {
      fieldIndex: 6,
      refusedDatums: substitutedIndexes.map((source_index) =>
        Data.to(
          {
            fraud_prover: prover,
            data: { ...sourceReceipt, source_index },
          },
          SDK.DirectReceiveStartDatum,
        ),
      ),
    },
  );
  await step(
    "initialize receive from completed source receipt",
    roles.start,
    roles.grammar.spendingScriptAddress,
    datum(state),
    ({ opening, ...indices }) =>
      Data.to(
        { Continue: [{ ...indices, outputs_opening: opening }] },
        SDK.DirectReceiveStartRedeemer,
      ),
    {
      refusedDatums: substitutedIndexes.map((source_index) =>
        datum({ ...state, source_index }),
      ),
    },
  );
  for (const [start, end] of batches(outputs.length)) {
    const next = plan.position(
      end === outputs.length ? "Walking" : "Certifying",
      end === outputs.length ? 0 : end,
    );
    const advanced = { ...state, cursor: next.cursor };
    await step(
      `grammar ${start}..${end}`,
      roles.grammar,
      end === outputs.length
        ? roles.outputs.spendingScriptAddress
        : roles.grammar.spendingScriptAddress,
      datum(advanced),
      ({ opening, ...indices }) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                outputs_opening: opening,
                checkpoint_bytes: plan.position("Certifying", start)
                  .checkpointBytes,
                budget: BigInt(end - start),
              },
            ],
          },
          SDK.DirectReceiveGrammarRedeemer,
        ),
    );
    state = advanced;
  }
  for (const [start, end] of batches(outputs.length)) {
    const advanced = {
      ...state,
      cursor: plan.position("Walking", end).cursor,
      credential_root: evidence.outputRoots[end]!,
      credential_count: BigInt(
        evidence.witnesses
          .slice(0, end)
          .filter((w) => typeof w === "object" && "FirstCredential" in w)
          .length,
      ),
      complete: end === outputs.length,
    };
    await step(
      `outputs ${start}..${end}`,
      roles.outputs,
      roles.outputs.spendingScriptAddress,
      datum(advanced),
      ({ opening, ...indices }) =>
        Data.to(
          {
            Continue: [
              {
                Scan: {
                  ...indices,
                  outputs_opening: opening,
                  checkpoint_bytes: plan.position("Walking", start)
                    .checkpointBytes,
                  witnesses: evidence.witnesses.slice(start, end),
                },
              },
            ],
          },
          SDK.DirectReceiveOutputRedeemer,
        ),
    );
    state = advanced;
  }
  await step(
    "forward exact output state",
    roles.outputs,
    roles.finish.spendingScriptAddress,
    datum(state),
    (indices) =>
      Data.to(
        {
          Continue: [
            {
              Finish: {
                input_index: indices.input_index,
                output_index: indices.output_index,
              },
            },
          ],
        },
        SDK.DirectReceiveOutputRedeemer,
      ),
  );
  const declaration: SDK.DirectReceiveDeclarationState = {
    bound: material.bound,
    source_index: sourceReceipt.source_index,
    output_root: evidence.outputRoot,
    expected_count: BigInt(evidence.entries.length),
    declared_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    declared_count: 0n,
    previous_credential: "",
    target_ordinal: BigInt(targetOrdinal),
    selected: null,
  };
  const declarationDatum = (value: SDK.DirectReceiveDeclarationState) =>
    Data.to(
      { fraud_prover: prover, data: value },
      SDK.DirectReceiveDeclarationDatum,
    );
  await step(
    "independent output exhaustion",
    roles.finish,
    roles.declaration.spendingScriptAddress,
    declarationDatum(declaration),
    ({ opening, ...indices }) =>
      Data.to(
        {
          Continue: [
            {
              ...indices,
              outputs_opening: opening,
              checkpoint_bytes: plan.position("Walking", outputs.length)
                .checkpointBytes,
              target_ordinal: BigInt(targetOrdinal),
            },
          ],
        },
        SDK.DirectReceiveFinishRedeemer,
      ),
  );
  const target = evidence.entries[targetOrdinal];
  if (target === undefined) throw new Error("N6 fixture target ordinal absent");
  const selected: SDK.DirectReceivePurpose = {
    purpose: {
      coordinate: { kind: 3n, index: BigInt(targetOrdinal) },
      script_hash: target.script_hash,
      subject: target.script_hash,
    },
    first_output_index: target.first_output_index,
  };
  let declared = declaration;
  for (const [start, end] of batches(evidence.entries.length)) {
    const advanced = {
      ...declared,
      declared_root: evidence.declarationRoots[end]!,
      declared_count: BigInt(end),
      previous_credential: evidence.entries[end - 1]!.script_hash,
      selected: targetOrdinal < end ? selected : null,
    };
    await step(
      `sorted exact credential declaration ${start}..${end}`,
      roles.declaration,
      roles.declaration.spendingScriptAddress,
      declarationDatum(advanced),
      (indices) =>
        Data.to(
          {
            Continue: [
              {
                Scan: {
                  input_index: indices.input_index,
                  output_index: indices.output_index,
                  entries: evidence.entries.slice(start, end),
                },
              },
            ],
          },
          SDK.DirectReceiveDeclarationRedeemer,
        ),
    );
    declared = advanced;
  }
  const selection: SDK.DirectPurposeSelection = {
    bound: material.bound,
    source_index: sourceReceipt.source_index,
    coordinate: { kind: 3n, index: BigInt(targetOrdinal) },
    purpose: selected.purpose,
  };
  const selectionDatum = (data: SDK.DirectPurposeSelection) =>
    Data.to({ fraud_prover: prover, data }, SDK.DirectPurposeSelectionDatum);
  const finalDatum = selectionDatum(selection);
  await step(
    "selected sorted receive ordinal",
    roles.declaration,
    credentialToAddress(network, { type: "Script", hash: returnHash }),
    finalDatum,
    (indices) =>
      Data.to(
        {
          Continue: [
            {
              Finish: {
                input_index: indices.input_index,
                output_index: indices.output_index,
              },
            },
          ],
        },
        SDK.DirectReceiveDeclarationRedeemer,
      ),
    {
      refusedDatums: substitutedIndexes.map((source_index) =>
        selectionDatum({ ...selection, source_index }),
      ),
    },
  );
  return measurements;
};
