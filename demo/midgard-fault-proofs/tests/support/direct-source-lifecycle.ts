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

import { createDirectFieldPlan } from "../../src/direct-script-derivation/index.js";
import { publishDirectReceiveFieldCarriage } from "./direct-receive-field-carriage.js";
import {
  directSourceAdmissionRedeemer,
  directSourceAuthenticationRedeemer,
  directSourceBatchFixture,
} from "./direct-source-fixture.js";
import { createDirectSourceStepper } from "./direct-source-stepper.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./submit-init-emulator-shared.js";

/** Seeded authenticated-bound prefix only. This exercises actual published
 * spending workers and full field/auth/map custody; it does not claim Init,
 * family proof mint or state-queue removal acceptance. */
export const driveDirectSourceLifecycle = async (
  options: {
    readonly maximumWitnessField?: boolean;
    readonly walletAddressType?: "Base" | "Enterprise";
  } = {},
) => {
  const { material, evidence, inputs, witnessSteps } =
    await directSourceBatchFixture(options);
  const blueprint = readBlueprint(realBlueprintPath);
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
  const policy = "cc".repeat(28),
    returnHash = "ee".repeat(28);
  const certificate = SDK.makeMintingPolicy(
    SDK.getUnappliedScript(
      SDK.parseFaultProofBlueprint(blueprint),
      "field_preimage_certificate.field_preimage_certificate.mint",
    ),
  );
  const roles = await Effect.runPromise(
    SDK.buildDirectSourceContracts({
      blueprint,
      network,
      computationThreadPolicyId: policy,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      nextStepScriptHash: returnHash,
    }),
  );
  const unit =
    policy +
    "0000002b" +
    computeHash28(SDK.encodeHeaderCbor(material.sourceBound.header)).toString(
      "hex",
    );
  const plans = new Map(
    [0, 1, 6].map((field) => [
      field,
      createDirectFieldPlan(material, field, prover),
    ]),
  );
  const plan = (field: number) => plans.get(field)!;
  let state: SDK.DirectSourceScanState = {
    cursor: plan(0).position("Walking", 0).cursor,
    source_index: { root: SDK.EMPTY_MERKLE_TREE_ROOT, count: 0n },
    complete: false,
  };
  const datum = (data: SDK.DirectSourceScanState) =>
    Data.to({ fraud_prover: prover, data }, SDK.DirectSourceScanDatum);
  const resolvedStateDatum = (data: SDK.DirectSourceResolvedState) =>
    Data.to({ fraud_prover: prover, data }, SDK.DirectSourceResolvedDatum);
  const pendingStateDatum = (data: SDK.DirectSourcePendingState) =>
    Data.to({ fraud_prover: prover, data }, SDK.DirectSourcePendingDatum);
  const initialDatum = Data.to(
    { fraud_prover: prover, data: material.bound },
    SDK.DirectScriptBoundDatum,
  );
  const emulator = new Emulator(
    [
      account,
      {
        ...account,
        address: roles.start.spendingScriptAddress,
        assets: { lovelace: 30_000_000n, [unit]: 1n },
        outputData: { inline: initialDatum },
      },
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(account.seedPhrase, {
    addressType: walletAddressType,
  });
  const references = new Map<string, UTxO>();
  for (const [name, role] of Object.entries(roles))
    references.set(
      role.spendingScriptHash,
      (
        await publishPlainReferenceScriptUtxo({
          lucid,
          script: role.spendingScript,
          label: `N6 source ${name}`,
        })
      ).utxo,
    );
  const carriage = new Map<number, UTxO[]>();
  for (const [field, fieldPlan] of plans)
    carriage.set(
      field,
      await publishDirectReceiveFieldCarriage({
        lucid,
        emulator,
        account,
        prover,
        plan: fieldPlan.openingPlan,
        certificate,
        compactCbor: material.bound.compact_cbor,
        witnessSetCompactCbor:
          material.material.proofSource.witnessSetCompactCbor.toString("hex"),
        walletAddressType,
      }),
    );
  const initialThread = (
    await lucid.utxosAt(roles.start.spendingScriptAddress)
  ).find((item) => item.assets[unit] === 1n)!;
  const { step, measurements, funding } = createDirectSourceStepper({
    lucid,
    prover,
    unit,
    initialThread,
    references,
    carriage,
    plan,
    certificatePolicyId: certificate.policyId,
  });
  await step(
    "initialize exact field zero",
    roles.start,
    roles.resolved.spendingScriptAddress,
    datum(state),
    [0],
    (indices, open) =>
      Data.to(
        { Continue: [{ ...indices, opening: open(0) }] },
        SDK.DirectSourceStartRedeemer,
      ),
  );
  for (const field of [0, 1]) {
    const selected = inputs.filter((entry) => entry.fieldIndex === field);
    const advanced = {
      ...state,
      cursor: plan(field).position("Walking", selected.length).cursor,
      complete: true,
    };
    const resolved: SDK.DirectSourceResolvedState = {
      resume_script_hash: roles.resolved.spendingScriptHash,
      resolved: {
        advanced,
        first_item_index: 0n,
        out_refs: plan(field).items.map((item) => item.toString("hex")),
      },
    };
    const resolvedDatum = resolvedStateDatum(resolved);
    const omitted = resolvedStateDatum({
      ...resolved,
      resolved: {
        ...resolved.resolved,
        out_refs: resolved.resolved.out_refs.slice(1),
      },
    });
    const reordered = resolvedStateDatum({
      ...resolved,
      resolved: {
        ...resolved.resolved,
        out_refs: [...resolved.resolved.out_refs].reverse(),
      },
    });
    const wrongCaller = resolvedStateDatum({
      ...resolved,
      resume_script_hash: roles.witnesses.spendingScriptHash,
    });
    await step(
      `field ${field} eight original outrefs`,
      roles.resolved,
      roles.authenticate.spendingScriptAddress,
      resolvedDatum,
      [field],
      (indices, open) =>
        Data.to(
          {
            Continue: [
              {
                ResolveScan: {
                  ...indices,
                  opening: open(field),
                  checkpoint: plan(field).position("Walking", 0)
                    .checkpointBytes,
                  budget: BigInt(selected.length),
                },
              },
            ],
          },
          SDK.DirectSourceResolveRedeemer,
        ),
      [omitted, reordered, wrongCaller],
    );
    const pending: SDK.DirectSourcePendingState = {
      resume_script_hash: roles.resolved.spendingScriptHash,
      pending: {
        advanced,
        candidates: selected.map((entry) => entry.source?.descriptor ?? null),
      },
    };
    const pendingDatum = pendingStateDatum(pending);
    const withoutNone = pendingStateDatum({
      ...pending,
      pending: {
        ...pending.pending,
        candidates: pending.pending.candidates.filter((item) => item !== null),
      },
    });
    const wrongRoot = pendingStateDatum({
      ...pending,
      pending: {
        ...pending.pending,
        advanced: {
          ...advanced,
          source_index: { ...advanced.source_index, root: "11".repeat(32) },
        },
      },
    });
    await step(
      `field ${field} exact ledger authentication`,
      roles.authenticate,
      roles.map.spendingScriptAddress,
      pendingDatum,
      [],
      directSourceAuthenticationRedeemer(
        selected.map((entry) => entry.witness),
      ),
      [withoutNone, wrongRoot],
      [
        directSourceAuthenticationRedeemer(
          selected.slice(1).map((entry) => entry.witness),
        ),
        directSourceAuthenticationRedeemer(
          [...selected, selected[0]!].map((entry) => entry.witness),
        ),
        directSourceAuthenticationRedeemer(
          [...selected].reverse().map((entry) => entry.witness),
        ),
      ],
    );
    state = {
      ...advanced,
      source_index:
        [...selected].reverse().find((entry) => entry.step !== null)?.step
          ?.after ?? state.source_index,
    };
    const wrongCursor = datum({
      ...state,
      cursor: { ...state.cursor, checkpoint_hash: "11".repeat(32) },
    });
    await step(
      `field ${field} exact optional admissions and actual resume`,
      roles.map,
      roles.resolved.spendingScriptAddress,
      datum(state),
      [],
      directSourceAdmissionRedeemer(
        selected.map((entry) => entry.step?.admission ?? null),
      ),
      [wrongCursor],
      [
        directSourceAdmissionRedeemer(
          selected.slice(1).map((entry) => entry.step?.admission ?? null),
        ),
        directSourceAdmissionRedeemer(
          [...selected, selected[0]!].map(
            (entry) => entry.step?.admission ?? null,
          ),
        ),
        directSourceAdmissionRedeemer(
          [...selected].reverse().map((entry) => entry.step?.admission ?? null),
        ),
      ],
      [roles.witnesses.spendingScriptAddress],
    );
    await step(
      `field ${field} forward committed exhaustion`,
      roles.resolved,
      roles.inputFinish.spendingScriptAddress,
      Data.to(
        {
          fraud_prover: prover,
          data: {
            resume_script_hash: roles.resolved.spendingScriptHash,
            state,
          },
        },
        SDK.DirectSourceResumeDatum,
      ),
      [],
      (indices) =>
        Data.to(
          { Continue: [{ Advance: indices }] },
          SDK.DirectSourceResolveRedeemer,
        ),
    );
    const nextField = field === 0 ? 1 : 6;
    state = {
      ...state,
      cursor: plan(nextField).position(
        nextField === 6 ? "Certifying" : "Walking",
        0,
      ).cursor,
      complete: false,
    };
    await step(
      `field ${field} independently reopen exhaustion and enter ${nextField}`,
      roles.inputFinish,
      nextField === 6
        ? roles.grammar.spendingScriptAddress
        : roles.resolved.spendingScriptAddress,
      datum(state),
      [field, nextField],
      (indices, open) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                opening: open(field),
                checkpoint: plan(field).position("Walking", selected.length)
                  .checkpointBytes,
                next_opening: open(nextField),
              },
            ],
          },
          SDK.DirectSourceInputFinishRedeemer,
        ),
    );
  }
  state = { ...state, cursor: plan(6).position("Walking", 0).cursor };
  await step(
    "certify exact witness envelope",
    roles.grammar,
    roles.witnesses.spendingScriptAddress,
    datum(state),
    [6],
    (indices, open) =>
      Data.to(
        {
          Continue: [
            {
              ...indices,
              opening: open(6),
              checkpoint: plan(6).position("Certifying", 0).checkpointBytes,
              budget: BigInt(witnessSteps.length),
            },
          ],
        },
        SDK.DirectSourceGrammarRedeemer,
      ),
  );
  const advanced = {
    ...state,
    cursor: plan(6).position("Walking", witnessSteps.length).cursor,
    complete: true,
  };
  const pending: SDK.DirectSourcePendingState = {
    resume_script_hash: roles.witnesses.spendingScriptHash,
    pending: {
      advanced,
      candidates: witnessSteps.map((entry) => entry.candidate.descriptor),
    },
  };
  await step(
    `${witnessSteps.length} actual witness sources`,
    roles.witnesses,
    roles.map.spendingScriptAddress,
    Data.to(
      { fraud_prover: prover, data: pending },
      SDK.DirectSourcePendingDatum,
    ),
    [6],
    (indices, open) =>
      Data.to(
        {
          Continue: [
            {
              WitnessScan: {
                ...indices,
                opening: open(6),
                checkpoint: plan(6).position("Walking", 0).checkpointBytes,
                budget: BigInt(witnessSteps.length),
              },
            },
          ],
        },
        SDK.DirectSourceWitnessRedeemer,
      ),
  );
  state = { ...advanced, source_index: evidence.index };
  await step(
    `${witnessSteps.length} witness map admissions return to witness caller`,
    roles.map,
    roles.witnesses.spendingScriptAddress,
    datum(state),
    [],
    (indices) =>
      Data.to(
        {
          Continue: [
            {
              ...indices,
              admissions: witnessSteps.map((entry) => entry.admission),
            },
          ],
        },
        SDK.DirectSourceMapRedeemer,
      ),
  );
  await step(
    "forward exact witness completion",
    roles.witnesses,
    roles.finish.spendingScriptAddress,
    datum(state),
    [],
    (indices) =>
      Data.to(
        { Continue: [{ Finish: indices }] },
        SDK.DirectSourceWitnessRedeemer,
      ),
  );
  const final = Data.to(
    {
      fraud_prover: prover,
      data: { bound: material.bound, source_index: evidence.index },
    },
    SDK.DirectSourceBoundIndexDatum,
  );
  await step(
    "independently reopen witness exhaustion",
    roles.finish,
    credentialToAddress(network, { type: "Script", hash: returnHash }),
    final,
    [6],
    (indices, open) =>
      Data.to(
        {
          Continue: [
            {
              ...indices,
              opening: open(6),
              checkpoint: plan(6).position("Walking", witnessSteps.length)
                .checkpointBytes,
            },
          ],
        },
        SDK.DirectSourceFinishRedeemer,
      ),
  );
  return {
    measurements,
    funding: await funding(account.assets.lovelace),
    index: evidence.index,
    inputCount: inputs.length,
    witnessCount: witnessSteps.length,
  };
};
