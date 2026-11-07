import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { computeHash28, computeHash32 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { createDirectFieldPlan } from "../../src/direct-script-derivation/index.js";
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

/** Maximum proof envelopes with opaque sibling hashes and an explicit seeded
 * model cursor/count. These are not full materialized honest 64-level tries,
 * nor a complete Init/family/SQ lifecycle. The ordinary source scenario uses a
 * genuine16-entry trie separately. */
export const runDirectSourceMaximumProof = async (
  kind: "64Branch" | "63LongFork",
  consumer: "authenticate" | "map",
) => {
  const { material, inputs } = await directSourceBatchFixture();
  const entry = inputs.find((item) => item.source !== null)!;
  if (entry.source === null) throw new Error("N6 maximum proof source absent");
  const candidate = entry.source.descriptor;
  // Both the Aiken ledger_outref_key and the real source producer use the
  // exact encoded original outref bytes already present in source_key.
  const keyBytes = Buffer.from(
    consumer === "authenticate" ? candidate.source_key : candidate.script_hash,
    "hex",
  );
  const value =
    consumer === "authenticate"
      ? Buffer.from(entry.witness.descriptor_cbor, "hex")
      : Buffer.from(Data.to(candidate, SDK.DirectScriptSource), "hex");
  const path = computeHash32(keyBytes).toString("hex");
  const steps = Array.from({ length: 64 }, (_, index) =>
    kind === "64Branch" || index === 63
      ? { type: "branch", skip: 0, neighbors: "11".repeat(128) }
      : {
          type: "fork",
          skip: 0,
          neighbor: {
            nibble: (parseInt(path[index]!, 16) + 1) % 16,
            prefix: "0f".repeat(62 - index),
            root: "11".repeat(32),
          },
        },
  );
  const proof = MpfProof.fromJSON(keyBytes, value, steps);
  const before = proof.verify(false),
    after = proof.verify(true);
  if (before === null || after === null)
    throw new Error("N6 maximum proof envelope invalid");
  const proofData = Data.from(proof.toCBOR().toString("hex"), SDK.Proof);
  expect(proofData).toHaveLength(64);
  const blueprint = readBlueprint(realBlueprintPath),
    policy = "cc".repeat(28),
    cert = "77".repeat(28),
    returnHash = "ee".repeat(28);
  const roles = await Effect.runPromise(
    SDK.buildDirectSourceContracts({
      blueprint,
      network,
      computationThreadPolicyId: policy,
      fieldPreimageCertificatePolicyId: cert,
      nextStepScriptHash: returnHash,
    }),
  );
  const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = getAddressDetails(account.address).paymentCredential!.hash;
  const plan = createDirectFieldPlan(material, entry.fieldIndex, prover);
  const unit =
    policy +
    "0000002b" +
    computeHash28(SDK.encodeHeaderCbor(material.sourceBound.header)).toString(
      "hex",
    );
  const state: SDK.DirectSourceScanState = {
    cursor: plan.position("Walking", entry.itemIndex + 1).cursor,
    source_index:
      consumer === "map"
        ? { root: before.toString("hex"), count: 129n }
        : { root: SDK.EMPTY_MERKLE_TREE_ROOT, count: 0n },
    complete: false,
  };
  if (consumer === "authenticate")
    state.cursor.bound.event_pre_root = after.toString("hex");
  const resolved: SDK.DirectSourceResolvedState = {
    resume_script_hash: roles.resolved.spendingScriptHash,
    resolved: {
      advanced: state,
      first_item_index: BigInt(entry.itemIndex),
      out_refs: [plan.items[entry.itemIndex]!.toString("hex")],
    },
  };
  const pending: SDK.DirectSourcePendingState = {
    resume_script_hash: roles.resolved.spendingScriptHash,
    pending: { advanced: state, candidates: [candidate] },
  };
  const initialDatum =
    consumer === "authenticate"
      ? Data.to(
          { fraud_prover: prover, data: resolved },
          SDK.DirectSourceResolvedDatum,
        )
      : Data.to(
          { fraud_prover: prover, data: pending },
          SDK.DirectSourcePendingDatum,
        );
  const initialRole =
    consumer === "authenticate" ? roles.authenticate : roles.map;
  const emulator = new Emulator(
    [
      account,
      {
        ...account,
        address: initialRole.spendingScriptAddress,
        assets: { lovelace: 30_000_000n, [unit]: 1n },
        outputData: { inline: initialDatum },
      },
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const references = new Map<string, UTxO>();
  for (const role of [initialRole, roles.map])
    if (!references.has(role.spendingScriptHash))
      references.set(
        role.spendingScriptHash,
        (
          await publishPlainReferenceScriptUtxo({
            lucid,
            script: role.spendingScript,
            label: `N6 maximum ${consumer} ${kind}`,
          })
        ).utxo,
      );
  const initialThread = (
    await lucid.utxosAt(initialRole.spendingScriptAddress)
  ).find((item) => item.assets[unit] === 1n)!;
  const { step, measurements } = createDirectSourceStepper({
    lucid,
    prover,
    unit,
    initialThread,
    references,
    carriage: new Map(),
    plan: () => plan,
    certificatePolicyId: cert,
  });
  if (consumer === "authenticate") {
    const witness = { ...entry.witness, proof: proofData };
    await step(
      `actual ${kind} ledger authentication`,
      roles.authenticate,
      roles.map.spendingScriptAddress,
      Data.to(
        { fraud_prover: prover, data: pending },
        SDK.DirectSourcePendingDatum,
      ),
      [],
      directSourceAuthenticationRedeemer([witness]),
      [],
      [
        directSourceAuthenticationRedeemer([]),
        directSourceAuthenticationRedeemer([
          { ...witness, descriptor_cbor: "00" },
        ]),
      ],
    );
  } else {
    const advanced = {
      ...state,
      source_index: { root: after.toString("hex"), count: 130n },
    };
    await step(
      `actual ${kind} source map insertion`,
      roles.map,
      roles.resolved.spendingScriptAddress,
      Data.to(
        { fraud_prover: prover, data: advanced },
        SDK.DirectSourceScanDatum,
      ),
      [],
      directSourceAdmissionRedeemer([{ First: { proof: proofData } }]),
      [],
      [
        directSourceAdmissionRedeemer([]),
        directSourceAdmissionRedeemer([{ First: { proof: [] } }]),
      ],
    );
  }
  return {
    kind,
    consumer,
    proofSteps: proofData.length,
    opaqueSiblingModel: true,
    measurements,
  };
};
