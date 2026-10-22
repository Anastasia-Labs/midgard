/** The validation-trace dispute runner at a CEK script selection, on the
 * emulator, over a deployment that installs the selection resolver and its
 * yields and names the script-decode deployment. Before Init the runner
 * records the decode of the script our trace selects, as journaled actions
 * of the workflow, and plays the successor's thread through the CEK resolver
 * to the selection resolver, whose hop to the award reads the record and
 * invokes both yields. The selection's hop needs more than the per-move
 * cap, so our wallet pays all of its fee and the thread reaches the award
 * with all its lovelace; every other move of the thread fits under the cap
 * and its thread pays it, and the awarded thread closes into our case. The
 * selection's successor yield refuses a selection whose record reference
 * names an output that only looks like the record. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Constr,
  coreToTxOutput,
  credentialToAddress,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { findScriptDecodeRecord } from "../src/script-decode/record.js";
import { validationTraceMoveWindow } from "../src/validation-trace-dispute/move-builders.js";
import {
  completeValidationTraceMove,
  VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
} from "../src/validation-trace-dispute/move-fee.js";
import {
  buildValidationTraceHop,
  type ValidationTraceHop,
  validationTraceHop,
  type ValidationTraceSelectionReads,
} from "../src/validation-trace-dispute/one-step-builders.js";
import { validationTraceOneStepHolder } from "../src/validation-trace-dispute/one-step-holders.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceSelectionPlan,
} from "../src/validation-trace-dispute/one-step-plan.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { network } from "./support/submit-init-emulator-shared.js";
import { expectWithinWalletMoveShape } from "./support/validation-trace-dispute-move-fees.js";
import {
  defendTwoThreads,
  SELECTION_STEP_FROM,
  threadClosed,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  playThreads,
  type PreparedMove,
  threadPlace,
} from "./support/validation-trace-dispute-one-step-play.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

const RECEIPT_LOVELACE = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace,
);

let dispute: TwoThreadCase;
/** The stage of each action the runner preflighted, in order. */
const order: string[] = [];
/** The successor thread's moves past the game, and its awarded close. */
let moves: PreparedMove[];
let close: PreparedMove | undefined;

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The body of a transaction the emulator accepted. */
const bodyOf = (hash: string) =>
  CML.Transaction.from_cbor_hex(dispute.scene.l1.signedCbors.get(hash)!).body();

/** The lovelace of output `outRef` of a transaction the emulator accepted. */
const lovelaceAt = (outRef: string) => {
  const [hash, index] = outRef.split("#");
  return bodyOf(hash!).outputs().get(Number(index)).amount().coin();
};

/** The index of the output of `cbor` that carries the successor's thread. */
const threadOutput = (cbor: string) => {
  const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
  for (let index = 0; index < outputs.len(); index += 1)
    if (coreToTxOutput(outputs.get(index)).assets[dispute.b.unit] === 1n)
      return index;
  return undefined;
};

/** The successor's thread where it stands. */
const placeOfB = async () => {
  const found = await threadPlace(
    dispute.scene,
    dispute.deployment,
    dispute.b.unit,
  );
  if (found === undefined) throw new Error("the thread is not live");
  return found;
};

/** Our plan of the selection, by the successor hash the holder keeps. */
const selectionPlan = async (
  utxo: UTxO,
): Promise<ValidationTraceSelectionPlan> => {
  const planning = await planValidationTraceOneStep({
    material: dispute.material,
    challengerHighHash: Data.from<string>(
      plutusConstrFieldCbor(utxo.datum!, [1, 0, 1, 3]),
    ),
    decodeRecords: true,
  });
  if (planning.kind !== "plan" || planning.plan.kind !== "selection")
    throw new Error("the thread's step is not a CEK script selection");
  return planning.plan;
};

/** What the selection's hop reads on the deployment. */
const selectionReads = async (
  plan: ValidationTraceSelectionPlan,
): Promise<ValidationTraceSelectionReads> => {
  const { scene, deployment, material } = dispute;
  const references = deployment.references.oneStep!.cekSelection!;
  const scriptDecode = deployment.scriptDecode!;
  return {
    trace: await material.steps(plan.stateIndex),
    yields: {
      references: [references.authenticate, references.successor],
      scripts: [
        scene.chain.yields.cekSelectionAuthenticate.withdrawalScript,
        scene.chain.yields.cekSelectionSuccessor.withdrawalScript,
      ],
    },
    record: await findScriptDecodeRecord({
      lucid: scene.h.proverLucid,
      deployment: scriptDecode,
      scriptHashHex: plan.script!.scriptHashHex,
    }),
    scriptDecodePolicyId: scriptDecode.validator.policyId,
  };
};

describe("validation-trace dispute runner: driving a thread at a CEK script selection", () => {
  beforeAll(async () => {
    const recordOrder =
      () =>
      (
        adapter: FraudProofFamilyWorkflowAdapter,
      ): FraudProofFamilyWorkflowAdapter => ({
        ...adapter,
        preflight: async (context) => {
          const preflight = await adapter.preflight(context);
          order.push(String(context.action.input.stage));
          return preflight;
        },
      });
    dispute = await defendTwoThreads({
      withRemoval: false,
      successorFrom: SELECTION_STEP_FROM,
      selections: true,
      wrap: recordOrder,
    });
    const { scene, runner, a, b } = dispute;
    await playThreads({
      scene,
      runner,
      sides: [a, b],
      until: async () => (await placeOfB()).holder === "selection",
      limit: 80,
    });
  }, 1_800_000);

  it("records the decode of the script our trace selects before Init", async () => {
    const { scene, deployment, material } = dispute;
    const finish = order.indexOf("script_decode_finish");
    expect(order[0]).toBe("script_decode_stage");
    expect(finish).toBeGreaterThan(0);
    expect(order.indexOf("init")).toBe(finish + 1);
    const plan = await selectionPlan((await placeOfB()).utxo);
    expect(plan.stateIndex).toBe(SELECTION_STEP_FROM);
    const record = await findScriptDecodeRecord({
      lucid: scene.h.proverLucid,
      deployment: deployment.scriptDecode!,
      scriptHashHex: plan.script!.scriptHashHex,
    });
    expect(record).toBeDefined();
    expect("Decoded" in Data.from(record!.datum!, SDK.ScriptDecodeRecord)).toBe(
      true,
    );
    // The script is the one the disputed transaction carries for it.
    expect(plan.script!.item).toStrictEqual(
      material.scriptItem(
        (await material.steps(SELECTION_STEP_FROM)).witnesses[
          SELECTION_STEP_FROM
        ]!.auxiliary as Parameters<typeof material.scriptItem>[0],
      ),
    );
  }, 300_000);

  it("builds the selection's hop to the award, which reads the record and both yields, withdraws zero from each, and passes every script it runs", async () => {
    const { scene, deployment, b } = dispute;
    const before = await placeOfB();
    const plan = await selectionPlan(before.utxo);
    const lucid = scene.h.proverLucid;
    const prover = scene.h.proverSigner.paymentKeyHash;
    const reads = await selectionReads(plan);
    const reference = validationTraceOneStepHolder(
      deployment,
      "selection",
    ).reference!;
    // Each script is evaluated as the ledger would: a refusal throws here.
    const built = await buildValidationTraceHop({
      lucid,
      thread: before.utxo,
      unit: b.unit,
      reference,
      hop: validationTraceHop({
        plan,
        holder: "selection",
        utxo: before.utxo,
        prover,
        selection: reads,
      }),
      address: scene.chain.award.spendingScriptAddress,
      prover,
      fee: 0n,
      ...validationTraceMoveWindow(lucid),
    }).complete({ localUPLCEval: true });
    const body = CML.Transaction.from_cbor_hex(built.toCBOR()).body();
    const read = new Set<string>();
    const references = body.reference_inputs()!;
    for (let index = 0; index < references.len(); index += 1) {
      const input = references.get(index);
      read.add(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
    }
    expect(read).toStrictEqual(
      new Set(
        [reference, ...reads.yields.references, reads.record!].map(outRefOf),
      ),
    );
    const withdrawals = body.withdrawals()!;
    const accounts = withdrawals.keys();
    expect(accounts.len()).toBe(2);
    for (let index = 0; index < accounts.len(); index += 1)
      expect(withdrawals.get(accounts.get(index))).toBe(0n);
    // The thread moves to the award.
    const award = body.outputs().get(0);
    expect(coreToTxOutput(award).assets[b.unit]).toBe(1n);
    expect(coreToTxOutput(award).address).toBe(
      scene.chain.award.spendingScriptAddress,
    );
  }, 300_000);

  it("refuses a selection whose decode record reference names a look-alike of the record without its token, at the successor yield", async () => {
    const { scene, deployment, b } = dispute;
    const before = await placeOfB();
    const plan = await selectionPlan(before.utxo);
    const lucid = scene.h.proverLucid;
    const prover = scene.h.proverSigner.paymentKeyHash;
    const reads = await selectionReads(plan);
    const record = reads.record!;
    // A real output at the record's address carrying the record's datum and
    // ADA, but not its token.
    const published = await (
      await lucid
        .newTx()
        .pay.ToContract(
          deployment.scriptDecode!.validator.spendingScriptAddress,
          { kind: "inline", value: record.datum! },
          { lovelace: record.assets.lovelace! },
        )
        .complete()
    ).sign
      .withWallet()
      .complete();
    const lookAlikeHash = await published.submit();
    await lucid.awaitTx(lookAlikeHash);
    const [lookAlike] = await lucid.utxosByOutRef([
      { txHash: lookAlikeHash, outputIndex: 0 },
    ]);
    expect(lookAlike?.datum).toBe(record.datum);
    const honest = validationTraceHop({
      plan,
      holder: "selection",
      utxo: before.utxo,
      prover,
      selection: reads,
    });
    // The honest hop, which also reads the look-alike, but for the record's
    // index, which names the look-alike.
    const hop: ValidationTraceHop = {
      ...honest,
      reads: {
        ...honest.reads!,
        references: [...honest.reads!.references, lookAlike!],
      },
      redeemer: (inputIndex, outputIndex, context) => {
        const redeemer = Data.from(
          honest.redeemer(inputIndex, outputIndex, context),
          SDK.ValidationCekExecutionSelectionSpendRedeemer,
        );
        if (!("Continue" in redeemer)) throw new Error("not a Continue");
        const [action] = redeemer.Continue;
        return Data.to(
          {
            Continue: [
              {
                ...action,
                record_reference_input_index: SDK.requireReferenceInputIndex(
                  context,
                  lookAlike!,
                  "look-alike record",
                ),
              },
            ],
          },
          SDK.ValidationCekExecutionSelectionSpendRedeemer,
        );
      },
    };
    const reference = validationTraceOneStepHolder(
      deployment,
      "selection",
    ).reference!;
    const window = validationTraceMoveWindow(lucid);
    await expectOnchainRefusal(
      async () =>
        await completeValidationTraceMove({
          lucid,
          stage: "resolve",
          referenced: [reference, ...hop.reads!.references],
          build: (fee) =>
            buildValidationTraceHop({
              lucid,
              thread: before.utxo,
              unit: b.unit,
              reference,
              hop,
              address: scene.chain.award.spendingScriptAddress,
              prover,
              fee,
              ...window,
            }),
        }),
      {
        refusedBy:
          "fraud_proofs/validation_trace/cek_execution_selection_yields",
        check:
          /^expect assets\.quantity_of\( value, script_decode_policy_id, record_script_hash_v1\(record\), \) == 1$/u,
      },
    );
    expect(outRefOf((await placeOfB()).utxo)).toBe(outRefOf(before.utxo));
  }, 600_000);
  it("moves the selection's thread to the award with our wallet paying all of the hop's fee, over the per-move cap, and closes it", async () => {
    const { scene, deployment, runner, prepared, a, b } = dispute;
    const atSelection = await placeOfB();
    const plan = await selectionPlan(atSelection.utxo);
    await playThreads({
      scene,
      runner,
      sides: [a, b],
      until: async () =>
        (await threadClosed(dispute, a.unit)) &&
        (await threadClosed(dispute, b.unit)),
      limit: 40,
    });
    moves = prepared.filter(
      ({ stage, cbor }) =>
        (stage === "enter_resolution" || stage === "resolve") &&
        threadOutput(cbor) !== undefined,
    );
    expect(moves.map(({ stage }) => stage)).toStrictEqual([
      "enter_resolution",
      "resolve",
      "resolve",
      "resolve",
    ]);
    const hop = moves[3]!;
    expect(hop.consumes).toBe(outRefOf(atSelection.utxo));
    close = prepared.find(
      ({ stage, consumes }) =>
        stage === "close_awarded" &&
        consumes === `${hop.hash}#${threadOutput(hop.cbor)!.toString()}`,
    );
    expect(close).toBeDefined();
    const body = bodyOf(hop.hash);
    // Our wallet's inputs pay the whole fee; the thread keeps its lovelace.
    expect(body.fee()).toBeGreaterThan(VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE);
    expect(body.inputs().len()).toBeGreaterThan(1);
    const award = body.outputs().get(threadOutput(hop.cbor)!);
    expect(coreToTxOutput(award).address).toBe(
      scene.chain.award.spendingScriptAddress,
    );
    expect(award.amount().coin()).toBe(atSelection.utxo.assets.lovelace);
    const read = new Set<string>();
    const references = body.reference_inputs()!;
    for (let index = 0; index < references.len(); index += 1) {
      const input = references.get(index);
      read.add(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
    }
    const record = await findScriptDecodeRecord({
      lucid: scene.h.proverLucid,
      deployment: deployment.scriptDecode!,
      scriptHashHex: plan.script!.scriptHashHex,
    });
    const selection = deployment.references.oneStep!.cekSelection!;
    for (const utxo of [
      record!,
      selection.semantic,
      selection.authenticate,
      selection.successor,
    ])
      expect(read.has(outRefOf(utxo))).toBe(true);
    // The reserve a VerifySource keeps covers this hop, as every other move.
    await expectWithinWalletMoveShape(
      scene,
      prepared
        .map(({ hash }) => hash)
        .filter((hash) => scene.l1.signedCbors.has(hash)),
    );
    for (const [index, label] of [
      "EnterResolution",
      "PrepareResolution",
      "PrepareSelected selection",
      "VerifyExecutionSelection",
    ].entries())
      reportFit(label, [moves[index]!.cbor]);
    reportFit("CloseAwardedThread", [close!.cbor]);
  }, 1_200_000);

  it("pays each other move of the thread, whose fee fits under the cap, from the thread itself", () => {
    for (const move of [...moves.slice(0, 3), close!]) {
      const body = bodyOf(move.hash);
      const fee = body.fee();
      expect(fee).toBeLessThanOrEqual(VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE);
      const thread = lovelaceAt(move.consumes!);
      if (move === close) {
        // The thread and the case are its only inputs; the rest of the
        // thread, less the receipt and the fee, is ours.
        expect(body.inputs().len()).toBe(2);
        const [hash, index] = move.consumes!.split("#");
        const tag = Data.to(new Constr(0, [hash!, BigInt(index!)]));
        const outputs = body.outputs();
        const rest = Array.from({ length: outputs.len() }, (_, at) =>
          coreToTxOutput(outputs.get(at)),
        ).filter(({ datum }) => datum === tag);
        expect(
          rest.map(({ address, assets }) => [address, assets]),
        ).toStrictEqual([
          [
            credentialToAddress(network, {
              type: "Key",
              hash: dispute.scene.h.proverSigner.paymentKeyHash,
            }),
            { lovelace: thread - RECEIPT_LOVELACE - fee },
          ],
        ]);
        continue;
      }
      expect(body.inputs().len()).toBe(1);
      expect(body.outputs().get(threadOutput(move.cbor)!).amount().coin()).toBe(
        thread - fee,
      );
    }
  });
});
