import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  encodeMidgardCekBlobChunk,
  hashMidgardCekBlobChunk,
  type MidgardCekProgramMaterialEntry,
  midgardCekProgramMaterialKindTag,
} from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardCekDataPairNode,
  hashMidgardCekDataPairNode,
} from "@al-ft/midgard-core/cek-semantic";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applyParamsToScript,
  type BuildTxWithRedeemer,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  validatorToAddress,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";

const measurements: VanRossemFitMeasurement[] = [];
let completed = 0;
const blueprintBytes = readFileSync(realBlueprintPath);
const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);
const ctPolicy = "a1".repeat(28);
const authPolicy = "b2".repeat(28);
const threadUnit = ctPolicy + "c3".repeat(32);
const appliedScript = (title: string, params: string[]): Script => {
  const validator = blueprint.validators.find((entry) => entry.title === title);
  if (validator === undefined) throw new Error(`Missing validator ${title}`);
  return {
    type: "PlutusV3",
    script: applyParamsToScript(validator.compiledCode, params),
  };
};
const spendingScript = appliedScript(
  "fraud_proofs/validation_trace/cek_material_traversal_v1.main.spend",
  ["d4".repeat(28), ctPolicy, authPolicy],
);
const stage = {
  spendingScript,
  spendingScriptAddress: validatorToAddress("Custom", spendingScript),
  spendingScriptHash: validatorToScriptHash(spendingScript),
};
const observe = (name: string, cbor: string) => {
  const m = measureCompleteSignedTransaction(cbor);
  measurements.push({
    name,
    maximumShape: name.split("/")[0]!,
    kind:
      m.executionMemory === 0n && m.executionSteps === 0n
        ? "publication"
        : "lifecycle",
    signedBytes: m.completeSignedBytes,
    memoryUnits: m.executionMemory,
    cpuUnits: m.executionSteps,
  });
};

afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(completed).toBe(2);
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-cek-material-task-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "validationTraceDispute/CEK material tasks",
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: JSON.parse(blueprintBytes.toString()).preamble.compiler
        .version,
      measurements,
    }),
  );
});

const maximumEntry = (
  kind: "program" | "data",
): MidgardCekProgramMaterialEntry => {
  if (kind === "program") {
    const bytes = Buffer.alloc(4095, 0x7a);
    return {
      kind: "blobChunk",
      root: hashMidgardCekBlobChunk(bytes),
      preimage: encodeMidgardCekBlobChunk(bytes),
    };
  }
  const node = {
    key: Buffer.alloc(32, 1),
    keyCborLength: 16_384n,
    keyMemory: 1n << 32n,
    value: Buffer.alloc(32, 2),
    valueCborLength: 16_384n,
    valueMemory: 1n << 32n,
    tail: Buffer.alloc(32, 3),
    length: 1_597_819n,
    payloadCborLength: 67_108_417n,
    memory: 1n << 48n,
  };
  return {
    kind: "dataPair",
    root: hashMidgardCekDataPairNode(node),
    preimage: encodeMidgardCekDataPairNode(node),
  };
};

describe("CEK material traversal published maximum task", () => {
  it.each(["program", "data"] as const)(
    "fits maximum %s task with a 64-step visited proof and refuses mutations",
    async (kind) => {
      const entry = maximumEntry(kind);
      const root = Buffer.from(entry.root).toString("hex");
      const task: SDK.CekMaterialTask = {
        kind: kind === "program" ? 3n : 7n,
        root,
        expected_length: kind === "data" ? 1_597_819n : -1n,
      };
      const proofJson = Array.from({ length: 64 }, (_, i) => ({
        type: "branch",
        skip: 0,
        neighbors: Buffer.concat(
          Array.from({ length: 4 }, (_, j) =>
            createHash("sha256")
              .update(Buffer.from([i, j]))
              .digest(),
          ),
        ).toString("hex"),
      }));
      const proof = MpfProof.fromJSON(
        Buffer.from(entry.root),
        Buffer.from([1]),
        proofJson,
      );
      const preRootBytes = proof.verify(false);
      const postRootBytes = proof.verify(true);
      if (preRootBytes === null || postRootBytes === null)
        throw new Error("Invalid synthetic proof");
      const preRoot = preRootBytes.toString("hex");
      const postRoot = postRootBytes.toString("hex");
      const visitedProof = Data.from(
        Buffer.from(proof.toCBOR()).toString("hex"),
        SDK.Proof,
      );
      const tail = "e5".repeat(32);
      const nextPending = SDK.pushCekMaterialTasks(
        SDK.cekMaterialChildren(entry),
        tail,
      );
      const before: SDK.CekMaterialTraversalState = {
        pending_root: SDK.pushCekMaterialTask(task, tail),
        visited_root: preRoot,
        node_count: 1_597_700n,
        byte_length: 67_090_000n,
        expected_node_count: 1_597_819n,
        expected_byte_length: 67_108_417n,
      };
      const after = {
        ...before,
        pending_root: nextPending,
        visited_root: postRoot,
        node_count: before.node_count + 1n,
        byte_length: before.byte_length + BigInt(entry.preimage.length),
      };
      const role =
        SDK.CEK_MATERIAL_TASK_YIELD_ROLES[kind === "program" ? 0 : 1];
      const roleUnit = SDK.referenceScriptAuthUnit(authPolicy, role.role);
      const yieldContract = {
        withdrawalScript: appliedScript(
          `fraud_proofs/validation_trace/cek_material_traversal_yields.${kind}.withdraw`,
          [stage.spendingScriptHash],
        ),
      };
      const wallet = generateEmulatorAccount({
        lovelace: 4_000_000_000n,
        [roleUnit]: 1n,
      });
      const owner = getAddressDetails(wallet.address).paymentCredential!.hash;
      const datum = Data.to(
        { fraud_prover: owner, data: before },
        SDK.CekMaterialTraversalDatum,
      );
      const nextDatum = Data.to(
        { fraud_prover: owner, data: after },
        SDK.CekMaterialTraversalDatum,
      );
      // Standalone authenticated-continuation envelope measurement; complete chain provenance is covered by the companion lifecycle suite.
      const emulator = new Emulator(
        [
          wallet,
          {
            ...wallet,
            address: stage.spendingScriptAddress,
            assets: { lovelace: 30_000_000n, [threadUnit]: 1n },
            outputData: { inline: datum },
          },
        ],
        PROTOCOL_PARAMETERS_DEFAULT,
      );
      const lucid = await Lucid(emulator, "Custom");
      lucid.selectWallet.fromSeed(wallet.seedPhrase);
      const publish = async (
        script: typeof stage.spendingScript,
        roleAsset?: string,
      ) => {
        const unsigned = await lucid
          .newTx()
          .pay.ToAddressWithData(
            stage.spendingScriptAddress,
            undefined,
            {
              lovelace: 50_000_000n,
              ...(roleAsset === undefined ? {} : { [roleAsset]: 1n }),
            },
            script,
          )
          .complete({ localUPLCEval: true });
        const signed = await unsigned.sign.withWallet().complete();
        observe(
          `${kind}/${roleAsset === undefined ? "spend" : "yield"}-publication`,
          signed.toCBOR(),
        );
        const hash = await signed.submit();
        await lucid.awaitTx(hash);
        return (
          await lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
        )[0]!;
      };
      const reference = await publish(stage.spendingScript);
      const taskReference = await publish(
        yieldContract.withdrawalScript,
        roleUnit,
      );
      const rewardAddress = validatorToRewardAddress(
        "Custom",
        yieldContract.withdrawalScript,
      );
      const registration = await lucid
        .newTx()
        .register.Stake(rewardAddress)
        .complete();
      const registrationSigned = await registration.sign
        .withWallet()
        .complete();
      await lucid.awaitTx(await registrationSigned.submit());
      const input = (await lucid.utxosAt(stage.spendingScriptAddress)).find(
        (utxo) => utxo.assets[threadUnit] === 1n,
      )!;
      const build = async (
        mutation?: "omit-yield" | "stack" | "visited" | "preimage",
      ) => {
        const redeemer = ((ctx) =>
          Data.to(
            {
              Continue: [
                {
                  input_index: SDK.requireInputIndex(
                    ctx,
                    input,
                    "maximum CEK task",
                  ),
                  output_index: SDK.requireUniqueOutputIndex(
                    ctx.outputs,
                    computationThreadOutputPredicate({
                      address: stage.spendingScriptAddress,
                      datum: nextDatum,
                      unit: threadUnit,
                    }),
                    "maximum CEK task",
                  ),
                  task,
                  tail_root: tail,
                  entry: {
                    kind: midgardCekProgramMaterialKindTag(entry.kind),
                    root,
                    preimage:
                      mutation === "preimage"
                        ? "00"
                        : entry.preimage.toString("hex"),
                  },
                  already_seen: mutation === "visited",
                  visited_proof: visitedProof,
                  yield_reference_input_index: SDK.requireReferenceInputIndex(
                    ctx,
                    taskReference,
                    role.role,
                  ),
                  next_pending_root:
                    mutation === "stack"
                      ? SDK.CEK_MATERIAL_EMPTY_STACK
                      : nextPending,
                },
              ],
            },
            SDK.CekMaterialTraversalRedeemer,
          )) satisfies BuildTxWithRedeemer;
        let tx = lucid
          .newTx()
          .collectFrom([input], redeemer)
          .readFrom([reference, taskReference])
          .pay.ToContract(
            stage.spendingScriptAddress,
            { kind: "inline", value: nextDatum },
            input.assets,
          )
          .addSignerKey(owner);
        if (mutation !== "omit-yield")
          tx = tx.withdraw(rewardAddress, 0n, Data.void());
        return await tx.complete({ localUPLCEval: true });
      };
      for (const mutation of [
        "omit-yield",
        "stack",
        "visited",
        "preimage",
      ] as const)
        await expect(build(mutation)).rejects.toThrow();
      const signed = await (await build()).sign.withWallet().complete();
      observe(`${kind}/maximum-task`, signed.toCBOR());
      await lucid.awaitTx(await signed.submit());
      completed++;
    },
    900_000,
  );
});
