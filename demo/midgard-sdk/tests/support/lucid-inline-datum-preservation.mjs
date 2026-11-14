import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { pathToFileURL } from "node:url";

const require = createRequire(import.meta.url);
const [format, mode] = process.argv.slice(2);
assert(["esm", "cjs"].includes(format));
assert(["static", "canonical", "delayed"].includes(mode));
const lucidRequirePath = require.resolve("@lucid-evolution/lucid");
const lucidPath = join(dirname(lucidRequirePath), "index.js");
const lucidRequire = createRequire(lucidPath);
const uplcPath = lucidRequire.resolve("@lucid-evolution/uplc");
const uplc = lucidRequire(uplcPath);
const descriptor = Object.getOwnPropertyDescriptor(uplc, "eval_phase_two_raw");
assert(descriptor?.writable && descriptor.configurable);
const original = descriptor.value;
const requests = [];
Object.defineProperty(uplc, "eval_phase_two_raw", {
  ...descriptor,
  value(...args) {
    const result = Reflect.apply(original, this, args);
    requests.push({
      tx: Buffer.from(args[0]).toString("hex"),
      redeemers: result.map((bytes) => Buffer.from(bytes).toString("hex")),
    });
    return result;
  },
});
let report;
try {
  const namespace = await import(pathToFileURL(uplcPath).href);
  assert.equal(namespace.eval_phase_two_raw, uplc.eval_phase_two_raw);
  const {
    CML,
    Emulator,
    Lucid,
    PROTOCOL_PARAMETERS_DEFAULT,
    applyDoubleCborEncoding,
    coreToTxOutput,
    credentialToAddress,
    validatorToAddress,
  } =
    format === "cjs"
      ? require(lucidRequirePath)
      : await import(pathToFileURL(lucidPath).href);
  const {
    aikenSerialisedPlutusDataCborPreservingMapOrder: orderedData,
  } = require("@al-ft/midgard-core/plutus-data-cbor");
  // alpha precedes beta by token bytes, whereas CBOR key-length sorting reverses
  // them. Construct the expected Data directly: typed Data.to must not normalize
  // either side of this regression into the same reordered map.
  const tokenMap = "a245616c70686105446265746107";
  const value = `a240a1401a00b71b00581c${"11".repeat(28)}${tokenMap}`;
  const datum = orderedData(`d8799f${value}9f${tokenMap}ffff`);
  const redeemerData = orderedData(`d87a9f${tokenMap}ff`);
  assert.notEqual(
    orderedData(CML.PlutusData.from_cbor_hex(datum).to_canonical_cbor_hex()),
    datum,
    "fixture must distinguish Plutus map order from canonical CBOR key order",
  );
  const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
  const address = credentialToAddress("Custom", {
    type: "Key",
    hash: key.to_public().hash().to_hex(),
  });
  const emulator = new Emulator([
    { address, assets: { lovelace: 1_000_000_000n } },
  ]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromPrivateKey(key.to_bech32());
  const blueprint = JSON.parse(
    readFileSync(
      new URL(
        "../../../midgard-node/blueprints/always-succeeds/plutus.json",
        import.meta.url,
      ),
      "utf8",
    ),
  );
  const script = {
    type: "PlutusV3",
    script: applyDoubleCborEncoding(blueprint.validators[0].compiledCode),
  };
  const scriptAddress = validatorToAddress("Custom", script);
  const setup = await lucid
    .newTx()
    .pay.ToContract(
      scriptAddress,
      { kind: "inline", value: datum },
      { lovelace: 20_000_000n },
    )
    .pay.ToContract(
      scriptAddress,
      { kind: "asHash", value: datum },
      { lovelace: 20_000_000n },
    )
    .pay.ToAddressWithData(
      address,
      { kind: "inline", value: datum },
      { lovelace: 2_000_000n },
    )
    .pay.ToAddress(address, { lovelace: 5_000_000n })
    .complete({ localUPLCEval: true });
  await lucid.awaitTx(
    await (await setup.sign.withWallet().complete()).submit(),
  );
  const scriptInputs = await lucid.utxosAt(scriptAddress);
  const inlineInput = scriptInputs.find((input) => input.datum != null);
  const hashInput = scriptInputs.find((input) => input.datumHash != null);
  const reference = (await lucid.utxosAt(address)).find(
    (input) => input.datum != null,
  );
  assert(inlineInput && hashInput && reference);
  assert.equal(orderedData(inlineInput.datum), datum);
  assert.equal(orderedData(reference.datum), datum);
  assert.equal(
    hashInput.datumHash,
    CML.hash_plutus_data(CML.PlutusData.from_cbor_hex(datum)).to_hex(),
  );
  const callbacks = [];
  const callback = (input) => (context) => {
    const index = context.inputIndex(input);
    assert.notEqual(index, undefined);
    assert.equal(context.inputs[Number(index)].txHash, input.txHash);
    assert.equal(context.inputs[Number(index)].outputIndex, input.outputIndex);
    const outputIndex = context.outputIndex((output) => output.datum != null);
    assert.notEqual(outputIndex, undefined);
    assert.equal(
      orderedData(context.outputs[Number(outputIndex)].datum),
      datum,
    );
    assert.equal(
      orderedData(
        context.referenceInputs.find(
          (output) =>
            output.txHash === reference.txHash &&
            output.outputIndex === reference.outputIndex,
        ).datum,
      ),
      datum,
    );
    callbacks.push({
      inputIndex: String(index),
      outputIndex: String(outputIndex),
    });
    return redeemerData;
  };
  const firstRequest = requests.length;
  const completed = await lucid
    .newTx()
    .collectFrom(
      [inlineInput],
      mode === "delayed" ? callback(inlineInput) : redeemerData,
    )
    .collectFrom(
      [{ ...hashInput, datum }],
      mode === "delayed" ? callback(hashInput) : redeemerData,
    )
    .readFrom([reference])
    .attach.SpendingValidator(script)
    .attachMetadata(674, { msg: ["ordered Plutus data preservation"] })
    // A deliberately sub-minimum amount exercises the builder's real top-up.
    .pay.ToAddressWithData(
      address,
      { kind: "inline", value: datum },
      { lovelace: 1n },
    )
    .complete({
      localUPLCEval: true,
      ...(mode === "canonical" ? { canonical: true } : {}),
    });
  const completedTx = completed.toTransaction();
  const signed = await completed.sign.withWallet().complete();
  const signedTx = CML.Transaction.from_cbor_hex(signed.toCBOR());
  const bodyView = (tx) => {
    const body = JSON.parse(tx.body().to_json());
    // The evaluator calculates units before completion refreshes this hash.
    delete body.script_data_hash;
    return body;
  };
  assert.deepEqual(bodyView(completedTx), bodyView(signedTx));
  const evaluations = requests.slice(firstRequest);
  const finalEvaluation = evaluations
    .filter(
      (request) =>
        JSON.stringify(bodyView(CML.Transaction.from_cbor_hex(request.tx))) ===
        JSON.stringify(bodyView(signedTx)),
    )
    .at(-1);
  assert(
    finalEvaluation,
    "the final complete body must undergo real local evaluation",
  );
  const assertData = (tx, evaluated = false) => {
    const outputs = tx.body().outputs();
    const inline = Array.from({ length: outputs.len() }, (_, i) => ({
      index: i,
      raw: outputs.get(i),
      output: coreToTxOutput(outputs.get(i)),
    })).filter(({ output }) => output.datum != null);
    assert.equal(inline.length, 1);
    assert.equal(orderedData(inline[0].output.datum), datum);
    const witness = tx.witness_set();
    const datums = witness.plutus_datums();
    assert.equal(datums?.len(), 1);
    assert.equal(orderedData(datums.get(0).to_cbor_hex()), datum);
    const redeemers = witness.redeemers().to_flat_format();
    assert.equal(redeemers.len(), 2);
    for (let i = 0; i < redeemers.len(); i++) {
      assert.equal(
        orderedData(redeemers.get(i).data().to_cbor_hex()),
        redeemerData,
      );
    }
    if (!evaluated) {
      assert.deepEqual(
        Array.from({ length: redeemers.len() }, (_, i) =>
          JSON.parse(redeemers.get(i).to_json()),
        ),
        finalEvaluation.redeemers.map((hex) =>
          JSON.parse(CML.LegacyRedeemer.from_cbor_hex(hex).to_json()),
        ),
      );
    }
    return { inline: inline[0], redeemers };
  };
  assertData(completedTx);
  assertData(CML.Transaction.from_cbor_hex(finalEvaluation.tx), true);
  const { inline, redeemers } = assertData(signedTx);
  const canonicalCompletedTx = CML.Transaction.from_cbor_hex(
    completed.toCBOR({ canonical: true }),
  );
  const canonicalSignedTx = CML.Transaction.from_cbor_hex(
    signed.toCBOR({ canonical: true }),
  );
  for (const tx of [
    completedTx,
    signedTx,
    canonicalCompletedTx,
    canonicalSignedTx,
  ]) {
    assertData(tx);
    const auxiliary = tx.auxiliary_data();
    assert(auxiliary, "metadata must survive the public serializer");
    assert.deepEqual(
      JSON.parse(auxiliary.to_json()),
      JSON.parse(completedTx.auxiliary_data().to_json()),
    );
    assert.equal(
      CML.hash_auxiliary_data(auxiliary).to_hex(),
      tx.body().auxiliary_data_hash().to_hex(),
    );
  }
  assert.deepEqual(bodyView(canonicalCompletedTx), bodyView(canonicalSignedTx));
  const canonicalSubmission = mode !== "static";
  // Explicit canonical serialization can change a default static body's ledger
  // encoding. Already canonical and delayed completion must be hash-stable, so
  // their existing signatures remain valid through submit({canonical:true}).
  if (canonicalSubmission) {
    for (const tx of [canonicalCompletedTx, canonicalSignedTx]) {
      assert.equal(
        CML.hash_transaction(tx.body()).to_hex(),
        completed.toHash(),
      );
    }
  }
  const minimumAda = CML.min_ada_required(
    inline.raw,
    PROTOCOL_PARAMETERS_DEFAULT.coinsPerUtxoByte,
  );
  assert.equal(inline.output.assets.lovelace, minimumAda);
  const submittedHash = await signed.submit({ canonical: canonicalSubmission });
  await lucid.awaitTx(submittedHash);
  const [providerOutput] = await lucid.utxosByOutRef([
    { txHash: submittedHash, outputIndex: inline.index },
  ]);
  assert(providerOutput);
  assert.equal(orderedData(providerOutput.datum), datum);
  assert.deepEqual(providerOutput.assets, inline.output.assets);
  let steps = 0n,
    memory = 0n;
  for (let i = 0; i < redeemers.len(); i++) {
    steps += redeemers.get(i).ex_units().steps();
    memory += redeemers.get(i).ex_units().mem();
  }
  report = {
    format,
    mode,
    submittedHash,
    completedHash: completed.toHash(),
    finalEvaluationMatched: true,
    evaluationCount: evaluations.length,
    providerDatumPreserved: true,
    witnessDatumPreserved: true,
    redeemerDataPreserved: true,
    minimumAda: String(minimumAda),
    outputLovelace: String(inline.output.assets.lovelace),
    executionSteps: String(steps),
    executionMemory: String(memory),
    callbackCount: callbacks.length,
    canonicalSerializersPreservedData: true,
    canonicalSubmission,
    canonicalBodyHashPreserved: canonicalSubmission ? true : null,
    metadataPreserved: true,
  };
} finally {
  Object.defineProperty(uplc, "eval_phase_two_raw", descriptor);
}
assert.equal(uplc.eval_phase_two_raw, original);
console.log(JSON.stringify({ ...report, hookRestored: true }));
