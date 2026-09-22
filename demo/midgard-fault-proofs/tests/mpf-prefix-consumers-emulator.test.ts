import { Proof } from "@aiken-lang/merkle-patricia-forestry";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type Script,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { getCompiledScript } from "../src/runtime.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  captureEmulatorSubmission,
  EMULATOR_PROTOCOL_PARAMETERS,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

// Pinned canonical roots shared with the Aiken compressed-prefix regressions.
const vectors = [
  {
    kind: "fork",
    before: "a3cb6c7cc11c8ab91629b7fa17089818c9751c5a9a0f256b0449acc17a6fb5b7",
    after: "aae9218b732ed0dd511191290953ea78b87bf1d3a9e53fa0d2753fe678c9ba9b",
    key: "abababababababababababababababababababababababababababab001f",
    value: "01",
    steps: [
      {
        type: "branch",
        skip: 0,
        neighbors:
          "bd3871c02105e5ec24751ba8fb1a5e6d285cdcc8399993a0ca82b26f5ae179d65fed1c2681e29bbb4baed7e7d2a0f618637b48308f351ef786980f3dcdf001205318b031fecedfbfb987a7db1e2fcbb1c1d1c82dbf31e099558ddab0e96e3d38c73adabe2c5d1f9741c04a9dc2e856e29ecb6b96346905f7fb7b266b7865c857",
      },
      {
        type: "branch",
        skip: 0,
        neighbors:
          "42a0d1b9e50273451d2a93f195a000843d6a0c114672d0b5fadbf8fce300fb90b6be92c2f492c9bce1b8ecac7b530487537b2f448a80be834ad7a21df127a5468fe7d543b2434538a0c6d78fae96ed7f767ed36752ce7c1f54242d57eeaaf059cec5360efb71d228292cdf2b52f551f199b32bda593c4a4745a9b94aa17f8c9a",
      },
      {
        type: "fork",
        skip: 1,
        neighbor: {
          nibble: 8,
          prefix: "",
          root: "2f425ececd6ad91ad57238470324c4623ff0bcd1b51f6393ce0105874e651ccb",
        },
      },
    ],
  },
  {
    kind: "leaf",
    before: "88e28232999483e47f2779227d77d46358aad506223f4de36808d08fc57f11c4",
    after: "3e090fbdb93ef5cbeb10616c4f7238e36149d89d9974620230c304e36f7a517b",
    key: "abcd",
    value: "01",
    steps: [
      {
        type: "leaf",
        skip: 1,
        neighbor: {
          key: "9706e52f00c679e548b5155af5026f5af4130d7a15c990a791fff8d652c464f5",
          value:
            "1111111111111111111111111111111111111111111111111111111111111111",
        },
      },
      {
        type: "branch",
        skip: 0,
        neighbors:
          "2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
      },
    ],
  },
];
it.each(vectors)(
  "publishes and verifies canonical $kind exclusion, rejecting changed roots",
  async (vector) => {
    const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
    const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, network);
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const script: Script = {
      type: "PlutusV3",
      script: getCompiledScript(
        readBlueprint(realBlueprintPath),
        "pexcludes.exclusion.withdraw",
      ),
    };
    const reference = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: "canonical exclusion prefix",
    });
    expect(
      reference.publicationMeasurement.completeSignedBytes,
    ).toBeLessThanOrEqual(15_872);
    const reward = validatorToRewardAddress(network, script);
    const registration = await lucid.newTx().register.Stake(reward).complete();
    await lucid.awaitTx(
      await (await registration.sign.withWallet().complete()).submit(),
    );
    const proof = Proof.fromJSON(
      Buffer.from(vector.key, "hex"),
      Buffer.from(vector.value, "hex"),
      vector.steps,
    );
    expect(proof.verify(false)?.toString("hex")).toBe(vector.after);
    const proofData = Data.from(proof.toCBOR().toString("hex"));
    const build = (root: string) =>
      lucid
        .newTx()
        .readFrom([reference.utxo])
        .withdraw(reward, 0n, Data.to([root, vector.key, proofData]))
        .complete({ localUPLCEval: true });
    await expect(build(vector.before)).rejects.toThrow();
    if (vector.kind === "fork")
      await expect(
        build(
          "c8aa9e6b96398745d8c5ec972fc6a0f3baaf7cbe154b9fcd5ae642d8ffa82f36",
        ),
      ).rejects.toThrow();
    const unsigned = await build(vector.after);
    const signed = await unsigned.sign.withWallet().complete();
    const captured = await captureEmulatorSubmission(emulator, () =>
      signed.submit(),
    );
    await lucid.awaitTx(captured.result);
    expect(captured.measurements).toHaveLength(1);
    const measurement = captured.measurements[0]!;
    expect(measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
    expect(measurement.executionMemory).toBeLessThanOrEqual(16_500_000n);
    expect(measurement.executionSteps).toBeLessThanOrEqual(10_000_000_000n);
    expect(measurement.redeemerCount).toBe(1);
  },
);
