import { readFileSync } from "node:fs";

import {
  encodeMidgardCekBlobChunk,
  hashMidgardCekProgramMaterialPreimage,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishedProgramMaterialEntries } from "../src/fibers/fetch-and-insert-tx-order-utxos.js";

describe("immutable CEK material publication", () => {
  it("publishes an ordinary blob chunk and ingests its exact typed material", async () => {
    const blueprint = SDK.parseFaultProofBlueprint(
      JSON.parse(
        readFileSync(
          process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
            new URL("../../../onchain/aiken/plutus.json", import.meta.url),
          "utf8",
        ),
      ),
    );
    const validator = blueprint.validators.find(
      (entry) => entry.title === SDK.CEK_PROGRAM_MATERIAL_SPEND_TITLE,
    );
    if (validator === undefined) {
      throw new Error("Blueprint omits the immutable CEK material validator");
    }
    expect(validator.parameters).toEqual([]);
    const spendingScript: Script = {
      type: "PlutusV3",
      script: validator.compiledCode,
    };
    const spendingScriptAddress = validatorToAddress("Custom", spendingScript);
    const contracts = {
      cekProgramMaterial: {
        spendingScript,
        spendingScriptCBOR: validator.compiledCode,
        spendingScriptHash: validatorToScriptHash(spendingScript),
        spendingScriptAddress,
      },
    } as SDK.MidgardValidators;
    const account = generateEmulatorAccount({ lovelace: 20_000_000n });
    const emulator = new Emulator([account]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);

    // This ordinary fixture is also used by tx-order-material-chain.test.ts.
    const preimage = encodeMidgardCekBlobChunk(Buffer.from("material"));
    const root = hashMidgardCekProgramMaterialPreimage("blobChunk", preimage);
    const entry = { kind: "blobChunk" as const, root, preimage };
    const publication = SDK.deriveCekProgramMaterialPublications([entry])[0]!;
    const unsigned = await SDK.unsignedCekProgramMaterial(lucid, contracts, {
      entries: [entry],
    });
    const signed = await unsigned.sign.withWallet().complete();
    const txHash = await signed.submit();
    emulator.awaitBlock();

    const outputs = await lucid.utxosAt(spendingScriptAddress);
    expect(outputs).toHaveLength(1);
    const output = outputs[0]!;
    expect(output.txHash).toBe(txHash);
    expect(output.scriptRef).toBeUndefined();
    expect(output.datum).toBe(publication.datumCbor);
    expect(Data.from(output.datum!, SDK.CekProgramMaterialDatum)).toEqual({
      kind: 3n,
      root: Buffer.from(root).toString("hex"),
      preimage: preimage.toString("hex"),
    });
    expect(output.assets.lovelace).toBeGreaterThanOrEqual(
      SDK.minimumLovelaceForCekProgramMaterialPublication({
        contracts,
        publication,
        coinsPerUtxoByte: (await emulator.getProtocolParameters())
          .coinsPerUtxoByte,
      }),
    );
    expect(publishedProgramMaterialEntries(outputs)).toMatchObject({
      malformedCount: 0,
      sourceStatus: "clean",
      entries: [entry],
    });
  }, 60_000);
});
