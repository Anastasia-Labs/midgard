import {
  decodeMidgardCekProgramMaterialSidecar,
  verifyMidgardCekProgramMaterial,
} from "@al-ft/midgard-core";
import { dataFromCbor } from "@harmoniclabs/plutus-data";
import { UPLCConst, UPLCEncoder, UPLCProgram } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";
import { buildMidgardCanonicalScriptArtifact } from "../src/cek-program.js";

/**
 * A script whose constant holds a Data map with a duplicate key, built the
 * way a user's script is built and checked by the program-material verifier:
 * the builder and the verifier keep every entry and commit to the root the
 * on-chain Aiken twin computes (`offchain_duplicate_key_map_root_vector` in
 * `onchain/aiken/lib/midgard/cek-data-v1.test.ak`).
 */

/** `Map [(I 1, I 2), (I 1, I 3)]`. */
const DUPLICATE_KEY_MAP_CBOR = "a201020103";
const DUPLICATE_KEY_MAP_ROOT =
  "aef1afbc4af14bed532f136ec0e9db7d1f297752cbe935c6af79d0f49b0817e5";

describe("a CEK Data constant with a duplicate map key", () => {
  it("commits every entry to the on-chain root", () => {
    const value = dataFromCbor(DUPLICATE_KEY_MAP_CBOR);
    expect(encodeMidgardCekPlutusData(value).toString("hex")).toBe(
      DUPLICATE_KEY_MAP_CBOR,
    );
    const committed = commitMidgardCekDataTree(value);
    expect(Buffer.from(committed.root).toString("hex")).toBe(
      DUPLICATE_KEY_MAP_ROOT,
    );
    expect(committed.cborLength).toBe(5n);
    expect(committed.memory).toBe(24n);
  });

  it("builds program material the verifier accepts with the same root", () => {
    const source = Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram(
          [1, 1, 0],
          UPLCConst.data(dataFromCbor(DUPLICATE_KEY_MAP_CBOR)),
        ),
      ),
    );
    const artifact = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });
    const verified = verifyMidgardCekProgramMaterial(
      artifact.canonicalProgram.envelope,
      decodeMidgardCekProgramMaterialSidecar(
        artifact.canonicalMaterialSidecarCbor,
      ),
    );
    expect(verified.constants).toHaveLength(1);
    const [constant] = verified.constants;
    expect(Buffer.from(constant!.semanticRoot).toString("hex")).toBe(
      DUPLICATE_KEY_MAP_ROOT,
    );
    expect(Buffer.from(constant!.payloadCbor).toString("hex")).toBe(
      DUPLICATE_KEY_MAP_CBOR,
    );
  });
});
