import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { NodeEd25519Verifier } from "../src/workers/utils/ed25519-verifier.js";

const enabled = process.env.WYCHEPROOF_ED25519_OPERATOR === "1";
const upstreamCommit = "fc24cd5b787d8e496bff31b0468af693a652b0f2";
const vectorSha256 =
  "70471c053c711731f2195ef4875b60ea7f5d6793939d99058ac12da810cb8e00";
const vectorUrl = `https://raw.githubusercontent.com/C2SP/wycheproof/${upstreamCommit}/testvectors_v1/ed25519_test.json`;

type WycheproofResult = "valid" | "invalid" | "acceptable";
type WycheproofVector = {
  readonly tcId: number;
  readonly comment: string;
  readonly flags: readonly string[];
  readonly msg: string;
  readonly sig: string;
  readonly result: WycheproofResult;
};
type WycheproofGroup = {
  readonly type: string;
  readonly publicKey: {
    readonly type: string;
    readonly curve: string;
    readonly pk: string;
  };
  readonly tests: readonly WycheproofVector[];
};
type WycheproofFile = {
  readonly algorithm: string;
  readonly schema: string;
  readonly numberOfTests: number;
  readonly testGroups: readonly WycheproofGroup[];
};

const nodeVerdict = (
  verifier: NodeEd25519Verifier,
  publicKeyBytes: Buffer,
  message: Buffer,
  signature: Buffer,
): boolean =>
  verifier.verify(message, {
    index: 0,
    keyHash: Buffer.alloc(28),
    vkey: publicKeyBytes,
    signature,
  });

const cmlVerdict = (
  publicKeyBytes: Buffer,
  message: Buffer,
  signatureBytes: Buffer,
): boolean => {
  if (publicKeyBytes.length !== 32 || signatureBytes.length !== 64)
    return false;
  try {
    const publicKey = CML.PublicKey.from_bytes(publicKeyBytes);
    try {
      const signature = CML.Ed25519Signature.from_raw_bytes(signatureBytes);
      try {
        return publicKey.verify(message, signature);
      } finally {
        signature.free();
      }
    } finally {
      publicKey.free();
    }
  } catch {
    return false;
  }
};

describe("pinned C2SP Wycheproof Ed25519 verifier gate", () => {
  it.skipIf(!enabled)(
    "matches expected results and CML for every applicable vector",
    async () => {
      const response = await fetch(vectorUrl, {
        signal: AbortSignal.timeout(30_000),
      });
      expect(response.ok).toBe(true);
      const bytes = Buffer.from(await response.arrayBuffer());
      expect(createHash("sha256").update(bytes).digest("hex")).toBe(
        vectorSha256,
      );
      const vectors = JSON.parse(bytes.toString("utf8")) as WycheproofFile;
      expect(vectors).toMatchObject({
        algorithm: "EDDSA",
        schema: "eddsa_verify_schema_v1.json",
        numberOfTests: 150,
      });

      const resultCounts = { valid: 0, invalid: 0, acceptable: 0 };
      const nodeVerifier = new NodeEd25519Verifier();
      const acceptableCases: Array<{
        readonly tcId: number;
        readonly flags: readonly string[];
        readonly verdict: boolean;
      }> = [];
      const divergences: Array<{
        readonly tcId: number;
        readonly expected: WycheproofResult;
        readonly node: boolean;
        readonly cml: boolean;
        readonly flags: readonly string[];
        readonly comment: string;
      }> = [];
      let applicable = 0;

      for (const group of vectors.testGroups) {
        expect(group).toMatchObject({
          type: "EddsaVerify",
          publicKey: {
            type: "EDDSAPublicKey",
            curve: "edwards25519",
          },
        });
        const publicKey = Buffer.from(group.publicKey.pk, "hex");
        for (const vector of group.tests) {
          applicable += 1;
          resultCounts[vector.result] += 1;
          const message = Buffer.from(vector.msg, "hex");
          const signature = Buffer.from(vector.sig, "hex");
          const node = nodeVerdict(nodeVerifier, publicKey, message, signature);
          const cml = cmlVerdict(publicKey, message, signature);
          if (node !== cml) {
            divergences.push({
              tcId: vector.tcId,
              expected: vector.result,
              node,
              cml,
              flags: vector.flags,
              comment: vector.comment,
            });
          }
          if (vector.result === "valid") {
            expect(node, `Node rejected valid tcId ${vector.tcId}`).toBe(true);
            expect(cml, `CML rejected valid tcId ${vector.tcId}`).toBe(true);
          } else if (vector.result === "invalid") {
            expect(node, `Node accepted invalid tcId ${vector.tcId}`).toBe(
              false,
            );
            expect(cml, `CML accepted invalid tcId ${vector.tcId}`).toBe(false);
          } else {
            acceptableCases.push({
              tcId: vector.tcId,
              flags: vector.flags,
              verdict: node,
            });
          }
        }
      }

      expect(applicable).toBe(vectors.numberOfTests);
      expect(resultCounts).toStrictEqual({
        valid: 88,
        invalid: 62,
        acceptable: 0,
      });
      expect(acceptableCases).toStrictEqual([]);
      expect(divergences).toStrictEqual([]);
      expect(nodeVerifier.stats()).toMatchObject({
        maxEntries: 4_096,
        evictions: 0,
      });
      expect(nodeVerifier.stats().size).toBeLessThanOrEqual(
        vectors.testGroups.length,
      );
      console.log(
        JSON.stringify({
          upstreamCommit,
          vectorSha256,
          applicable,
          resultCounts,
          acceptableCases,
          divergences,
        }),
      );
    },
    60_000,
  );
});
